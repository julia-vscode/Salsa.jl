# We create one trace per derived function, which tracks all the salsa computations called
# by the current derived function, so that they can be recorded as input dependencies. We
# create one instance per derived function to limit contention in multithreaded code, and
# protect insertions with a lock to allow concurrency within the body of the function.
#
# TODO: For best parallel performance, we might consider using Thread-local storage (not to
# be confused with Task-local storage) to collect dependencies, so that we can remove the
# lock entirely: By storing one vector per OS-thread, and aggregating at the end. The
# potential downside is losing some ordering information if part of the function is serial
# and part is parallel.
#
# Initial capacity of a trace's dependency containers. (Oversized traces are
# reset to `TRACE_CONTAINER_SHRINK_THRESHOLD` capacity on release instead; see
# `empty_trace!`.)
const TRACE_INITIAL_CAPACITY = 30

# NOTE: This is a mutable struct to allow us to modify the call_stack. This is fine because
# it's not going to be isbits anyway due to all of the Vectors, Sets, and Locks, it
# contains, and we are going to keep these in the trace pool (see explanation below).
mutable struct TraceOfDependencyKeys
    # Performance Optimization: De-duplicating Derived Function Traces:
    #   We want to maintain the order in which the dependencies were encountered (to
    #   preserve correctness when checking dependencies for invalidation), but we also
    #   don't want to track exact duplicate dependencies, so we use the seen_deps to filter.
    # TODO: We may want to consider using a linked list instead of a vector to prevent
    # array growth events from causing contention when spawning parallel derived functions.
    ordered_deps::Vector{DependencyKey}
    seen_deps::Set{DependencyKey}

    # We lock around modifications to the list of dependencies, since a derived function may
    # spawn multiple threads, which may themselves call derived functions. This doesn't add
    # too much contention, because it's only blocking around the spawning of the immediate
    # children of node.
    lock::Base.ReentrantLock

    # The trace of Salsa functions that represents the current computation in progress.
    # Since derived functions may spawn threads, this represents a pure stack trace back to
    # the root call into Salsa, which potentially traverses across threads.  It's an
    # immutable structure (linked list) to make it thread-safe.
    call_stack::Union{Nothing,SalsaStackFrame}

    # This is set to false during recursive "still_valid" checks to avoid unused dependency
    # tracking, to save time and allocs.
    should_trace::Bool

    # We always create a new, empty TraceOfDependencyKeys for each derived function, since
    # we're only tracing the immediate dependencies of that function.
    function TraceOfDependencyKeys()
        # Pre-allocate the containers to be non-empty, to minimize allocations at runtime.
        # Traces are recycled through the trace pool, so this cost is only paid when the
        # pool grows.
        return new(
            sizehint!(Vector{DependencyKey}(), TRACE_INITIAL_CAPACITY),
            sizehint!(Set{DependencyKey}(), TRACE_INITIAL_CAPACITY),
            Base.ReentrantLock(),
            nothing,
            true,
        )
    end
end

function push_key!(trace::TraceOfDependencyKeys, depkey)
    # (Don't need to lock around this since it can never be modified in parallel.)
    if !trace.should_trace
        return
    end
    @lock trace.lock begin
        # Performance Optimization: De-duplicating Derived Function Traces
        if depkey ∉ trace.seen_deps
            push!(trace.ordered_deps, depkey)
            push!(trace.seen_deps, depkey)
        end
    end
    return nothing
end

function collect_trace(trace::TraceOfDependencyKeys)
    # NOTE: We do not need to lock around this access, because the user's function is
    # guaranteed to have finished by this point, and it is _not supported_ to register
    # more derived functions on the current trace after that. We should have exclusive
    # access to the trace at this point, and it's about to be handed back to the trace pool!

    # Derived functions that spawn tasks currently *must* wait until all tasks have
    # completed before returning. Calling new derived functions from spawned tasks after
    # returning is an error! (But unfortunately we have no way to detect/prevent it.)

    return copy(trace.ordered_deps)
end
