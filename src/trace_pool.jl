########## Salsa Trace Free List

# This section defines a thread-local storage free-list for salsa derived function
# traces. This is needed because each derived function needs its own vector to track
# dependencies, and vectors have to be heap allocated, but allocations are expensive. We can
# avoid those allocations by pre-allocating a pool of trace objects, and reusing them with a
# freelist. We have one instance of pool+freelist per OS Thread, so that no locking is
# required when taking/putting trace objects into/out of the freelist.
#
# The implementation works as follows:
# On each thread we keep a pool and a freelist:
#   - The pool stores all the TraceOfDependencyKeys objects that are allocated, and
#   - The freelist stores _indexes_ into that pool vector, representing traces that are
#     free to use.
#
# When a derived function starts, it takes an index out of the freelist, which is stored in
# the instance of the _TracingRuntime. Then, while executing user code, when any subsequent
# derived functions are called (bringing us back into the Salsa codebase), we access the
# trace via the provided index, and register the newly called derived function dependency.
#
# We use an Int index rather than simply a reference to the trace itself in order to ensure
# that `_TracingRuntime` remains isbits, since we create one instance per derived function
# call, and if it is not isbits, it will incur allocations.
#
# When the derived function call finishes, we copy the dependencies out of the trace, clear
# it, and put its index back into the freelist. If we ever attempt to take out a new index,
# but the pool is empty, the pool will be doubled with new traces. This should be fairly
# rare, since it will only happen the first time we exceed some number of derived functions
# in flight.
#
# NOTE: In the future, we intend to rewrite Salsa to use a Stack, as depth-first-search,
# rather than using function calls as we do now. Based on our early sketches, we think such
# a design will allow us to remove all of this silliness since each Salsa Task will have its
# own local variables that can be freely reused, much more simply. For now, this suffices.

const g_threadlocal_trace_pools = Vector{TraceOfDependencyKeys}[]
const g_threadlocal_trace_freelists = Vector{Int}[]
const g_threadlocal_pool_locks = Base.ReentrantLock[]

# Start off with some large number of traces in the pool. This is arbitrary, but we want it
# to be big enough where it doesn't have to get doubled very often. The traces will double
# whenever we have more active derived functions than available traces, which should only
# happen for very-long chains of derived functions or very-wide task parallelism.
const N_INIT_TRACES = 1024

# This function is called in Salsa.__init__() because we don't know the
# number of threads until runtime. (__init__() is defined at the end of this file.)
function _init_thread_local_pools_and_freelists()
    append!(
        g_threadlocal_trace_pools,
        TraceOfDependencyKeys[TraceOfDependencyKeys() for _ = 1:N_INIT_TRACES] for _ = 1:Threads.nthreads()
    )
    append!(
        g_threadlocal_trace_freelists,
        Int[i for i = 1:N_INIT_TRACES] for _ = 1:Threads.nthreads()
    )
    for _ = 1:Threads.nthreads()
        push!(g_threadlocal_pool_locks, Base.ReentrantLock())
    end
    return nothing
end

# Thread-safe accessors: provides one pool+freelist per OS thread.
tls_trace_pool() = g_threadlocal_trace_pools[Threads.threadid()]
tls_trace_freelist() = g_threadlocal_trace_freelists[Threads.threadid()]
tls_trace_pool_lock() = g_threadlocal_pool_locks[Threads.threadid()]

# This is sort of terrible, but since a _TracingRuntime can be _referenced_ from a different
# thread than the one that created it, we need both a thread id and a trace id to retrieve
# its trace from the new thread. Note that this is safe because the parent thread will never
# release its trace until all children have returned, so there can be no dangling references
# to the trace.
struct TraceId
    thread_idx::Int
    trace_idx::Int
end

# We store the call_stack in the Trace instead of in the runtime, because the trace is a
# linked-list (meaning not isbits), and we want to keep the Runtime isbits to avoid
# allocations. Since the Traces are pre-allocated it's okay for them to contain heap ptrs.
@inline function get_trace_with_call_stack(call_stack::Union{SalsaStackFrame,Nothing})
    trace_id = get_free_trace_id()
    get_trace(trace_id).call_stack = call_stack
    return trace_id
end

function get_trace(id::TraceId)
    return g_threadlocal_trace_pools[id.thread_idx][id.trace_idx]
end

function get_free_trace_id()::TraceId
    trace_freelist = tls_trace_freelist()
    trace_pool = tls_trace_pool()
    if isempty(trace_freelist)
        # Even with a thread-local pool we could potentially run into race conditions here
        # if we didn't lock around this section, when multiple tasks on the same thread
        # attempt to grow the pool but block inside this function on e.g. the `@info` call.
        @lock tls_trace_pool_lock() begin

            # Because we only acquire the lock once we've noticed the empty freelist, we can
            # have multiple tasks ending up in the critical section here, one after
            # another. Of course only the first one should actually perform the doubling. We
            # therefore have to check again whether the freelist is still empty. The
            # alternative would be to lock _before_ checking if the freelist is empty, but
            # that seems like unnecessary overhead since the empty freelist should be a very
            # rare occurrence.
            if !isempty(trace_freelist)
                return TraceId(Threads.threadid(), pop!(trace_freelist))
            end

            @debug_mode @info "DOUBLING SALSA TRACE POOL ON THREAD $(Threads.threadid())"
            old_size = length(trace_pool)
            num_new_traces = old_size # Double the pool
            @debug_mode @assert num_new_traces > 0
            # Grow the pool:
            append!(
                trace_pool,
                TraceOfDependencyKeys[TraceOfDependencyKeys() for _ = 1:num_new_traces],
            )
            for i = (old_size + 1) : (old_size + num_new_traces)
                push!(trace_freelist, i)
            end
        end
    end
    return TraceId(Threads.threadid(), pop!(trace_freelist))
end

function release_trace_id(id::TraceId)
    @assert id.thread_idx === Threads.threadid()
    # NOTE: no locking is needed here because this is all happening per-thread.
    empty_trace!(tls_trace_pool()[id.trace_idx])
    push!(tls_trace_freelist(), id.trace_idx)
    return nothing
end

function empty_trace!(trace::TraceOfDependencyKeys)
    empty!(trace.ordered_deps)
    empty!(trace.seen_deps)
end
