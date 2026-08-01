########## Salsa Trace Pool

# This section defines a pool of salsa derived function traces. This is needed because
# each derived function needs its own containers to track dependencies, and those have to
# be heap allocated, but allocations are expensive (~10 allocations / >1KiB per fresh
# trace, dominated by the Set, Vector and ReentrantLock). We avoid that by recycling
# released traces through a free-list and reusing them for later derived function calls.
#
# The free-list is implemented as a set of *Treiber stacks*: lock-free stacks where the
# `top` pointer is an atomic field, and push/pop are each a single compare-and-swap (CAS).
# Any task running on any thread may acquire and release traces, so the pool is safe under
# task migration (unlike the previous design, which indexed per-thread pools by
# `Threads.threadid()` and corrupted its free-lists when a task migrated between acquire
# and release, or even mid-acquire).
#
# ABA safety: the classic hazard of a Treiber stack is a stale `next` pointer being
# CAS'd back in after the node it was read from has been popped, *recycled*, and pushed
# again. We are immune because nodes are never recycled: `release_trace` wraps the trace
# in a freshly allocated node (32 bytes), and the GC guarantees a node cannot be
# reallocated while another task still holds a reference to it.
#
# Striping: all threads CASing a single `top` pointer would contend, so we keep one
# independent stack ("stripe") per thread id, sized in `__init__` once the runtime thread
# count is known. This is purely load-spreading; any thread may pop from a stripe that a
# different thread pushed to, so it is harmless that `Threads.threadid()` can change
# across yield points, and that threads adopted after startup (whose ids exceed the
# init-time count) wrap onto shared stripes via the index mask.
#
# The pool starts empty and grows organically: if a task finds its stripe empty, it simply
# allocates a fresh trace, which enters the pool when released. Steady-state pool size is
# therefore the high-water mark of concurrently-live traces (rather than a fixed
# per-thread pre-allocation).
#
# NOTE: In the future, we intend to rewrite Salsa to use a Stack, as depth-first-search,
# rather than using function calls as we do now. Based on our early sketches, we think such
# a design will allow us to remove all of this silliness since each Salsa Task will have its
# own local variables that can be freely reused, much more simply. For now, this suffices.

mutable struct TracePoolNode
    const trace::TraceOfDependencyKeys
    @atomic next::Union{TracePoolNode,Nothing}
    TracePoolNode(trace::TraceOfDependencyKeys) = new(trace, nothing)
end

mutable struct TraceStack
    @atomic top::Union{TracePoolNode,Nothing}
    TraceStack() = new(nothing)
end

# Placeholder size for precompile time; `__init__` resizes to cover every thread id.
# Never empty: an empty array would make the mask below -1 (i.e. `@inbounds` UB) for
# anything touching the pool before `__init__`, such as a future precompile workload.
# The length is always a power of two (so the modulus below is a bit-mask) and never
# changes after `__init__`, which makes the unsynchronized `length` read below safe.
const g_trace_pool_stripes = [TraceStack()]

@inline function _trace_pool_stripe()::TraceStack
    idx = Threads.threadid() & (length(g_trace_pool_stripes) - 1) + 1
    return @inbounds g_trace_pool_stripes[idx]
end

# We store the call_stack in the Trace instead of in the runtime because the trace is
# already mutable, while the runtime is immutable.
@inline function get_trace_with_call_stack(call_stack::Union{SalsaStackFrame,Nothing})
    trace = _acquire_trace!()
    trace.call_stack = call_stack
    return trace
end

function _acquire_trace!()::TraceOfDependencyKeys
    stack = _trace_pool_stripe()
    while true
        node = @atomic :acquire stack.top
        # Empty stripe: grow the pool by allocating a fresh trace. (It joins the pool
        # when released.)
        node === nothing && return TraceOfDependencyKeys()
        next = @atomic :monotonic node.next
        _, ok = @atomicreplace :acquire_release :monotonic stack.top node => next
        ok && return node.trace
    end
end

function release_trace(trace::TraceOfDependencyKeys)
    empty_trace!(trace)
    # A fresh node per push is what makes the stack ABA-safe; see the note above.
    node = TracePoolNode(trace)
    stack = _trace_pool_stripe()
    while true
        old = @atomic :monotonic stack.top
        @atomic :monotonic node.next = old
        _, ok = @atomicreplace :acquire_release :monotonic stack.top old => node
        ok && return nothing
    end
end

# Internal: snapshot of all currently-pooled traces (for tests/debugging only).
function _pooled_traces()::Vector{TraceOfDependencyKeys}
    traces = TraceOfDependencyKeys[]
    for stack in g_trace_pool_stripes
        node = @atomic stack.top
        while node !== nothing
            push!(traces, node.trace)
            node = @atomic node.next
        end
    end
    return traces
end

# Traces that recorded more than this many dependencies get fresh containers on
# release instead of being cleared in place. Pooled containers keep their
# high-water-mark capacity forever, and the replacement matters for different
# reasons per container:
#   - `seen_deps` (Set): `empty!` on a Dict/Set costs O(capacity) even when it
#     holds no elements (every slot is swept unconditionally), so an oversized
#     Set would tax every later release that reuses this pooled trace.
#   - `ordered_deps` (Vector): `empty!` on a Vector is O(length), never
#     O(capacity), so clearing stays cheap — replacing it only releases the
#     retained high-water-mark memory.
# Fresh containers are also cheaper than shrinking in place
# (`sizehint!(c, n; shrink=true)` pays O(old capacity) on the release path to
# rehash/copy the old table).
const TRACE_CONTAINER_SHRINK_THRESHOLD = 256

function empty_trace!(trace::TraceOfDependencyKeys)
    # Locked to guard against a straggler task racing `push_key!` against this
    # release. Spawning tasks that outlive their derived function is documented
    # as unsupported (see `collect_trace`) but cannot be detected; without the
    # lock, such a task could observe one old and one new container around the
    # replacement below, leaving `seen_deps` and `ordered_deps` inconsistent for
    # the trace's next user. Uncontended in correct usage, so this is cheap.
    @lock trace.lock begin
        # Don't retain the call-stack linked list while parked in the pool.
        trace.call_stack = nothing
        # The dependency-array swap optimization in `_memoized_lookup_internal`
        # can leave `ordered_deps` empty while `seen_deps` still holds every
        # recorded key, so both containers must be considered.
        n = max(length(trace.ordered_deps), length(trace.seen_deps))
        if n == 0
            # Nothing to clear. This is the hot path: verification-only lookups
            # (`should_trace` off) release their trace untouched, and must not
            # pay the O(capacity) Set sweep for whatever used this trace before.
        elseif n > TRACE_CONTAINER_SHRINK_THRESHOLD
            # Reset to the threshold capacity, not TRACE_INITIAL_CAPACITY: the
            # in-place branch below already tolerates containers of this size,
            # and a derived function that is consistently wide would otherwise
            # pay Vector reallocs and Set rehashes on every run to regrow from
            # a tiny container (rehashing is especially costly here because
            # `hash(::DependencyKey)` allocates).
            trace.ordered_deps =
                sizehint!(Vector{DependencyKey}(), TRACE_CONTAINER_SHRINK_THRESHOLD)
            trace.seen_deps =
                sizehint!(Set{DependencyKey}(), TRACE_CONTAINER_SHRINK_THRESHOLD)
        else
            empty!(trace.ordered_deps)
            empty!(trace.seen_deps)
        end
    end
    return
end
