# We create another version of the Runtime when are evaluating a derived/input function,
# which is able to keep track of the dependencies invoked from that derived function, so
# they can be recorded together with the function's results.
#
# _TracingRuntime is an immutable struct, because it is branched for each derived function
# call with a fresh dependency trace, so that derived functions can be safely called in
# parallel on multiple threads. This makes the Runtime thread-safe, as long as it's used
# correctly.
struct _TracingRuntime{CT,ST<:AbstractSalsaStorage} <: Runtime{CT,ST}
    # NOTE: We store a Ptr{} to the parent Runtime rather than the Storage instance itself,
    # in order to keep this struct as `isbits`. This is super critical because we create a
    # new instance of this struct on _every function call_ in Salsa!  We also use this
    # pointer to allow derived functions to access their Runtime's context.
    #
    # This is guaranteed to be safe, since the parent Runtime will always exist until the
    # function call that created this tracing runtime has completed.
    tl_runtime::Ptr{_TopLevelRuntime{CT,ST}}

    # A trace structure to store the dependencies of derived functions as they are
    # encountered. This field is mutable, but we create a new, empty trace every time we
    # branch the Runtime. It's locked internally to allow spawned derived functions on
    # separate threads to record dependencies to the parent thread.
    #
    # NOTE: We store an index into `tls_trace_pool()`, rather than a `TraceOfDependencyKeys`
    # object itself here, in order to keep this struct as `isbits`. This is super critical
    # because we create a new instance of this struct on _every function call_ in Salsa!
    immediate_dependencies_id::TraceId

    # The nesting depth of derived-function calls that led to this runtime. Used to hop
    # onto a fresh task stack every STACK_SEGMENT_DEPTH levels, since deep chains of
    # derived functions would otherwise overflow the native stack (see memoized_lookup).
    depth::Int32

    function @__MODULE__().new_trace_runtime!(
        old_rt::_TopLevelRuntime{CT,ST},
        key::DependencyKey,
    )::_TracingRuntime{CT,ST} where {CT,ST<:AbstractSalsaStorage}
        new{CT,ST}(
            reinterpret(Ptr{ST}, pointer_from_objref(old_rt)),
            # Start a new, empty trace (with the provided call stack if in debug mode)
            if Salsa.Debug.debug_enabled()
                get_trace_with_call_stack(SalsaStackFrame(key, nothing))
            else
                get_trace_with_call_stack(nothing)
            end,
            Int32(1),
        )
    end

    function @__MODULE__().new_trace_runtime!(
        old_rt::_TracingRuntime{CT,ST},
        key::DependencyKey,
    )::_TracingRuntime{CT,ST} where {CT,ST<:AbstractSalsaStorage}
        # Push the new computation onto the current Runtime (if it's not there already)
        push_key!(old_rt, key)
        # Create a new linked list node (pointing to the old stack trace if debug mode).
        new_trace = if Salsa.Debug.debug_enabled()
            get_trace_with_call_stack(SalsaStackFrame(key, trace(old_rt).call_stack))
        else
            get_trace_with_call_stack(nothing)
        end
        new{CT,ST}(old_rt.tl_runtime, new_trace, old_rt.depth + Int32(1))
    end
end

# Each nested derived-function call costs several KiB of native stack across the Salsa
# machinery, so a fresh task stack (4MiB on 64-bit, 2MiB on 32-bit) comfortably fits
# STACK_SEGMENT_DEPTH levels of Salsa frames while leaving at least half the stack for
# the user functions' own frames.
const STACK_SEGMENT_DEPTH = Int32(Sys.WORD_SIZE == 64 ? 512 : 256)

# Only nested derived-function calls recurse arbitrarily deep; input lookups never
# re-enter user code (see also the fallback in runtime_generic.jl).
function _needs_fresh_stack(rt::_TracingRuntime, ::DerivedKey)
    return rt.depth % STACK_SEGMENT_DEPTH == 0
end

function push_key!(rt::_TracingRuntime, depkey)
    tr = trace(rt)

    # Test for cycles if in debug mode
    @debug_mode if stack_has_key(tr.call_stack, depkey)
        throw(DependencyCycleException(depkey))
    end

    push_key!(tr, depkey)
    return nothing
end

########## Implementation of Runtime API

context(rt::_TracingRuntime) = unsafe_load(rt.tl_runtime).context

storage(rt::_TracingRuntime) = unsafe_load(rt.tl_runtime).storage

trace(rt::_TracingRuntime) = get_trace(rt.immediate_dependencies_id)

collect_call_stack(rt::_TracingRuntime) = _collect_call_stack_frames(trace(rt).call_stack)
_collect_call_stack_frames(::Nothing) = DependencyKey[]
_collect_call_stack_frames(frame::SalsaStackFrame) = collect(frame)

collect_trace(rt::_TracingRuntime) = collect_trace(trace(rt))

function destruct_trace!(rt::_TracingRuntime)
    release_trace_id(rt.immediate_dependencies_id)
    return nothing
end

# Safety overload.
function unmemoized_input_lookup(rt::_TracingRuntime, key)
    error("Attempted an unmemoized lookup of $key inside of a derived function.")
end

function previous_output(rt::_TracingRuntime)
    dependency_key = trace(rt).call_stack.dp
    return _unwrap_salsa_value(rt, _previous_output_internal(rt, dependency_key))
end
