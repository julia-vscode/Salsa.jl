# We create another version of the Runtime when are evaluating a derived/input function,
# which is able to keep track of the dependencies invoked from that derived function, so
# they can be recorded together with the function's results.
#
# _TracingRuntime is an immutable struct, because it is branched for each derived function
# call with a fresh dependency trace, so that derived functions can be safely called in
# parallel on multiple threads. This makes the Runtime thread-safe, as long as it's used
# correctly.
struct _TracingRuntime{CT,ST<:AbstractSalsaStorage} <: Runtime{CT,ST}
    # The parent Runtime, through which derived functions access their Runtime's context
    # and storage.
    #
    # NOTE: This struct holds heap references but is itself immutable, so creating one per
    # derived function call does not allocate (immutable structs with references are
    # stack-allocated since Julia 1.5). The isbits/Ptr tricks previously used here are no
    # longer needed.
    tl_runtime::_TopLevelRuntime{CT,ST}

    # A trace structure to store the dependencies of derived functions as they are
    # encountered. The trace is mutable, but we take a fresh, empty one from the trace pool
    # every time we branch the Runtime. It's locked internally to allow spawned derived
    # functions on separate threads to record dependencies to the parent task's trace.
    immediate_dependencies::TraceOfDependencyKeys

    function @__MODULE__().new_trace_runtime!(
        old_rt::_TopLevelRuntime{CT,ST},
        key::DependencyKey,
    )::_TracingRuntime{CT,ST} where {CT,ST<:AbstractSalsaStorage}
        new{CT,ST}(
            old_rt,
            # Start a new, empty trace (with the provided call stack if in debug mode)
            if Salsa.Debug.debug_enabled()
                get_trace_with_call_stack(SalsaStackFrame(key, nothing))
            else
                get_trace_with_call_stack(nothing)
            end,
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
        new{CT,ST}(old_rt.tl_runtime, new_trace)
    end
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

context(rt::_TracingRuntime) = rt.tl_runtime.context

storage(rt::_TracingRuntime) = rt.tl_runtime.storage

trace(rt::_TracingRuntime) = rt.immediate_dependencies

collect_call_stack(rt::_TracingRuntime) = _collect_call_stack_frames(trace(rt).call_stack)
_collect_call_stack_frames(::Nothing) = DependencyKey[]
_collect_call_stack_frames(frame::SalsaStackFrame) = collect(frame)

collect_trace(rt::_TracingRuntime) = collect_trace(trace(rt))

function destruct_trace!(rt::_TracingRuntime)
    release_trace(rt.immediate_dependencies)
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
