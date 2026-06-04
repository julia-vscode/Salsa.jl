# Per-derived-function tracing support.
#
# When a Salsa `Runtime` is created with `tracing=true`, every (re)computation of a derived
# function is wrapped in a `TraceLogging.trace` span. The span is named after the derived
# function and carries the function's arguments as keyword attributes. When `tracing=false`
# (the default), `_run_user_func` skips the tracing machinery with a simple `if` check and
# calls the user function directly.
#
# Span correlation (parent/root operation ids) and timing are handled entirely by
# `TraceLogging`: each span generates a fresh operation id, records the enclosing span as its
# parent, and inherits the stable root trace id from the enclosing scope. Log messages
# emitted during a computation observe the same scope and can be correlated with the span.

# Build the span name (the derived function's name) for a derived computation.
function _derived_func_name(::DerivedKey{F}) where {F}
    return isdefined(F, :instance) ? nameof(F.instance) : nameof(F)
end

# Entry point used by storage backends in place of a direct `user_func(runtime, key.args...)`
# call. When tracing is enabled on the runtime, wrap the computation in a `TraceLogging.trace`
# span named after the derived function, attaching its arguments as keyword attributes.
# Otherwise, call the user function directly.
@inline function _run_user_func(runtime, user_func, key)
    if _tracing(runtime)
        name = string(_derived_func_name(key))
        attributes = NamedTuple{_derived_arg_names(key)}(key.args)
        return TraceLogging.trace(name; attributes...) do
            user_func(runtime, key.args...)
        end
    else
        return user_func(runtime, key.args...)
    end
end
