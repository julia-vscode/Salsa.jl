# Per-derived-function tracing support.
#
# Every (re)computation of a derived function is wrapped in a `TraceLogging.@trace_span`. The
# span is named after the derived function and carries the function's arguments as
# attributes. Whether anything is actually recorded is decided dynamically by `TraceLogging`:
# when no trace receiver is active (the default), `@trace_span` expands to just the user-func
# call and neither the span name nor the attributes are computed, so the only overhead is a
# scoped-value read and a branch.
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
# call. Wraps the computation in a `TraceLogging.@trace_span` named after the derived function,
# attaching its arguments as attributes. The span is only materialized when a trace receiver is
# active; otherwise this is just the user-func call.
@inline function _run_user_func(runtime, user_func, key)
    return TraceLogging.@trace_span(
        string(_derived_func_name(key)),
        NamedTuple{_derived_arg_names(key)}(key.args),
        user_func(runtime, key.args...)
    )
end
