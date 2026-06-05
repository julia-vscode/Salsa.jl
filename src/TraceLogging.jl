module TraceLogging

using Logging: Logging

export trace, @trace, TraceSpan, AbstractTraceReceiver,
    receive_span, receive_log, is_tracing_active, with_tracing, TraceContextLogger,
    current_span_id, current_trace_id, format_span_id, format_trace_id

# The receiver for the dynamically-enclosing trace scope. Tracing is *off* whenever this is
# `nothing` (the default), which is the case unless some caller has established a scope with
# [`with_tracing`](@ref). The `trace`/`@trace` entry points read this value and
# short-circuit immediately when it is `nothing`, so the cost of leaving tracing calls in
# place when nothing is listening is a single scoped-value read plus a branch.
const TRACE_RECEIVER = Base.ScopedValues.ScopedValue{Union{Nothing,Any}}(nothing)

# Trace/span ids are stored as raw integers matching the OpenTelemetry wire widths: span ids
# are 64-bit and trace ids are 128-bit. We keep them as integers (rather than formatted hex
# strings) to avoid a string allocation per span on the hot path; hex formatting is deferred
# to the serialization boundary via [`format_span_id`](@ref) / [`format_trace_id`](@ref).
const CURRENT_SPAN_ID = Base.ScopedValues.ScopedValue{Union{Nothing,UInt64}}(nothing)
const TRACE_ID = Base.ScopedValues.ScopedValue{Union{Nothing,UInt128}}(nothing)

# Generate a fresh, non-zero OpenTelemetry-compatible span id (64 bits). `rand` uses the
# task-local RNG, so this is safe to call concurrently from multiple tasks/threads. The
# OpenTelemetry spec forbids an all-zero id; the guard's retry probability is ~2^-64.
@inline function _new_span_id()
    id = rand(UInt64)
    while id == 0
        id = rand(UInt64)
    end
    return id
end

# Generate a fresh, non-zero OpenTelemetry-compatible trace id (128 bits). See
# [`_new_span_id`](@ref) for the task-local RNG and non-zero guarantees.
@inline function _new_trace_id()
    id = rand(UInt128)
    while id == 0
        id = rand(UInt128)
    end
    return id
end

"""
    format_span_id(id::UInt64) -> String

Format a span id as the OpenTelemetry-canonical 16-character, zero-padded, lowercase hex
string (no dashes).
"""
format_span_id(id::UInt64) = string(id, base = 16, pad = 16)

"""
    format_trace_id(id::UInt128) -> String

Format a trace id as the OpenTelemetry-canonical 32-character, zero-padded, lowercase hex
string (no dashes).
"""
format_trace_id(id::UInt128) = string(id, base = 16, pad = 32)

"""
    current_span_id() -> Union{Nothing,UInt64}

Return the span id of the currently-executing trace span, or `nothing` if not currently
inside a [`trace`](@ref) scope. The id is a raw 64-bit integer; use [`format_span_id`](@ref)
to render it as OpenTelemetry-canonical hex.
"""
current_span_id() = CURRENT_SPAN_ID[]

"""
    current_trace_id() -> Union{Nothing,UInt128}

Return the root trace id of the current trace tree, or `nothing` if not currently inside a
[`trace`](@ref) scope. Stable across the whole tree; intended as the shared OpenTelemetry
trace id. The id is a raw 128-bit integer; use [`format_trace_id`](@ref) to render it as
OpenTelemetry-canonical hex.
"""
current_trace_id() = TRACE_ID[]

"""
    AbstractTraceReceiver

Supertype for trace receivers. A receiver is the sink that completed spans and (optionally)
correlated log records are delivered to. Establish one for a dynamic scope with
[`with_tracing`](@ref); while a receiver is active, [`trace`](@ref) and friends record spans
and hand them to the receiver via [`receive_span`](@ref).

Concrete subtypes must implement [`receive_span`](@ref). They may also implement
[`receive_log`](@ref) to handle correlated log records emitted through a
[`TraceContextLogger`](@ref) (the default is a no-op).
"""
abstract type AbstractTraceReceiver end

"""
    receive_span(receiver::AbstractTraceReceiver, span::TraceSpan)

Deliver a completed `span` to `receiver`. Called synchronously, on the task that completed the
span, immediately after it finishes. Concrete receivers must implement this method.
"""
function receive_span end

"""
    receive_log(receiver::AbstractTraceReceiver, log::NamedTuple)

Deliver a correlated log record to `receiver`. Called by [`TraceContextLogger`](@ref) for log
messages emitted while a receiver is active. The default implementation is a no-op, so
receivers that only care about spans need not implement it.

The `log` named tuple has the fields `level`, `message`, `trace_id`, `span_id`, `time_ns`,
`_module`, `group`, `id`, `file`, `line` and `kwargs`. `time_ns` is the raw monotonic
`time_ns()` value captured when the record was handled; `trace_id` is the enclosing trace's
root id (a `UInt128`, or `nothing`) and `span_id` the enclosing span id (a `UInt64`, or
`nothing`).
"""
receive_log(::AbstractTraceReceiver, ::NamedTuple) = nothing

"""
    is_tracing_active() -> Bool

Return `true` when a trace receiver is established for the current dynamic scope (i.e. some
enclosing caller used [`with_tracing`](@ref)). Useful as a guard to skip building span
metadata when nothing is listening.
"""
is_tracing_active() = TRACE_RECEIVER[] !== nothing

"""
    with_tracing(f, receiver::AbstractTraceReceiver)

Run `f()` with `receiver` established as the active trace receiver for the dynamic extent of
the call. Spans recorded by [`trace`](@ref) and [`@trace`](@ref) inside
`f` (and on tasks spawned from within it) are delivered to `receiver`.
"""
with_tracing(f, receiver::AbstractTraceReceiver) =
    Base.ScopedValues.with(f, TRACE_RECEIVER => receiver)

# A completed trace span. `start_time_ns` is the raw monotonic `time_ns()` value captured
# when the span started; converting it to a wall-clock time is the responsibility of the
# consumer (e.g. the language server instance), which owns a `(time(), time_ns())` reference
# pair. Keeping the raw nanosecond value here avoids any precision loss in this layer.
#
# `span_id`/`parent_span_id`/`trace_id` are raw integers matching the OpenTelemetry wire
# widths (64-bit span ids, 128-bit trace ids); use [`format_span_id`](@ref) /
# [`format_trace_id`](@ref) to render them as canonical hex. `attributes` is `nothing` when
# the span carries no attributes (the common case, avoiding a `Dict` allocation) and a
# `Dict{Symbol,Any}` otherwise. The struct is deliberately non-parametric so that every span
# has the same concrete type, keeping `Vector{TraceSpan}` and `receive_span` free of
# per-span dynamic dispatch.
struct TraceSpan
    name::String
    span_id::UInt64
    parent_span_id::Union{Nothing,UInt64}
    trace_id::UInt128
    start_time_ns::UInt64
    duration_ns::UInt64
    attributes::Union{Nothing,Dict{Symbol,Any}}
end

# Normalize span attributes to the stored representation: `nothing` for the empty case (no
# allocation), otherwise a `Dict{Symbol,Any}`. The `NamedTuple` input keeps the ergonomic
# `(; x=1)` call syntax at the trace sites while the stored type stays uniform.
_to_attrs(::Nothing) = nothing
_to_attrs(nt::NamedTuple) = isempty(nt) ? nothing : Dict{Symbol,Any}(pairs(nt))

# Slow path for `trace`: a receiver is known to be active. Kept out-of-line so the common
# (no-receiver) path through `trace` stays tiny and allocation-free.
@noinline function _trace_active(f, receiver, name, attributes)
    span_id = _new_span_id()
    root_id = TRACE_ID[]
    if root_id === nothing
        root_id = _new_trace_id()
    end
    parent_id = CURRENT_SPAN_ID[]

    v, start_time_ns, duration = Base.ScopedValues.with(TRACE_ID => root_id, CURRENT_SPAN_ID => span_id) do
        t0 = time_ns()
        ret = f()
        duration = time_ns() - t0

        return ret, t0, duration
    end

    receive_span(receiver, TraceSpan(name, span_id, parent_id, root_id, start_time_ns, duration, _to_attrs(attributes)))

    return v
end

"""
    trace(f, name, attributes_thunk=nothing)

Run `f()` as a trace span named `name`. When a trace receiver is active (see
[`with_tracing`](@ref)), the span's timing is recorded and a [`TraceSpan`](@ref) is delivered
to the receiver when `f` completes; otherwise `f()` is called directly and no span is built.

`attributes_thunk`, if given, is a zero-argument callable returning a `NamedTuple` of span
attributes. It is only invoked when a receiver is active, so building the attributes costs
nothing when tracing is off. Pass `nothing` (the default) for no attributes.

The first span established seeds a root trace id; nested `trace` calls inherit it and record
the enclosing span as their parent.

For hot paths, prefer [`@trace`](@ref), which additionally avoids constructing the `f`
closure when tracing is off.
"""
@inline function trace(f, name, attributes_thunk=nothing)
    receiver = TRACE_RECEIVER[]
    receiver === nothing && return f()
    attributes = attributes_thunk === nothing ? NamedTuple() : attributes_thunk()
    return _trace_active(f, receiver, name, attributes)
end

"""
    @trace call(args...)
    @trace name body
    @trace name attributes body

Instrument an expression as a trace span. What the macro does depends on how many arguments it
is given:

- **One argument — a function call** `@trace foo(a, b, c)`: the span is named after the called
  function and the call arguments are recorded as stringified attributes. The call is
  instrumented inline (no closure) and each argument is evaluated exactly once.
- **Two arguments** `@trace name body`: `body` is run as a span named `name`, with no
  attributes.
- **Three arguments** `@trace name attributes body`: `body` is run as a span named `name` with
  the given `attributes` (an expression evaluating to a `NamedTuple`).

In every form, when no trace receiver is active (see [`with_tracing`](@ref)) the macro expands
to just the underlying expression: the name/attributes are not evaluated and no closure is
constructed, so the only overhead is a scoped-value read and a branch. When a receiver is
active, the completed [`TraceSpan`](@ref) is delivered to it.

```julia
@trace foo(a, b, c)

@trace "my_op" begin
    do_work()
end

@trace "my_op" (; x=1, y=2) begin
    do_work()
end
```

See also [`trace`](@ref) for a function form usable with `do` blocks.
"""
macro trace(args...)
    if length(args) == 1
        return _trace_call_expr(args[1])
    elseif length(args) == 2
        return _trace_span_expr(args[1], :(NamedTuple()), args[2])
    elseif length(args) == 3
        return _trace_span_expr(args[1], args[2], args[3])
    else
        error("`@trace` expects 1, 2 or 3 arguments; see the `@trace` docstring for the accepted forms")
    end
end

# Build the instrumented expansion for an explicit `(name, attributes, body)` span. When no
# receiver is active, this is just `body` (neither `name` nor `attributes` is evaluated and no
# closure is constructed). Used by the two- and three-argument forms of `@trace`.
function _trace_span_expr(name, attributes, body)
    return quote
        local receiver = $(TRACE_RECEIVER)[]
        if receiver === nothing
            $(esc(body))
        else
            local span_id = $(_new_span_id)()
            local root_id = $(TRACE_ID)[]
            root_id === nothing && (root_id = $(_new_trace_id)())
            local parent_id = $(CURRENT_SPAN_ID)[]
            local span_name = $(esc(name))
            local span_attrs = $(_to_attrs)($(esc(attributes)))
            local t0 = time_ns()
            local result = Base.ScopedValues.@with $(TRACE_ID) => root_id $(CURRENT_SPAN_ID) => span_id $(esc(body))
            local duration = time_ns() - t0
            $(receive_span)(receiver, $(TraceSpan)(span_name, span_id, parent_id, root_id, t0, duration, span_attrs))
            result
        end
    end
end

# Build the instrumented expansion for a call expression `foo(a, b, …)`: the span is named
# after the called function and the call arguments are recorded as stringified attributes.
# Each argument is evaluated exactly once in both paths. Used by the single-argument form of
# `@trace`.
function _trace_call_expr(ex)
    Meta.isexpr(ex, :call) ||
        error("the single-argument form of `@trace` expects a function-call expression, e.g. `@trace foo(a, b, c)`")

    fname = ex.args[1]
    name = string(fname)

    params = nothing
    positional = Any[]
    for a in ex.args[2:end]
        if Meta.isexpr(a, :parameters)
            params = a
        else
            push!(positional, a)
        end
    end

    assignments = Expr[]
    attr_pairs = Expr[]
    call_args = Any[]            # arg expressions for the on-path call (gensym temporaries)
    raw_call_args = Any[]        # arg expressions for the off-path call (evaluated inline once)
    for a in positional
        if Meta.isexpr(a, :...)
            # Splatted arguments are inlined directly (still evaluated once) and are not
            # turned into attributes.
            push!(call_args, esc(a))
            push!(raw_call_args, esc(a))
        else
            tmp = gensym(:arg)
            push!(assignments, :(local $tmp = $(esc(a))))
            push!(attr_pairs, Expr(:(=), Symbol(string(a)), :(string($tmp))))
            push!(call_args, tmp)
            push!(raw_call_args, esc(a))
        end
    end

    # On-path call: uses the gensym temporaries (each arg evaluated once, then reused for both
    # the call and its stringified attribute).
    callexpr = Expr(:call, esc(fname))
    params === nothing || push!(callexpr.args, esc(params))
    append!(callexpr.args, call_args)

    # Off-path call: inlines the argument expressions directly, so each is still evaluated
    # exactly once and no temporaries (which the off-path would otherwise leave undefined) are
    # referenced.
    raw_callexpr = Expr(:call, esc(fname))
    params === nothing || push!(raw_callexpr.args, esc(params))
    append!(raw_callexpr.args, raw_call_args)

    attrs = isempty(attr_pairs) ? :(NamedTuple()) : Expr(:tuple, attr_pairs...)

    return quote
        local receiver = $(TRACE_RECEIVER)[]
        if receiver === nothing
            $raw_callexpr
        else
            $(assignments...)
            local span_id = $(_new_span_id)()
            local root_id = $(TRACE_ID)[]
            root_id === nothing && (root_id = $(_new_trace_id)())
            local parent_id = $(CURRENT_SPAN_ID)[]
            local t0 = time_ns()
            local result = Base.ScopedValues.@with $(TRACE_ID) => root_id $(CURRENT_SPAN_ID) => span_id $callexpr
            local duration = time_ns() - t0
            $(receive_span)(receiver, $(TraceSpan)(
                $name,
                span_id,
                parent_id,
                root_id,
                t0,
                duration,
                $(_to_attrs)($attrs),
            ))
            result
        end
    end
end

"""
    TraceContextLogger(inner::AbstractLogger)

An `AbstractLogger` that enriches log records with the current trace/span correlation and
delivers them to the active trace receiver, while forwarding every record on to `inner`.

When a trace receiver is active (see [`with_tracing`](@ref)), each handled record is packaged
into a named tuple (see [`receive_log`](@ref)) carrying the enclosing `trace_id`/`span_id` and
passed to the receiver, and is also forwarded to `inner` with `trace_id`/`span_id` added as
keyword arguments. When no receiver is active, records are forwarded to `inner` untouched.

Both the span `start_time_ns` (on [`TraceSpan`](@ref)) and the log `time_ns` are raw monotonic
timestamps; converting them to wall-clock times is left to the consumer, which can own a
`(time(), time_ns())` reference pair and pick whatever high-resolution representation it needs
downstream.

This composes with the loggers in `LoggingExtras` (`TeeLogger`, `TransformerLogger`, …): wrap
whatever logger you want enriched records to end up in.
"""
struct TraceContextLogger{L<:Logging.AbstractLogger} <: Logging.AbstractLogger
    inner::L
end

Logging.min_enabled_level(logger::TraceContextLogger) = Logging.min_enabled_level(logger.inner)

function Logging.shouldlog(logger::TraceContextLogger, level, _module, group, id)
    # When a receiver is active we want to capture everything for correlation; otherwise defer
    # entirely to the inner logger's decision.
    TRACE_RECEIVER[] === nothing || return true
    return Logging.shouldlog(logger.inner, level, _module, group, id)
end

Logging.catch_exceptions(logger::TraceContextLogger) = Logging.catch_exceptions(logger.inner)

function Logging.handle_message(logger::TraceContextLogger, level, message, _module, group, id, file, line; kwargs...)
    receiver = TRACE_RECEIVER[]
    if receiver === nothing
        # Nothing is listening for correlation; forward unchanged.
        Logging.handle_message(logger.inner, level, message, _module, group, id, file, line; kwargs...)
        return nothing
    end

    trace_id = TRACE_ID[]
    span_id = CURRENT_SPAN_ID[]

    receive_log(receiver, (
        level = level,
        message = message,
        trace_id = trace_id,
        span_id = span_id,
        time_ns = time_ns(),
        _module = _module,
        group = group,
        id = id,
        file = file,
        line = line,
        kwargs = kwargs,
    ))

    # Forward to the inner logger as well, enriching with correlation ids so downstream
    # handlers can pick them up.
    Logging.handle_message(logger.inner, level, message, _module, group, id, file, line; trace_id=trace_id, span_id=span_id, kwargs...)

    return nothing
end

end # module TraceLogging
