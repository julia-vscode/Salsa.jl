@testmodule TraceLoggingSetup begin
    import Salsa
    const TraceLogging = Salsa.TraceLogging

    # A simple recording receiver for tests.
    struct RecordingReceiver <: TraceLogging.AbstractTraceReceiver
        spans::Vector{TraceLogging.TraceSpan}
        logs::Vector{NamedTuple}
    end
    RecordingReceiver() = RecordingReceiver(TraceLogging.TraceSpan[], NamedTuple[])
    TraceLogging.receive_span(r::RecordingReceiver, span) = (push!(r.spans, span); nothing)
    TraceLogging.receive_log(r::RecordingReceiver, log::NamedTuple) = (push!(r.logs, log); nothing)
end

@testitem "TraceLogging off-path returns body and is allocation-free" setup=[TraceLoggingSetup] begin
    using .TraceLoggingSetup: TraceLogging

    # is_tracing_active is false by default
    @test TraceLogging.is_tracing_active() == false

    # @trace 3-arg form off-path returns the body value
    result = TraceLogging.@trace "noop" NamedTuple() (1 + 1)
    @test result == 2

    # @trace 2-arg form off-path returns the body value
    @test (TraceLogging.@trace "noop" (2 + 3)) == 5

    # trace() off-path returns f()
    @test TraceLogging.trace(() -> 42, "noop") == 42

    # @trace call form off-path returns the call value
    f(x) = x * 10
    @test (TraceLogging.@trace f(3)) == 30

    # Allocation check for the macro off-path. Warm up first to compile.
    g() = TraceLogging.@trace "n" NamedTuple() (1 + 1)
    g()
    allocs = @allocated g()
    @test allocs == 0
end

@testitem "TraceLogging active receiver records spans" setup=[TraceLoggingSetup] begin
    using .TraceLoggingSetup: TraceLogging, RecordingReceiver

    recv = RecordingReceiver()
    out = TraceLogging.with_tracing(recv) do
        @test TraceLogging.is_tracing_active() == true
        TraceLogging.@trace "outer" (; a = 1) begin
            TraceLogging.@trace "inner" NamedTuple() begin
                7
            end
        end
    end
    @test out == 7
    @test length(recv.spans) == 2

    inner = recv.spans[1]  # inner completes first
    outer = recv.spans[2]

    @test inner.name == "inner"
    @test outer.name == "outer"
    @test outer.attributes == Dict(:a => 1)

    # Root id is shared across the tree; inner's parent is outer; outer (the root) has no
    # parent. The root span's `trace_id` is a distinct fresh id (the OpenTelemetry trace id),
    # separate from its own `span_id`.
    @test inner.trace_id == outer.trace_id
    @test outer.parent_span_id === nothing
    @test inner.parent_span_id == outer.span_id
    @test outer.trace_id != outer.span_id

    # Ids are stored as OpenTelemetry-width integers and are non-zero.
    @test outer.span_id isa UInt64
    @test outer.trace_id isa UInt128
    @test outer.span_id != 0
    @test outer.trace_id != 0

    # `inner` has no attributes, which is stored as `nothing` (no Dict allocation).
    @test inner.attributes === nothing
end

@testitem "TraceLogging function form with lazy attributes thunk" setup=[TraceLoggingSetup] begin
    using .TraceLoggingSetup: TraceLogging, RecordingReceiver

    recv = RecordingReceiver()
    TraceLogging.with_tracing(recv) do
        TraceLogging.trace(() -> 1, "op", () -> (; x = 99))
    end
    @test length(recv.spans) == 1
    @test recv.spans[1].attributes == Dict(:x => 99)

    # Thunk must NOT be called when tracing is off.
    called = Ref(false)
    TraceLogging.trace(() -> 1, "op", () -> (called[] = true; (; x = 1)))
    @test called[] == false
end

@testitem "TraceLogging @trace builds string attributes from args" setup=[TraceLoggingSetup] begin
    using .TraceLoggingSetup: TraceLogging, RecordingReceiver

    recv = RecordingReceiver()
    h(a, b) = a + b
    x = 2
    y = 5
    TraceLogging.with_tracing(recv) do
        TraceLogging.@trace h(x, y)
    end
    @test length(recv.spans) == 1
    sp = recv.spans[1]
    @test sp.name == "h"
    @test sp.attributes == Dict(:x => "2", :y => "5")
end

@testitem "TraceLogging TraceContextLogger forwards and enriches" setup=[TraceLoggingSetup] begin
    using .TraceLoggingSetup: TraceLogging, RecordingReceiver
    using Logging

    recv = RecordingReceiver()
    inner = Logging.ConsoleLogger(IOBuffer(), Logging.Debug)

    logger = TraceLogging.TraceContextLogger(inner)

    # When no receiver active: receive_log NOT called.
    Logging.with_logger(logger) do
        @info "no receiver"
    end
    @test isempty(recv.logs)

    # When receiver active: receive_log called with correlation ids.
    TraceLogging.with_tracing(recv) do
        TraceLogging.@trace "scope" NamedTuple() begin
            Logging.with_logger(logger) do
                @info "in scope" foo = 1
            end
        end
    end
    @test length(recv.logs) == 1
    lg = recv.logs[1]
    @test lg.message == "in scope"
    @test lg.trace_id !== nothing
    @test lg.span_id !== nothing
    @test lg.trace_id isa UInt128
    @test lg.span_id isa UInt64
end

@testitem "TraceLogging id formatters produce OTel-canonical hex" setup=[TraceLoggingSetup] begin
    using .TraceLoggingSetup: TraceLogging, RecordingReceiver

    # Span ids format to 16 lowercase hex chars; trace ids to 32. Zero-padded, no dashes.
    @test TraceLogging.format_span_id(UInt64(0x1)) == "0000000000000001"
    @test TraceLogging.format_span_id(typemax(UInt64)) == "ffffffffffffffff"
    @test TraceLogging.format_trace_id(UInt128(0x1)) == "00000000000000000000000000000001"
    @test TraceLogging.format_trace_id(typemax(UInt128)) == "ffffffffffffffffffffffffffffffff"

    # Formatted ids captured from a live trace scope have the right shape.
    recv = RecordingReceiver()
    TraceLogging.with_tracing(recv) do
        TraceLogging.@trace "op" NamedTuple() begin
            sid = TraceLogging.format_span_id(TraceLogging.current_span_id())
            tid = TraceLogging.format_trace_id(TraceLogging.current_trace_id())
            @test length(sid) == 16
            @test length(tid) == 32
            @test occursin(r"^[0-9a-f]{16}$", sid)
            @test occursin(r"^[0-9a-f]{32}$", tid)
            @test !occursin('-', sid)
            @test !occursin('-', tid)
        end
    end
end

