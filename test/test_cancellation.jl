@testmodule CancellationSetup begin
    import Salsa
    using Salsa: Runtime

    new_test_rt() = Runtime()

    # Wait (cooperatively) until `f()` is true, failing the test on timeout.
    function wait_for(f; timeout = 10.0)
        return timedwait(f, timeout; pollint = 0.01) === :ok
    end
end

@testitem "cancellation: pre-cancelled token throws immediately, unwrapped" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt
    using CancellationTokens

    call_count = Ref(0)
    @derived function counted(rt)::Int
        call_count[] += 1
        return 42
    end

    rt = new_test_rt()
    src = CancellationTokenSource()
    cancel(src)
    rt2 = Salsa.with_cancellation(rt, get_token(src))

    # The machinery cancel point fires before the user function ever runs, and the
    # exception arrives as OperationCanceledException, NOT DerivedFunctionException.
    @test_throws OperationCanceledException counted(rt2)
    @test call_count[] == 0

    # The plain runtime is unaffected.
    @test counted(rt) == 42
end

@testitem "cancellation: unwrapped through a nested derived chain" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt
    using CancellationTokens

    entered = Base.Event()
    go = Base.Event()

    @derived function inner_blocking(rt)::Int
        notify(entered)
        wait(go)
        Salsa.throw_if_cancellation_requested(rt)
        return 1
    end
    @derived function middle(rt)::Int
        return inner_blocking(rt) + 1
    end
    @derived function outer(rt)::Int
        return middle(rt) + 1
    end

    rt = new_test_rt()
    src = CancellationTokenSource()
    rt2 = Salsa.with_cancellation(rt, get_token(src))

    canceller = @async begin
        wait(entered)
        cancel(src)
        notify(go)
    end

    # Cancellation fires three derived frames deep; it must arrive at the top level
    # unwrapped (not inside a DerivedFunctionException per frame).
    @test_throws OperationCanceledException outer(rt2)
    wait(canceller)
end

@testitem "cancellation: user polling inside a derived function" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt
    using CancellationTokens

    src = CancellationTokenSource()

    @derived function self_cancelling(rt)::Int
        # Deterministically simulate "cancelled mid-computation".
        cancel(src)
        Salsa.throw_if_cancellation_requested(rt)
        return 1
    end

    rt2 = Salsa.with_cancellation(new_test_rt(), get_token(src))
    @test_throws OperationCanceledException self_cancelling(rt2)
end

@testitem "cancellation: machinery cancel point between nested calls, no user polling" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt
    using CancellationTokens

    src = CancellationTokenSource()
    inner_calls = Ref(0)

    @derived function cheap_leaf(rt, i::Int)::Int
        inner_calls[] += 1
        return i
    end
    @derived function looping_caller(rt)::Int
        total = 0
        for i = 1:100
            if i == 5
                # Neither function polls the token; only the machinery check in
                # `memoized_lookup` (on the next cheap_leaf call) observes this.
                cancel(src)
            end
            total += cheap_leaf(rt, i)
        end
        return total
    end

    rt2 = Salsa.with_cancellation(new_test_rt(), get_token(src))
    @test_throws OperationCanceledException looping_caller(rt2)
    @test inner_calls[] == 4  # calls 1-4 ran; the 5th lookup threw before running
end

@testitem "cancellation: nothing cached, runtime fully usable afterwards" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt
    using CancellationTokens

    @declare_input base(rt)::Int

    src = CancellationTokenSource()
    run_count = Ref(0)
    @derived function cancellable_compute(rt)::Int
        run_count[] += 1
        Salsa.throw_if_cancellation_requested(rt)
        return base(rt) * 10
    end

    rt = new_test_rt()
    Salsa.new_epoch!(rt)
    set_base!(rt, 1)

    cancel(src)
    # Machinery throws before the user function runs; nothing is cached.
    @test_throws OperationCanceledException cancellable_compute(
        Salsa.with_cancellation(rt, get_token(src)),
    )
    @test run_count[] == 0

    # A cancelled *mid-function* call caches nothing either.
    src2 = CancellationTokenSource()
    @derived function cancel_mid_run(rt)::Int
        run_count[] += 1
        cancel(src2)
        Salsa.throw_if_cancellation_requested(rt)
        return base(rt) * 10
    end
    @test_throws OperationCanceledException cancel_mid_run(
        Salsa.with_cancellation(rt, get_token(src2)),
    )
    @test run_count[] == 1

    # The plain runtime still works and recomputes from scratch...
    @test cancel_mid_run(rt) == 10
    @test run_count[] == 2
    # ...and inputs can still be set (derived_functions_active unwound to 0).
    Salsa.new_epoch!(rt)
    set_base!(rt, 2)
    @test cancel_mid_run(rt) == 20
end

@testitem "swap-path integrity: a throwing recompute must not leave a stale-but-valid entry" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt

    @declare_input in_a(rt)::Int
    @declare_input in_b(rt)::Int

    should_throw = Ref(false)
    @derived function a_plus_b(rt)::Int
        va = in_a(rt)  # recorded as a dependency before the throw below
        should_throw[] && error("boom")
        return va + in_b(rt)
    end

    rt = new_test_rt()
    Salsa.new_epoch!(rt)
    set_in_a!(rt, 1)
    set_in_b!(rt, 2)
    @test a_plus_b(rt) == 3  # cached with dependencies [in_a, in_b]

    # Invalidate via in_b, then make the recompute throw after touching only in_a.
    Salsa.new_epoch!(rt)
    set_in_b!(rt, 10)
    should_throw[] = true
    @test_throws DerivedFunctionException a_plus_b(rt)

    # Regression: the failed recompute above must have dropped the cache entry. If it
    # instead left the old value behind with the partial dependency list [in_a], the
    # validity check would walk only in_a (unchanged), wrongly validate, and serve the
    # stale value 3.
    should_throw[] = false
    @test a_plus_b(rt) == 11
end

@testitem "cancellation: concurrent runtimes with independent tokens" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt, wait_for
    using CancellationTokens

    done = [false, false]
    @derived function poll_until_done(rt, id::Int)::Int
        while !done[id]
            Salsa.throw_if_cancellation_requested(rt)
            yield()
        end
        return id * 10
    end

    rt = new_test_rt()
    src1 = CancellationTokenSource()
    src2 = CancellationTokenSource()
    t1 = @async poll_until_done(Salsa.with_cancellation(rt, get_token(src1)), 1)
    t2 = @async poll_until_done(Salsa.with_cancellation(rt, get_token(src2)), 2)

    cancel(src1)
    @test wait_for(() -> istaskdone(t1))
    @test istaskfailed(t1)
    @test t1.result isa OperationCanceledException

    # The other runtime (same storage, different token) is unaffected.
    @test !istaskdone(t2)
    done[2] = true
    @test fetch(t2) == 20
end

@testitem "cancellation: shared memoization, accessors, do-block, and @cancellable" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt
    using CancellationTokens

    run_count = Ref(0)
    @derived function memoized_val(rt, x::Int)::Int
        run_count[] += 1
        return x + 1
    end

    rt = new_test_rt()
    src = CancellationTokenSource()
    token = get_token(src)

    @test Salsa.cancellation_token(rt) === nothing
    rt2 = Salsa.with_cancellation(rt, token)
    @test Salsa.cancellation_token(rt2) === token

    # Values computed through a cancellable runtime are cached in the shared storage...
    @test memoized_val(rt2, 1) == 2
    @test run_count[] == 1
    # ...and visible through the plain runtime (and vice versa).
    @test memoized_val(rt, 1) == 2
    @test run_count[] == 1
    @test memoized_val(rt, 2) == 3
    @test memoized_val(rt2, 2) == 3
    @test run_count[] == 2

    # Do-block form.
    v = Salsa.with_cancellation(rt, token) do rt
        @test Salsa.cancellation_token(rt) === token
        memoized_val(rt, 1)
    end
    @test v == 2

    # @cancellable macro rewrites an existing call site; the token expression is
    # evaluated exactly once.
    token_evals = Ref(0)
    get_tok() = (token_evals[] += 1; token)
    @test (Salsa.@cancellable get_tok() memoized_val(rt, 1)) == 2
    @test token_evals[] == 1
    @test run_count[] == 2  # everything above was served from cache
end

@testitem "cancellation: lazy input computing task cancelled, waiter recovers" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt, wait_for
    using CancellationTokens

    src = CancellationTokenSource()
    token = get_token(src)

    attempts = Ref(0)
    computing_started = Base.Event()
    release = Base.Event()

    function flaky_lazy_callback(ctx, id::Int)
        attempts[] += 1
        if attempts[] == 1
            notify(computing_started)
            wait(release)
            # Lazy callbacks receive the context, not the runtime, so a cancellable
            # callback observes a token captured from its surroundings.
            throw(OperationCanceledException(token))
        end
        return id * 100
    end

    @declare_input flaky_lazy(rt, id::Int)::Int flaky_lazy_callback

    rt = new_test_rt()

    t_computer = @async flaky_lazy(Salsa.with_cancellation(rt, token), 7)
    wait(computing_started)

    # A second caller starts waiting on the in-progress sentinel.
    t_waiter = @async flaky_lazy(rt, 7)
    @test wait_for(() -> attempts[] == 1)
    sleep(0.1)  # give the waiter time to reach the sentinel wait

    cancel(src)
    notify(release)

    # The computing task fails with the (unwrapped) cancellation...
    @test wait_for(() -> istaskdone(t_computer))
    @test istaskfailed(t_computer)
    @test t_computer.result isa OperationCanceledException

    # ...and the waiter recovers: it wakes, finds no value and no sentinel, and
    # computes the lazy input itself.
    @test fetch(t_waiter) == 700
    @test attempts[] == 2
end

@testitem "cancellation: lazy input waiter honors its own token" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt, wait_for
    using CancellationTokens

    call_count = Ref(0)
    computing_started = Base.Event()
    release = Base.Event()

    function slow_lazy_callback(ctx, id::Int)
        call_count[] += 1
        notify(computing_started)
        wait(release)
        return id * 100
    end

    @declare_input slow_lazy(rt, id::Int)::Int slow_lazy_callback

    rt = new_test_rt()
    src = CancellationTokenSource()

    t_computer = @async slow_lazy(rt, 7)  # not cancellable
    wait(computing_started)

    t_waiter = @async slow_lazy(Salsa.with_cancellation(rt, get_token(src)), 7)
    sleep(0.1)  # give the waiter time to reach the sentinel wait

    # Cancelling the waiter's token wakes it promptly, while the computer is still
    # blocked inside the lazy callback.
    cancel(src)
    @test wait_for(() -> istaskdone(t_waiter))
    @test istaskfailed(t_waiter)
    @test t_waiter.result isa OperationCanceledException
    @test !istaskdone(t_computer)

    # The computer finishes normally and the value is cached.
    notify(release)
    @test fetch(t_computer) == 700
    @test slow_lazy(rt, 7) == 700
    @test call_count[] == 1
end

@testitem "cancellation: fires during the cache-validation walk" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt
    using CancellationTokens

    @declare_input base_in(rt)::Int
    @declare_input trigger(rt)::Int

    src = CancellationTokenSource()

    @derived function leaf(rt, i::Int)::Int
        if i == 25
            # Depend on `trigger` so this leaf (alone) needs recomputation below; the
            # recompute returns the same value (backdating), so the validation walk
            # continues to leaf 26 — whose _derived_changed_at cancel point then fires.
            trigger(rt)
            cancel(src)
        end
        return base_in(rt) + i
    end
    @derived function top(rt)::Int
        return sum(leaf(rt, i) for i = 1:50)
    end

    rt = new_test_rt()
    Salsa.new_epoch!(rt)
    set_base_in!(rt, 0)
    set_trigger!(rt, 1)
    @test top(rt) == sum(1:50)

    # Bump `trigger`: only leaf(25) is invalidated; verifying `top` walks all 50 leaves.
    Salsa.new_epoch!(rt)
    set_trigger!(rt, 2)

    # The entry cancel point passes (token not yet cancelled); leaf(25)'s recompute
    # cancels mid-walk; the walk's own cancel point throws, unwrapped, mid-validation.
    @test_throws OperationCanceledException top(Salsa.with_cancellation(rt, get_token(src)))

    # The storage is intact: a plain call still verifies/recomputes correctly.
    @test top(rt) == sum(1:50)
end

@testitem "cancellation: timeout token cancels a blocking derived call" setup=[CancellationSetup] begin
    using .CancellationSetup: new_test_rt
    using CancellationTokens

    @derived function spin_forever(rt)::Int
        while true
            Salsa.throw_if_cancellation_requested(rt)
            sleep(0.01)
        end
    end

    src = CancellationTokenSource(0.2)  # auto-cancels after 0.2s
    rt2 = Salsa.with_cancellation(new_test_rt(), get_token(src))
    @test_throws OperationCanceledException spin_forever(rt2)
end
