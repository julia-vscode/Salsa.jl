@testitem "evict_derived! removes matching entries only" begin
    import Salsa
    using Salsa: Runtime

    Salsa.@declare_input evict_test_input(rt, key::String)::Int

    compute_count = Ref(0)

    Salsa.@derived function evict_test_double(rt, key::String)
        compute_count[] += 1
        return evict_test_input(rt, key) * 2
    end

    rt = Runtime()
    set_evict_test_input!(rt, "a", 1)
    set_evict_test_input!(rt, "b", 10)

    @test evict_test_double(rt, "a") == 2
    @test evict_test_double(rt, "b") == 20
    @test compute_count[] == 2

    # Cached: no recompute.
    @test evict_test_double(rt, "a") == 2
    @test compute_count[] == 2

    n = Salsa.evict_derived!(key -> "a" in key.args, rt)
    @test n == 1

    # The evicted entry recomputes on next access; the other stays cached.
    @test evict_test_double(rt, "a") == 2
    @test compute_count[] == 3
    @test evict_test_double(rt, "b") == 20
    @test compute_count[] == 3

    # Non-matching predicate evicts nothing.
    @test Salsa.evict_derived!(key -> false, rt) == 0
end

@testitem "evict_derived! matches on the derived function" begin
    import Salsa
    using Salsa: Runtime, DerivedKey

    Salsa.@declare_input evict_test_input2(rt, key::String)::Int

    Salsa.@derived function evict_test_triple(rt, key::String)
        return evict_test_input2(rt, key) * 3
    end

    Salsa.@derived function evict_test_quadruple(rt, key::String)
        return evict_test_input2(rt, key) * 4
    end

    rt = Runtime()
    set_evict_test_input2!(rt, "a", 1)

    @test evict_test_triple(rt, "a") == 3
    @test evict_test_quadruple(rt, "a") == 4

    # Evict only entries belonging to one derived function.
    n = Salsa.evict_derived!(key -> key isa DerivedKey{typeof(evict_test_triple)}, rt)
    @test n == 1
end
