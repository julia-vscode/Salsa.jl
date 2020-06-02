module MiniSalsaTest

# Inline debugging tests

using Test
using Salsa
using Salsa: Runtime, InputKey, DependencyKey


@testset "usage" begin
    rt = Runtime()

    # Simple function with no inputs
    @derived function f1(rt, x::Int)
        y = x + 1
        return y
    end

    s = Runtime()
    @test f1(s, 0) === 1

    # Works with named or unnamed arguments ✔︎
    @declare_input mymap(_, ::String, ::Int) :: String
    @declare_input mymap(rt, x::String, y::Int) :: Int
    @declare_input mymap(rt::Runtime, x::String, y::Int) :: Int

    set_mymap!(s, "hello", 2, 10)
    @test mymap(s, "hello", 2) === 10
    # TODO: Is it valid to still read from a committed transaction?
    @test mymap(s, "hello", 2) === 10


    s = Runtime()
    @test_throws KeyError mymap(s, "hello", 2)
    set_mymap!(s, "hello", 2, 10)
    @test mymap(s, "hello", 2) === 10

    @derived function add_to_mapped(s,x,y)
        mymap(s,"hello",x) + y
    end
    @derived function timesed(s,x,y)
        add_to_mapped(s,x,1)*y
    end

    @test timesed(s, 2,2) === 22

    s = Runtime()
    set_mymap!(s, "hello", 2, 5)

    @test timesed(s, 2,2) === 12
end
@testset "Early Exit Optimization" begin
    s = Runtime()
    @declare_input x(rt)::Int
    @declare_input y(rt)::Int

    set_x!(s, 0)
    set_y!(s, 1)

    @derived function add_em(s)
        x(s) + y(s)
    end
    @derived function square_it(s)
        add_em(s) * add_em(s)
    end

    square_it(s)

    s = Runtime()
    set_x!(s, 3)
    set_y!(s, 4)
    square_it(s)

    s = Runtime()
    set_x!(s, 3)
    set_y!(s, 4)
    square_it(s)

    s = Runtime()
    set_x!(s, 4)
    set_y!(s, 3)
    square_it(s)
end


println()


###########################
# Simple API usage

@derived function foofunc(db, x::Int, y::Vector{Int}) :: Int
    sum([1 + x , y...])
end

@testset "Simple API Usage" begin
    @derived function foofunc(db, x::Int, y::Vector{Int}) :: Int
        sum([1 + x , y...])
    end

    @derived f2(db) = 1

    rt = Runtime()
    @test f2(rt) == 1
    @test foofunc(rt, 1, [1,1]) == 4

    @declare_input x(rt)::Int

    set_x!(rt, 1)
    @test x(rt) == 1
end

@testset "Multiple inputs different types same runtime same key" begin
    @declare_input a(rt, x::Int)::String
    @declare_input b(rt, x::Int)::Int

    rt = Runtime()
    set_a!(rt, 1, "hi")
    set_b!(rt, 1, 10)

    @test a(rt, 1) == "hi"
    @test b(rt, 1) == 10
end


struct LoggingContext
    io::IOBuffer
end
@derived function add(rt::Runtime{LoggingContext}, a,b)
    out = a+b
    println(Salsa.context(rt).io, "$out")
    return out
end
@testset "Custom Context" begin
    io1 = IOBuffer()
    io2 = IOBuffer()
    rt = Runtime{LoggingContext}(LoggingContext(io1))
    add(rt, 2,3)

    rt.context = LoggingContext(io2)
    add(rt, 2,3)  # Doesn't print, because already cached.
    add(rt, 2,2)

    s1 = String(take!(io1))
    s2 = String(take!(io2))
    @test s1 == "5\n"
    @test s2 == "4\n"
end

# Task Parallelism Test
@time @testset "Parallel Salsa Derived functions!" begin
    @declare_input range_names(rt) :: NTuple{N,Symbol} where N
    @declare_input named_range(rt, name::Symbol) :: AbstractRange

    @derived function sum_all_ranges(rt) :: Number
        # Oh man, it might take a long time to add all those ranges. Better
        # spawn some tasks to make it faster!
        @sync begin
            tasks = [Threads.@spawn sum_range(rt, name)
                    for name in range_names(rt)]
            return sum(fetch(t)::Int for t in tasks)
        end
    end
    @derived function sum_range(rt, name::Symbol) :: Number
        sum(named_range(rt, name))
    end

    rt = Runtime()
    # Initialize the inputs
    I = 1000  # Number of concurrent tasks scheduled
    _names = Tuple(Symbol("range$i") for i in 1:I)
    N = 10_000

    set_range_names!(rt, _names)
    for (n,r) in zip(_names, Tuple(1:N for _ in 1:I))
        set_named_range!(rt, n, r)
    end

    @assert sum_range(rt, _names[1]) === sum(1:N)
    @test sum_all_ranges(rt) === sum(1:N) * I
end


const NUM_TRACE_TEST_CALLS = Salsa.N_INIT_TRACES + 5  # Plus a few extra for good measure.

# NOTE: This test is testing internal aspects of the package, not the public API.
@testset "Growing the trace pool freelist" begin
    @derived function recursive_cause_pool_growth(rt, n::Int)::Int
        # Verify that things still work after at least one pool growth
        if n <= NUM_TRACE_TEST_CALLS
            return recursive_cause_pool_growth(rt, n+1) + 1
        else
            return base_value(rt)
        end
    end

    @declare_input base_value(rt)::Int

    rt = Runtime()

    set_base_value!(rt, 0)

    # Create more than Salsa.N_INIT_TRACES derived function calls to force a growth
    # event of the trace pool + freelist.
    @test recursive_cause_pool_growth(rt, 1) == NUM_TRACE_TEST_CALLS

    # Now test that the dependencies were recorded correctly, and everything reruns
    set_base_value!(rt, 1)

    @test recursive_cause_pool_growth(rt, 1) == NUM_TRACE_TEST_CALLS + 1
end

@testset "task parallel derived functions invalidation" begin
    @declare_input i(_, ::Int)::Int

    # Test that derived functions spawned on other threads still record their dependencies
    # correctly.
    @derived a(rt, x) = i(rt, x)
    # The two separate calls to `a()` will happen on different threads. Then we will test
    # that their calls to `a()` were correctly recorded as deps on the parent runtime.
    @derived function b(rt)
        outs = Int[0,0]
        # Use Threads.@threads to force running these on different threads.
        Threads.@threads for i in 1:2
            outs[i] = a(rt, i)
        end
        return sum(outs)
    end

    rt = Runtime()
    set_i!(rt, 1, 1)
    set_i!(rt, 2, 1)
    @assert b(rt) == 2

    # Now, assuming that the deps on a() were correctly recorded, changing i() should
    # trigger re-evaluation of b().
    set_i!(rt, 1, 10)  # Test the change written on same thread (thread 1)
    @test b(rt) == 11

    set_i!(rt, 2, 10)  # Test the change written on the _different_ thread (thread 2)
    @test b(rt) == 20
end

# ==============================================
#  Package Health / Performance Tests
# ==============================================

@testset "No unbound type parameters (performance hazard)" begin
    # Make sure that there aren't any unbound params, which can be a performance problem.
    # https://discourse.julialang.org/t/unused-where-t-causes-a-function-to-become-very-slow/39727/4
    # NOTE: Was having StackOverflow problems when I set recursive=true, so I'm manually
    # unrolling this test.
    @test isempty(Test.detect_unbound_args(Salsa, recursive=false))
    @test isempty(Test.detect_unbound_args(Salsa._DefaultSalsaStorage, recursive=false))
    @test isempty(Test.detect_unbound_args(Salsa.Debug, recursive=false))
end

end # MiniSalsaTest
