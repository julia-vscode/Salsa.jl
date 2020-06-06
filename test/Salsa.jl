module SalsaTest

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
    @declare_input mymap(_, ::String, ::Int) :: Int
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

    # Track when derived functions are called.
    ftrace = Set([])

    @derived function add_em(s)
        push!(ftrace, add_em)
        x(s) + y(s)
    end
    @derived function square_it(s)
        push!(ftrace, square_it)
        add_em(s) * add_em(s)
    end
    @derived function negate_it(s)
        push!(ftrace, negate_it)
        -square_it(s)
    end

    # First call runs all functions
    empty!(ftrace)
    @assert negate_it(s) == -1
    @test ftrace == Set([add_em, square_it, negate_it])

    # Second call gets cached values, and re-runs nothing
    empty!(ftrace)
    @assert negate_it(s) == -1
    @test ftrace == Set([])

    # Setting same values also reruns nothing
    set_x!(s, x(s))
    set_y!(s, y(s))
    empty!(ftrace)
    @assert negate_it(s) == -1
    @test ftrace == Set([])

    # Setting new values with same sum exits after add_em
    set_x!(s, x(s) + 1)
    set_y!(s, y(s) - 1)
    empty!(ftrace)
    @assert negate_it(s) == -1
    @test ftrace == Set([add_em])
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
@testset "Multiple methods" begin
    @declare_input x(rt)::Int
    @declare_input x(rt, x)::Any
    @declare_input x(rt, x::Int)::Int
    @declare_input x(rt, ::Int, ::String)::String

    rt = Runtime()
    set_x!(rt, 1)
    set_x!(rt, "hi", "hey")
    set_x!(rt, 1, 2)
    set_x!(rt, 1, "hi", "ho")

    @test x(rt) == 1
    @test x(rt, "hi") == "hey"
    @test x(rt, 1) == 2
    @test x(rt, 1, "hi") == "ho"
end

@testset "inputs and derived functions support docstrings" begin
    @test @macroexpand(begin
        """ My Input """
        Salsa.@declare_input manifest(rt)::Set{Int}

        """ My derived function """
        Salsa.@derived function foo(db) end
    end) isa Expr
end

module ErrorHandlingTests
    using Salsa

    @declare_input val(rt) :: Int
    @declare_input map(rt, key::Int) :: Int

    Salsa.@derived function square_root(rt)
        sqrt(val(rt))
    end
    Salsa.@derived function get_val(rt, key)
        map(rt, key)
    end
    Salsa.@derived function val_times_sqrt(rt, key)
        get_val(rt, key) * square_root(rt)
    end
end

@testset "Robust to derived functions that throw errors" begin
    db = Runtime()

    # Setting a value that should work as expected
    ErrorHandlingTests.set_val!(db, 1)
    @test ErrorHandlingTests.square_root(db) == 1

    # Setting a value that will cause square_root() to throw an Exception
    ErrorHandlingTests.set_val!(db, -1)
    @test_throws DomainError ErrorHandlingTests.square_root(db)

    # Now test that it's recovered gracefully from the error, and we can still use the DB
    ErrorHandlingTests.set_val!(db, 1)
    @test ErrorHandlingTests.square_root(db) == 1
end

@testset "Multi-level derived functions that throw errors #1180" begin
    db = Runtime()

    # Cause `square_root()` to throw an exeception, when being called from within another
    # derived function.
    ErrorHandlingTests.set_val!(db, -1)
    ErrorHandlingTests.set_map!(db, 1, 2)
    # Attempts 2 * sqrt(-1), and throws an error
    @test_throws DomainError ErrorHandlingTests.val_times_sqrt(db, 1)

    # Now test that it's recovered gracefully from the error, and we can still use the DB
    ErrorHandlingTests.set_val!(db, 1)
    @test ErrorHandlingTests.square_root(db) == 1
    @test ErrorHandlingTests.val_times_sqrt(db, 1) == 2  # 2 * sqrt(1)

    # Now check that we also recover from KeyErrors when reading from a map:
    # Throw error:
    @test_throws KeyError ErrorHandlingTests.val_times_sqrt(db, 100)  # No key 100
    # But this call still works:
    @test ErrorHandlingTests.val_times_sqrt(db, 1) == 2  # 2 * sqrt(1)
end

@testset "Key Deletions" begin
    @declare_input all_student_ids(rt)::Set{Int}  # TODO: Use an immutable type
    @declare_input student_grade(rt, id::Int)::Float64

    @derived function average_grade(state)
        tot = sum(student_grade(state, id) for id in all_student_ids(state))
        tot / length(all_student_ids(state))
    end

    rt = Runtime()

    set_all_student_ids!(rt, Set([1, 2]))
    set_student_grade!(rt, 1, 4.0)
    set_student_grade!(rt, 2, 2.0)
    @test average_grade(rt) == 3

    # Delete student `1`
    delete_student_grade!(rt, 1)
    set_all_student_ids!(rt, Set([2]))
    @test average_grade(rt) == 2.0

    # <Test that Salsa correctly throws an error given a programming bug>
    # Delete student only from student_grade but leave in all_student_ids (a programming error)
    delete_student_grade!(rt, 2)
    @test_throws KeyError average_grade(rt)
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

end # SalsaTest
