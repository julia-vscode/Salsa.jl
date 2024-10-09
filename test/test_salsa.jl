@testmodule SalsaSetup begin
    import Salsa
    using Salsa: Runtime, @derived

    # The Salsa.jl test is configurable to run with arbitrary Runtime instances, by
    # providing your own implementation of this function before running `"test/Salsa.jl"`.
    new_test_rt() = Runtime()
    new_test_rt(ctx::Context) where Context = Runtime{Context}(ctx)

    @derived function foofunc(db, x::Int, y::Vector{Int}) :: Int
        sum([1 + x , y...])
    end

    struct LoggingContext
        io::IOBuffer
    end

    @derived function add(rt::Runtime{LoggingContext}, a,b)
        out = a+b
        println(Salsa.context(rt).io, "$out")
        return out
    end

    const NUM_TRACE_TEST_CALLS = Salsa.N_INIT_TRACES + 5  # Plus a few extra for good measure.
end

# NOTE: This test file expects `new_test_rt([ctx,])` to be defined before it is called,
# which is used to construct new Runtime() instances for the tests. This is so that
# you can run these tests for abitrary Runtime types.

@testitem "hashing" begin
    using Salsa: InputKey, DerivedKey

    # Here we make sure that all parts of a dependency key are incorporated into its hash.

    function a end
    function b end

    input_key_a1 = InputKey{typeof(a)}(("123",))
    input_key_a2 = InputKey{typeof(a)}((0,))
    input_key_b = InputKey{typeof(b)}(("123",))

    @test hash(input_key_a1) != hash(input_key_a2)
    @test hash(input_key_a1) != hash(input_key_b)

    derived_key_a1 = DerivedKey{typeof(a)}(("123",))
    derived_key_a2 = DerivedKey{typeof(a)}((0,))
    derived_key_b = DerivedKey{typeof(b)}((123,))

    @test hash(derived_key_a1) != hash(input_key_a1)
    @test hash(derived_key_a2) != hash(input_key_a2)
    @test hash(derived_key_a1) != hash(derived_key_a2)
    @test hash(derived_key_a1) != hash(derived_key_b)
end

@testitem "usage" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt

    rt = new_test_rt()

    # Simple function with no inputs
    @derived function f1(rt, x::Int)
        y = x + 1
        return y
    end

    s = new_test_rt()
    @test f1(s, 0) === 1

    # Works with named or unnamed arguments ✔︎
    @declare_input mymap(_, ::String, ::Int) :: Int
    @declare_input mymap(rt, x::String, y::Int) :: Int
    @declare_input mymap(rt::Runtime, x::String, y::Int) :: Int

    set_mymap!(s, "hello", 2, 10)
    @test mymap(s, "hello", 2) === 10
    # TODO: Is it valid to still read from a committed transaction?
    @test mymap(s, "hello", 2) === 10


    s = new_test_rt()
    @test_throws DerivedFunctionException{KeyError} mymap(s, "hello", 2)
    set_mymap!(s, "hello", 2, 10)
    @test mymap(s, "hello", 2) === 10

    @derived function add_to_mapped(s,x,y)
        mymap(s,"hello",x) + y
    end
    @derived function timesed(s,x,y)
        add_to_mapped(s,x,1)*y
    end

    @test timesed(s, 2,2) === 22

    s = new_test_rt()
    set_mymap!(s, "hello", 2, 5)

    @test timesed(s, 2,2) === 12
end

@testitem "Early Exit Optimization" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt

    s = new_test_rt()
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
    Salsa.new_epoch!(s)
    set_x!(s, x(s))
    set_y!(s, y(s))
    empty!(ftrace)
    @assert negate_it(s) == -1
    @test ftrace == Set([])

    # Setting new values with same sum exits after add_em
    Salsa.new_epoch!(s)
    set_x!(s, x(s) + 1)
    set_y!(s, y(s) - 1)
    empty!(ftrace)
    @assert negate_it(s) == -1
    @test ftrace == Set([add_em])
end


###########################
# Simple API usage

@testitem "Simple API Usage" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt

    @derived function foofunc(db, x::Int, y::Vector{Int}) :: Int
        sum([1 + x , y...])
    end

    @derived f2(db) = 1

    rt = new_test_rt()
    @test f2(rt) == 1
    @test foofunc(rt, 1, [1,1]) == 4

    @declare_input x(rt)::Int

    set_x!(rt, 1)
    @test x(rt) == 1
end

@testitem "Multiple inputs different types same runtime same key" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt

    @declare_input a(rt, x::Int)::String
    @declare_input b(rt, x::Int)::Int

    rt = new_test_rt()
    set_a!(rt, 1, "hi")
    set_b!(rt, 1, 10)

    @test a(rt, 1) == "hi"
    @test b(rt, 1) == 10
end

@testitem "Multiple methods" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt

    @declare_input x(rt)::Int
    @declare_input x(rt, x)::Any
    @declare_input x(rt, x::Int)::Int
    @declare_input x(rt, ::Int, ::String)::String

    rt = new_test_rt()
    set_x!(rt, 1)
    set_x!(rt, "hi", "hey")
    set_x!(rt, 1, 2)
    set_x!(rt, 1, "hi", "ho")

    @test x(rt) == 1
    @test x(rt, "hi") == "hey"
    @test x(rt, 1) == 2
    @test x(rt, 1, "hi") == "ho"
end

@testitem "macro usage corner cases" begin
    @testset "where clauses" begin
        # A simple derived function with a where clause
        Salsa.@derived where_func_x(db, x::T) where T = sizeof(T)
        @test where_func_x(Runtime(), 0) == sizeof(typeof(0))
        @test where_func_x(Runtime(), Int8(0)) == 1

        # Derived function with typed argument
        Salsa.@derived where_func_T(db, ::Type{T}) where T = sizeof(T)
        @test where_func_T(Runtime(), Int) == sizeof(Int)

        # Where clauses on inputs aren't yet supported. Do they even make sense?
        #Salsa.@declare_input where_input(db, ::Type{T})::T where T
    end
    @testset "default values - derived functions" begin
        @declare_input source_text(rt, name::String)::String
        @derived default_source(rt, name::String="stdlib") = source_text(rt, name)

        rt = Runtime()
        set_source_text!(rt, "stdlib", "hello")
        @test default_source(rt) == default_source(rt, "stdlib") == "hello"
    end
    # Default values are not supported for inputs.
    #@testset "default values - inputs" begin
    #    @declare_input currency_value(rt, country="US")::Float64
    #
    #    rt = Runtime()
    #    set_currency_value!(rt, 1.0)
    #end
end

@testitem "inputs and derived functions support docstrings" begin
    @test @macroexpand(begin
        """ My Input """
        Salsa.@declare_input manifest(rt)::Set{Int}

        """ My derived function """
        Salsa.@derived function foo(db) end
    end) isa Expr
end

@testmodule ErrorHandlingTests begin
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
    Salsa.@derived function cycle_oh_no(rt, key)
        cycle_oh_no(rt, key) * 2
    end
    Salsa.@derived function inconspicuous(rt, key)
        subtle_cycle(rt, key) + 10
    end
    Salsa.@derived function subtle_cycle(rt, key)
        inconspicuous(rt, key) * 2
    end
end

@testitem "Robust to derived functions that throw errors" setup=[SalsaSetup, ErrorHandlingTests] begin
    using .SalsaSetup: new_test_rt

    db = new_test_rt()

    # Setting a value that should work as expected
    Salsa.new_epoch!(db)
    ErrorHandlingTests.set_val!(db, 1)
    @test ErrorHandlingTests.square_root(db) == 1

    # Setting a value that will cause square_root() to throw an Exception
    Salsa.new_epoch!(db)
    ErrorHandlingTests.set_val!(db, -1)
    @test_throws DerivedFunctionException{DomainError} ErrorHandlingTests.square_root(db)

    # Now test that it's recovered gracefully from the error, and we can still use the DB
    Salsa.new_epoch!(db)
    ErrorHandlingTests.set_val!(db, 1)
    @test ErrorHandlingTests.square_root(db) == 1
end

@testitem "Cycle detection" setup=[SalsaSetup, ErrorHandlingTests] begin
    using Salsa: DependencyCycleException
    using .SalsaSetup: new_test_rt

    Salsa.@debug_mode begin
        db = new_test_rt()

        # Setting a value that should work as expected
        Salsa.new_epoch!(db)
        ErrorHandlingTests.set_val!(db, 1)
        @test ErrorHandlingTests.square_root(db) == 1

        @test_throws DerivedFunctionException{DependencyCycleException} ErrorHandlingTests.cycle_oh_no(db, 1)
        @test_throws DerivedFunctionException{DependencyCycleException} ErrorHandlingTests.subtle_cycle(db, 1)
    end
end

@testitem "Multi-level derived functions that throw errors #1180" setup=[SalsaSetup, ErrorHandlingTests] begin
    using .SalsaSetup: new_test_rt

    db = new_test_rt()

    # Cause `square_root()` to throw an exeception, when being called from within another
    # derived function.
    Salsa.new_epoch!(db)
    ErrorHandlingTests.set_val!(db, -1)
    ErrorHandlingTests.set_map!(db, 1, 2)
    # Attempts 2 * sqrt(-1), and throws an error
    @test_throws DerivedFunctionException{DomainError} ErrorHandlingTests.val_times_sqrt(db, 1)

    # Now test that it's recovered gracefully from the error, and we can still use the DB
    Salsa.new_epoch!(db)
    ErrorHandlingTests.set_val!(db, 1)
    @test ErrorHandlingTests.square_root(db) == 1
    @test ErrorHandlingTests.val_times_sqrt(db, 1) == 2  # 2 * sqrt(1)

    # Now check that we also recover from KeyErrors when reading from a map:
    Salsa.new_epoch!(db)
    # Throw error (No key 100):
    @test_throws DerivedFunctionException{KeyError} ErrorHandlingTests.val_times_sqrt(db, 100)
    # But this call still works:
    @test ErrorHandlingTests.val_times_sqrt(db, 1) == 2  # 2 * sqrt(1)
end

@testitem "Key Deletions" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt

    @declare_input all_student_ids(rt)::Set{Int}  # TODO: Use an immutable type
    @declare_input student_grade(rt, id::Int)::Float64

    @derived function average_grade(state)
        tot = sum(student_grade(state, id) for id in all_student_ids(state))
        tot / length(all_student_ids(state))
    end

    rt = new_test_rt()

    Salsa.new_epoch!(rt)
    set_all_student_ids!(rt, Set([1, 2]))
    set_student_grade!(rt, 1, 4.0)
    set_student_grade!(rt, 2, 2.0)
    @test average_grade(rt) == 3

    # Delete student `1`
    Salsa.new_epoch!(rt)
    delete_student_grade!(rt, 1)
    set_all_student_ids!(rt, Set([2]))
    @test average_grade(rt) == 2.0

    # <Test that Salsa correctly throws an error given a programming bug>
    # Delete student only from student_grade but leave in all_student_ids (a programming error)
    Salsa.new_epoch!(rt)
    delete_student_grade!(rt, 2)
    @test_throws DerivedFunctionException{KeyError} average_grade(rt)
end


@testitem "Custom Context" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt, LoggingContext, add

    io1 = IOBuffer()
    io2 = IOBuffer()
    rt = new_test_rt(LoggingContext(io1))
    add(rt, 2,3)

    rt.context = LoggingContext(io2)
    add(rt, 2,3)  # Doesn't print, because already cached.
    add(rt, 2,2)

    s1 = String(take!(io1))
    s2 = String(take!(io2))
    @test s1 == "5\n"
    @test s2 == "4\n"
end

# TODO: these tests don't pass after changes to julia's scheduler
# # Task Parallelism Test
# @time @testset "Parallel Salsa Derived functions!" begin
#     @declare_input range_names(rt) :: NTuple{N,Symbol} where N
#     @declare_input named_range(rt, name::Symbol) :: AbstractRange

#     @derived function sum_all_ranges(rt) :: Number
#         # Oh man, it might take a long time to add all those ranges. Better
#         # spawn some tasks to make it faster!
#         @sync begin
#             tasks = [Threads.@spawn sum_range(rt, name)
#                     for name in range_names(rt)]
#             return sum(fetch(t)::Int for t in tasks)
#         end
#     end
#     @derived function sum_range(rt, name::Symbol) :: Number
#         sum(named_range(rt, name))
#     end

#     rt = new_test_rt()
#     # Initialize the inputs
#     # XXX DO NOT make `I` too big, e.g. `I = 1000`, otherwise `_names::NTuple{I,Symbol}` may
#     # cause the internal compiler error (see https://github.com/JuliaLang/julia/issues/38364)
#     I = 100 # Number of concurrent tasks scheduled
#     _names = Tuple(Symbol("range$i") for i in 1:I)
#     N = 10_000

#     set_range_names!(rt, _names)
#     for (n,r) in zip(_names, Tuple(1:N for _ in 1:I))
#         set_named_range!(rt, n, r)
#     end

#     @assert sum_range(rt, _names[1]) === sum(1:N)
#     @test sum_all_ranges(rt) === sum(1:N) * I
# end




# NOTE: This test is testing internal aspects of the package, not the public API.
@testitem "Growing the trace pool freelist" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt

    @derived function recursive_cause_pool_growth(rt, n::Int)::Int
        # Verify that things still work after at least one pool growth
        if n <= SalsaSetup.NUM_TRACE_TEST_CALLS
            return recursive_cause_pool_growth(rt, n+1) + 1
        else
            return base_value(rt)
        end
    end

    @declare_input base_value(rt)::Int

    rt = new_test_rt()

    set_base_value!(rt, 0)

    # Create more than Salsa.N_INIT_TRACES derived function calls to force a growth
    # event of the trace pool + freelist.
    @test recursive_cause_pool_growth(rt, 1) == SalsaSetup.NUM_TRACE_TEST_CALLS

    # Now test that the dependencies were recorded correctly, and everything reruns
    Salsa.new_epoch!(rt)
    set_base_value!(rt, 1)

    @test recursive_cause_pool_growth(rt, 1) == SalsaSetup.NUM_TRACE_TEST_CALLS + 1
end

@testitem "task parallel derived functions invalidation" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt

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

    rt = new_test_rt()
    set_i!(rt, 1, 1)
    set_i!(rt, 2, 1)
    @assert b(rt) == 2

    # Now, assuming that the deps on a() were correctly recorded, changing i() should
    # trigger re-evaluation of b().
    Salsa.new_epoch!(rt)
    set_i!(rt, 1, 10)  # Test the change written on same thread (thread 1)
    @test b(rt) == 11

    Salsa.new_epoch!(rt)
    set_i!(rt, 2, 10)  # Test the change written on the _different_ thread (thread 2)
    @test b(rt) == 20
end

# ==============================================
#  Package Health / Performance Tests
# ==============================================

@testitem "No unbound type parameters (performance hazard)" begin
    # Make sure that there aren't any unbound params, which can be a performance problem.
    # https://discourse.julialang.org/t/unused-where-t-causes-a-function-to-become-very-slow/39727/4
    # NOTE: Was having StackOverflow problems when I set recursive=true, so I'm manually
    # unrolling this test.
    @test isempty(Test.detect_unbound_args(Salsa, recursive=false))
    @test isempty(Test.detect_unbound_args(Salsa._DefaultSalsaStorage, recursive=false))
    @test isempty(Test.detect_unbound_args(Salsa.Debug, recursive=false))
end
