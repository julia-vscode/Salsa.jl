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

    # Deep enough to exhaust any pooled traces and force the pool-growth (fresh
    # allocation) path many times over.
    const NUM_TRACE_TEST_CALLS = 517
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
@testitem "Growing the trace pool" setup=[SalsaSetup] begin
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

    # Create a deep chain of derived function calls to force the pool to grow (an empty
    # stripe allocates a fresh trace, which joins the pool on release).
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

# ==============================================
#  Lazy Input Tests
# ==============================================

@testitem "basic lazy input" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt
    using Base.Threads: Atomic, atomic_add!

    call_count = Atomic{Int}(0)

    function my_lazy_callback(ctx, name::String)
        atomic_add!(call_count, 1)
        return name == "Alice" ? 3.5 : 2.0
    end

    @declare_input grade(rt, name::String)::Float64 my_lazy_callback

    @derived function grade_letter(rt, name::String)::String
        g = grade(rt, name)
        return g >= 3.0 ? "A" : "B"
    end

    rt = new_test_rt()

    # First access triggers the lazy callback
    @test grade(rt, "Alice") == 3.5
    @test call_count[] == 1

    # Second access should be cached — callback not called again
    @test grade(rt, "Alice") == 3.5
    @test call_count[] == 1

    # Different key triggers callback again
    @test grade(rt, "Bob") == 2.0
    @test call_count[] == 2

    # Derived function works with lazy input
    @test grade_letter(rt, "Alice") == "A"
    @test grade_letter(rt, "Bob") == "B"
    @test call_count[] == 2  # No extra calls — values were cached
end

@testitem "lazy input with derived function invalidation" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt
    using Base.Threads: Atomic, atomic_add!

    lazy_call_count = Atomic{Int}(0)

    function lookup_score(ctx, id::Int)
        atomic_add!(lazy_call_count, 1)
        return id * 10.0
    end

    @declare_input score(rt, id::Int)::Float64 lookup_score

    derived_call_count = Ref(0)
    @derived function doubled_score(rt, id::Int)::Float64
        derived_call_count[] += 1
        return score(rt, id) * 2
    end

    rt = new_test_rt()

    # Lazy input feeds derived function
    @test doubled_score(rt, 1) == 20.0
    @test lazy_call_count[] == 1
    @test derived_call_count[] == 1

    # Cached on second access
    @test doubled_score(rt, 1) == 20.0
    @test lazy_call_count[] == 1
    @test derived_call_count[] == 1

    # set_input! overrides the lazy-computed value and invalidates derived
    Salsa.new_epoch!(rt)
    set_score!(rt, 1, 99.0)
    @test doubled_score(rt, 1) == 198.0
    @test lazy_call_count[] == 1  # Lazy callback was NOT called again
    @test derived_call_count[] == 2  # Derived function DID re-run
end

@testitem "lazy input callback runs exactly once under concurrency" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt
    using Base.Threads: Atomic, atomic_add!

    if Threads.nthreads() < 2
        @info "Skipping concurrency test: requires multiple threads"
        @test_skip false
    else

    call_count = Atomic{Int}(0)

    function slow_lazy_callback(ctx, id::Int)
        atomic_add!(call_count, 1)
        sleep(0.05)  # Simulate a slow side-effecting operation
        return id * 100
    end

    @declare_input slow_input(rt, id::Int)::Int slow_lazy_callback

    @derived function use_slow(rt, id::Int)::Int
        return slow_input(rt, id) + 1
    end

    rt = new_test_rt()

    # Access the same lazy input from multiple threads concurrently
    results = Vector{Int}(undef, 4)
    Threads.@threads for i in 1:4
        results[i] = use_slow(rt, 42)
    end

    # All threads should get the same result
    @test all(r -> r == 4201, results)
    # The lazy callback should have been called exactly once for key 42
    @test call_count[] == 1

    end # if nthreads
end

@testitem "lazy input error handling" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt
    using Salsa: DerivedFunctionException
    using Base.Threads: Atomic, atomic_add!

    call_count = Atomic{Int}(0)
    should_fail = Ref(true)

    function flaky_callback(ctx, id::Int)
        atomic_add!(call_count, 1)
        if should_fail[]
            error("transient failure")
        end
        return id * 10
    end

    @declare_input flaky(rt, id::Int)::Int flaky_callback

    @derived function use_flaky(rt, id::Int)::Int
        return flaky(rt, id) + 1
    end

    rt = new_test_rt()

    # First attempt fails — error propagates
    @test_throws DerivedFunctionException use_flaky(rt, 1)
    @test call_count[] == 1

    # The key should NOT be cached on error — retry should call callback again
    should_fail[] = false
    @test use_flaky(rt, 1) == 11
    @test call_count[] == 2

    # Now it's cached — no more calls
    @test use_flaky(rt, 1) == 11
    @test call_count[] == 2
end

@testitem "lazy input deletion and recomputation" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt
    using Base.Threads: Atomic, atomic_add!

    call_count = Atomic{Int}(0)

    function recomputable_callback(ctx, id::Int)
        atomic_add!(call_count, 1)
        return id * 5
    end

    @declare_input recomp(rt, id::Int)::Int recomputable_callback

    @derived function use_recomp(rt, id::Int)::Int
        return recomp(rt, id) + 1
    end

    rt = new_test_rt()

    # First access — lazy callback runs
    @test use_recomp(rt, 1) == 6
    @test call_count[] == 1

    # Delete the input and re-access — callback should run again
    Salsa.new_epoch!(rt)
    delete_recomp!(rt, 1)
    @test use_recomp(rt, 1) == 6
    @test call_count[] == 2
end

@testitem "lazy input and set_input! override" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt
    using Base.Threads: Atomic, atomic_add!

    call_count = Atomic{Int}(0)

    function override_callback(ctx, id::Int)
        atomic_add!(call_count, 1)
        return id * 3
    end

    @declare_input overrideable(rt, id::Int)::Int override_callback

    rt = new_test_rt()

    # set_input! before any lazy access — callback should never be called
    set_overrideable!(rt, 1, 999)
    @test overrideable(rt, 1) == 999
    @test call_count[] == 0

    # set_input! after lazy access — overrides the lazy value
    @test overrideable(rt, 2) == 6  # lazy: 2*3 = 6
    @test call_count[] == 1

    Salsa.new_epoch!(rt)
    set_overrideable!(rt, 2, 777)
    @test overrideable(rt, 2) == 777
    @test call_count[] == 1  # No extra lazy calls
end

# NOTE: This test is testing internal aspects of the package, not the public API.
@testitem "Trace pool: wide traces do not permanently retain capacity" setup=[SalsaSetup] begin
    using .SalsaSetup: new_test_rt

    @declare_input entry(rt, i::Int)::Int

    @derived function wide_sum(rt, n::Int)::Int
        s = 0
        for i in 1:n
            s += entry(rt, i)
        end
        return s
    end

    rt = new_test_rt()

    n = 4 * Salsa.TRACE_CONTAINER_SHRINK_THRESHOLD
    for i in 1:n
        set_entry!(rt, i, i)
    end

    @test wide_sum(rt, n) == sum(1:n)

    # Releasing the wide trace must not leave oversized containers in the pool.
    # Pooled containers otherwise keep their high-water-mark capacity forever,
    # and clearing a Dict/Set costs O(capacity) even when it is empty — so a
    # single wide derived function would tax every later lookup that reuses its
    # pooled trace.
    max_slots = maximum(
        length(tr.seen_deps.dict.slots) for tr in Salsa._pooled_traces()
    )
    @test max_slots <= 4 * Salsa.TRACE_CONTAINER_SHRINK_THRESHOLD

    # The replaced containers must still trace correctly: invalidate one input
    # and verify the wide function recomputes through the same pooled traces.
    Salsa.new_epoch!(rt)
    set_entry!(rt, 1, 101)
    @test wide_sum(rt, n) == sum(1:n) + 100
end
