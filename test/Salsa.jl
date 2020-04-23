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

# Task Parallelism Test
@time @testset "Parallel Salsa Derived functions!" begin
    @declare_input range_names(rt) :: NTuple{N,Symbol} where N
    @declare_input named_range(rt, name::Symbol) :: AbstractRange

    @derived function sum_all_ranges(rt) :: Number
        # Oh man, it might take a long time to add all those ranges. Better
        # spawn some tasks to make it faster!
        @sync begin
            tasks = [Threads.@spawn sum_range(rt, $name)
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


end # MiniSalsaTest
