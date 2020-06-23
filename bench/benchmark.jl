using Salsa
using BenchmarkTools

@declare_input in1(rt, ::Int)::Int

@derived function d1(rt, x::Int)
    in1(rt, x) + 1
end

function simple_bench(rt=Runtime(); N=10, iters=100)
    rt = Runtime()
    for j in 1:iters
        for i in 1:N
            set_in1!(rt, i, i+j)
        end
        for i in 1:N
            d1(rt, i)
        end
    end
end

function parallel_bench(rt=Runtime(); N=10, iters=100)
    rt = Runtime()
    for j in 1:iters
        for i in 1:N
            set_in1!(rt, i, i+j)
        end
        for i in 1:N
            Threads.@spawn begin
                d1(rt, i)
            end
        end
    end
end

function run()
    @btime simple_bench()
    @btime parallel_bench()
end