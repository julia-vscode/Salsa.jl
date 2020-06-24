using Salsa
using BenchmarkTools

@declare_input in1(rt, ::Int)::Int

@derived function d1(rt, x::Int)
    in1(rt, x) + 1
end

@derived function d2(rt, x::Int)
    d1(rt, x) + 1
end

function simple_bench(rt::Runtime; N, iters)
    for j in 1:iters
        Salsa.new_epoch!(rt)
        for i in 1:N
            set_in1!(rt, i, i+j)
        end
        for i in 1:N
            d2(rt, i)
        end
    end
end

function parallel_bench(rt::Runtime; N, iters)
    for j in 1:iters
        Salsa.new_epoch!(rt)
        for i in 1:N
            set_in1!(rt, i, i+j)
        end
        @sync for i in 1:N
            Threads.@spawn begin
                d2(rt, i)
            end
        end
    end
end

# Run all the above benchmarks and report the results to stdout
function run(; N, iters)
    @btime simple_bench(rt; N=$N, iters=$iters) setup=(rt=Runtime())
    @btime parallel_bench(rt; N=$N, iters=$iters) setup=(rt=Runtime())
end
