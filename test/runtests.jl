module SalsaTest

using Test
using Salsa

# The Salsa.jl test is configurable to run with arbitrary Runtime instances, by
# providing your own implementation of this function before running `"test/Salsa.jl"`.
new_test_rt() = Runtime()
new_test_rt(ctx::Context) where Context = Runtime{Context}(ctx)

@testset "Salsa.jl" begin
    include("Salsa.jl")
end

end # SalsaTest
