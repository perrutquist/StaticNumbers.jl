using SIMD

@testset "SIMD - Vec generator" begin
    @test Vec(i^2 for i ∈ static(1:3)) === Vec((1, 4, 9))
end
