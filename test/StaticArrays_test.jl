# Running these tests requires StaticArrays

using Test
using StaticArrays
using StaticNumbers

@testset "mmatrix test" begin

    m = Matrix{Float64}(undef, static(3), static(2))
    @test m isa MMatrix
    @test eltype(m) == Float64
    @test size(m) === (3, 2)

    m = Matrix(undef, static(3), static(2))
    @test m isa MMatrix
    @test eltype(m) == Any
    @test size(m) === (3, 2)

end
