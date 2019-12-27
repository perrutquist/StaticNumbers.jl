using StaticArrays

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

@testset "static length" begin
    @test SOneTo(StaticOneTo(3)) === SOneTo(3)
    @test StaticOneTo(SOneTo(3)) === StaticOneTo(3)
    @test StaticArrays.SUnitRange(LengthUnitRange(3:4)) === StaticArrays.SUnitRange(3,4)
    @test LengthUnitRange(StaticArrays.SUnitRange(3,4)) === static(3):static(4)
    @test staticlength(StaticArrays.SUnitRange(3,4)) === static(3):static(4)

    # TODO:
    # for x in ([1, 2, 3, 4], @MVector([1, 2, 3, 4]))
    #     @test x[static(1):static(2)] === @MVector([1,2])
    # end
end
