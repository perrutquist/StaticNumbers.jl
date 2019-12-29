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

@testset "static length indexing" begin
    @test SOneTo(StaticOneTo(3)) === SOneTo(3)
    @test StaticOneTo(SOneTo(3)) === StaticOneTo(3)
    @test StaticArrays.SUnitRange(LengthUnitRange(3:4)) === StaticArrays.SUnitRange(3,4)
    @test LengthUnitRange(StaticArrays.SUnitRange(3,4)) === static(3):static(4)
    @test staticlength(StaticArrays.SUnitRange(3,4)) === static(3):static(4)

    for x in ([1, 2, 3, 4], @MVector([1, 2, 3, 4]))
        xi = x[static(1):static(2)]
        v = @MVector([1,2])
        @test xi == v
        @test typeof(xi) == typeof(v)
        xi = @stat x[1:2]
        @test xi == v
        @test typeof(xi) == typeof(v)

        # Test of trystatic(lastindex, x)
        xi = @stat x[1:end-2]
        @test xi == v
        @test typeof(xi) == typeof(v)
    end

    for x in (@SVector([1, 2, 3, 4]), )
        xi = x[static(1):static(2)]
        v = @SVector([1,2])
        @test xi == v
        @test typeof(xi) == typeof(v)

        f(t) = @stat t[2:end-1]
        @test f(x) === SVector((2,3))
        Test.@inferred f((1,2,3,4))

        @stat g(t,k) = t[k .+ (0:1)]
        @test g(x, 2) === SVector((2,3))
        Test.@inferred g((1,2,3,4), 2)
    end
end
