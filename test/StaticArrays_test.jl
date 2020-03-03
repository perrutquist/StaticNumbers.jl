using StaticArrays

@testset "StaticArrays Size" begin
    @test Size(static(2:4)) === Size(3)
    @test Size(static(3)) === Size(3)
    @test Size(static(3), static(4)) === Size(3, 4)
    @test static(Size(3)) === (static(3),)
    @test typeof(Size()) === Size{()} # Issue #8
    @test typeof(Size(())) === Size{()} # Issue #8
end

@testset "static length indexing" begin
    @test SOneTo(StaticOneTo(3)) === SOneTo(3)
    @test StaticOneTo(SOneTo(3)) === StaticOneTo(3)
    @test StaticArrays.SUnitRange(LengthUnitRange(3:4)) === StaticArrays.SUnitRange(3,4)
    @test LengthUnitRange(StaticArrays.SUnitRange(3,4)) === static(3):static(4)
    @test staticlength(StaticArrays.SUnitRange(3,4)) === static(3):static(4)

    for x in ([1, 2, 3, 4], @MVector([1, 2, 3, 4]), [1 3; 2 4], [1 2; 3 4]', @MMatrix([1 3; 2 4]), @MMatrix([1 2; 3 4])')
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
        if x isa MArray
            @test xi isa MArray
        else
            @test !(xi isa MArray)
        end
    end

    for x in (@SVector([1, 2, 3, 4]), )
        xi = x[static(1):static(2)]
        v = @SVector([1,2])
        @test xi == v
        @test typeof(xi) == typeof(v)

        f(t) = @stat t[2:end-1]
        @test f(x) === SVector((2,3))
        Test.@inferred f(x)
        Test.@inferred f((1,2,3,4))

        @stat g(t,k) = t[k .+ (0:1)]
        @test g(x, 2) === SVector((2,3))
        Test.@inferred g((1,2,3,4), 2)
    end

    @test SVector(@stat 1:2) === SVector{2}([1,2])
    @test SVector(i^2 for i in @stat 1:2) === SVector{2}([1,4])

    f(k) = SVector(@stat 1:k)
    @test f(static(2)) === SVector{2}([1,2])
    Test.@inferred(f(static(2)))

    g(k) = SVector(i^2 for i in @stat 1:k)
    @test g(static(2)) === SVector{2}([1,4])
    Test.@inferred(g(static(2)))

    @test SVector(i^2 for i in static(1):static(3)) === SVector{3}([1, 4, 9])

    @test CleanNamespace.get_one_to(SVector(1, 2, 3), static(2)) === SVector(1, 2)
    @test CleanNamespace.get_one_to(MVector(1, 2, 3), static(2)) == MVector(1, 2)
    @test CleanNamespace.get_one_to(MVector(1, 2, 3), static(2)) isa MVector
end

@inline function testmul(A::SMatrix, B::SMatrix)
    SArray((Base.@_inline_meta; +(Tuple(A[i,k]*B[k,j] for k in static(axes(A,2)))...))
        for i in static(axes(A,1)), j in static(axes(B,2)))
end

@testset "SMatrix generators" begin
    A = SArray(i + j for i = static(2:2:4), j = static(10:10:30))
    @test A === @SMatrix [12 22 32; 14 24 34]
    B = SArray(10i + j for i = static(1:3), j = static(4:5))
    @test B === @SMatrix [14 15; 24 25; 34 35]
    @test testmul(A, B) == [1784  1850; 1928  2000]
    Test.@inferred testmul(A, B)
end

@testset "SMatrix from view" begin
    A = rand(5,5)
    v = view(A, range(2, length=static(3)), range(3, length=static(2)))
    S = Test.@inferred SMatrix(v)
    @test S isa SMatrix
    @test S == A[2:4,3:4]
end
