using StaticNumbers
using Test
using Test: @inferred

@testset "simple tests" begin
    @test static(1) === StaticInteger{1}()
    @test static(1) == 1
    @test sin(static(1)) == sin(1)
    @test static(1+im) === StaticNumber{1+im}()

    @test static(1) + static(1) == 2

    @test promote_type(StaticInteger{1}, StaticInteger{1}) == Int
    @test promote_type(StaticInteger{1}, StaticInteger{2}) == Int

    @test zero(static(1)) === 0
    @test zero(static(1.0)) === 0.0
    @test zero(typeof(static(1))) === 0
    @test zero(typeof(static(1.0))) === 0.0

    @test static(true) == true
    @test static(false) == false

    @test unstatic(1) === 1
    @test unstatic(static(1)) === 1
end

@testset "ambiguities" begin
    ambiguities = detect_ambiguities(Base, StaticNumbers)
    for a in ambiguities
        println(a[1], "\n", a[2], "\n")
    end
    @test length(detect_ambiguities(StaticNumbers)) == 0
    @test length(ambiguities) <= 5
end

@testset "static math" begin
    for x in (-1.0, -1, 0, 0.0, false, true, 2, 3, 1.5, 2.0, 3.1, pi, 3//2, 3.0+im, Inf)
        for f in (:round, :ceil, :floor, :sign, :cos, :sin, :log, :exp, :isfinite, :isnan, :abs, :abs2, :iszero, :isone)
            r = try
                @eval $f($x)
            catch
                nothing
            end
            if r != nothing
                #println(f, (x,), " == ", r)
                @test @eval $f(static($x)) == $r
            end
        end
        for y in (-1.0, -1, -1//2, -0.5, 0, 0.0, false, true, 2, 3, 1.5, 2.0, 3.1, pi, 3//2, 3.0+im, Inf)
            @test static(x) + y === x + y
            @test x + static(y) === x + y
            @test static(x) + static(y) === x + y
            for f in (:-, :*, :/, :^, :widemul, :rem, :mod, :div, :(<<), :(>>), :(==), :(<), :(<=), :(>), :(>=))
                r = try
                    @eval $f($x,$y)
                catch
                    nothing
                end
                if r != nothing
                    #println(f, (x,y), " ≈ ", r)
                    if isnan(r)
                        @test @eval isnan($f(static($x), $y))
                        @test @eval isnan($f($x, static($y)))
                        @test @eval isnan($f(static($x), static($y)))
                    else
                        @test @eval $f(static($x), $y) ≈ $r
                        @test @eval $f($x, static($y)) ≈ $r
                        @test @eval $f(static($x), static($y)) ≈ $r
                    end
                end
            end
        end
    end
end

@testset "static types" begin
    @test static(1.5) === StaticReal{1.5}()

    @test Static{1}() === static(1)

    @test Float64(static(1.5)) === 1.5

    @test big(static(1)) == big(1)
    @test typeof(big(static(1))) == BigInt

    @test Complex{Float64}(static(2)) === ComplexF64(2)

    @test 2+static(2) == 4
    @test 1+static(1//2) === 3//2
    @test static(1//2)+1 === 3//2

    @test static(static(1)) == static(1)
    @test_throws ErrorException StaticInteger{StaticInteger{1}()}()
    @test_throws InexactError convert(StaticInteger{1}, 2)

    @test StaticInteger{0}() < 1
    @test static(1) isa Integer
    @test +static(1) === static(1)
    @test -static(1) == -1
    @test ~static(1) == -2
    @test isqrt(static(1)) == 1
    @test zero(static(1)) == 0
    @test one(static(2)) == 1
    @test oneunit(static(2)) == 1

    # Skipping these since it might be a bad idea to add methods to Val
    #@test Val(static(false)) === Val(false)
    #@test Val(static(1)) === Val(1)
    #@test Val(static(3.1)) === Val(3.1)
    #@test Val(static(3+im)) === Val(3+im)

    @test static(1):static(3) isa LengthUnitRange{Int64,<:StaticInteger,<:StaticInteger}
    @test static(1:3) === static(1):static(3)
    @test StaticOneTo(3) === static(1):static(3)
    @test static(1):static(3) === @stat 1:3

    @test StaticReal{3.1}(3.1) === static(3.1)
    @test StaticReal{3.25}(3.25f0) === static(3.25)
    @test_throws InexactError StaticReal{3.1}(3.11)

    @test StaticNumber{3+im}(3+im) === static(3+im)
    @test StaticNumber{3+im}(Int32(3)+im) === static(3+im)
    @test_throws InexactError StaticReal{3+im}(3)

    @test static(static(3.14)) === static(3.14)
    @test static(static(3+im)) === static(3+im)

    @test promote_type(StaticInteger{3}, StaticInteger{3}) === Int
    @test promote_type(typeof(static(3.14)), typeof(pi)) === Float64

    for x in (static(0), static(0.0), static(0//1), static(0 + 0im), static(0.0 + 0.0im))
        @test promote(x,x) === (x+0, x+0)
        @test Base.promote_typeof(x,x) === typeof(x+0)
        @test promote_rule(typeof(x), Bool) === typeof(x+0)
    end
    @test promote(static(1), static(1)) === (1,1)
    @test Base.promote_typeof(static(1), static(1)) === Int

    @test static(1):static(2):5 isa LengthStepRange{Int64,StaticInteger{-1},StaticInteger{2},Int64}

    @test Rational{Int}(static(3//2)) === 3//2

    @test Base.promote_rule(StaticInteger{3}, StaticReal{3.14}) === Float64

    @test widemul(static(1), true) === widemul(1, true)
    @test widemul(static(1.0), false) === widemul(1.0, false)

    @test isnan(static(NaN)) === true
    @test isnan(static(0.0)) === false
end

@testset "show" begin
    @test sprint(show, static(1)) == "static(1)"
    @test sprint(show, static(1//2)) == "static(1//2)"
    @test sprint(show, static(0.5)) == "static(0.5)"
    @test sprint(show, static(1:2:3)) == "static(1:2:3)"
    @test sprint(show, static(2:2:3)) == "static(2:2:2)"
end

@testset "broadcasting" begin
    @test static(1:2) .^ 2 == [1, 4]
    @test static(1:2) .* static(3:4)' == [3 4; 6 8]
    @test static(1:2) .* static(3:4) == [3, 8]
end

@testset "trystatic" begin
    @test trystatic(0, static(1)) === 0
    @test trystatic(1, static(1)) === static(1)
    @test trystatic(0, static(0), static(1)) === static(0)
    @test trystatic(1, static(0), static(1)) === static(1)
    @test trystatic(2, static(0), static(1)) === 2

    @test trystatic(0, 1) === 0
    @test trystatic(static(0), 1) === static(0)
    @test trystatic(1, 1) === static(1)
    @test trystatic(0, 0, 1) === static(0)
    @test trystatic(1, 0, 1) === static(1)
    @test trystatic(2, 0, 1) === 2
    @test trystatic(2, (0, 1)) === 2
    @test trystatic(2, (0, 1, 2)) === static(2)
    @test 2 ⩢ (0, 1) === 2
    @test 2 ⩢ (0, 1, 2) === static(2)
    @test 2 ⩢ 0 ⩢ 1 ⩢ 2 === static(2)
    @test 1 ⩢ 0 ⩢ 1 ⩢ 2 === static(1)
    @test 2 ⩢ 0 ⩢ 1 === 2

    @test trystatic(0, NaN) === 0
    @test trystatic(0, static(NaN)) === 0
    @test trystatic(0.0, static(NaN)) === 0.0
    @test trystatic(NaN, NaN) === static(NaN)
    @test trystatic(NaN, static(NaN)) === static(NaN)
    @test trystatic(0.0/0.0, NaN) === static(NaN)
    @test 0.0 ⩢ NaN === 0.0
    @test NaN ⩢ NaN === static(NaN)

    @test trystatic(2, 1:3) == static(2)
    @test trystatic(4, 1:3) == 4
end

@testset "@generate_static_methods macro" begin
    #println(macroexpand(StaticNumbers, :(@generate_static_methods (0, 1) (Base.Math.sinpi, Base.Math.cospi) (Base.:+, Base.:-) )))
    @generate_static_methods (0, 1) (Base.Math.sinpi, Base.Math.cospi) (Base.:+, Base.:-)
    @test sinpi(StaticInteger{1}()) === StaticInteger{0}()
    @test StaticInteger{1}() - StaticInteger{1}() === StaticInteger{0}()

    # Test that sqrt(-1) doesn't cause problems with @generate_static_methods
    @generate_static_methods (-1, 0, 1) (Base.sqrt,) (Base.rem,)
    @test sqrt(static(1)) === static(1)
    @test rem(static(1), static(1)) === static(0)
end

@testset "various" begin
    @test ntuple(identity, static(5)) === static.(ntuple(identity, Val(5)))
    @inferred ntuple(identity, static(1))
    @inferred ntuple(identity, static(5))
    @inferred ntuple(identity, static(25))
    @inferred ntuple(static, static(1))
    @inferred ntuple(static, static(5))
    @inferred ntuple(static, static(25))
    @inferred ntuple(x->x^2, static(1))
    @inferred ntuple(x->x^2, static(5))
    @inferred ntuple(x->x^2, static(25))

    # Test StaticRanges

    @test range(1, length=static(3)) === staticlength(1:3)
    @test range(1, length=static(3)) == 1:3
    @test range(3, step=static(2), length=static(3)) == 3:2:7
    @test range(3, step=2, length=static(3)) == 3:2:7
    @test range(1, length=3) == 1:3
    @test length(range(1, length=static(3))) === static(3)
    @test length(staticlength(1:3)) === static(3)

    @test range(2, length=static(3)) == 2:4
    @test typeof(range(1, length=static(3))) == typeof(range(2, length=static(3)))

    @test staticlength(1:3) isa LengthRange
    @test staticlength(Base.OneTo(3)) isa LengthRange
    @test length(staticlength(Base.OneTo(3))) === static(3)

    r = LengthStepRange(1, 2, static(3))

    @test r isa LengthRange
    @test r isa LengthRange{Int, Int, Int, <:Static}
    @test r isa LengthRange{Int, Int, Int, <:Static{3}}
    @test r isa LengthRange{Int, Int, Int, StaticInteger{3}}
    @test r isa StaticNumbers.LengthStepRange{Int, Int, Int, StaticInteger{3}}
    @test all(r .== 3:2:7)
    @test r[2] == 5
    @test all(collect(r) .== [3, 5, 7])
    @test all(2*r .== 2*(3:2:7))
    @test all(r*5 .== (3:2:7)*5)
    @test all(2 .* r .== 2 .* (3:2:7))
    @test all(r .* 5 .== (3:2:7) .* 5)
    @test all(7 .+ r .== 7 .+ (3:2:7))
    @test all(r .+ 7 .== (3:2:7) .+ 7)
    @test all(7 .- r .== 7 .- (3:2:7))
    @test all(r .- 7 .== (3:2:7) .- 7)
    @test all(-r .== -(3:2:7))
    @test +r === r
    @test .+r === r
    @test .-(.-r) === r
    @test -(-r) === r
    @test typeof(7 .+ r) == typeof(r)
    @test typeof(r .+ 7) == typeof(r)
    @test typeof(7 .- r) == typeof(r)
    @test typeof(r .- 7) == typeof(r)
    @test typeof(7 .* r) == typeof(r)
    @test typeof(r .* 7) == typeof(r)
    @test typeof(7 .+ 5 .* r) == typeof(r)
    @test typeof(r .* 5 .+ 7) == typeof(r)
    @test all( (7:3:100)[r] .== (7:3:100)[3:2:7] )
    @test LengthRange(r) == r

    @test !(static(false)) === true
    @test !(static(true)) === false
    @test true isa StaticOrBool
    @test static(true) isa StaticOrBool

    @test 1 isa StaticOrInt
    @test static(1) isa StaticOrInt

    @test @stat(oftype(static(1), static(2))) === static(2)
    @test @stat(oftype(1, static(2))) === static(2)
    @test @stat(oftype(1.0, 2)) === 2.0
    @test @stat(oftype(static(1.0), 2)) === 2.0
    @test @stat(oftype(1.0, static(2))) === static(2.0)

    ur = LengthUnitRange(2, static(3))
    @test ur isa LengthRange
    @test ur isa LengthRange{Int, Int, <:Static, <:Static}
    @test ur isa LengthRange{Int, Int, <:Static{1}, <:Static{3}}
    @test ur isa LengthRange{Int, Int, StaticInteger{1}, StaticInteger{3}}
    @test ur isa StaticNumbers.LengthUnitRange{Int, Int, StaticInteger{3}}
    @test all(ur .== 3:5)
    @test ur[2] == 4
    @test all(collect(ur) .== [3, 4, 5])
    @test all(2*ur .== 2*(3:5))
    @test all(ur*5 .== (3:5)*5)
    @test all(2 .* ur .== 2 .* (3:5))
    @test all(ur .* 5 .== (3:5) .* 5)
    @test all(7 .+ ur .== 7 .+ (3:5))
    @test all(ur .+ 7 .== (3:5) .+ 7)
    @test all(-ur .== -(3:5))
    @test +ur === ur
    @test .+ur === ur
    @test .-(.-ur) === ur
    @test -(-ur) === ur
    @test typeof(7 .+ ur) == typeof(ur)
    @test typeof(ur .+ 7) == typeof(ur)
    @test length(7 .- ur) == static(3)
    @test typeof(ur .- 7) == typeof(ur)
    @test length(7 .* ur) === static(3)
    @test length(ur .* 7) === static(3)
    @test length(7 .+ 5 .* ur) === static(3)
    @test length(ur .* 5 .+ 7) === static(3)
    @test all( (7:3:100)[ur] .== (7:3:100)[3:5] )

    # Test types
    @test LengthRange(1:3) isa LengthUnitRange
    @test LengthRange(1:2:4) isa LengthStepRange

    # Test that type inferrence is working
    @inferred 2*r
    @inferred 2*ur

    f3(x) = trystatic(x, 3, 4)
    # @show Base.return_types(f3, (Int,))
    g3() = f3(4)
    @inferred g3()

    f4(x) = trystatic(x, static(3), static(4))
    # @show Base.return_types(f4, (Int,))
    g4() = f4(4)
    @inferred g4()

     # f5(x) = trystatic(mod(x,4), LengthStepRange(static(-1),static(1),static(4)))
     # @show Base.return_types(f5, (Int,))
     # g5() = f5(2)
     # @show Base.return_types(g5, ())

    # Test array handling with static ranges
    A = rand(16,16)
    B = rand(static(16),static(16))
    C = A[staticlength(5:8),StaticOneTo(4)]
    @test all(C .== A[staticlength(5:8),StaticOneTo(4)])
    A[StaticOneTo(4),StaticOneTo(4)] = C
    @test all(A[StaticOneTo(4),StaticOneTo(4)] .== C)
    @test all(staticlength(3:4).^2 == (3:4).^2)

    @test Unsigned(static(2)) === Unsigned(2)
end

# Test with a new numeric type
struct MyType1 <: Real
    x::Float64
end

@testset "various II" begin
    @test MyType1(static(3)) === MyType1(3)
    @test MyType1(static(3.0)) === MyType1(3.0)

    @test maybe_static(+, 2, 2) === 4
    @test maybe_static(+, static(2), 2) === 4
    @test maybe_static(+, 2, static(2)) === 4
    @test maybe_static(+, static(2), static(2)) === static(4)
    @inferred maybe_static(+, 2, 2)
    @inferred maybe_static(+, static(2), 2)
    @inferred maybe_static(+, 2, static(2))
    @inferred maybe_static(+, static(2), static(2))

    # Test @tostatic macro

    x = 0
    @test StaticNumbers.@tostatic(x, 0, 3) === static(0)
    @test StaticNumbers.@tostatic(x, -1, 0) === static(0)
    @test StaticNumbers.@tostatic(x, 0, 0) === static(0)

    x = 3
    @test StaticNumbers.@tostatic(x, 0, 3) === static(3)
    @test StaticNumbers.@tostatic(x, 2, 7) === static(3)
    @test StaticNumbers.@tostatic(x, 3, 3) === static(3)
    @test StaticNumbers.@tostatic(x, 3, 4) === static(3)

    StaticNumbers.@tostatic x 2 5 begin
        @test x === static(3)
    end

    x = static(3)
    @test StaticNumbers.@tostatic(x, 0, 0) === static(3)

    @test eachindex(static(2:3)) === StaticOneTo(2)

    @test static(3) & static(3) === static(3)
    @test static(3) | static(3) === static(3)
    @test static(3) ⊻ static(3) === static(0)
    @test fma(static(3), static(3), static(3)) === 3*3+3
    @test fma(static(3.0), static(3.0), static(3.0)) === 3.0*3.0+3.0
end

@testset "tuple indexing" begin
    for t in ((1,2,3,4), (1, 2.0, 3//1, 4.0f0))
        @test t[static(2)] === t[2]
        @inferred t[static(2)]
        @test t[static(2):static(3)] === t[2:3]
        @inferred t[static(2):static(3)]
        @test t[static(2):static(2):static(4)] === t[2:2:4]
        @inferred t[static(2):static(2):static(4)]
        @test t[static(1):static(2):static(3)] === t[1:2:3]
        @inferred t[static(1):static(2):static(2)]
    end
end

@testset "more tests" begin
    @test StaticOneTo(Base.OneTo(3)) === StaticOneTo(3)
    @test StaticOneTo(1:3) === StaticOneTo(3)
    @test LengthUnitRange(2:4) isa LengthUnitRange
    @test LengthUnitRange(2:4) == 2:4
end

@testset "@stat macro" begin
    @test @stat(2+2) === static(4)
    x = 2
    x2 = @stat x + 2
    @test x2 === 4
    y = static(2)
    y2 = @stat y + 2
    @test y2 === static(4)

    T = (1,2,3,4)
    A = [1,2,3,4]
    @test @stat(length(A)) === 4
    @test @stat(length(T)) === static(4)
    @test @stat(firstindex(T)) === static(1)
    @test @stat(lastindex(A)) === 4
    @test @stat(lastindex(T)) === static(4)

    r = @stat 2:5
    @test r[1] === first(r)
    @test first(r) === 2
    @test static(2) === @stat first(r)
    @test r[end] === last(r)
    @test last(r) === 5
    @test static(5) === @stat last(r)
    @test static(3) === @stat r[2]
    i = 2
    @test 3 === @stat r[i]
    #@test static(3:4) === @stat (1:5)[3:4] # TODO

    f(t) = @stat t[2:end-1]
    @test f(T) === (2,3)
    @inferred f((1,2,3,4))

    @stat g(t,k) = t[k .+ (0:1)]
    @test g(T, 2) === (2,3)
    @inferred g((1,2,3,4), 2)

    @test @stat(first(2:3)) === static(2)
    @test @stat(last(2:3)) === static(3)
    @test static(2:3)[2] === 3
    @test static(2:3)[static(2)] === 3
    @test @stat((2:3)[2]) === static(3)

    # At the moment, we don't want @stat to create static unsigned numbers. (Subject to change.)
    @test UInt(1) === @stat UInt(1)
    @test UInt(1) === @stat unsigned(1)

    T = (1,2,3,4)
    f1(T) = @stat T[2:end-1]
    @test (2,3) === f1(T)
    @inferred f1(T)
    f2(T) = @stat T[range(2, length=2)]
    @test (2,3) === f2(T)
    @inferred f2(T)
    @inferred T[range(2, length=static(2))]
end

tuple_test(n) = Tuple(i for i in static(1):n)
tuple_test2(n) = Tuple(tuple_test(n) for i in static(1):n)
tuple_test3(n) = Tuple(Tuple(i+j for i in static(1):n) for j in static(1:n))

@testset "Tuple generators" begin
    @test Tuple(10i+j for i=static(1:3), j=static(1:2)) === Tuple(10i+j for i=1:3, j=1:2)
    @test Tuple(i+j for i=static(2:3), j=static(10:10:20)) === (12, 13, 22, 23)
    @test tuple_test(static(2)) === static.((1, 2))
    @test tuple_test2(static(2)) === (static.((1, 2)), static.((1, 2)))
    @test tuple_test3(static(2)) === ((2, 3), (3, 4))

    if VERSION >= v"1.3"
        # Some of the inference tests are too hard for Julia 1.0
        @inferred Tuple(i+j for i=static(2:3), j=static(10:10:20))
        @inferred tuple_test(static(2))
        @inferred tuple_test2(static(2))
        @inferred tuple_test3(static(2))
    end
end

@testset "examples for doc" begin
    i = 2
    s = static(2)
    @test 4 === s + s
    @test static(4) === @stat s + s
    @test static(4) === @stat s + 2
    @test 4 === @stat s + i
    @test (1, 4, 9, 16) === Tuple(i^2 for i in static(1):static(4))
    @inferred Tuple(i^2 for i in static(1):static(4))
end

# NOTE: There's still a lot of work to do on unsigned
@testset "unsigned" begin
    @test static(UInt(2)) > -1
    @test UInt(2) > static(-1)
    @test static(UInt(2)) > static(-1)
    @test static(UInt(2)) >= -1
    @test UInt(2) >= static(-1)
    @test static(UInt(2)) >= static(-1)
    @test static(1) << UInt(2) === 4
    @test static(4) >> UInt(2) === 1
end

@testset "setindex(::Tuple, _, ::StaticInteger)" begin
    @test @inferred(Base.setindex((1, 2, 3), nothing, static(1))) == (nothing, 2, 3)
    @test @inferred(Base.setindex((1, 2, 3), nothing, static(2))) == (1, nothing, 3)
    @test @inferred(Base.setindex((1, 2, 3), nothing, static(3))) == (1, 2, nothing)
    @test_throws BoundsError Base.setindex((1, 2, 3), nothing, static(0))
    @test_throws BoundsError Base.setindex((1, 2, 3), nothing, static(4))
end

@testset "StaticNumbers.map" begin
    long_tuple = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)
    @test StaticNumbers.map(sin, (1,2,3)) === Base.map(sin, (1,2,3))
    @test StaticNumbers.map(sin, long_tuple) === Base.map(sin, long_tuple)
    @test StaticNumbers.map(+, (1,2,3), (1,2,3)) === Base.map(+, (1,2,3), (1,2,3))
    @test StaticNumbers.map(+, long_tuple, long_tuple) === Base.map(+, long_tuple, long_tuple)
    @test StaticNumbers.map(+, (1,2,3), (1,2,3), (1,2,3)) === Base.map(+, (1,2,3), (1,2,3), (1,2,3))
    @test StaticNumbers.map(+, long_tuple, long_tuple, long_tuple) === Base.map(+, long_tuple, long_tuple, long_tuple)
    if VERSION >= v"1.3"
        @inferred StaticNumbers.map(sin, (1,2,3))
        @inferred StaticNumbers.map(sin, long_tuple)
        @inferred StaticNumbers.map(+, (1,2,3), (1,2,3))
        @inferred StaticNumbers.map(+, long_tuple, long_tuple)
        @inferred StaticNumbers.map(+, (1,2,3), (1,2,3), (1,2,3))
        @inferred StaticNumbers.map(+, long_tuple, long_tuple, long_tuple)
    end
end

@testset "Tuple from view" begin
    A = rand(5,5)
    v = view(A, range(2, length=static(3)), range(3, length=static(2)))
    S = @inferred Tuple(v)
    @test S === Tuple(A[2:4,3:4])
end

include("StaticArrays_test.jl")

include("SIMD_test.jl")

@testset "ambiguities (extended)" begin
    ambiguities = detect_ambiguities(Base, StaticNumbers, StaticArrays, SIMD)
    for a in ambiguities
        println(a[1], "\n", a[2], "\n")
    end
    @test length(ambiguities) <= 6
end

# Run all tests in optional packages, together with StaticNumbers,
# to make sure we didn't break anything.
if false
    # This currenlty breaks because both SIMD and StaticArrays export "setindex"
    for m in (StaticArrays, SIMD)
        println("Running tests from ", m)
        include(joinpath(dirname(dirname(pathof(m))), "test", "runtests.jl"))
    end
end
