using StaticNumbers
using Test

@test Static(1) === StaticInteger{1}()
@test Static(1) == 1
@test sin(Static(1)) == sin(1)
@test Static(1+im) === StaticNumber{1+im}()

@test Static(1) + Static(1) == 2

@test promote_type(StaticInteger{1}, StaticInteger{1}) == Int
@test promote_type(StaticInteger{1}, StaticInteger{2}) == Int

@test zero(Static(1)) === 0
@test zero(Static(1.0)) === 0.0

@test Static(true) == true
@test Static(false) == false

for x in (-1.0, -1, 2, 3, 1.5, 2.0, 3.1, pi, 3//2, 3.0+im)
    for f in (:round, :ceil, :floor, :sign, :cos, :sin, :log, :exp, :isfinite, :isnan)
        r = try
            @eval $f($x)
        catch
            nothing
        end
        if r != nothing
            #println(f, (x,), " == ", r)
            @test @eval $f(Static($x)) == $r
        end
    end
    for y in (-1.0, -1, 2, 3, 1.5, 2.0, 3.1, pi, 3//2, 3.0+im)
        @test Static(x) + y === x + y
        @test x + Static(y) === x + y
        @test Static(x) + Static(y) === x + y
        for f in (:-, :*, :/, :^, :rem, :mod, :(<<), :(>>), :(==), :(<), :(<=), :(>), :(>=))
            r = try
                @eval $f($x,$y)
            catch
                nothing
            end
            if r != nothing
                #println(f, (x,y), " ≈ ", r)
                @test @eval $f(Static($x), $y) ≈ $r
                @test @eval $f($x, Static($y)) ≈ $r
                @test @eval $f(Static($x), Static($y)) ≈ $r
            end
        end
    end
end

@test Static(1.5) === StaticReal{1.5}()

@test Static{1}() === Static(1)

@test Float64(Static(1.5)) === 1.5

@test big(Static(1)) == big(1)
@test typeof(big(Static(1))) == BigInt

@test Complex{Float64}(Static(2)) === ComplexF64(2)

@test 2+Static(2) == 4
@test 1+Static(1//2) === 3//2
@test Static(1//2)+1 === 3//2

@test Static(Static(1)) == Static(1)
@test_throws ErrorException StaticInteger{StaticInteger{1}()}()
@test_throws InexactError convert(StaticInteger{1}, 2)

@test StaticInteger{0}() < 1
@test Static(1) isa Integer
@test +Static(1) === Static(1)
@test -Static(1) == -1
@test ~Static(1) == -2
@test isqrt(Static(1)) == 1
@test zero(Static(1)) == 0
@test one(Static(2)) == 1
@test oneunit(Static(2)) == 1

@test sprint(show, Static(1)) == "Static(1)"
@test sprint(show, Static(1//2)) == "Static(1//2)"
@test sprint(show, Static(0.5)) == "Static(0.5)"

@test trystatic(0, Static(1)) === 0
@test trystatic(1, Static(1)) === Static(1)
@test trystatic(0, Static(0), Static(1)) === Static(0)
@test trystatic(1, Static(0), Static(1)) === Static(1)
@test trystatic(2, Static(0), Static(1)) === 2

@test trystatic(0, 1) === 0
@test trystatic(Static(0), 1) === Static(0)
@test trystatic(1, 1) === Static(1)
@test trystatic(0, 0, 1) === Static(0)
@test trystatic(1, 0, 1) === Static(1)
@test trystatic(2, 0, 1) === 2
@test trystatic(2, (0, 1)) === 2
@test trystatic(2, (0, 1, 2)) === Static(2)
@test 2 ⩢ (0, 1) === 2
@test 2 ⩢ (0, 1, 2) === Static(2)
@test 2 ⩢ 0 ⩢ 1 ⩢ 2 === Static(2)
@test 2 ⩢ 0 ⩢ 1 === 2

@test trystatic(2, 1:3) == Static(2)
@test trystatic(4, 1:3) == 4

#println(macroexpand(StaticNumbers, :(@staticnumbers (0, 1) (Base.Math.sinpi, Base.Math.cospi) (Base.:+, Base.:-) )))
@staticnumbers (0, 1) (Base.Math.sinpi, Base.Math.cospi) (Base.:+, Base.:-)
@test sinpi(StaticInteger{1}()) === StaticInteger{0}()
@test StaticInteger{1}() - StaticInteger{1}() === StaticInteger{0}()

# Test that sqrt(-1) doesn't cause problems with @staticnumbers
@staticnumbers (-1, 0, 1) (Base.sqrt,) (Base.rem,)
@test sqrt(Static(1)) === Static(1)
@test rem(Static(1), Static(1)) === Static(0)

@test ntuple(identity, Static(5)) === ntuple(identity, Val(5))

# Test StaticRanges

@test staticlength(1:3) isa StaticRange
@test length(staticlength(1:3)) === Static(3)

r = StaticStepRange(1, 2, Static(3))

@test r isa StaticRange
@test r isa StaticRange{Int, Int, Int, <:Static}
@test r isa StaticRange{Int, Int, Int, <:Static{3}}
@test r isa StaticRange{Int, Int, Int, StaticInteger{3}}
@test r isa StaticNumbers.StaticStepRange{Int, Int, Int, StaticInteger{3}}
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

@test !(Static(false)) === true
@test !(Static(true)) === false
@test true isa StaticOrBool
@test Static(true) isa StaticOrBool

@test 1 isa StaticOrInt
@test Static(1) isa StaticOrInt

@test ofstatictype(Static(1), 2) === Static(2)
@test ofstatictype(Static(1), 2) === Static(2)
@test ofstatictype(1.0, 2) === 2.0

ur = StaticUnitRange(2, Static(3))
@test ur isa StaticRange
@test ur isa StaticRange{Int, Int, <:Static, <:Static}
@test ur isa StaticRange{Int, Int, <:Static{1}, <:Static{3}}
@test ur isa StaticRange{Int, Int, StaticInteger{1}, StaticInteger{3}}
@test ur isa StaticNumbers.StaticUnitRange{Int, Int, StaticInteger{3}}
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
@test length(7 .- ur) == Static(3)
@test typeof(ur .- 7) == typeof(ur)
@test length(7 .* ur) === Static(3)
@test length(ur .* 7) === Static(3)
@test length(7 .+ 5 .* ur) === Static(3)
@test length(ur .* 5 .+ 7) === Static(3)
@test all( (7:3:100)[ur] .== (7:3:100)[3:5] )

# Test types
@test StaticRange(1:3) isa StaticUnitRange
@test StaticRange(1:2:4) isa StaticStepRange

# Test that type inferrence is working
@test Base.return_types(*, (Int, typeof(r)))[1] === typeof(r)
@test Base.return_types(*, (Int, typeof(ur)))[1] === typeof(r)

f1(x) = trystatic(x, Static(3))
@test Base.return_types(f1, (Int64,))[1] == Union{StaticInteger{3}, Int64}

f2(x) = trystatic(x, 3)
@test Base.return_types(f2, (Int64,))[1] == Union{StaticInteger{3}, Int64}

f3(x) = trystatic(x, 3, 4)
@show Base.return_types(f3, (Int,))
g3() = f3(4)
@test Base.return_types(g3, ())[1] == StaticInteger{4}

f4(x) = trystatic(x, Static(3), Static(4))
@show Base.return_types(f4, (Int,))
g4() = f4(4)
@test Base.return_types(g4, ())[1] == StaticInteger{4}

f5(x) = trystatic(mod(x,4), StaticStepRange(Static(-1),Static(1),Static(4)))
@show Base.return_types(f5, (Int,))
g5() = f5(2)
@show Base.return_types(g5, ())

# Test array handling with static ranges
A = rand(16,16)
B = rand(Static(16),Static(16))
C = A[staticlength(5:8),StaticOneTo(4)]
@test all(C .== A[staticlength(5:8),StaticOneTo(4)])
A[StaticOneTo(4),StaticOneTo(4)] = C
@test all(A[StaticOneTo(4),StaticOneTo(4)] .== C)
@test all(staticlength(3:4).^2 == (3:4).^2)
