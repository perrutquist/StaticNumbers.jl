using FixedNumbers
using Test

@test Fixed(1) === FixedInteger{1}()
@test Fixed(1) == 1
@test sin(Fixed(1)) == sin(1)
@test Fixed(1+im) === FixedNumber{1+im}()

@test Fixed(1) + Fixed(1) == 2

for x in (-1.0, -1, 2, 3, 1.5, 2.0, 3.1, pi, 3//2, 3.0+im)
    for f in (:round, :ceil, :floor, :sign, :cos, :sin, :log, :exp, :isfinite, :isnan)
        r = try
            @eval $f($x)
        catch
            nothing
        end
        if r != nothing
            #println(f, (x,), " == ", r)
            @test @eval $f(Fixed($x)) == $r
        end
    end
    for y in (-1.0, -1, 2, 3, 1.5, 2.0, 3.1, pi, 3//2, 3.0+im)
        @test Fixed(x) + y === x + y
        @test x + Fixed(y) === x + y
        @test Fixed(x) + Fixed(y) === x + y
        for f in (:-, :*, :/, :^, :rem, :mod, :(<<), :(>>), :(==), :(<), :(<=), :(>), :(>=))
            r = try
                @eval $f($x,$y)
            catch
                nothing
            end
            if r != nothing
                #println(f, (x,y), " ≈ ", r)
                @test @eval $f(Fixed($x), $y) ≈ $r
                @test @eval $f($x, Fixed($y)) ≈ $r
                @test @eval $f(Fixed($x), Fixed($y)) ≈ $r
            end
        end
    end
end

@test Fixed(1.5) === FixedReal{1.5}()

@test Fixed{1}() === Fixed(1)

@test Float64(Fixed(1.5)) === 1.5

@test big(Fixed(1)) == big(1)
@test typeof(big(Fixed(1))) == BigInt

@test Complex{Float64}(Fixed(2)) === ComplexF64(2)

@test 2+Fixed(2) == 4
@test 1+Fixed(1//2) === 3//2
@test Fixed(1//2)+1 === 3//2

@test Fixed(Fixed(1)) == Fixed(1)
@test_throws ErrorException FixedInteger{FixedInteger{1}()}()

@test FixedInteger{0}() < 1
@test Fixed(1) isa Integer
@test +Fixed(1) === Fixed(1)
@test -Fixed(1) == -1
@test ~Fixed(1) == -2
@test isqrt(Fixed(1)) == 1
@test zero(Fixed(1)) == 0
@test one(Fixed(2)) == 1
@test oneunit(Fixed(2)) == 1

@test sprint(show, Fixed(1)) == "Fixed(1)"
@test sprint(show, Fixed(1//2)) == "Fixed(1//2)"
@test sprint(show, Fixed(0.5)) == "Fixed(0.5)"

@test fix(0, Fixed(1)) === 0
@test fix(1, Fixed(1)) === Fixed(1)
@test fix(0, Fixed(0), Fixed(1)) === Fixed(0)
@test fix(1, Fixed(0), Fixed(1)) === Fixed(1)
@test fix(2, Fixed(0), Fixed(1)) === 2

@test fix(0, 1) === 0
@test fix(1, 1) === Fixed(1)
@test fix(0, 0, 1) === Fixed(0)
@test fix(1, 0, 1) === Fixed(1)
@test fix(2, 0, 1) === 2

#println(macroexpand(FixedNumbers, :(@fixednumbers (0, 1) (Base.Math.sinpi, Base.Math.cospi) (Base.:+, Base.:-) )))
@fixednumbers (0, 1) (Base.Math.sinpi, Base.Math.cospi) (Base.:+, Base.:-)
@test sinpi(FixedInteger{1}()) === FixedInteger{0}()
@test FixedInteger{1}() - FixedInteger{1}() === FixedInteger{0}()

# Test that sqrt(-1) doesn't cause problems with @fixednumbers
@fixednumbers (-1, 0, 1) (Base.sqrt,) (Base.rem,)
@test sqrt(Fixed(1)) === Fixed(1)
@test rem(Fixed(1), Fixed(1)) === Fixed(0)
