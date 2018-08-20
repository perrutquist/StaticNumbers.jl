using FixedNumbers
using Test

@test Fixed(1) === FixedInteger{1}()
@test Fixed(1) == 1
@test sin(Fixed(1)) == sin(1)
@test Fixed(1+im) === FixedNumber{1+im}()

@test Fixed(1.5) === FixedReal{1.5}()

@test Float64(Fixed(1.5)) === 1.5

@test big(Fixed(1)) == big(1)
@test typeof(big(Fixed(1))) == BigInt

@test Complex{Float64}(Fixed(2)) === ComplexF64(2)

@test 2+Fixed(2) == 4
@test 1+Fixed(1//2) === 3//2

@test Fixed(Fixed(1)) == Fixed(1)
@test_throws ErrorException FixedInteger{FixedInteger{1}()}()

@test FixedInteger{0}() < 1
@test Fixed(1) isa Integer
@test -Fixed(1) == 1

#println(macroexpand(FixedNumbers, :(@fixednumbers (0, 1) (Base.Math.sinpi, Base.Math.cospi) (+, -) )))
@fixednumbers (0, 1) (Base.Math.sinpi, Base.Math.cospi) (+, -)
@eval :( @test sinpi(FixedInteger{1}()) === FixedInteger{0}() )
@eval :( @test FixedInteger{1}() - FixedInteger{1}() === FixedInteger{0}() )
