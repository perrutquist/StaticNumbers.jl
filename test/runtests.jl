using FixedNumbers
using Test

@test Fixed(1.0) === FixedInteger{1}()
@test Fixed(1+0im) === FixedInteger{1}()

@test Fixed(1.5) === FixedReal{1.5}()
@test Fixed(1+im) === FixedNumber{1+im}()

@test Float64(Fixed(1.0)) === 1.0

@test big(Fixed(1)) == big(1)
@test typeof(big(Fixed(1))) == BigInt

@test Complex{Float64}(Fixed(2)) === ComplexF64(2)
