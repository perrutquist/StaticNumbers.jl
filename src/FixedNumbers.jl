module FixedNumbers

export Fixed, FixedInteger, FixedReal, FixedNumber

const FixedError = ErrorException("Illegal type parameter for Fixed.")

"""
A `FixedInteger` is an `Integer` whose value is stored in the type, and which
contains no runtime data.
"""
struct FixedInteger{X} <: Integer
    function FixedInteger{X}() where {X}
        X isa Integer && isimmutable(X) || throw(FixedError)
        new{X}()
    end
end

FixedInteger(x::Integer) = FixedInteger{x}()
Base.convert(x::Type{FixedInteger{X}}, y::Number) where {X} = X == y ? x : InexactError(:convert, FixedInteger{X}(), y)

"""
A `FixedReal` is a `Real` whose value is stored in the type, and which
contains no runtime data.
"""
struct FixedReal{X} <: Real
    function FixedReal{X}() where {X}
        X isa Real && isimmutable(X) || throw(FixedError)
        new{X}()
    end
end

FixedReal(x::Real) = FixedReal{x}()
Base.convert(x::Type{FixedReal{X}}, y::Number) where {X} = X == y ? x : InexactError(:convert, FixedReal{X}(), y)

"""
A `FixedNumber` is a `Number` whose value is stored in the type, and which
contains no runtime data.
"""
struct FixedNumber{X} <: Number
    function FixedInteger{X}() where {X}
        X isa Number && isimmutable(X) || throw(FixedError)
        new{X}()
    end
end

FixedNumber(x::Number) = FixedNumber{x}()
Base.convert(x::Type{FixedNumber{X}}, y::Number) where {X} = X == y ? x : InexactError(:convert, FixedNumber{X}(), y)

"""
`Fixed` is short-hand for the `Union` of all `FixedInteger`s, `FixedReal`s and
`FixedNumber`s.

`Fixed(x)` constructs one of those types (depending on the value of x).
"""
const Fixed = Union{FixedInteger, FixedReal, FixedNumber}

function Fixed(x::Number)
    imag(x) != 0 && return FixedNumber(x)
    x = real(x)
    round(x) != x && return FixedReal(x)
    return FixedInteger(Integer(x))
end

end # module
