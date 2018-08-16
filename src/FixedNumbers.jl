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
FixedInteger(x::Bool) = FixedInteger{Int(x)}() # Use FixedInteger{false}() if you must...
Base.convert(::Type{FixedInteger{X}}, y::Number) where {X} = FixedInteger{X}() == y ? x : InexactError(:convert, FixedInteger{X}(), y)
Base.convert(::Type{T}, ::FixedInteger{X}) where {T<:Number,X} = convert(T, X)

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
Base.convert(x::Type{FixedReal{X}}, y::Number) where {X} = X == y ? FixedReal{X}() : InexactError(:convert, FixedReal{X}(), y)
Base.convert(::Type{T}, ::FixedReal{X}) where {T<:Number,X} = convert(T, X)

"""
A `FixedNumber` is a `Number` whose value is stored in the type, and which
contains no runtime data.
"""
struct FixedNumber{X} <: Number
    function FixedNumber{X}() where {X}
        X isa Number && isimmutable(X) || throw(FixedError)
        new{X}()
    end
end

FixedNumber(x::Number) = FixedNumber{x}()
Base.convert(x::Type{FixedNumber{X}}, y::Number) where {X} = X == y ? FixedNumber{X}() : InexactError(:convert, FixedNumber{X}(), y)
Base.convert(::Type{T}, ::FixedNumber{X}) where {T<:Number,X} = convert(T, X)

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

Base.promote_rule(::Type{<:FixedInteger}, ::Type{<:FixedInteger}) = Int
Base.promote_rule(::Type{<:FixedReal}, ::Type{<:Union{FixedReal,FixedInteger}}) = Float64
Base.promote_rule(::Type{<:FixedNumber}, ::Type{<:Fixed}) = Complex{Float64}

Base.promote_rule(::Type{<:Fixed}, ::Type{T}) where {T<:Number} = T

# Some of the more common constructors that do not default to `convert`
for T in (:Bool, :Int32, :UInt32, :Int64, :UInt64, :Int128)
    @eval Base.$T(::FixedInteger{X}) where X = $T(X)
end
for T in (:Float32, :Float64)
    @eval Base.$T(::Union{FixedInteger{X}, FixedReal{X}}) where X = $T(X)
end
for T in (:ComplexF32, :ComplexF64)
    @eval Base.$T(::Union{FixedInteger{X}, FixedReal{X}, FixedNumber{X}}) where X = $T(X)
end
# big(x) still defaults to convert.

end # module
