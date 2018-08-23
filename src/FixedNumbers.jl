module FixedNumbers

export Fixed, FixedInteger, FixedReal, FixedNumber, @fixednumbers, fix

const FixedError = ErrorException("Illegal type parameter for Fixed.")

"""
A `FixedInteger` is an `Integer` whose value is stored in the type, and which
contains no runtime data.
"""
struct FixedInteger{X} <: Integer
    function FixedInteger{X}() where {X}
        X isa Integer && !(X isa Fixed) && isimmutable(X) || throw(FixedError)
        new{X}()
    end
end

FixedInteger(x::Integer) = FixedInteger{x}()

"""
A `FixedReal` is a `Real` whose value is stored in the type, and which
contains no runtime data.
"""
struct FixedReal{X} <: Real
    function FixedReal{X}() where {X}
        X isa Real && !(X isa Integer) && !(X isa Fixed) && isimmutable(X) || throw(FixedError)
        new{X}()
    end
end

FixedReal(x::Real) = FixedReal{x}()

"""
A `FixedNumber` is a `Number` whose value is stored in the type, and which
contains no runtime data.
"""
struct FixedNumber{X} <: Number
    function FixedNumber{X}() where {X}
        X isa Number && !(X isa Real) && !(X isa Fixed) && isimmutable(X) || throw(FixedError)
        new{X}()
    end
end

FixedNumber(x::Number) = FixedNumber{x}()

"""
`Fixed{X}` is short-hand for the `Union` of `FixedInteger{X}`, `FixedReal{X}`
and `FixedNumber{X}`.
"""
const Fixed{X} = Union{FixedInteger{X}, FixedReal{X}, FixedNumber{X}}

# We'll allow this constructor, but not recommend it.
Fixed{X}() where X = Fixed(X)

# This is the recommended constructor.
"""
`Fixed(X)` is shorthand for `FixedInteger{X}()`, `FixedReal{X}()` or `FixedNumber{X}()`,
depending on the type of `X`.
"""
Base.@pure Fixed(X::Fixed) = X
Base.@pure Fixed(X::Irrational) = X # These are already defined by their type.
Base.@pure Fixed(X::Integer) = FixedInteger{X}()
Base.@pure Fixed(X::Real) = FixedReal{X}()
Base.@pure Fixed(X::Number) = FixedNumber{X}()

Base.promote_rule(::Type{<:Fixed{X}}, ::Type{<:Fixed{X}}) where {X} =
    typeof(X)
Base.promote_rule(::Type{<:AbstractIrrational}, ::Type{<:Fixed{X}}) where {X} =
    promote_type(Float64, typeof(X))

# We need to override promote and promote_typeof because they don't even call
# promote_rule for all-same types.
for T in (FixedInteger, FixedReal, FixedNumber)
    @eval Base.promote(::$T{X}, ys::$T{X}...) where {X} = ntuple(i->X, 1+length(ys))
    @eval Base.promote_typeof(::$T{X}, ::$T{X}...) where {X} = typeof(X)
end

Base.promote_rule(::Type{<:Fixed{X}}, ::Type{<:Fixed{Y}}) where {X,Y} =
    promote_type(typeof(X),typeof(Y))

Base.promote_rule(::Type{<:Fixed{X}}, ::Type{T}) where {X,T<:Number} =
    promote_type(typeof(X), T)

function Base.convert(T::Type{<:Fixed{X}}, y::Number) where {X}
    X == y || throw(InexactError(:convert, T, y))
    return T()
end

Base.convert(::Type{T}, ::Fixed{X}) where {T<:Number,X} = convert(T, X)

# TODO: Constructors to avoid Fixed{Fixed}

# Some of the more common constructors that do not default to `convert`
for T in (:Bool, :Int32, :UInt32, :Int64, :UInt64, :Int128, :Integer)
    @eval Base.$T(::FixedInteger{X}) where X = $T(X)
end
for T in (:Float32, :Float64, :AbstractFloat, :Rational)
    @eval Base.$T(::Union{FixedInteger{X}, FixedReal{X}}) where X = $T(X)
end
for T in (:ComplexF32, :ComplexF64, :Complex)
    @eval Base.$T(::Fixed{X}) where X = $T(X)
end
Rational{T}(::Union{FixedInteger{X}, FixedReal{X}}) where {T,X} = Rational{T}(X)
Complex{T}(::Fixed{X}) where {T,X} = Rational{T}(X)
# big(x) still defaults to convert.

# Single-argument functions that do not already work.
for fun in (:-, :zero, :one, :oneunit, :trailing_zeros, :widen, :decompose)
    @eval Base.$fun(::Fixed{X}) where X = Base.$fun(X)
end
for fun in (:trunc, :floor, :ceil, :round)
    @eval Base.$fun(::Union{FixedReal{X}, FixedNumber{X}}) where {X} = Base.$fun(X)
end
for fun in (:zero, :one, :oneunit)
    @eval Base.$fun(::Type{<:Fixed{X}}) where {X} = Base.$fun(typeof(X))
end

# For complex-valued inputs, there's no auto-convert to floating-point.
# We only support a limited subset of functions, which the user can extend
# as needed.
# TODO: Should have a macro for making functions accept Fixed input.
for fun in (:abs, :cos, :sin, :exp, :log, :isinf, :isfinite, :isnan)
    @eval Base.$fun(::FixedNumber{X}) where {X} = Base.$fun(X)
end

# Other functions that do not already work
Base.:(<<)(::FixedInteger{X}, y::UInt64) where {X} = X << y
Base.:(>>)(::FixedInteger{X}, y::UInt64) where {X} = X >> y

# Two-argument functions that have methods in promotion.jl that give no_op_err:
for f in (:+, :-, :*, :/, :^)
    @eval Base.$f(::Fixed{X}, ::Fixed{X}) where {X} = $f(X,X)
end
# ...where simplifications are possible:
Base.:&(::FixedInteger{X}, ::FixedInteger{X}) where {X} = X
Base.:|(::FixedInteger{X}, ::FixedInteger{X}) where {X} = X
Base.:xor(::FixedInteger{X}, ::FixedInteger{X}) where {X} = zero(X)
Base.:<(::Fixed{X}, ::Fixed{X}) where {X} = false
Base.:<=(::Fixed{X}, ::Fixed{X}) where {X} = true
Base.:rem(::Fixed{X}, ::Fixed{X}) where {X} = zero(X)
Base.:mod(::Fixed{X}, ::Fixed{X}) where {X} = zero(X)

# Three-argument function that gives no_op_err
fma(x::Fixed{X}, y::Fixed{X}, z::Fixed{X}) where {X} = fma(X,X,X)

# Fixed powers using Base.literal_pow.
# This avoids DomainError in some cases?
for T in (Int32, Int64, Float32, Float64, ComplexF32, ComplexF64)
    Base.:^(x::T, ::FixedInteger{p}) where {p} = Base.literal_pow(^, x, Val(p))
end
Base.:^(x::Fixed{X}, ::FixedInteger{p}) where {X,p} = Base.literal_pow(^, X, Val(p))
Base.:^(x::Fixed{X}, ::FixedInteger{X}) where {X} = Base.literal_pow(^, X, Val(X)) #disambig

# For brevity, all `Fixed` numbers are displayed as `Fixed(X)`, rather than, for
# example, `FixedInteger{X}()`. It is possible to discern between the different
# types of `Fixed` by looking at `X`.
# To get the default behaviour back, run:
#   methods(Base.show, (IO, Fixed{X} where X)) |> first |> Base.delete_method
function Base.show(io::IO, x::Fixed{X}) where X
    print(io, "Fixed(")
    show(io, X)
    print(io, ")")
end

"""
fix(x, y1, y2, ...)
Test if a number `x` is equal to any of the `Fixed` numbers `y1`, `y2`, ...,
and in that case return the fixed number. Otherwise, or if
`x` is already a `Fixed` number, it is also returned unchanged.

This function can be used to call specialized methods for certain input values.
For example, `f(x, fix(y, Fixed(0)))` will call `f(x, y)` if `y` is nonzero, but
`f(x, Fixed(0)) if y is zero. This is useful if it enables optimizations that
outweigh the cost of branching.
"""
@inline fix(x::Number) = x
@inline fix(x::Number, y::Fixed, ys::Fixed...) = x == y ? y : fix(x, ys...)
@inline fix(x::Fixed, ys::Fixed...) = x # shortcut
@inline fix(x::Number, ys::Number...) = fix(x, map(Fixed, ys)...)
# TODO: Use a tree search for long, sorted lists.

include("macros.jl")

end # module
