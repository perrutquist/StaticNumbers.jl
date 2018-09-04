module FixedNumbers

export Fixed, FixedInteger, FixedReal, FixedNumber, FixedOrInt, FixedOrBool,
       @fixednumbers, tryfixed, offixedtype, ⩢, fixedmod

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

Base.@pure FixedInteger(x::Integer) = FixedInteger{x}()

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

Base.@pure FixedReal(x::Real) = FixedReal{x}()

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

Base.@pure FixedNumber(x::Number) = FixedNumber{x}()

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

# Functions that take only `Int` may be too restrictive.
# The FixedOrInt type union is often a better choice.
const FixedOrInt = Union{FixedInteger, Int}

# Promotion
Base.promote_rule(::Type{<:Fixed{X}}, ::Type{<:Fixed{X}}) where {X} =
    typeof(X)
Base.promote_rule(::Type{<:AbstractIrrational}, ::Type{<:Fixed{X}}) where {X} =
    promote_type(Float64, typeof(X))

# We need to override promote and promote_typeof because they don't even call
# promote_rule for all-same types.
for T in (FixedInteger, FixedReal, FixedNumber)
    @eval Base.promote(::$T{X}, ys::$T{X}...) where {X} = ntuple(i->X, 1+length(ys))
    @eval Base.promote_type(::Type{$T{X}}, ::Type{$T{X}}) where {X} = typeof(X)
    @eval Base.promote_typeof(::$T{X}, ::$T{X}...) where {X} = typeof(X)
    # To avoid infinite recursion, we need this:
    @eval Base.promote_type(::Type{$T{X}}, S::Type...) where {X} = promote_type(typeof(X), promote_type(S...))
end

Base.promote_rule(::Type{<:Fixed{X}}, ::Type{<:Fixed{Y}}) where {X,Y} =
    promote_type(typeof(X),typeof(Y))

Base.promote_rule(::Type{<:Fixed{X}}, ::Type{T}) where {X,T<:Number} =
    promote_type(typeof(X), T)


# Bool has a special rule that we need to override?
#Base.promote_rule(::Type{Bool}, ::Type{FixedInteger{X}}) where {X} = promote_type(Bool, typeof(X))

function Base.convert(T::Type{<:Fixed{X}}, y::Number) where {X}
    X == y || throw(InexactError(:convert, T, y))
    return T()
end

Base.convert(::Type{T}, ::Fixed{X}) where {T<:Number,X} = convert(T, X)

"offixedtype(x,y) - like oftype(x,y), but return a `Fixed` `x` is a `Fixed`."
offixedtype(::Fixed{X}, y) where {X} = Fixed(oftype(X, y))
offixedtype(x, y) = oftype(x, y)

# TODO: Constructors to avoid Fixed{Fixed}

# Some of the more common constructors that do not default to `convert`
# TODO:  We should have a (::Type{T})(x::Fixed) where {T<:Number} constructor
# instead of all of these.
# Need to figure out a way to avoid ambiguities.
for T in (:Bool, :Int32, :UInt32, :Int64, :UInt64, :Int128, :Integer)
    @eval Base.$T(::FixedInteger{X}) where X = $T(X)
end
(::Type{T})(x::Union{FixedReal{X}, FixedInteger{X}}) where {T<:AbstractFloat, X} = T(X)
(::Type{T})(x::Fixed{X}) where {T<:Complex, X} = T(X)
for T in (:ComplexF32, :ComplexF64, :Complex)
    @eval Base.$T(::Fixed{X}) where X = $T(X)
end
Rational{T}(::Union{FixedInteger{X}, FixedReal{X}}) where {T,X} = Rational{T}(X)
Complex{T}(::Fixed{X}) where {T,X} = Complex{T}(X)
# big(x) still defaults to convert.

# Single-argument functions that do not already work.
# Note: We're not attempting to support every function in Base.
# TODO: Should have a macro for easily extending support.
for fun in (:-, :zero, :one, :oneunit, :trailing_zeros, :widen, :decompose)
    @eval Base.$fun(::Fixed{X}) where X = Base.$fun(X)
end
for fun in (:trunc, :floor, :ceil, :round)
    @eval Base.$fun(::Union{FixedReal{X}, FixedNumber{X}}) where {X} = Base.$fun(X)
end
for fun in (:zero, :one, :oneunit)
    @eval Base.$fun(::Type{<:Fixed{X}}) where {X} = Base.$fun(typeof(X))
end

# It's a pity there's no AbstractBool supertype.
const FixedBool = Union{FixedInteger{false}, FixedInteger{true}}
const FixedOrBool = Union{FixedBool, Bool}
Base.:!(x::FixedBool) = !Bool(x)

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

# ntuple accepts Val, so it should also accept fixed
@inline Base.ntuple(f::F, ::FixedInteger{N}) where {F,N} = Base.ntuple(f, Val(N))

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

include("macros.jl")

include("FixedRanges.jl")

include("tryfixed.jl")

end # module
