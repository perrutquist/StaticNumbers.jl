module StaticNumbers

using Requires
import Base.Broadcast: broadcasted, DefaultArrayStyle

export Static, static,
       StaticBool, StaticInteger, StaticReal, StaticNumber, StaticOrInt, StaticOrBool,
       @staticnumbers, @generate_static_methods, ofstatictype

function __init__()
    @require StaticArrays="90137ffa-7385-5640-81b9-e52037218182" include("StaticArrays_glue.jl")
end

const StaticError = ErrorException("Illegal type parameter for Static.")

"""
A `StaticInteger` is an `Integer` whose value is stored in the type, and which
contains no runtime data.
"""
struct StaticInteger{X} <: Integer
    function StaticInteger{X}() where {X}
        X isa Integer && !(X isa Static) && isimmutable(X) || throw(StaticError)
        new{X}()
    end
end
StaticInteger{X}(x::Number) where {X} = x==X ? StaticInteger{X}() : throw(InexactError(:StaticInteger, StaticInteger{X}, x))
Base.@pure StaticInteger(x::Number) = StaticInteger{Integer(x)}()

"""
A `StaticReal` is a `Real` whose value is stored in the type, and which
contains no runtime data.
"""
struct StaticReal{X} <: Real
    function StaticReal{X}() where {X}
        X isa Real && !(X isa Integer) && !(X isa Static) && isimmutable(X) || throw(StaticError)
        new{X}()
    end
end
StaticReal{X}(x::Number) where {X} = x==X ? StaticReal{X}() : throw(InexactError(:StaticReal, StaticReal{X}, x))
Base.@pure StaticReal(x::Number) = StaticReal{Real(x)}()

"""
A `StaticNumber` is a `Number` whose value is stored in the type, and which
contains no runtime data.
"""
struct StaticNumber{X} <: Number
    function StaticNumber{X}() where {X}
        X isa Number && !(X isa Real) && !(X isa Static) && isimmutable(X) || throw(StaticError)
        new{X}()
    end
end
StaticNumber{X}(x::Number) where {X} = x==X ? StaticNumber{X}() : throw(InexactError(:StaticNumber, StaticReal{X}, x))
Base.@pure StaticNumber(x::Number) = StaticNumber{x}()

"""
`Static{X}` is short-hand for the `Union` of `StaticInteger{X}`, `StaticReal{X}`
and `StaticNumber{X}`.
"""
const Static{X} = Union{StaticInteger{X}, StaticReal{X}, StaticNumber{X}}

# We'll define this constructor, but not recommend it.
Static{X}() where X = static(X)

# This is the recommended constructor.
"""
`static(X)` is shorthand for `StaticInteger{X}()`, `StaticReal{X}()` or `StaticNumber{X}()`,
depending on the type of `X`.
"""
static(x::StaticInteger) = x
static(x::StaticReal) = x
static(x::StaticNumber) = x
Base.@pure static(x::Integer) = StaticInteger(x)
Base.@pure static(x::Real) = StaticReal(x)
Base.@pure static(x::Number) = StaticNumber(x)
static(x::Irrational) = x # These are already defined by their type.
Base.@pure static(x::Bool) = x ? StaticInteger{true}() : StaticInteger{false}() # help inference

# There's no point crating a Val{Static{X}} since any function that would accept
# it should treat it as equivalent to Val{X}.
Base.@pure Base.Val(::Static{X}) where X = Val(X)

# # Helper function that returns the tuple (1, 2, ..., n)
# Base.@pure _tuple_to(n::Int) = n <= 0 ? () : (_tuple_to(n-1)..., n)
#
# # This does the same thing as Base.ntuple, but implemnts it differently.
# # Commented out, because benchmarks showeed regressions.
# @inline ntuple(f::F, ::Val{N}) where {F,N} = map(f, _tuple_to(Int(N)))
# @inline ntuple(f::F, ::StaticInteger{N}) where {F,N} = map(f, _tuple_to(Int(N)))
# @inline ntuple(f::F, n::Int) where {F} = map(f, _tuple_to(n))

@inline Base.ntuple(f::F, ::StaticInteger{N}) where {F,N} = ntuple(f, Val(N))

# Functions that take only `Int` may be too restrictive.
# The StaticOrInt type union is often a better choice.
const StaticOrInt = Union{StaticInteger, Int}

# Promotion
# We need to override promote and promote_typeof because they don't even call
# promote_rule for all-same types.
Base.promote(::ST, ys::ST...) where {ST <: Static{X}} where {X} = ntuple(i->X, static(1+length(ys)))
Base.promote_type(::Type{ST}, ::Type{ST})  where {ST <: Static{X}} where {X} = typeof(X)
Base.promote_typeof(::ST, ::ST...) where {ST <: Static{X}} where {X} = typeof(X)

# To avoid infinite recursion, we need this:
Base.promote_type(::Type{<:Static{X}}, T::Type...) where {X} = promote_type(typeof(X), promote_type(T...))

# Loop over all three types specifically, instead of dispatching on the Union.
for ST in (StaticInteger, StaticReal, StaticNumber)
    Base.promote_rule(::Type{ST{X}}, ::Type{T}) where {X,T<:Number} = promote_type(typeof(X), T)

    # Constructors
    (::Type{Complex{T}})(::ST{X}) where {T<:Real, X} = Complex{T}(X)
    (::Type{Rational{T}})(::ST{X}) where {T<:Integer, X} = Rational{T}(X)
end

"ofstatictype(x,y) - like oftype(x,y), but return a `Static` `x` is a `Static`."
ofstatictype(::Static{X}, y) where {X} = static(oftype(X, y))
ofstatictype(x, y) = oftype(x, y)

# TODO: Constructors to avoid Static{Static}

# Some of the more common constructors that do not default to `convert`
# Note:  We cannot have a (::Type{T})(x::Static) where {T<:Number} constructor
# instead of all of these, because of ambiguities with user-defined types.

for T in (:Bool, :Int32, :UInt32, :Int64, :UInt64, :Int128, :BigInt, :Unsigned, :Integer)
    @eval Base.$T(::StaticInteger{X}) where X = $T(X)
end
(::Type{T})(x::Union{StaticReal{X}, StaticInteger{X}}) where {T<:AbstractFloat, X} = T(X)

# big(x) still defaults to convert.

# Single-argument functions that do not already work.
# Note: We're not attempting to support every function in Base.
# TODO: Should have a macro for easily extending support.
for fun in (:-, :zero, :one, :oneunit, :trailing_zeros, :decompose)
    @eval Base.$fun(::Static{X}) where X = Base.$fun(X)
end
for fun in (:trunc, :floor, :ceil, :round, :isnan)
    @eval Base.$fun(::Union{StaticReal{X}, StaticNumber{X}}) where {X} = Base.$fun(X)
end
for fun in (:zero, :one, :oneunit)
    @eval Base.$fun(::Type{<:Static{X}}) where {X} = Base.$fun(typeof(X))
end
Base.widen(::Static{X}) where {X} = X isa Bool ? X : widen(X)

# It's a pity there's no AbstractBool supertype.
const StaticBool = Union{StaticInteger{false}, StaticInteger{true}}
const StaticOrBool = Union{StaticBool, Bool}

Base.:!(x::StaticBool) = !Bool(x)

# false is a strong zero
for T in (Integer, Real, Number, Complex{<:Real}, StaticInteger, StaticReal, StaticNumber)
    Base.:*(::StaticInteger{false}, y::T) = zero(y)
    Base.:*(x::T, ::StaticInteger{false}) = zero(x)
end
Base.:*(::StaticInteger{false}, ::StaticInteger{false}) = false # disambig

# Handle static(Inf)*false
Base.:*(x::Bool, ::StaticReal{Y}) where Y = x*Y
Base.:*(::StaticReal{X}, y::Bool) where X = X*y

# Until https://github.com/JuliaLang/julia/pull/32117 is merged
Base.:*(::StaticInteger{false}, ::AbstractIrrational) = 0.0
Base.:*(::AbstractIrrational, ::StaticInteger{false}) = 0.0

# For complex-valued inputs, there's no auto-convert to floating-point.
# We only support a limited subset of functions, which the user can extend
# as needed.
# TODO: Should have a macro for making functions accept Static input.
for fun in (:abs, :abs2, :cos, :sin, :exp, :log, :isinf, :isfinite, :isnan)
    @eval Base.$fun(::StaticNumber{X}) where {X} = Base.$fun(X)
end
Base.sign(::StaticInteger{X}) where {X} = Base.sign(X) # work around problem with Bool

# Other functions that do not already work
Base.:(<<)(::StaticInteger{X}, y::UInt64) where {X} = X << y
Base.:(>>)(::StaticInteger{X}, y::UInt64) where {X} = X >> y

# Two-argument functions that have methods in promotion.jl that give no_op_err:
for f in (:+, :-, :*, :/, :^)
    @eval Base.$f(::Static{X}, ::Static{X}) where {X} = $f(X,X)
end
# ...where simplifications are possible:
# Note: We allow creation of specific static numbers, like 1 and 0 (as an exception)
# since this cannot lead to the set of static numbers growing uncontrollably.
Base.:&(x::ST, ::ST) where {ST<:StaticInteger} = x
Base.:|(x::ST, ::ST) where {ST<:StaticInteger} = x
Base.xor(::ST, ::ST) where {ST<:StaticInteger} = static(zero(ST))
Base.rem(::ST, ::ST) where {ST<:Static{X}} where {X} = (X==0 || isinf(X)) ? X isa AbstractFloat ? static(oftype(X, NaN)) : throw(DivideError()) : static(zero(X))
Base.mod(::ST, ::ST) where {ST<:Static{X}} where {X} = (X==0 || isinf(X)) ? X isa AbstractFloat ? static(oftype(X, NaN)) : throw(DivideError()) : static(zero(X))
Base.div(::ST, ::ST) where {ST<:Static{X}} where {X} = (X == 0 || isinf(X)) ? ( X isa Integer ? throw(DivideError()) : oftype(X, NaN) ) : static(one(X)) # Needed for Julia > 1.3
Base.:<(::ST, ::ST) where {ST<:StaticReal{X}} where {X} = false
Base.:<=(::ST, ::ST) where {ST<:StaticReal{X}} where {X} = true
# Bypass promotion in comparisons involving static unsigned integers
for fun in (:(<), :(<=))
    @eval Base.$fun(::StaticInteger{X}, y::Integer) where {X} = $fun(X, y)
    @eval Base.$fun(x::Integer, ::StaticInteger{Y}) where {Y} = $fun(x, Y)
    @eval Base.$fun(::StaticInteger{X}, ::StaticInteger{Y}) where {X,Y} = $fun(X, Y)
end

# Three-argument function that gives no_op_err
Base.fma(::ST, ::ST, ::ST) where {ST<:Static{X}} where {X} = fma(X,X,X)

# Static powers using Base.literal_pow.
# This avoids DomainError in some cases?
for T in (Bool, Int32, Int64, Float32, Float64, ComplexF32, ComplexF64, Irrational)
    Base.:^(x::T, ::StaticInteger{p}) where {p} = Base.literal_pow(^, x, Val(p))
end
Base.:^(x::Static{X}, ::StaticInteger{p}) where {X,p} = Base.literal_pow(^, X, Val(p))
Base.:^(x::ST, ::ST) where {ST<:StaticInteger{X}} where {X} = Base.literal_pow(^, X, Val(X)) #disambig

# For brevity, all `Static` numbers are displayed as `static(X)`, rather than, for
# example, `StaticInteger{X}()`. It is possible to discern between the different
# types of `Static` by looking at `X`.
# To get the default behaviour back, run:
#   methods(Base.show, (IO, Static{X} where X)) |> first |> Base.delete_method
function Base.show(io::IO, x::Static{X}) where X
    print(io, "static(")
    show(io, X)
    print(io, ")")
end

# Dont promote when it's better to treat real and imaginary parts separately
Base.:/(::StaticNumber{X}, y::Real) where {X} = X/y
Base.:*(::StaticNumber{X}, y::Real) where {X} = X*y
Base.:*(x::Real, ::StaticNumber{Y}) where {Y} = x*Y

@inline function Base.setindex(x::Tuple, v, i::StaticInteger)
    @boundscheck 1 <= i <= length(x) || throw(BoundsError(x, i))
    return ntuple(n -> n == i ? v : x[n], static(length(x)))
end

include("generate_static_methods.jl")

include("LengthRanges.jl")

include("trystatic.jl")

include("stat_macro.jl")

end # module
