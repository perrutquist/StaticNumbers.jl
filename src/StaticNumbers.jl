module StaticNumbers

# TODO: This module should be renamed StaticNumbers, and the word `static`
# replaced by `static` everywhere.

export Static, static,
       StaticInteger, StaticReal, StaticNumber, StaticOrInt, StaticOrBool,
       @staticnumbers, ofstatictype

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

# Functions that take only `Int` may be too restrictive.
# The StaticOrInt type union is often a better choice.
const StaticOrInt = Union{StaticInteger, Int}

# Promotion
Base.promote_rule(::Type{<:Static{X}}, ::Type{<:Static{X}}) where {X} =
    typeof(X)
Base.promote_rule(::Type{<:AbstractIrrational}, ::Type{<:Static{X}}) where {X} =
    promote_type(Float64, typeof(X))

# We need to override promote and promote_typeof because they don't even call
# promote_rule for all-same types.
for T in (StaticInteger, StaticReal, StaticNumber)
    @eval Base.promote(::$T{X}, ys::$T{X}...) where {X} = ntuple(i->X, 1+length(ys))
    @eval Base.promote_type(::Type{$T{X}}, ::Type{$T{X}}) where {X} = typeof(X)
    @eval Base.promote_typeof(::$T{X}, ::$T{X}...) where {X} = typeof(X)
    # To avoid infinite recursion, we need this:
    @eval Base.promote_type(::Type{$T{X}}, S::Type...) where {X} = promote_type(typeof(X), promote_type(S...))
end

Base.promote_rule(::Type{<:Static{X}}, ::Type{<:Static{Y}}) where {X,Y} =
    promote_type(typeof(X),typeof(Y))

Base.promote_rule(::Type{<:Static{X}}, ::Type{T}) where {X,T<:Number} =
    promote_type(typeof(X), T)


# Bool has a special rule that we need to override?
#Base.promote_rule(::Type{Bool}, ::Type{StaticInteger{X}}) where {X} = promote_type(Bool, typeof(X))

(::Type{T})(::Static{X}) where {T<:Number,X} = T(X)
Base.BigInt(::Static{X}) where {X} = BigInt(X)

"ofstatictype(x,y) - like oftype(x,y), but return a `Static` `x` is a `Static`."
ofstatictype(::Static{X}, y) where {X} = static(oftype(X, y))
ofstatictype(x, y) = oftype(x, y)

# TODO: Constructors to avoid Static{Static}

# Some of the more common constructors that do not default to `convert`
# TODO:  We should have a (::Type{T})(x::Static) where {T<:Number} constructor
# instead of all of these.
# Need to figure out a way to avoid ambiguities.
for T in (:Bool, :Int32, :UInt32, :Int64, :UInt64, :Int128, :Integer)
    @eval Base.$T(::StaticInteger{X}) where X = $T(X)
end
(::Type{T})(x::Union{StaticReal{X}, StaticInteger{X}}) where {T<:AbstractFloat, X} = T(X)
(::Type{T})(x::Static{X}) where {T<:Complex, X} = T(X)
for T in (:ComplexF32, :ComplexF64, :Complex)
    @eval Base.$T(::Static{X}) where X = $T(X)
end
Rational{T}(::Union{StaticInteger{X}, StaticReal{X}}) where {T<:Integer,X} = Rational{T}(X)
Complex{T}(::Static{X}) where {T<:Real,X} = Complex{T}(X)
# big(x) still defaults to convert.

# Single-argument functions that do not already work.
# Note: We're not attempting to support every function in Base.
# TODO: Should have a macro for easily extending support.
for fun in (:-, :zero, :one, :oneunit, :trailing_zeros, :widen, :decompose)
    @eval Base.$fun(::Static{X}) where X = Base.$fun(X)
end
for fun in (:trunc, :floor, :ceil, :round)
    @eval Base.$fun(::Union{StaticReal{X}, StaticNumber{X}}) where {X} = Base.$fun(X)
end
for fun in (:zero, :one, :oneunit)
    @eval Base.$fun(::Type{<:Static{X}}) where {X} = Base.$fun(typeof(X))
end

# It's a pity there's no AbstractBool supertype.
const StaticBool = Union{StaticInteger{false}, StaticInteger{true}}
const StaticOrBool = Union{StaticBool, Bool}
Base.:!(x::StaticBool) = !Bool(x)

# For complex-valued inputs, there's no auto-convert to floating-point.
# We only support a limited subset of functions, which the user can extend
# as needed.
# TODO: Should have a macro for making functions accept Static input.
for fun in (:abs, :cos, :sin, :exp, :log, :isinf, :isfinite, :isnan)
    @eval Base.$fun(::StaticNumber{X}) where {X} = Base.$fun(X)
end

# Other functions that do not already work
Base.:(<<)(::StaticInteger{X}, y::UInt64) where {X} = X << y
Base.:(>>)(::StaticInteger{X}, y::UInt64) where {X} = X >> y

# Two-argument functions that have methods in promotion.jl that give no_op_err:
for f in (:+, :-, :*, :/, :^)
    @eval Base.$f(::Static{X}, ::Static{X}) where {X} = $f(X,X)
end
# ...where simplifications are possible:
Base.:&(::StaticInteger{X}, ::StaticInteger{X}) where {X} = X
Base.:|(::StaticInteger{X}, ::StaticInteger{X}) where {X} = X
Base.:xor(::StaticInteger{X}, ::StaticInteger{X}) where {X} = zero(X)
Base.:<(::Static{X}, ::Static{X}) where {X} = false
Base.:<=(::Static{X}, ::Static{X}) where {X} = true
Base.:rem(::Static{X}, ::Static{X}) where {X} = zero(X)
Base.:mod(::Static{X}, ::Static{X}) where {X} = zero(X)

# Three-argument function that gives no_op_err
fma(x::Static{X}, y::Static{X}, z::Static{X}) where {X} = fma(X,X,X)

# Static powers using Base.literal_pow.
# This avoids DomainError in some cases?
for T in (Int32, Int64, Float32, Float64, ComplexF32, ComplexF64, Irrational)
    Base.:^(x::T, ::StaticInteger{p}) where {p} = Base.literal_pow(^, x, Val(p))
end
Base.:^(x::Static{X}, ::StaticInteger{p}) where {X,p} = Base.literal_pow(^, X, Val(p))
Base.:^(x::Static{X}, ::StaticInteger{X}) where {X} = Base.literal_pow(^, X, Val(X)) #disambig

# ntuple accepts Val, so it should also accept static
@inline Base.ntuple(f::F, ::StaticInteger{N}) where {F,N} = Base.ntuple(f, Val(N))

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

include("macros.jl")

include("LengthRanges.jl")

include("trystatic.jl")

end # module
