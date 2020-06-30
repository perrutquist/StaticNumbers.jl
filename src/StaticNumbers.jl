module StaticNumbers

using Requires
import Base.Broadcast: broadcasted, DefaultArrayStyle

export Static, static,
       StaticBool, StaticInteger, StaticReal, StaticNumber, StaticOrInt, StaticOrBool,
       @staticnumbers, @generate_static_methods, unstatic

function __init__()
    @require StaticArrays="90137ffa-7385-5640-81b9-e52037218182" include("StaticArrays_glue.jl")
    @require SIMD="fdea26ae-647d-5447-a871-4b548cad5224" include("SIMD_glue.jl")
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
@inline StaticInteger{X}(x::Number) where {X} = x==X ? StaticInteger{X}() : throw(InexactError(:StaticInteger, StaticInteger{X}, x))
@inline StaticInteger(x::Number) = StaticInteger{Integer(x)}()

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
@inline StaticReal{X}(x::Number) where {X} = x==X ? StaticReal{X}() : throw(InexactError(:StaticReal, StaticReal{X}, x))
@inline StaticReal(x::Number) = StaticReal{Real(x)}()

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
@inline StaticNumber{X}(x::Number) where {X} = x==X ? StaticNumber{X}() : throw(InexactError(:StaticNumber, StaticReal{X}, x))
@inline StaticNumber(x::Number) = StaticNumber{x}()

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
@inline static(x::Integer) = StaticInteger{x}()
@inline static(x::Real) = StaticReal{x}()
@inline static(x::Number) = StaticNumber{x}()
@inline static(x::Bool) = x ? StaticInteger{true}() : StaticInteger{false}() # help inference
@inline static(x::StaticInteger) = x
@inline static(x::StaticReal) = x
@inline static(x::StaticNumber) = x

Base.Irrational{X}(::StaticReal{Y}) where {X, Y} = Irrational{X}(Y)

# There's no point crating a Val{Static{X}} since any function that would accept
# it should treat it as equivalent to Val{X}.
# But maybe it is a bad idea to add a method to a @pure function?
# @inline Base.Val(::Static{X}) where X = Val(X)

"""
`StaticNumbers.map(f, t::Tuple)` works the same as `Base.map(f, t)`, but does not stop
in-lining after a certain tuple length. Long tuples will cause code blowup.
"""
# Code copied from julia/base/tuples.jl and slightly modified
@inline map(f::F, t::Tuple{}) where {F} = ()
@inline map(f::F, t::Tuple) where {F} = (f(t[1]), map(f, Base.tail(t))...)
@inline map(f::F, t::Tuple{}, s::Tuple{}) where {F} = ()
@inline map(f::F, t::Tuple, s::Tuple) where {F} = (f(t[1],s[1]), map(f, Base.tail(t), Base.tail(s))...)
@inline heads(ts::Tuple...) = map(first, ts)
@inline tails(ts::Tuple...) = map(Base.tail, ts)
@inline map(f, ::Tuple{}...) = ()
@inline map(f::F, t1::Tuple{Any,Vararg{Any,N}}, t2::Tuple{Any,Vararg{Any,N}}, ts::Tuple{Any,Vararg{Any,N}}...) where {F,N} = (f(heads(t1, t2, ts...)...), map(f, tails(t1, t2, ts...)...)...)
@inline map(args...) = Base.map(args...) # fallback for non-tuples

"""
`ntuple(f, static(n))` yields the tuple `(f(static(1)), f(static(2)), ..., f(static(n)))`.
This will ususally yield the same result as `ntuple(f, n)` but may make the compiler try
even harder to constant-propagate the integer input into `f`.
"""
# Recursive version only worked for tuple lengths up to 32, so resorting to @generated.
@generated function Base.ntuple(f::F, ::StaticInteger{N}) where {F,N}
    N::Int
    (N >= 0) || throw(ArgumentError(string("tuple length should be ≥ 0, got ", N)))
    v = Expr(:tuple, [ :( f(StaticInteger{$i}()) ) for i in 1:N ]... )
    :( Base.@_inline_meta; $v )
end

"""
`StaticOrInt` is the type union of `Int` and `StaticInteger`

(Functions that take only `Int` may be too restrictive.)
"""
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

# TODO: Constructors to avoid Static{Static}

# Some of the more common constructors that do not default to `convert`
# Note:  We cannot have a (::Type{T})(x::Static) where {T<:Number} constructor
# instead of all of these, because of ambiguities with user-defined types.
for T in (:Bool, :Integer, :AbstractFloat, :Unsigned, :Signed,
          :BigInt, :Int128, :Int16, :Int32, :Int64, :Int8,
          :UInt128, :UInt16, :UInt32, :UInt64, :UInt8,
          :BigFloat, :Float16, :Float32, :Float64)
    @eval Base.$T(::Static{X}) where {X} = $T(X)
end

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
"StaticBool is a shorthand for Union{StaticInteger{false}, StaticInteger{true}}"
const StaticBool = Union{StaticInteger{false}, StaticInteger{true}}
"StaticOrBool can be either a `Bool` or a `StaticBool`"
const StaticOrBool = Union{StaticBool, Bool}

Base.:!(x::StaticBool) = !Bool(x)

# false is a strong zero
for T in (Integer, Rational, Real, Number, Complex{<:Real}, Complex{Bool}, StaticInteger, StaticReal, StaticNumber)
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
    @eval Base.$f(::ST, ::ST) where {ST <: Static{X}} where {X} = $f(X,X)
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
    @eval Base.$fun(::StaticInteger{X}, y::BigInt) where {X} = $fun(X, y)
    @eval Base.$fun(x::BigInt, ::StaticInteger{Y}) where {Y} = $fun(x, Y)
    @eval Base.$fun(::StaticInteger{X}, ::StaticInteger{Y}) where {X,Y} = $fun(X, Y)
end

Base.:(==)(x::AbstractIrrational, ::StaticReal{Y}) where {Y} = x == Y
Base.:(==)(::StaticReal{X}, y::AbstractIrrational) where {X} = X == y

# Three-argument function that gives no_op_err
Base.fma(::ST, ::ST, ::ST) where {ST<:Static{X}} where {X} = fma(X,X,X)

# Static powers using Base.literal_pow.
# This avoids DomainError in some cases?
for T in (Bool, Int32, Int64, Float32, Float64, ComplexF32, ComplexF64, Irrational)
    Base.:^(x::T, ::StaticInteger{p}) where {p} = Base.literal_pow(^, x, Val(p))
end
Base.:^(x::Static{X}, ::StaticInteger{p}) where {X,p} = Base.literal_pow(^, X, Val(p))
Base.:^(x::ST, ::ST) where {ST<:StaticInteger{p}} where {p} = Base.literal_pow(^, x, Val(p)) #disambig
Base.:^(::Irrational{:ℯ}, p::StaticInteger) = exp(p) #disambig

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

for f in (:isfinite, :isnan, :isinf)
    @eval Base.$f(::StaticReal{X}) where {X} = $f(X)
end

# Dont promote when it's better to treat real and imaginary parts separately
Base.:/(::StaticNumber{X}, y::Real) where {X} = X/y
Base.:*(::StaticNumber{X}, y::Real) where {X} = X*y
Base.:*(x::Real, ::StaticNumber{Y}) where {Y} = x*Y

@inline function Base.setindex(x::Tuple, v, i::StaticInteger)
    @boundscheck 1 <= i <= length(x) || throw(BoundsError(x, i))
    return ntuple(n -> n == i ? v : x[n], static(length(x)))
end

Base.oftype(::Static{X}, y) where {X} = oftype(X, y)

"""
`unstatic(x)` returns a non-static version of `x`.
This function is rarely needed, as most operations on a static number (e.g. `x+0`)
will yield a non-static result.
"""
unstatic(x) = x
unstatic(::Static{X}) where {X} = X

include("generate_static_methods.jl")

include("stat_macro.jl")

include("LengthRanges.jl")

include("trystatic.jl")

include("deprecated.jl")

end # module
