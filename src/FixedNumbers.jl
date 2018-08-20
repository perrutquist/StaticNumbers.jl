module FixedNumbers

export Fixed, FixedInteger, FixedReal, FixedNumber, @fixednumbers

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

"""
`Fixed(X)` is shorthand for `FixedInteger{X}()`, `FixedReal{X}()` or `FixedNumber{X}()`,
depending on the type of `X`.
"""
Fixed(X::Integer) = FixedInteger{X}()
Fixed(X::Real) = FixedReal{X}()
Fixed(X::Number) = FixedNumber{X}()
Fixed(X::Fixed) = X

Base.promote_rule(::Type{<:Fixed{X}}, ::Type{<:Fixed{Y}}) where {X,Y} =
    promote_type(typeof(X),typeof(Y))

Base.promote_rule(::Type{<:Fixed{X}}, ::Type{T}) where {X,T<:Number} =
    promote_type(typeof(X), T)

Base.convert(T::Type{<:Fixed{X}}, y::Number) where {X} = X == y ? T() : InexactError(:convert, T, y)

Base.convert(::Type{T}, ::Fixed{X}) where {T<:Number,X} = convert(T, X)

# TODO: Constructors to avoid Fixed{Fixed}

# Some of the more common constructors that do not default to `convert`
for T in (:Bool, :Int32, :UInt32, :Int64, :UInt64, :Int128, :Integer)
    @eval Base.$T(::FixedInteger{X}) where X = $T(X)
end
for T in (:Float32, :Float64, :AbstractFloat)
    @eval Base.$T(::Union{FixedInteger{X}, FixedReal{X}}) where X = $T(X)
end
for T in (:ComplexF32, :ComplexF64, :Complex)
    @eval Base.$T(::Fixed{X}) where X = $T(X)
end
# big(x) still defaults to convert.

function genfixedmethod1(fun, arg1, targets)
    r = @eval $fun($arg1)
    ts = filter(isequal(r), targets)
    if length(ts) == 1
        return :( $fun(::$(typeof(Fixed(arg1)))) = $(Fixed(first(ts))) )
    end
    return nothing
end

function genfixedmethod2(fun, arg1, arg2, targets)
    r = @eval $fun($arg1, $arg2)
    ts = filter(isequal(r), targets)
    if length(ts) == 1
        return :( $fun(::$(typeof(Fixed(arg1))),::$(typeof(Fixed(arg2)))) = $(Fixed(first(ts))) )
    end
    return nothing
end

function genfixedmethods1(funs, args, targets)
    m = Vector{Expr}()
    tu = union(args, targets)
    for f in funs
        for a in args
            e = genfixedmethod1(f, a, tu)
            if e isa Expr
                push!(m, e)
            end
        end
    end
    return m
end

function genfixedmethods2(funs, args, targets)
    m = Vector{Expr}()
    tu = union(args, targets)
    for f in funs
        for a1 in args
            for a2 in args
                e = genfixedmethod2(f, a1, a2, tu)
                if e isa Expr
                    push!(m, e)
                end
            end
        end
    end
    return m
end

macro fixednumbers(args::Expr, funs1::Expr, funs2::Expr, targets::Expr=:(()))
    args.head == :tuple || error("Expected a Tuple of numbers")
    funs1.head == :tuple || error("Expected a Tuple of 1-arg functions")
    funs2.head == :tuple || error("Expected a Tuple of 2-arg functions")
    targets.head == :tuple || error("Expected a Tuple of target numbers")
    return esc(Expr(:block,vcat(
       genfixedmethods1(funs1.args, args.args, targets.args),
       genfixedmethods2(funs2.args, args.args, targets.args))))
end

end # module
