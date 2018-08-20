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

"""
Create an expression a method for `Fixed` input to `fun` if the result
is among the `targets`
"""
function genfixedmethod1(fun, arg1, targets)
    r = try
        @eval $fun($arg1)
    catch
        return nothing
    end
    ts = filter(isequal(r), targets)
    if length(ts) == 1
        return :( $fun(::$(typeof(Fixed(arg1)))) = $(Fixed(first(ts))) )
    end
    return nothing
end

"""
Create an expression a method for two `Fixed` inputs to `fun` if the result
is among the `targets`
"""
function genfixedmethod2(fun, arg1, arg2, targets)
    r = try
        @eval $fun($arg1, $arg2)
    catch
        return nothing
    end
    ts = filter(isequal(r), targets)
    if length(ts) == 1
        return :( $fun(::$(typeof(Fixed(arg1))),::$(typeof(Fixed(arg2)))) = $(Fixed(first(ts))) )
    end
    return nothing
end

"""
Create an array of all the methods for `Fixed` 1-argument inputs to `fun`
where the result is either among the possible inputs, `args`, or among
the additional `targets`.
"""
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

"""
Create an array of all the methods for `Fixed` 2-argument inputs to `fun`
where the result is either among the possible inputs, `args`, or among
the additional `targets`.
"""
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

"""
@fixednumbers numbers 1argfuns 2argfuns

This macro creates methods that return `Fixed` numbers when
functions are called with only `Fixed` arguments.

The inputs should be a list of literal numbers that will be tested as
inputs and outputs of all functions.

Optionally a fourth argument can give a list of numbers that will only
be considered as results, but not as inputs.

Example:
```
@fixednumbers (0, 1) (sin, cos) (+, -)
```
will create all the following method definitions:
```
sin(::FixedInteger{0}) = FixedInteger{0}()
cos(::FixedInteger{0}) = FixedInteger{1}()
+(::FixedInteger{0}, ::FixedInteger{0}) = FixedInteger{0}()
+(::FixedInteger{0}, ::FixedInteger{1}) = FixedInteger{1}()
+(::FixedInteger{1}, ::FixedInteger{0}) = FixedInteger{1}()
-(::FixedInteger{0}, ::FixedInteger{0}) = FixedInteger{0}()
-(::FixedInteger{1}, ::FixedInteger{0}) = FixedInteger{0}()
-(::FixedInteger{1}, ::FixedInteger{1}) = FixedInteger{1}()
```
(Note: The macro will run in the local scope. Functions from `Base`
must be imported before they can be extended.)
"""
macro fixednumbers(args::Expr, funs1::Expr, funs2::Expr, targets::Expr=:(()))
    args.head == :tuple || error("Expected a Tuple of numbers")
    funs1.head == :tuple || error("Expected a Tuple of 1-arg functions")
    funs2.head == :tuple || error("Expected a Tuple of 2-arg functions")
    targets.head == :tuple || error("Expected a Tuple of target numbers")
    return esc(Expr(:block,vcat(
       genfixedmethods1(funs1.args, args.args, targets.args),
       genfixedmethods2(funs2.args, args.args, targets.args)
       )))
end

end # module
