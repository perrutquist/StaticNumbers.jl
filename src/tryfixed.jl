export tryfixed, tofixed, ⩢, fixedmod

# TODO: tryconvert(T, x) should return convert(T, x) unless that would throw
# an error. tryconvert(FixedInteger{0}, x) would be equivalent to the current
# tryfixed(x, Fixed(0))

"""
tryfixed(x, y1, y2, ...)
x ⩢ y1 ⩢ y2 ...

Test if a number `x` is equal to any of the numbers `y1`, `y2`, ..., and in that
case return `Fixed(y)`. Otherwise, or if `x` is already a `Fixed` number, `x is
returned unchanged.

The inferred return type will typically be a small `Union`, which Julia
can handle efficiently.

This function can be used to call specialized methods for certain input values.
For example, `f(x, y ⩢ 0)` will call `f(x, y)` if `y` is nonzero, but
`f(x, Fixed(0))` if y is zero. This is useful if it enables optimizations that
outweigh the cost of branching.

NOTE: When the list of y-values is longer than one, y1, y2, ... must be `Fixed`
numbers, or inferrence will not work. (In which case `tryfixed` is not more
efficient than `Fixed(x)`.)
"""
@inline tryfixed(x::Fixed, ys::Number...) = x
@inline tryfixed(x::Fixed, y::Fixed) = x #disambig
@inline tryfixed(x::Number) = x
@inline tryfixed(x::Number, y::Fixed) = x==y ? y : x
@inline tryfixed(x::Number, y::Number) = tryfixed(x, Fixed(y))
@inline tryfixed(x::Number, y::Number, ys::Number...) = tryfixed(tryfixed(x, y), ys...)

@inline ⩢(x, y) = tryfixed(x,y)

@inline tryfixed(x::Number, t::T) where {T<:Tuple} = tryfixed(x, Fixed.(t)...)

#The numbers `y1`, `y2`, ..., or the range `r` should be such that they can be
#computed at inference. I.e. they should be constructed using literals, `Fixed`
#numbers, and other constants that are deducible from types types.

"""
tryfixed(x, r)
Tests if an integer `x` is in the range `r`, and if so, returns a `Fixed`
integer from the range. (Otherwise, `x` is returned unchanged.)

NOTE: The range must be completely fixed, or inferrence will not work.
"""
@inline tryfixed(x::FixedInteger, r::OrdinalRange{<:Integer, <:Integer}) = x
@inline tryfixed(x::Integer, r::OrdinalRange{<:Integer, <:Integer}) =
    tryfixed(x::Integer, FixedStepRange(Fixed(zeroth(r)), Fixed(step(r)), Fixed(length(r))))
@inline tryfixed(x::Integer, r::FixedStepRange{<:Integer, <:FixedInteger, <:FixedInteger, <:FixedInteger}) = x in r ? tofixed(x, r) : x
@inline tryfixed(x::Integer, r::UnitRange{<:Integer}) =
    tryfixed(x::Integer, FixedUnitRange(Fixed(zeroth(r)), Fixed(length(r))))
@inline tryfixed(x::Integer, r::FixedUnitRange{<:Integer, <:FixedInteger, <:FixedInteger}) = x in r ? tofixed(x, r) : x

#@inline tofixed(x::Integer, r::StepRange) = tofixed(x, FixedStepRange(Fixed(zeroth(r)), Fixed(step(r)), Fixed(lenght(r)))
#@inline tofixed(x::Integer, r::UnitRange) = tofixed(x, FixedUnitRange(Fixed(zeroth(r)), Fixed(lenght(r)))

"""
tofixed(x, r)
Returns a `Fixed` integer, equal to `x` from the range `r`. If no element in
`r` is equal to `x`, then the behaviour of this function is undefined.
"""
@inline tofixed(x::Fixed, r::OrdinalRange{<:Integer, <:Integer}) = x

function tofixedexpr(z, s, l, blk=identity, x=:x)
    if l<=1
        blk(:( FixedInteger{$(z+s)}() ))
    else
        mid = l÷2
        :( $(s>0 ? :(<=) : :(>=))($x, $(z + s*mid)) ? $(tofixedexpr(z, s, mid, blk, x)) : $(tofixedexpr(z+mid*s, s, l-mid, blk, x)) )
    end
end

@generated function tofixed(x::Integer, r::FixedStepRange{<:Integer, FixedInteger{Z}, FixedInteger{S}, FixedInteger{L}}) where {Z, S, L}
    quote
        Base.@_inline_meta
        $(tofixedexpr(Z, S, L))
    end
end
@generated function tofixed(x::Integer, r::FixedUnitRange{<:Integer, FixedInteger{Z}, FixedInteger{L}}) where {Z, L}
    quote
        Base.@_inline_meta
        $(tofixedexpr(Z, 1, L))
    end
end

# do-constructs are probably not worth the effort
# @generated function tofixed(f, x::Integer, r::FixedUnitRange{<:Integer, FixedInteger{Z}, FixedInteger{L}}) where {Z, L}
#     quote
#         Base.@_inline_meta
#         $(tofixedexpr(Z, 1, L, y->:(f($y))))
#     end
# end
# @generated function tofixed(f, x::Integer, r::FixedStepRange{<:Integer, FixedInteger{Z}, FixedInteger{S}, FixedInteger{L}}) where {Z, S, L}
#     quote
#         Base.@_inline_meta
#         $(tofixedexpr(Z, S, L, y->:(f($y))))
#     end
# end

# # TODO: Make cleaner macro interface!
# macro tofixed(x::Symbol, n::Int, blk)
#     xx = esc(x)
#     xblk = esc(blk)
#     FixedNumbers.tofixedexpr(0, 1, n, y->:( let $xx=$y; $xblk; end), xx)
# end

"""
`fixedmod(x,y)` returns `Fixed(mod(x,y))` if `y` is `Fixed`.

If `y` is not `Fixed`, then `fixedmod(x,y)` is the same as `mod(x,y)`.
"""
fixedmod(x, y) = mod(x,y)
@inline fixedmod(x::Integer, y::FixedInteger) = tofixed(mod(x,y), FixedUnitRange(Fixed(-1), y))

# do-constructs. Remove?
# fixedmod(f, x, y) = f(mod(x,y))
# @inline fixedmod(f, x::Integer, y::FixedInteger) = tofixed(f, mod(x,y), FixedUnitRange(Fixed(-1), y))
