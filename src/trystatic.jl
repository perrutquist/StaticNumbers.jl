export trystatic, tostatic, ⩢, staticmod

# TODO: tryconvert(T, x) should return convert(T, x) unless that would throw
# an error. tryconvert(StaticInteger{0}, x) would be equivalent to the current
# trystatic(x, static(0))

"""
trystatic(x, y1, y2, ...)
x ⩢ y1 ⩢ y2 ...

Test if a number `x` is equal to any of the numbers `y1`, `y2`, ..., and in that
case return `static(y)`. Otherwise, or if `x` is already a `Static` number, `x is
returned unchanged.

The inferred return type will typically be a small `Union`, which Julia
can handle efficiently.

This function can be used to call specialized methods for certain input values.
For example, `f(x, y ⩢ 0)` will call `f(x, y)` if `y` is nonzero, but
`f(x, static(0))` if y is zero. This is useful if it enables optimizations that
outweigh the cost of branching.

NOTE: When the list of y-values is longer than one, y1, y2, ... must be `Static`
numbers, or inferrence will not work. (In which case `trystatic` is not more
efficient than `static(x)`.)
"""
@inline trystatic(x::Static, ys::Number...) = x
@inline trystatic(x::Static, y::Static) = x #disambig
@inline trystatic(x::Number) = x
@inline trystatic(x::Number, y::Static) = x==y ? y : x
@inline trystatic(x::Number, y::Number) = trystatic(x, static(y))
@inline trystatic(x::Number, y::Number, ys::Number...) = trystatic(trystatic(x, y), ys...)

@inline ⩢(x, y) = trystatic(x,y)

@inline trystatic(x::Number, t::T) where {T<:Tuple} = trystatic(x, static.(t)...)

#The numbers `y1`, `y2`, ..., or the range `r` should be such that they can be
#computed at inference. I.e. they should be constructed using literals, `Static`
#numbers, and other constants that are deducible from types types.

"""
trystatic(x, r)
Tests if an integer `x` is in the range `r`, and if so, returns a `Static`
integer from the range. (Otherwise, `x` is returned unchanged.)

NOTE: The range must be completely static, or inferrence will not work.
"""
@inline trystatic(x::StaticInteger, r::OrdinalRange{<:Integer, <:Integer}) = x
@inline trystatic(x::Integer, r::OrdinalRange{<:Integer, <:Integer}) =
    trystatic(x::Integer, LengthStepRange(static(zeroth(r)), static(step(r)), static(length(r))))
@inline trystatic(x::Integer, r::LengthStepRange{<:Integer, <:StaticInteger, <:StaticInteger, <:StaticInteger}) = x in r ? tostatic(x, r) : x
@inline trystatic(x::Integer, r::UnitRange{<:Integer}) =
    trystatic(x::Integer, LengthUnitRange(static(zeroth(r)), static(length(r))))
@inline trystatic(x::Integer, r::LengthUnitRange{<:Integer, <:StaticInteger, <:StaticInteger}) = x in r ? tostatic(x, r) : x

#@inline tostatic(x::Integer, r::StepRange) = tostatic(x, LengthStepRange(static(zeroth(r)), static(step(r)), static(lenght(r)))
#@inline tostatic(x::Integer, r::UnitRange) = tostatic(x, LengthUnitRange(static(zeroth(r)), static(lenght(r)))

"""
tostatic(x, r)
Returns a `Static` integer, equal to `x` from the range `r`. If no element in
`r` is equal to `x`, then the behaviour of this function is undefined.
"""
@inline tostatic(x::Static, r::OrdinalRange{<:Integer, <:Integer}) = x

function tostaticexpr(z, s, l, blk=identity, x=:x)
    if l<=1
        blk(:( StaticInteger{$(z+s)}() ))
    else
        mid = l÷2
        :( $(s>0 ? :(<=) : :(>=))($x, $(z + s*mid)) ? $(tostaticexpr(z, s, mid, blk, x)) : $(tostaticexpr(z+mid*s, s, l-mid, blk, x)) )
    end
end

@generated function tostatic(x::Integer, r::LengthStepRange{<:Integer, StaticInteger{Z}, StaticInteger{S}, StaticInteger{L}}) where {Z, S, L}
    quote
        Base.@_inline_meta
        $(tostaticexpr(Z, S, L))
    end
end
@generated function tostatic(x::Integer, r::LengthUnitRange{<:Integer, StaticInteger{Z}, StaticInteger{L}}) where {Z, L}
    quote
        Base.@_inline_meta
        $(tostaticexpr(Z, 1, L))
    end
end

# do-constructs are probably not worth the effort
# @generated function tostatic(f, x::Integer, r::LengthUnitRange{<:Integer, StaticInteger{Z}, StaticInteger{L}}) where {Z, L}
#     quote
#         Base.@_inline_meta
#         $(tostaticexpr(Z, 1, L, y->:(f($y))))
#     end
# end
# @generated function tostatic(f, x::Integer, r::LengthStepRange{<:Integer, StaticInteger{Z}, StaticInteger{S}, StaticInteger{L}}) where {Z, S, L}
#     quote
#         Base.@_inline_meta
#         $(tostaticexpr(Z, S, L, y->:(f($y))))
#     end
# end

# # TODO: Make cleaner macro interface!
# macro tostatic(x::Symbol, n::Int, blk)
#     xx = esc(x)
#     xblk = esc(blk)
#     StaticNumbers.tostaticexpr(0, 1, n, y->:( let $xx=$y; $xblk; end), xx)
# end

"""
`staticmod(x,y)` returns `static(mod(x,y))` if `y` is `Static`.

If `y` is not `Static`, then `staticmod(x,y)` is the same as `mod(x,y)`.
"""
staticmod(x, y) = mod(x,y)
@inline staticmod(x::Integer, y::StaticInteger) = tostatic(mod(x,y), LengthUnitRange(static(-1), y))

# do-constructs. Remove?
# staticmod(f, x, y) = f(mod(x,y))
# @inline staticmod(f, x::Integer, y::StaticInteger) = tostatic(f, mod(x,y), LengthUnitRange(static(-1), y))
