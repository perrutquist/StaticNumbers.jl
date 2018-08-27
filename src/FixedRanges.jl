export FixedRange, FixedOrdinalRange, FixedUnitRange, zeroth, FixedOneTo, fixedlength

const FixedRangeError = ErrorException("Not a valid FixedRange.")

"""
A `FixedOrdinalRange` is the most general type of FixedRange.
"""
struct FixedOrdinalRange{T,Z,S,L} <: OrdinalRange{T,S}
    zeroth::Z
    step::S
    length::L
    function FixedOrdinalRange{T,Z,S,L}(z::Z, s::S, l::L) where {T, Z, S, L<:Integer}
        if L<:FixedInteger
            l >= 0 || throw(FixedRangeError)
        else
            l = max(l,zero(l))::L
        end
        new{T,Z,S,L}(z,s,l)
    end
end

FixedOrdinalRange(z::Z, s::S, l::L) where {Z, S, L<:Integer} =
    FixedOrdinalRange{typeof(z+1*s), Z, S, L}(z, s, l)
FixedOrdinalRange{T,Z,S,L}() where {T, Z<:Fixed, S<:Fixed, L<:FixedInteger} =
    FixedOrdinalRange{T,Z,S,L}(Z(), S(), L())

"""
A `FixedUnitRange` is a type that is identical to `FixedOrdinalRange` but
where the step is fixed to 1. It is a subtype of UnitRange.

(It would be much simpler to just use `FixedOrdinalRange` with a `Fixed` step
but then methods that expect a `UnitRange` would not work.)
"""
struct FixedUnitRange{T,Z,L} <: AbstractUnitRange{T}
    zeroth::Z
    step::FixedInteger{1}
    length::L
    function FixedUnitRange{T,Z,L}(z::Z, l::L) where {T, Z, L<:Integer}
        if L<:FixedInteger
            l >= 0 || throw(FixedRangeError)
        else
            l = max(l,zero(l))::L
        end
        new{T,Z,L}(z, Fixed(1), l)
    end
end

FixedUnitRange{T,Z,L}() where {T, Z<:Fixed, L<:FixedInteger} =
    FixedUnitRange{T, Z, L}(Z(), L())
FixedUnitRange(z::Z, l::L) where {Z, L<:Integer} =
    FixedUnitRange{typeof(z+zero(z)), Z, L}(z, l)

"""
`FixedRange(zeroth, step, length)`

A FixedRange is a range which allows (but does not require) its `zeroth`, `step`
and/or `length` to be `Fixed` numbers.

The `zeroth` element of the range is the element before the first.
Although it is not part of the range, it is frequently useful for it
to remain fixed. For example, if the common 1:n range is multiplied by
a scalar, the zeroth element can remain `FixedInteger{0}`

The `step` is the distance between successive elements of the range.

The `length` must be an `Integer`. A `FixedRange` is parameterized by its
length, rather than its last element. This makes it possible for the length
to remain `Fixed` when an offset is added to the range.

`FixedRange` is the union of `FixedOrdinalRange` and `FixedUnitRange`
which are subtypes of `OrdinalRange` and `AbstractUnitRange` respectively.
"""
const FixedRange{T,Z,S,L} = Union{FixedOrdinalRange{T,Z,S,L}, FixedUnitRange{T,Z,L}}

# Currently having problems making the FixedRange constructors type-stable
# Work-around: Use FixedOrdinalRange or FixedUnitRange directly.

#FixedRange(z::Z, s::S, l::L) where {Z, S, L<:Integer} =
#    FixedOrdinalRange{typeof(z+0*s), Z, S, L}(z, s, l)
#FixedRange(z::Z, ::FixedInteger{1}, l::L) where {Z, L<:Integer} =
#    FixedUnitRange{typeof(z+0), Z, L}(z, l)

#FixedRange{T,Z,S,L}() where {T, Z<:Fixed, S<:Fixed, L<:FixedInteger} =
#    FixedOrdinalRange{T,Z,S,L}(Z(), S(), L())
#FixedRange{T,Z,FixedInteger{1},L}() where {T, Z<:Fixed, L<:FixedInteger} =
#    FixedUnitRange{T,Z,L}(Z(), L())

FixedRange(r::OrdinalRange) = FixedOrdinalRange(r)
FixedOrdinalRange(r::OrdinalRange) = FixedOrdinalRange(first(r)-step(r), step(r), length(r))
FixedRange(r::AbstractUnitRange) = FixedUnitRange(r)
FixedUnitRange(r::UnitRange) = FixedUnitRange(first(r)-step(r), length(r))

zeroth(r::FixedRange) = r.zeroth
zeroth(r::AbstractRange) = first(r)-step(r)

Base.step(r::FixedRange) = r.step
Base.length(r::FixedRange) = r.length
Base.unsafe_length(r::FixedRange) = r.length

# `first` and `last` return elements of the array, so type T, never `Fixed`.
Base.first(r::FixedRange{T}) where {T} = convert(T, r.zeroth + r.step)
Base.last(r::FixedRange{T}) where {T} = convert(T, r.zeroth + r.step*r.length)

function Base.getindex(r::FixedRange{T}, i::Integer)::T where {T}
    @boundscheck (0 < i <= length(r)) || Base.throw_boundserror(r, i)
    convert(T, r.zeroth + i*r.step)
end

"""
FixedOneTo{N} - Like Base.OneTo{Int}(N) but with the length fixed by the type.
"""
const FixedOneTo{N} = FixedUnitRange{Int, FixedInteger{0}, FixedInteger{N}}

Base.@pure FixedOneTo(n::Integer) = FixedOneTo{n}()
Base.@pure FixedOneTo(::FixedInteger{N}) where N = FixedOneTo{N}()

Base.OneTo(n::FixedInteger) = FixedOneTo(n)
Base.:(:)(a::FixedInteger{1}, b::FixedInteger) = FixedOneTo(b)

function Base.show(io::IO, r::FixedOneTo{N}) where N
    print(io, "FixedOneTo(", N, ")")
end

"""
`fixedlength(range)` converts to a range where the length is `Fixed`.
"""
fixedlength(r::OrdinalRange) = FixedOrdinalRange(zeroth(r), step(r), Fixed(length(r)))
fixedlength(r::OrdinalRange{<:Any, Fixed{1}}) = FixedUnitRange(zeroth(r), Fixed(length(r)))

function Base.show(io::IO, r::FixedRange{<:Integer, <:Integer, <:Integer, <:FixedInteger})
    print(io, "fixedlength(", first(r), ":")
    step(r) == 1 || print(io, step(r), ":")
    print(io, last(r), ")")
end

import Base.Broadcast: broadcasted, DefaultArrayStyle

broadcasted(::DefaultArrayStyle{1}, ::typeof(+), a::Number, r::FixedOrdinalRange) = FixedOrdinalRange(a+r.zeroth, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::FixedOrdinalRange, a::Number) = FixedOrdinalRange(a+r.zeroth, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), a::Real, r::FixedUnitRange) = FixedUnitRange(a+r.zeroth, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::FixedUnitRange, a::Real) = FixedUnitRange(a+r.zeroth, r.length)

Base.:-(r::FixedOrdinalRange) = FixedOrdinalRange(-r.zeroth, -r.step, r.length)
Base.:-(r::FixedOrdinalRange{<:Any,<:Integer,FixedInteger{-1}}) = FixedUnitRange(-r.zeroth, r.length)
Base.:-(r::FixedUnitRange) = FixedOrdinalRange(-r.zeroth, Fixed(-1), r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::FixedRange) = -r

Base.:*(a::Number, r::FixedRange) = FixedOrdinalRange(a*r.zeroth, a*r.step, r.length)
Base.:*(a::Number, r::FixedRange{<:Any, FixedInteger{0}}) = FixedOrdinalRange(Fixed(0), a*r.step, r.length)
Base.:*(r::FixedRange, a::Number) = a*r
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), a::Number,r::FixedRange) = a*r
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::FixedRange,a::Number) = a*r

Base.eachindex(r::FixedRange{<:Any, <:Any, <:Any, <:FixedInteger}) =
    FixedOneTo(r.length)
