export LengthRange, LengthStepRange, LengthUnitRange, zeroth, StaticOneTo, staticlength

const LengthRangeError = ErrorException("Not a valid LengthStepRange.")

# TODO:
# LengthRange (LengthRange) should maybe be a typealias for the case where
# the length is Static (Static)?

"""
A `LengthStepRange` is a range which is parameterized by its zeroth element,
step and length (as oppsed to first, step, and last). The type of each
of the parameters is part of the range type, and hence it is possible to
use this type of range efficiently with `Static` parameters.
"""
struct LengthStepRange{T,Z,S,L} <: OrdinalRange{T,S}
    zeroth::Z
    step::S
    length::L
    function LengthStepRange{T,Z,S,L}(z::Z, s::S, l::L) where {T, Z, S, L<:Integer}
        if L<:StaticInteger
            l >= 0 || throw(unsafe_lengthRangeError)
        else
            l = max(l,zero(l))::L
        end
        new{T,Z,S,L}(z,s,l)
    end
end

LengthStepRange(z::Z, s::S, l::L) where {Z, S, L<:Integer} =
    LengthStepRange{typeof(z+1*s), Z, S, L}(z, s, l)
LengthStepRange{T}(z::Z, s::S, l::L) where {T, Z, S, L<:Integer} =
    LengthStepRange{T, Z, S, L}(z, s, l)
LengthStepRange{T,Z,S,L}() where {T, Z<:Static, S<:Static, L<:StaticInteger} =
    LengthStepRange{T,Z,S,L}(Z(), S(), L())

"""
A `LengthUnitRange` is a type that is identical to `LengthStepRange` but
where the step is fixed to 1. It is a subtype of UnitRange.

(It would be much simpler to just use `LengthStepRange` with a `Static` step of 1
but then methods that expect a `UnitRange` would not work.)
"""
struct LengthUnitRange{T,Z,L} <: AbstractUnitRange{T}
    zeroth::Z
    step::StaticInteger{1}
    length::L
    function LengthUnitRange{T,Z,L}(z::Z, l::L) where {T, Z, L<:Integer}
        if L<:StaticInteger
            l >= 0 || throw(LengthRangeError)
        else
            l = max(l,zero(l))::L
        end
        new{T,Z,L}(z, static(1), l)
    end
end

LengthUnitRange{T}(z::Z, l::L) where {T, Z, L<:Integer} =
    LengthUnitRange{T, Z, L}(z, l)
LengthUnitRange(z::Z, l::L) where {Z, L<:Integer} =
    LengthUnitRange{typeof(z+zero(z)), Z, L}(z, l)
LengthUnitRange{T,Z,L}() where {T, Z<:Static, L<:StaticInteger} =
    LengthUnitRange{T, Z, L}(Z(), L())

"""
`LengthRange(zeroth, step, length)`

A LengthRange is a range which allows (but does not require) its `zeroth`, `step`
and/or `length` to be `Static` numbers.

The `zeroth` element of the range is the element before the first.
Although it is not part of the range, it is frequently useful for it
to remain static. For example, if the common 1:n range is multiplied by
a scalar, the zeroth element can remain `StaticInteger{0}`

The `step` is the distance between successive elements of the range.

The `length` must be an `Integer`. A `LengthRange` is parameterized by its
length, rather than its last element. This makes it possible for the length
to remain `Static` when an offset is added to the range.

`LengthRange` is the union of `LengthStepRange` and `LengthUnitRange`
which are subtypes of `OrdinalRange` and `AbstractUnitRange` respectively.
"""
const LengthRange{T,Z,S,L} = Union{LengthStepRange{T,Z,S,L}, LengthUnitRange{T,Z,L}}

# Currently having problems making the LengthRange constructors type-stable
# Work-around: Use LengthStepRange or LengthUnitRange directly.

#LengthRange(z::Z, s::S, l::L) where {Z, S, L<:Integer} =
#    LengthStepRange{typeof(z+0*s), Z, S, L}(z, s, l)
#LengthRange(z::Z, ::StaticInteger{1}, l::L) where {Z, L<:Integer} =
#    LengthUnitRange{typeof(z+0), Z, L}(z, l)

#LengthRange{T,Z,S,L}() where {T, Z<:Static, S<:Static, L<:StaticInteger} =
#    LengthStepRange{T,Z,S,L}(Z(), S(), L())
#LengthRange{T,Z,StaticInteger{1},L}() where {T, Z<:Static, L<:StaticInteger} =
#    LengthUnitRange{T,Z,L}(Z(), L())

LengthRange(r::StepRange) = LengthStepRange(r)
LengthStepRange(r::StepRange) = LengthStepRange(first(r)-step(r), step(r), length(r))
LengthRange(r::AbstractUnitRange) = LengthUnitRange(r)
LengthUnitRange(r::UnitRange) = LengthUnitRange(first(r)-step(r), length(r))

@inline zeroth(r::LengthRange) = r.zeroth
@inline zeroth(r::AbstractRange) = first(r)-step(r)

@inline Base.step(r::LengthRange) = r.step
@inline Base.length(r::LengthRange) = r.length
@inline Base.unsafe_length(r::LengthRange) = r.length

# `first` and `last` return elements of the array, so are of type T, never `Static`.
@inline Base.first(r::LengthRange{T}) where {T} = convert(T, r.zeroth + r.step)
@inline Base.last(r::LengthRange{T}) where {T} = convert(T, r.zeroth + r.step*r.length)

@inline function Base.getindex(r::LengthRange{T}, i::Integer)::T where {T}
    @boundscheck checkbounds(r, i)
    convert(T, r.zeroth + i*r.step)
end

@inline function Base.getindex(r::StepRange{T}, s::LengthRange{<:Integer}) where {T}
    @boundscheck checkbounds(r, s)
    LengthStepRange{T}(zeroth(r) + zeroth(s)*step(r), step(r)*step(s), length(s))
end

@inline function Base.getindex(r::AbstractUnitRange{T}, s::LengthUnitRange{<:Integer}) where {T}
    @boundscheck checkbounds(r, s)
    LengthUnitRange{T}(zeroth(r) + zeroth(s)*step(r), length(s))
end

"""
StaticOneTo{N} - Like Base.OneTo{Int}(N) but with the length fixed by the type.
"""
const StaticOneTo{N} = LengthUnitRange{Int, StaticInteger{0}, StaticInteger{N}}

Base.@pure StaticOneTo(n::Integer) = StaticOneTo{n}()
Base.@pure StaticOneTo(::StaticInteger{N}) where N = StaticOneTo{N}()

@inline Base.OneTo(n::StaticInteger) = StaticOneTo(n)
@inline Base.:(:)(a::StaticInteger{1}, b::StaticInteger) = StaticOneTo(b)

if VERSION >= v"1.1.0-"
Base.Broadcast.axistype(a::StaticOneTo, b::Base.OneTo) = b
Base.Broadcast.axistype(a::Base.OneTo, b::StaticOneTo) = a
end

# These functions from base/abstractarray.jl need to be extended to also accept StaticOneTo
Base.similar(a::AbstractArray, ::Type{T}, dims::Tuple{Union{Integer, Base.OneTo, StaticOneTo}, Vararg{Union{Integer, Base.OneTo, StaticOneTo}}}) where {T} = similar(a, T, Base.to_shape(dims))
Base.similar(::Type{T}, shape::Tuple{Union{Integer, Base.OneTo, StaticOneTo}, Vararg{Union{Integer, Base.OneTo, StaticOneTo}}}) where {T<:AbstractArray} = similar(T, Base.to_shape(shape))
Base.to_shape(r::StaticOneTo) = Int(length(r))

function Base.show(io::IO, r::StaticOneTo{N}) where N
    print(io, "StaticOneTo(", N, ")")
end

"""
`staticlength(range)` converts to a range where the length is `Static`.
"""
staticlength(r::StepRange) = LengthStepRange(zeroth(r), step(r), static(length(r)))
staticlength(r::UnitRange) = LengthUnitRange(zeroth(r), static(length(r)))

function Base.show(io::IO, r::LengthRange{<:Integer, <:Integer, <:Integer, <:StaticInteger})
    print(io, "staticlength(", first(r), ":")
    step(r) == 1 || print(io, step(r), ":")
    print(io, last(r), ")")
end

import Base.Broadcast: broadcasted, DefaultArrayStyle

broadcasted(::DefaultArrayStyle{1}, ::typeof(+), a::Number, r::LengthStepRange) = LengthStepRange(a+r.zeroth, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::LengthStepRange, a::Number) = LengthStepRange(a+r.zeroth, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), a::Real, r::LengthUnitRange) = LengthUnitRange(a+r.zeroth, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::LengthUnitRange, a::Real) = LengthUnitRange(a+r.zeroth, r.length)

Base.:-(r::LengthStepRange) = LengthStepRange(-r.zeroth, -r.step, r.length)
Base.:-(r::LengthStepRange{<:Any,<:Integer,StaticInteger{-1}}) = LengthUnitRange(-r.zeroth, r.length)
Base.:-(r::LengthUnitRange) = LengthStepRange(-r.zeroth, static(-1), r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::LengthRange) = -r
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), a::Number, r::LengthRange) = LengthStepRange(a-r.zeroth, -r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::LengthStepRange, a::Number) = LengthStepRange(r.zeroth-a, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::LengthUnitRange, a::Real) = LengthUnitRange(r.zeroth-a, r.length)

Base.:*(a::Number, r::LengthRange) = LengthStepRange(a*r.zeroth, a*r.step, r.length)
Base.:*(a::Number, r::LengthRange{<:Any, StaticInteger{0}}) = LengthStepRange(static(0), a*r.step, r.length)
Base.:*(r::LengthRange, a::Number) = a*r
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), a::Number,r::LengthRange) = a*r
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::LengthRange,a::Number) = a*r

@inline Base.eachindex(r::LengthRange{<:Any, <:Any, <:Any, <:StaticInteger}) =
    StaticOneTo(r.length)
