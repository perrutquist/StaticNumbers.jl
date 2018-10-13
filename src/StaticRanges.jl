export StaticRange, StaticStepRange, StaticUnitRange, zeroth, StaticOneTo, staticlength

const StaticRangeError = ErrorException("Not a valid StaticRange.")

# TODO: StaticRange should be renamed LengthRange. It is not Static per se.
# StaticRange (StaticRange) should maybe be a typealias for the case where
# the length is Static (Static)?

"""
A `StaticStepRange` is the most general type of StaticRange.
"""
struct StaticStepRange{T,Z,S,L} <: OrdinalRange{T,S}
    zeroth::Z
    step::S
    length::L
    function StaticStepRange{T,Z,S,L}(z::Z, s::S, l::L) where {T, Z, S, L<:Integer}
        if L<:StaticInteger
            l >= 0 || throw(StaticRangeError)
        else
            l = max(l,zero(l))::L
        end
        new{T,Z,S,L}(z,s,l)
    end
end

StaticStepRange(z::Z, s::S, l::L) where {Z, S, L<:Integer} =
    StaticStepRange{typeof(z+1*s), Z, S, L}(z, s, l)
StaticStepRange{T}(z::Z, s::S, l::L) where {T, Z, S, L<:Integer} =
    StaticStepRange{T, Z, S, L}(z, s, l)
StaticStepRange{T,Z,S,L}() where {T, Z<:Static, S<:Static, L<:StaticInteger} =
    StaticStepRange{T,Z,S,L}(Z(), S(), L())

"""
A `StaticUnitRange` is a type that is identical to `StaticStepRange` but
where the step is fixed to 1. It is a subtype of UnitRange.

(It would be much simpler to just use `StaticStepRange` with a `Static` step
but then methods that expect a `UnitRange` would not work.)
"""
struct StaticUnitRange{T,Z,L} <: AbstractUnitRange{T}
    zeroth::Z
    step::StaticInteger{1}
    length::L
    function StaticUnitRange{T,Z,L}(z::Z, l::L) where {T, Z, L<:Integer}
        if L<:StaticInteger
            l >= 0 || throw(StaticRangeError)
        else
            l = max(l,zero(l))::L
        end
        new{T,Z,L}(z, Static(1), l)
    end
end

StaticUnitRange{T}(z::Z, l::L) where {T, Z, L<:Integer} =
    StaticUnitRange{T, Z, L}(z, l)
StaticUnitRange(z::Z, l::L) where {Z, L<:Integer} =
    StaticUnitRange{typeof(z+zero(z)), Z, L}(z, l)
StaticUnitRange{T,Z,L}() where {T, Z<:Static, L<:StaticInteger} =
    StaticUnitRange{T, Z, L}(Z(), L())

"""
`StaticRange(zeroth, step, length)`

A StaticRange is a range which allows (but does not require) its `zeroth`, `step`
and/or `length` to be `Static` numbers.

The `zeroth` element of the range is the element before the first.
Although it is not part of the range, it is frequently useful for it
to remain static. For example, if the common 1:n range is multiplied by
a scalar, the zeroth element can remain `StaticInteger{0}`

The `step` is the distance between successive elements of the range.

The `length` must be an `Integer`. A `StaticRange` is parameterized by its
length, rather than its last element. This makes it possible for the length
to remain `Static` when an offset is added to the range.

`StaticRange` is the union of `StaticStepRange` and `StaticUnitRange`
which are subtypes of `OrdinalRange` and `AbstractUnitRange` respectively.
"""
const StaticRange{T,Z,S,L} = Union{StaticStepRange{T,Z,S,L}, StaticUnitRange{T,Z,L}}

# Currently having problems making the StaticRange constructors type-stable
# Work-around: Use StaticStepRange or StaticUnitRange directly.

#StaticRange(z::Z, s::S, l::L) where {Z, S, L<:Integer} =
#    StaticStepRange{typeof(z+0*s), Z, S, L}(z, s, l)
#StaticRange(z::Z, ::StaticInteger{1}, l::L) where {Z, L<:Integer} =
#    StaticUnitRange{typeof(z+0), Z, L}(z, l)

#StaticRange{T,Z,S,L}() where {T, Z<:Static, S<:Static, L<:StaticInteger} =
#    StaticStepRange{T,Z,S,L}(Z(), S(), L())
#StaticRange{T,Z,StaticInteger{1},L}() where {T, Z<:Static, L<:StaticInteger} =
#    StaticUnitRange{T,Z,L}(Z(), L())

StaticRange(r::StepRange) = StaticStepRange(r)
StaticStepRange(r::StepRange) = StaticStepRange(first(r)-step(r), step(r), length(r))
StaticRange(r::AbstractUnitRange) = StaticUnitRange(r)
StaticUnitRange(r::UnitRange) = StaticUnitRange(first(r)-step(r), length(r))

@inline zeroth(r::StaticRange) = r.zeroth
@inline zeroth(r::AbstractRange) = first(r)-step(r)

@inline Base.step(r::StaticRange) = r.step
@inline Base.length(r::StaticRange) = r.length
@inline Base.unsafe_length(r::StaticRange) = r.length

# `first` and `last` return elements of the array, so are of type T, never `Static`.
@inline Base.first(r::StaticRange{T}) where {T} = convert(T, r.zeroth + r.step)
@inline Base.last(r::StaticRange{T}) where {T} = convert(T, r.zeroth + r.step*r.length)

@inline function Base.getindex(r::StaticRange{T}, i::Integer)::T where {T}
    @boundscheck checkbounds(r, i)
    convert(T, r.zeroth + i*r.step)
end

@inline function Base.getindex(r::StepRange{T}, s::StaticRange{<:Integer}) where {T}
    @boundscheck checkbounds(r, s)
    StaticStepRange{T}(zeroth(r) + zeroth(s)*step(r), step(r)*step(s), length(s))
end

@inline function Base.getindex(r::AbstractUnitRange{T}, s::StaticUnitRange{<:Integer}) where {T}
    @boundscheck checkbounds(r, s)
    StaticUnitRange{T}(zeroth(r) + zeroth(s)*step(r), length(s))
end

"""
StaticOneTo{N} - Like Base.OneTo{Int}(N) but with the length fixed by the type.
"""
const StaticOneTo{N} = StaticUnitRange{Int, StaticInteger{0}, StaticInteger{N}}

Base.@pure StaticOneTo(n::Integer) = StaticOneTo{n}()
Base.@pure StaticOneTo(::StaticInteger{N}) where N = StaticOneTo{N}()

@inline Base.OneTo(n::StaticInteger) = StaticOneTo(n)
@inline Base.:(:)(a::StaticInteger{1}, b::StaticInteger) = StaticOneTo(b)

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
staticlength(r::StepRange) = StaticStepRange(zeroth(r), step(r), Static(length(r)))
staticlength(r::UnitRange) = StaticUnitRange(zeroth(r), Static(length(r)))

function Base.show(io::IO, r::StaticRange{<:Integer, <:Integer, <:Integer, <:StaticInteger})
    print(io, "staticlength(", first(r), ":")
    step(r) == 1 || print(io, step(r), ":")
    print(io, last(r), ")")
end

import Base.Broadcast: broadcasted, DefaultArrayStyle

broadcasted(::DefaultArrayStyle{1}, ::typeof(+), a::Number, r::StaticStepRange) = StaticStepRange(a+r.zeroth, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::StaticStepRange, a::Number) = StaticStepRange(a+r.zeroth, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), a::Real, r::StaticUnitRange) = StaticUnitRange(a+r.zeroth, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::StaticUnitRange, a::Real) = StaticUnitRange(a+r.zeroth, r.length)

Base.:-(r::StaticStepRange) = StaticStepRange(-r.zeroth, -r.step, r.length)
Base.:-(r::StaticStepRange{<:Any,<:Integer,StaticInteger{-1}}) = StaticUnitRange(-r.zeroth, r.length)
Base.:-(r::StaticUnitRange) = StaticStepRange(-r.zeroth, Static(-1), r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::StaticRange) = -r
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), a::Number, r::StaticRange) = StaticStepRange(a-r.zeroth, -r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::StaticStepRange, a::Number) = StaticStepRange(r.zeroth-a, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::StaticUnitRange, a::Real) = StaticUnitRange(r.zeroth-a, r.length)

Base.:*(a::Number, r::StaticRange) = StaticStepRange(a*r.zeroth, a*r.step, r.length)
Base.:*(a::Number, r::StaticRange{<:Any, StaticInteger{0}}) = StaticStepRange(Static(0), a*r.step, r.length)
Base.:*(r::StaticRange, a::Number) = a*r
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), a::Number,r::StaticRange) = a*r
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::StaticRange,a::Number) = a*r

@inline Base.eachindex(r::StaticRange{<:Any, <:Any, <:Any, <:StaticInteger}) =
    StaticOneTo(r.length)
