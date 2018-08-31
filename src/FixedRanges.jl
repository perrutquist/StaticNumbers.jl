export FixedRange, FixedStepRange, FixedUnitRange, zeroth, FixedOneTo, fixedlength

const FixedRangeError = ErrorException("Not a valid FixedRange.")

"""
A `FixedStepRange` is the most general type of FixedRange.
"""
struct FixedStepRange{T,Z,S,L} <: OrdinalRange{T,S}
    zeroth::Z
    step::S
    length::L
    function FixedStepRange{T,Z,S,L}(z::Z, s::S, l::L) where {T, Z, S, L<:Integer}
        if L<:FixedInteger
            l >= 0 || throw(FixedRangeError)
        else
            l = max(l,zero(l))::L
        end
        new{T,Z,S,L}(z,s,l)
    end
end

FixedStepRange(z::Z, s::S, l::L) where {Z, S, L<:Integer} =
    FixedStepRange{typeof(z+1*s), Z, S, L}(z, s, l)
FixedStepRange{T}(z::Z, s::S, l::L) where {T, Z, S, L<:Integer} =
    FixedStepRange{T, Z, S, L}(z, s, l)
FixedStepRange{T,Z,S,L}() where {T, Z<:Fixed, S<:Fixed, L<:FixedInteger} =
    FixedStepRange{T,Z,S,L}(Z(), S(), L())

"""
A `FixedUnitRange` is a type that is identical to `FixedStepRange` but
where the step is fixed to 1. It is a subtype of UnitRange.

(It would be much simpler to just use `FixedStepRange` with a `Fixed` step
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

FixedUnitRange{T}(z::Z, l::L) where {T, Z, L<:Integer} =
    FixedUnitRange{T, Z, L}(z, l)
FixedUnitRange(z::Z, l::L) where {Z, L<:Integer} =
    FixedUnitRange{typeof(z+zero(z)), Z, L}(z, l)
FixedUnitRange{T,Z,L}() where {T, Z<:Fixed, L<:FixedInteger} =
    FixedUnitRange{T, Z, L}(Z(), L())

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

`FixedRange` is the union of `FixedStepRange` and `FixedUnitRange`
which are subtypes of `OrdinalRange` and `AbstractUnitRange` respectively.
"""
const FixedRange{T,Z,S,L} = Union{FixedStepRange{T,Z,S,L}, FixedUnitRange{T,Z,L}}

# Currently having problems making the FixedRange constructors type-stable
# Work-around: Use FixedStepRange or FixedUnitRange directly.

#FixedRange(z::Z, s::S, l::L) where {Z, S, L<:Integer} =
#    FixedStepRange{typeof(z+0*s), Z, S, L}(z, s, l)
#FixedRange(z::Z, ::FixedInteger{1}, l::L) where {Z, L<:Integer} =
#    FixedUnitRange{typeof(z+0), Z, L}(z, l)

#FixedRange{T,Z,S,L}() where {T, Z<:Fixed, S<:Fixed, L<:FixedInteger} =
#    FixedStepRange{T,Z,S,L}(Z(), S(), L())
#FixedRange{T,Z,FixedInteger{1},L}() where {T, Z<:Fixed, L<:FixedInteger} =
#    FixedUnitRange{T,Z,L}(Z(), L())

FixedRange(r::StepRange) = FixedStepRange(r)
FixedStepRange(r::StepRange) = FixedStepRange(first(r)-step(r), step(r), length(r))
FixedRange(r::AbstractUnitRange) = FixedUnitRange(r)
FixedUnitRange(r::UnitRange) = FixedUnitRange(first(r)-step(r), length(r))

@inline zeroth(r::FixedRange) = r.zeroth
@inline zeroth(r::AbstractRange) = first(r)-step(r)

@inline Base.step(r::FixedRange) = r.step
@inline Base.length(r::FixedRange) = r.length
@inline Base.unsafe_length(r::FixedRange) = r.length

# `first` and `last` return elements of the array, so are of type T, never `Fixed`.
@inline Base.first(r::FixedRange{T}) where {T} = convert(T, r.zeroth + r.step)
@inline Base.last(r::FixedRange{T}) where {T} = convert(T, r.zeroth + r.step*r.length)

@inline function Base.getindex(r::FixedRange{T}, i::Integer)::T where {T}
    @boundscheck checkbounds(r, i)
    convert(T, r.zeroth + i*r.step)
end

@inline function Base.getindex(r::StepRange{T}, s::FixedRange{<:Integer}) where {T}
    @boundscheck checkbounds(r, s)
    FixedStepRange{T}(zeroth(r) + zeroth(s)*step(r), step(r)*step(s), length(s))
end

@inline function Base.getindex(r::AbstractUnitRange{T}, s::FixedUnitRange{<:Integer}) where {T}
    @boundscheck checkbounds(r, s)
    FixedUnitRange{T}(zeroth(r) + zeroth(s)*step(r), length(s))
end

"""
FixedOneTo{N} - Like Base.OneTo{Int}(N) but with the length fixed by the type.
"""
const FixedOneTo{N} = FixedUnitRange{Int, FixedInteger{0}, FixedInteger{N}}

Base.@pure FixedOneTo(n::Integer) = FixedOneTo{n}()
Base.@pure FixedOneTo(::FixedInteger{N}) where N = FixedOneTo{N}()

@inline Base.OneTo(n::FixedInteger) = FixedOneTo(n)
@inline Base.:(:)(a::FixedInteger{1}, b::FixedInteger) = FixedOneTo(b)

# These functions from base/abstractarray.jl need to be extended to also accept FixedOneTo
Base.similar(a::AbstractArray, ::Type{T}, dims::Tuple{Union{Integer, Base.OneTo, FixedOneTo}, Vararg{Union{Integer, Base.OneTo, FixedOneTo}}}) where {T} = similar(a, T, Base.to_shape(dims))
Base.similar(::Type{T}, shape::Tuple{Union{Integer, Base.OneTo, FixedOneTo}, Vararg{Union{Integer, Base.OneTo, FixedOneTo}}}) where {T<:AbstractArray} = similar(T, Base.to_shape(shape))
Base.to_shape(r::FixedOneTo) = Int(length(r))

function Base.show(io::IO, r::FixedOneTo{N}) where N
    print(io, "FixedOneTo(", N, ")")
end

"""
`fixedlength(range)` converts to a range where the length is `Fixed`.
"""
fixedlength(r::StepRange) = FixedStepRange(zeroth(r), step(r), Fixed(length(r)))
fixedlength(r::UnitRange) = FixedUnitRange(zeroth(r), Fixed(length(r)))

function Base.show(io::IO, r::FixedRange{<:Integer, <:Integer, <:Integer, <:FixedInteger})
    print(io, "fixedlength(", first(r), ":")
    step(r) == 1 || print(io, step(r), ":")
    print(io, last(r), ")")
end

import Base.Broadcast: broadcasted, DefaultArrayStyle

broadcasted(::DefaultArrayStyle{1}, ::typeof(+), a::Number, r::FixedStepRange) = FixedStepRange(a+r.zeroth, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::FixedStepRange, a::Number) = FixedStepRange(a+r.zeroth, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), a::Real, r::FixedUnitRange) = FixedUnitRange(a+r.zeroth, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::FixedUnitRange, a::Real) = FixedUnitRange(a+r.zeroth, r.length)

Base.:-(r::FixedStepRange) = FixedStepRange(-r.zeroth, -r.step, r.length)
Base.:-(r::FixedStepRange{<:Any,<:Integer,FixedInteger{-1}}) = FixedUnitRange(-r.zeroth, r.length)
Base.:-(r::FixedUnitRange) = FixedStepRange(-r.zeroth, Fixed(-1), r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::FixedRange) = -r
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), a::Number, r::FixedRange) = FixedStepRange(a-r.zeroth, -r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::FixedStepRange, a::Number) = FixedStepRange(r.zeroth-a, r.step, r.length)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::FixedUnitRange, a::Real) = FixedUnitRange(r.zeroth-a, r.length)

Base.:*(a::Number, r::FixedRange) = FixedStepRange(a*r.zeroth, a*r.step, r.length)
Base.:*(a::Number, r::FixedRange{<:Any, FixedInteger{0}}) = FixedStepRange(Fixed(0), a*r.step, r.length)
Base.:*(r::FixedRange, a::Number) = a*r
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), a::Number,r::FixedRange) = a*r
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::FixedRange,a::Number) = a*r

@inline Base.eachindex(r::FixedRange{<:Any, <:Any, <:Any, <:FixedInteger}) =
    FixedOneTo(r.length)
