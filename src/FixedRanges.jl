const FixedRangeError = ErrorException("Not a valid FixedRange.")

"""
A `FixedOrdinalRange` is the most general type of FixedRange.
"""
struct FixedOrdinalRange{T,Z,S,L} <: OrdinalRange{T,S}
    zeroth::Z
    step::S
    length::L
    function FixedOrdinalRange{T,Z,S,L}(z::Z, s::S, l::L) where {T, Z, S, L<:Integer}
        T == promote_type(Z,S) || throw(FixedRangeError)
        if L<:FixedInteger
            l >= 0 || throw(FixedRangeError)
        else
            l = max(l,zero(l))::L
        end
        new{T,Z,S,L}(z,s,l)
    end
end

"""
A `FixedUnitRange` is a type that is identical to `FixedOrdinalRange` but
where the step is `FixedInteger{1}`. It is a subtype of UnitRange
"""
struct FixedUnitRange{T,Z,L} <: AbstractUnitRange{T}
    zeroth::Z
    step::FixedInteger{1}
    length::L
    function FixedUnitRange{T,Z,L}(z::Z, l::L) where {T, Z, L<:Integer}
        T == promote_type(Z,Int) || throw(FixedRangeError)
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
The `FixedRange` constructor will construct the appropriate type, depending on
whether the `step` parameter is `Fixed(1)`.
"""
const FixedRange{T,Z,S,L} = Union{FixedOrdinalRange{T,Z,S,L}, FixedUnitRange{T,Z,L}}

FixedRange(z::Z, s::S, l::L) where {Z, S, L<:Integer} =
    FixedOrdinalRange{promote_type(Z, S), Z, S, L}(z, s, l)
FixedUnitRange(z::Z, l::L) where {Z, L<:Integer} =
    FixedUnitRange{promote_type(Z, S), Z, L}(z, l)

FixedRange(r::OrdinalRange) = FixedRange(first(r)-step(r), step(r), length(r))
FixedRange(r::UnitRange) = FixedUnitRange(first(r)-step(r), length(r))

function FixedRange{T,Z,S,L}() where {T, Z<:Fixed, S<:Fixed, L<:FixedInteger}
    FixedOrdinalRange{T,Z,S,L}(Z(), S(), L())
end
function FixedRange{T,Z,FixedInteger{1},L}() where {T, Z<:Fixed, L<:FixedInteger}
    FixedUnitRange{T,Z,L}(Z(), L())
end

zeroth(r::FixedRange) = r.zeroth
zeroth(r::AbstractRange) = first(r)-step(r)

Base.step(r::FixedRange) = r.step
Base.length(r::FixedRange) = r.length

# `first` and `last` might be used to initialize the index in a for-loop.
# so they should never return `Fixed`.
Base.first(r::FixedRange{T}) where {T} = T(r.zeroth + r.step)
Base.last(r::FixedRange{T}) where {T} = T(r.zeroth + r.step*r.length)

function Base.getindex(r::FixedRange, i::Integer)
    @boundscheck (0 < i <= length(r)) || Base.throw_boundserror(r, i)
    r.zeroth + i*r.step
end

"""
FixedOneTo{N} - Like Base.OneTo{Int}(N) but with the length fixed by the type.
"""
const FixedOneTo{N} = FixedUnitRange{Int, FixedInteger{0}, FixedInteger{N}}

Base.@pure FixedOneTo(n::Integer) = FixedOneTo{n}()
Base.@pure FixedOneTo(::FixedInteger{N}) where N = FixedOneTo{N}()

Base.OneTo(n::FixedInteger) = FixedOneTo(n)
Base.:(:)(a::FixedInteger{1}, b::FixedInteger) = FixedOneTo(b)

function Base.show(io::IO, r::FixedOneTo)
    print(io, "FixedOneTo(", r.length, ")")
end

"""
`fixedlength(range)` converts to a range where the length is `Fixed`.
"""
fixedlength(r::OrdinalRange) = FixedRange(zeroth(r), step(r), Fixed(length(r)))

function Base.show(io::IO, r::FixedRange{<:Integer, <:Integer, <:Integer, <:FixedInteger})
    print(io, "fixedlength(", first(r), ":")
    step(r) == 1 || print(io, step(r), ":")
    print(io, last(r), ")")
end

Base.:+(a,r::FixedRange) = FixedRange(a+r.zeroth, r.step, r.length)
Base.:+(r::FixedRange,a) = a+r

Base.:*(a::Number,r::FixedRange) = FixedRange(a*r.zeroth, a*r.step, r.length)
Base.:*(a::Number,r::FixedRange{<:Any, FixedInteger{0}}) = FixedRange(Fixed(0), a*r.step, r.length)
Base.:*(r::FixedRange,a::Number) = a*r

Base.eachindex(r::FixedRange{<:Any, <:Any, <:Any, <:FixedInteger}) =
    FixedOneTo(r.length)
