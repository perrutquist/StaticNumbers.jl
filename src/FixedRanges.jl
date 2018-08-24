const FixedRangeError = ErrorException("Not a valid FixedRange.")

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
"""
struct FixedRange{T,Z,S,L} <: OrdinalRange{T,S}
    zeroth::Z
    step::S
    length::L
    function FixedRange{T,Z,S,L}(z::Z, s::S, l::L) where {T<:Real, Z<:Real, S<:Real, L<:Integer}
        T == promote_type(Z,S) || throw(FixedRangeError)
        if L<:FixedInteger
            l >= 0 || throw(FixedRangeError)
        else
            @boundscheck l = max(l,zero(l))::L
        end
        new{T,Z,S,L}(z,s,l)
    end
end

Base.@propagate_inbounds FixedRange(z::Z, s::S, l::L) where {Z<:Real, S<:Real, L<:Integer} =
    FixedRange{promote_type(Z, S), Z, S, L}(z, s, l)

FixedRange(r::OrdinalRange) = FixedRange(first(r)-step(r),step(r),length(r))

zeroth(r::FixedRange) = r.zeroth
zeroth(r::AbstractRange) = first(r)-step(r)

Base.first(r::FixedRange) = r.zeroth + r.step
Base.step(r::FixedRange) = r.step
Base.length(r::FixedRange) = r.length
Base.last(r::FixedRange) = r.zeroth + r.step*r.length

function Base.getindex(r::FixedRange, i::Integer)
    @boundscheck (0 < i <= length(r)) || Base.throw_boundserror(r, i)
    r.zeroth + i*r.step
end

Base.OneTo(n::FixedInteger) = FixedRange(Fixed(0), Fixed(1), n)

function Base.show(io::IO, r::FixedRange{Int, FixedInteger{0}, FixedInteger{1}, <:Integer})
    print(io, "OneTo(", r.length, ")")
end

"""
`fixedlength(range)` converts to a range where the length is `Fixed`.
"""
fixedlength(r::OrdinalRange) = FixedRange(zeroth(r), step(r), Fixed(length(r)))

function Base.show(io::IO, r::FixedRange{Int, <:Integer, <:Integer, <:FixedInteger})
    print(io, "fixedlength(", first(r), ":")
    step(r) == 1 || print(io, step(r), ":")
    print(io, last(r), ")")
end

Base.:+(a,r::FixedRange) = FixedRange(a+r.zeroth, r.step, r.length)
Base.:+(r::FixedRange,a) = a+r

Base.:*(a::Number,r::FixedRange) = FixedRange(a*r.zeroth, a*r.step, r.length)
Base.:*(r::FixedRange,a::Number) = a*r

Base.:(:)(a::FixedInteger{1}, b::FixedInteger) = Base.OneTo(b)
