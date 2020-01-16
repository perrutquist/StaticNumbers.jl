import .StaticArrays: StaticArray, MArray, SArray, SVector, MVector, MMatrix, SOneTo, SUnitRange, Size, index_size, index_sizes, StaticIndexing, _ind

import Base: Matrix, Array

# TODO: Add methods for other matrix-creation functions, such as zeros() and rand().

SOneTo(::StaticOneTo{L}) where {L} = SOneTo{L}()
StaticOneTo(::SOneTo{L}) where {L} = StaticOneTo{L}()
# TODO: Maybe just SOneTo from StaticArrays instead of defining StaticOneTo?

Base.@pure SUnitRange(r::LengthUnitRange) = SUnitRange{Int(first(r)), Int(length(r))}()
LengthUnitRange(::SUnitRange{Start, L}) where {Start, L} = LengthUnitRange(static(Start-1), static(L))
staticlength(r::SUnitRange) = LengthUnitRange(r)

Size(::LengthRange{T,Z,S,StaticInteger{L}}) where {T,Z,S,L} = Size(Int(L))
Size(s::Vararg{StaticInteger}) = Size(Int.(s))
static(::Size{S}) where {S} = static.(S)

for AT in (Array, AbstractArray), RT in (LengthStepRange{T,Z,S,StaticInteger{L}} where {T,Z,S,L}, LengthUnitRange{T,Z,StaticInteger{L}} where {T,Z,L})
    Base.getindex(A::AT, r::RT) = MVector(ntuple(i -> A[r[i]], length(r)))
end

for RT in (LengthStepRange{T,Z,S,StaticInteger{L}} where {T,Z,S,L}, LengthUnitRange{T,Z,StaticInteger{L}} where {T,Z,L})
    Base.getindex(A::SArray, r::RT) = SVector(ntuple(i -> A[r[i]], length(r)))
end

# Allow "end" to become static when indexing into static arrays
@inline maybe_static(::typeof(lastindex), A::StaticArray) = static(lastindex(A))
@inline maybe_static(::typeof(lastindex), A::StaticArray, d::StaticInteger) = static(lastindex(A, i))

@inline maybe_static(::typeof(size), A::StaticArray) = map(static, size(A))
@inline maybe_static(::typeof(size), A::StaticArray, d::Integer) = static(size(A, d))

@inline (::Type{SA})(g::Base.Generator{<:LengthRange,F}) where {SA<:StaticArray, F} = SA(Tuple(g))
@inline (::Type{SA})(iter::LengthRange) where {SA<:StaticArray} = SA(Tuple(iter))

# TODO: Interface to StaticArrays for multi-dimensional indexing
