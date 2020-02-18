import .StaticArrays: StaticArray, StaticVector, MArray, SArray, SVector, MVector, MMatrix, SOneTo, SUnitRange, Size, index_size, index_sizes, StaticIndexing, _ind, similar_type
import .StaticArrays: LinearAlgebra

import Base: Matrix, Array

# TODO: Add methods for other matrix-creation functions, such as zeros() and rand().

SOneTo(::StaticOneTo{L}) where {L} = SOneTo{L}()
StaticOneTo(::SOneTo{L}) where {L} = StaticOneTo{L}()
staticlength(r::SOneTo) = StaticOneTo(r)
static(r::SOneTo) = StaticOneTo(r)

# TODO: Maybe just SOneTo from StaticArrays instead of defining StaticOneTo?

Base.@pure SUnitRange(r::LengthUnitRange) = SUnitRange{Int(first(r)), Int(length(r))}()
LengthUnitRange(::SUnitRange{Start, L}) where {Start, L} = LengthUnitRange(static(Start-1), static(L))
staticlength(r::SUnitRange) = LengthUnitRange(r)
static(r::SUnitRange) = LengthUnitRange(r)

Size(::LengthRange{T,Z,S,StaticInteger{L}}) where {T,Z,S,L} = Size(Int(L))
Size(s::Tuple{StaticInteger, Vararg{StaticInteger}}) = Size(Int.(s))
Size(s::StaticInteger, ss::StaticInteger...) = Size((s, ss...))
static(::Size{S}) where {S} = static.(S)

for AT in (Array, MArray, LinearAlgebra.Adjoint{<:Any, <:Array}, LinearAlgebra.Transpose{<:Any, <:Array}), RT in (LengthStepRange{<:Integer,Z,S,<:StaticInteger} where {Z,S}, LengthUnitRange{<:Integer,Z,<:StaticInteger} where {Z})
    Base.getindex(A::AT, r::RT) = MVector(ntuple(i -> A[r[i]], length(r)))
end

for AT in (SArray,), RT in (LengthStepRange{<:Integer,Z,S,<:StaticInteger} where {Z,S}, LengthUnitRange{<:Integer,Z,<:StaticInteger} where {Z})
    Base.getindex(A::AT, r::RT) = SVector(ntuple(i -> A[r[i]], length(r)))
end

# Allow "end" to become static when indexing into static arrays
@inline maybe_static(::typeof(lastindex), A::StaticArray) = static(lastindex(A))
@inline maybe_static(::typeof(lastindex), A::StaticArray, d::StaticInteger) = static(lastindex(A, i))

@inline maybe_static(::typeof(size), A::StaticArray) = map(static, size(A))
@inline maybe_static(::typeof(size), A::StaticArray, d::Integer) = static(size(A, d))

@inline (::Type{T})(g::Base.Generator{<:LengthRange}) where {T<:StaticVector} = T(Tuple(g))
@inline (::Type{T})(iter::LengthRange) where {T<:StaticVector} = T(Tuple(iter))

@inline function (::Type{T})(g::Base.Generator{<:Base.Iterators.ProductIterator{<:Tuple{StaticLengthRange, Vararg{StaticLengthRange}}}}) where {T<:StaticArray}
    sz = Size(size(g))
    data = Tuple(g)
    ST = similar_type(T, eltype(data), sz)
    ST <: T || error("Generator yields the wrong type of static array.")
    ST(data)
end

# TODO: Interface to StaticArrays for multi-dimensional indexing
