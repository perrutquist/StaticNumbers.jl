import .StaticArrays: StaticArray, MArray, SArray, SVector, MVector, SOneTo, SUnitRange, Size, index_size, index_sizes, StaticIndexing, _ind

import Base: Matrix, Array

# static size array creation defaults to MArray
Array{T, N}(::UndefInitializer, d::NTuple{N,StaticInteger}) where {T,N} =  MArray{Tuple{Int.(d)...}, T, N, prod(d)}(undef)
Array{T, 1}(::UndefInitializer, ::Tuple{StaticInteger{D}}) where {T, D} = MArray{Tuple{Int(D)}, T, 1, Int(D)}(undef)
Array{T, N}(::UndefInitializer, d::Vararg{StaticInteger,N}) where {T, N} = Array{T, N}(undef, d)
Matrix(::UndefInitializer, m::StaticInteger, n::StaticInteger) = Matrix{Any}(undef, m, n)
Vector(::UndefInitializer, n::StaticInteger) = Vector{Any}(undef, n)
# TODO: Add methods for other matrix-creation functions, such as zeros() and rand().

SOneTo(::StaticOneTo{L}) where {L} = SOneTo{L}()
StaticOneTo(::SOneTo{L}) where {L} = StaticOneTo{L}()
# TODO: Maybe just SOneTo from StaticArrays instead of defining StaticOneTo?

Base.@pure SUnitRange(r::LengthUnitRange) = SUnitRange{Int(first(r)), Int(length(r))}()
LengthUnitRange(::SUnitRange{Start, L}) where {Start, L} = LengthUnitRange(static(Start-1), static(L))
staticlength(r::SUnitRange) = LengthUnitRange(r)

Size(::LengthRange{T,Z,S,StaticInteger{L}}) where {T,Z,S,L} = Size(Int(L))
Size(::StaticInteger{L}) where {L} = Size(Int(L))

for AT in (Array, AbstractArray), RT in (LengthStepRange{T,Z,S,StaticInteger{L}} where {T,Z,S,L}, LengthUnitRange{T,Z,StaticInteger{L}} where {T,Z,L})
    Base.getindex(A::AT, r::RT) = MVector(ntuple(i -> A[r[i]], length(r)))
end

for RT in (LengthStepRange{T,Z,S,StaticInteger{L}} where {T,Z,S,L}, LengthUnitRange{T,Z,StaticInteger{L}} where {T,Z,L})
    Base.getindex(A::SArray, r::RT) = SVector(ntuple(i -> A[r[i]], length(r)))
end

# Allow "end" to become static when indexing into static arrays
trystatic(::typeof(lastindex), A::StaticArray) = static(lastindex(A))
trystatic(::typeof(lastindex), A::StaticArray, d::StaticInteger) = static(lastindex(A, i))

# TODO: Interface to StaticArrays for multi-dimensional indexing
