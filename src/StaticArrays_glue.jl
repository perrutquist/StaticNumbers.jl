# TODO: Add methods for other matrix-creation functions, such as zeros() and rand().

StaticArrays.SOneTo(::StaticNumbers.StaticOneTo{L}) where {L} = StaticArrays.SOneTo{L}()
StaticNumbers.StaticOneTo(::StaticArrays.SOneTo{L}) where {L} = StaticNumbers.StaticOneTo{L}()
StaticNumbers.staticlength(r::StaticArrays.SOneTo) = StaticNumbers.StaticOneTo(r)
StaticNumbers.static(r::StaticArrays.SOneTo) = StaticNumbers.StaticOneTo(r)

# TODO: Maybe just SOneTo from StaticArrays instead of defining StaticOneTo?

@inline StaticArrays.SUnitRange(r::StaticNumbers.LengthUnitRange) = StaticArrays.SUnitRange{Int(first(r)), Int(length(r))}()
StaticNumbers.LengthUnitRange(::StaticArrays.SUnitRange{Start, L}) where {Start, L} = StaticNumbers.LengthUnitRange(StaticNumbers.static(Start-1), StaticNumbers.static(L))
StaticNumbers.staticlength(r::StaticArrays.SUnitRange) = StaticNumbers.LengthUnitRange(r)
StaticNumbers.static(r::StaticArrays.SUnitRange) = StaticNumbers.LengthUnitRange(r)

StaticArrays.Size(::LengthRange{T,Z,S,StaticInteger{L}}) where {T,Z,S,L} = StaticArrays.Size(Int(L))
StaticArrays.Size(s::Tuple{StaticInteger, Vararg{StaticInteger}}) = StaticArrays.Size(Int.(s))
StaticArrays.Size(s::StaticInteger, ss::StaticInteger...) = StaticArrays.Size((s, ss...))
StaticNumbers.static(::StaticArrays.Size{S}) where {S} = StaticNumbers.static.(S)

for AT in (Array, StaticArrays.MArray), RT in (LengthStepRange{<:Integer,Z,S,<:StaticInteger} where {Z,S}, LengthUnitRange{<:Integer,Z,<:StaticInteger} where {Z})
    Base.getindex(A::AT, r::RT) = StaticArrays.MVector(ntuple(i -> A[r[i]], length(r)))
end

for AT in (StaticArrays.SArray,), RT in (LengthStepRange{<:Integer,Z,S,<:StaticInteger} where {Z,S}, LengthUnitRange{<:Integer,Z,<:StaticInteger} where {Z})
    Base.getindex(A::AT, r::RT) = StaticArrays.SVector(ntuple(i -> A[r[i]], length(r)))
end

# Allow "end" to become static when indexing into static arrays
@inline StaticNumbers.maybe_static(::typeof(lastindex), A::StaticArrays.StaticArray) = StaticNumbers.static(lastindex(A))
@inline StaticNumbers.maybe_static(::typeof(lastindex), A::StaticArrays.StaticArray, d::StaticInteger) = StaticNumbers.static(lastindex(A, i))

@inline StaticNumbers.maybe_static(::typeof(size), A::StaticArrays.StaticArray) = map(static, size(A))
@inline StaticNumbers.maybe_static(::typeof(size), A::StaticArrays.StaticArray, d::Integer) = static(size(A, d))

@inline (::Type{T})(g::Base.Generator{<:LengthRange}) where {T<:StaticVector} = T(Tuple(g))
@inline (::Type{T})(iter::StaticNumbers.LengthRange) where {T<:StaticVector} = T(Tuple(iter))

@inline function (::Type{T})(g::Base.Generator{<:Base.Iterators.ProductIterator{<:Tuple{StaticNumbers.StaticLengthRange, Vararg{StaticNumbers.StaticLengthRange}}}}) where {T<:StaticArrays.StaticArray}
    sz = StaticArrays.Size(size(g))
    data = Tuple(g)
    ST = similar_type(T, eltype(data), sz)
    ST <: T || error("Generator yields the wrong type of static array.")
    ST(data)
end

Base.@propagate_inbounds function Base.convert(::Type{SA}, a::SubArray{T,N,<:AbstractArray{T},<:Tuple{StaticNumbers.StaticLengthRange, Vararg{StaticNumbers.StaticLengthRange}}}) where {SA <: StaticArrays.StaticArray, T, N}
    Z = similar_type(SA, T, StaticArrays.Size(size(a)))
    Z <: SA || error("Input array yields the wrong type of static array.")
    Z(Tuple(a))
end

# # TODO: Interface to StaticArrays for multi-dimensional indexing
