import .StaticArrays: MArray

import Base: Matrix, Array

Array{T, N}(::UndefInitializer, d::NTuple{N,StaticInteger}) where {T,N} =  MArray{Tuple{Int.(d)...}, T, N, prod(d)}(undef)
Array{T, 1}(::UndefInitializer, ::Tuple{StaticInteger{D}}) where {T, D} = MArray{Tuple{Int(D)}, T, 1, Int(D)}(undef)
Array{T, N}(::UndefInitializer, d::Vararg{StaticInteger,N}) where {T, N} = Array{T, N}(undef, d)
Matrix(::UndefInitializer, m::StaticInteger, n::StaticInteger) = Matrix{Any}(undef, m, n)
Vector(::UndefInitializer, n::StaticInteger) = Vector{Any}(undef, n)
