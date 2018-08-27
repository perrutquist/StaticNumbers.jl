module MatMatMulExample

using FixedNumbers
using StaticArrays

const StaticSubMatrix{M,N,T} =
    SubArray{T,2,<:AbstractArray{T,2},
        <:Tuple{
            <:FixedUnitRange{<:Integer,<:Integer,FixedInteger{M}},
            <:FixedUnitRange{<:Integer,<:Integer,FixedInteger{N}}
        },B} where {B}

#StaticArrays.SMatrix(m::StaticSubMatrix{M,N,T}) where {T<:Number, M, N} = SMatrix{M,N,T}(m)
gm(m::StaticSubMatrix{M,N,T}) where {T<:Number, M, N} = SMatrix{M,N,T}(m)

A = rand(16,16)
B = view(A, fixedlength(1:4), fixedlength(5:8))
C = view(A, fixedlength(5:8), fixedlength(1:4))

#sB = SMatrix(B)
sB = gm(B)
C .= sB

end # module
