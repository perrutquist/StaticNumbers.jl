module MatMatMulExample

using FixedNumbers
using StaticArrays

const StaticSubMatrix{M,N,T} =
    SubArray{T,2,<:AbstractArray{T,2},
        <:Tuple{
            <:FixedUnitRange{<:Integer,<:Integer,FixedInteger{M}},
            <:FixedUnitRange{<:Integer,<:Integer,FixedInteger{N}}
        },B} where {B}

#StaticArrays.SMatrix
s(m::StaticSubMatrix{M,N,T}) where {T<:Number, M, N} = SMatrix{M,N,T}(m)

#Base.*
#⊗(a::AbstractMatrix{T}, b::AbstractMatrix{T}) where {T} = a*b
#⊗(a::StaticSubMatrix{M,N,T}, b::StaticSubMatrix{N,M,T}) where {M,N,T} = s(a)*s(b)

#Local version of Base.size that preserves `Fixed` size of StaticSubMatrix
#size(args...) = Base.size(args...)
#size(a::StaticSubMatrix{M,N}) where {M,N} = (Fixed(M), Fixed(N))

const dimerr = DimensionMismatch("Wrong size matrix")
const aliaserr = ErrorException("Destination matrix cannot be one of inputs")

function mymul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
        m::Integer, n::Integer, k::Integer,
        mnks::Integer...; kwargs...)
    (N,M) = size(C)
    K = size(A,2)
    N == size(A,1) && M == size(B,2) && K == size(B,1) || throw(dimerr)
    (C===A || C===B) && throw(aliaserr)

    unsafe_mul!(C, A, B,
        M÷m, N÷n, K÷k,
        M%m, N%n, K%k,
        Fixed(m), Fixed(n), Fixed(k),
        map(Fixed, mnks)...;
        kwargs...)
end

"""
C <- A*B + beta*C
`unsafe_mul` assumes that dimensions are correct.
Calling this function with incorrect inputs can lead to memory corruption.
"""
#function unsafe_mul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
#        tm::Integer, tn::Integer, tk::Integer,
#        rm::Integer, rn::Integer, rk::Integer,
#        m::Integer, n::Integer, k::Integer,
#        mnks::Integer...;
#        beta::Number=Fixed(false))
#end

function unsafe_mul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
        tm::Integer, tn::Integer, tk::Integer,
        rm::Integer, rn::Integer, rk::Integer,
        m::Integer, n::Integer, k::Integer,
        beta::Number=Fixed(false))
    for i=0:tm-1
        mm = i*m .+ FixedOneTo(m)
        unsafe_rowsmul!(C, A, B, mm, tn, tk, rn, rk, n, k, beta)
    end
    if rm>0
        mm = tm*m .+ FixedOneTo(rm)
        unsafe_rowsmul!(C, A, B, mm, tn, tk, rn, rk, n, k, beta)
    end
end

"""
C[mm,:] <- A[mm,:]*B + beta*C[mm,:]
"""
function unsafe_rowsmul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
        mm::AbstractVector{<:Integer}, tn::Integer, tk::Integer,
        rn::Integer, rk::Integer, n::Integer, k::Integer, beta::Number)
    for j=0:tn-1
        nn = j*n .+ FixedOneTo(n)
        unsafe_submatmul!(C, A, B, mm, nn, tk, rk, k, beta)
    end
    if rn>0
        nn = tn*n .+ FixedOneTo(rn)
        unsafe_submatmul!(C, A, B, mm, nn, tk, rk, k, beta)
    end
end

"""
C[mm,nn] <- A[mm,:]*B[:,nn] + beta*C[mm,nn]
"""
function unsafe_submatmul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
        mm::AbstractVector{<:Integer}, nn::AbstractVector{<:Integer},
        tk::Integer, rk::Integer, k::Integer, beta::Number)
    Cv = @inbounds view(C, mm, nn)
    X = beta * s(Cv)
    for h=0:tk-1
        kk = h*k .+ FixedOneTo(k)
        X += s(@inbounds view(A, mm, kk)) * s(@inbounds view(B, kk, nn))
    end
    if rk>0
        kk = tk*k .+ FixedOneTo(rk)
        X += s(@inbounds view(A, mm, kk)) * s(@inbounds view(B, kk, nn))
    end
    Cv .= X
    return nothing
end

A = randn(16,16)
Av1 = view(A, fixedlength(1:4), fixedlength(5:8))
Av2 = view(A, fixedlength(5:8), fixedlength(1:4))

sAv1 = s(Av1)
Av2 .= sAv1

B = randn(16,16)
C = zeros(16,16)

mymul!(C, A, B, 4, 4, 4)

println("error / (BLAS error) = ", maximum(abs.(C .-  Float64.(big.(A)*big.(B)))) / maximum(abs.(A*B .-  Float64.(big.(A)*big.(B)))))

end # module
