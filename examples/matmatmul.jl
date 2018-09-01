# For beenchmarking, use: julia -O3

module MatMatMulExample

using FixedNumbers
using StaticArrays
using LinearAlgebra

# Note: I'd like to be able to do this with `view`s that result
# in fixed-size subarrays, but at the moment, those are still allocating.
# So passing ranges as separate arguments instead.

"A struct that stores the block sizes used in matmatmul"
struct BlockSizes{TM<:Integer,TN<:Integer,TK<:Integer}
    m::TM
    n::TN
    k::TK
end

const dimerr = DimensionMismatch("Wrong size matrix")
const aliaserr = ErrorException("Destination matrix cannot be one of inputs")

function mymul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
        mnk::BlockSizes, mnks::BlockSizes...)
    (M,N) = size(C)
    K = size(A,2)
    M == size(A,1) && N == size(B,2) && K == size(B,1) || throw(dimerr)
    (C===A || C===B) && throw(aliaserr)

    unsafe_mul!(C, A, B,
        M÷mnk.m, N÷mnk.n, K÷mnk.k,
        M%mnk.m, N%mnk.n, Fixed(K%mnk.k),
        mnk.m, mnk.n, mnk.k,
        mnks...)
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
@inline function unsafe_rowsmul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
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

# This is allocating unless k and rk are `FixedInteger`s
"""
C[mm,nn] <- A[mm,:]*B[:,nn] + beta*C[mm,nn]
"""
@inline function unsafe_submatmul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
        mm::AbstractVector{<:Integer}, nn::AbstractVector{<:Integer},
        tk::Integer, rk::Integer, k::Integer, beta::Number)
    X = beta * load(SMatrix, C, mm, nn)
    for h=0:tk-1
        kk = h*k .+ FixedOneTo(k)
        X += load(SMatrix, A, mm, kk) * load(SMatrix, B, kk, nn)
    end
    if rk>0
        kk = tk*k .+ FixedOneTo(rk)
        X += load(SMatrix, A, mm, kk) * load(SMatrix, B, kk, nn)
    end
    store!(C, mm, nn, X)
    return nothing
end

@inline function unsafe_submatmul!b(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
        mm::AbstractVector{<:Integer}, nn::AbstractVector{<:Integer},
        tk::Integer, rk::Integer, k::Integer, beta::Number, bs::BlockSizes...)
    kk = FixedOneTo(k)
    submatmul!(C, A, B, mm, nn, kk, beta, bs...)
    for h=1:tk-1
        kk = h*k .+ FixedOneTo(k)
        submatmul!(C, A, B, mm, nn, kk, Fixed(1), bs...)
    end
    if rk>0
        kk = tk*k .+ FixedOneTo(rk)
        submatmul!(C, A, B, mm, nn, kk, Fixed(1), bs...)
    end
    return nothing
end

# Fast, zero-size LinearIndices for Static matrices if we define:
Base.axes(A::StaticArray) = map(FixedOneTo, size(A))

"Read a small subset of a StaticMatrix into a StaticMatrix of given type"
@generated function load(::Type{T}, A::StaticMatrix{M,N},
        mm::FixedUnitRange{Int,IM,FixedInteger{m}},
        nn::FixedUnitRange{Int,IN,FixedInteger{n}}) where {T,M,N,IM,IN,m,n}
    # TODO @boundscheck checkbounds
    a = Vector{Expr}()
    L = LinearIndices((M,N))
    for j=1:n
        for i=1:m
            push!(a, :( A[k+$(L[i,j])] ))
        end
    end
    return quote
        Base.@_inline_meta
        k = M*zeroth(nn) + zeroth(mm)
        @inbounds T{m,n}($(Expr(:tuple, a...)))
    end
end
# Note, reading into a transpose is slow. Probably best to read first,
# then transpose.

"Store a small StaticMatrix into a subset of a StaticMatrix"
@generated function store!(A::StaticMatrix{M,N},
        mm::FixedUnitRange{Int,IM,FixedInteger{m}},
        nn::FixedUnitRange{Int,IN,FixedInteger{n}},
        Y::StaticMatrix{m,n}) where {M,N,IM,IN,m,n}
    a = Vector{Expr}()
    y = Vector{Expr}()
    L = LinearIndices((M,N))
    Ly = LinearIndices((m,n))
    for j=1:n
        for i=1:m
            push!(a, :( A[k+$(L[i,j])] ))
            push!(y, :( Y[$(Ly[i,j])] ))
        end
    end
    return quote
        Base.@_inline_meta
        k = M*zeroth(nn) + zeroth(mm)
        @inbounds $(Expr(:tuple, a...)) = $(Expr(:tuple, y...))
        nothing
    end
end

# TODO Transpose and Adjoint
#load(T::Type, A::Transpose, mm::FixedUnitRange, nn::FixedUnitRange) =
#   transpose(load(T, parent(A), nn, mm))
#store!(A::Transpose, mm::FixedUnitRange, nn::FixedUnitRange, Y::StaticMatrix) =
#   store!(T, parent(A), nn, mm, transpose(Y))


end # module

using FixedNumbers
using StaticArrays
using LinearAlgebra
using BenchmarkTools

m=17
n=18
k=19

A = MMatrix{m,k}(randn(m,k))
B = MMatrix{k,n}(randn(k,n))
C = MMatrix{m,n}(zeros(m,n))

MatMatMulExample.mymul!(C, A, B, MatMatMulExample.BlockSizes(Fixed(4), Fixed(4), Fixed(4)))

println("Relative inaccuracy compared to BLAS = ", maximum(abs.(C .-  Float64.(big.(A)*big.(B)))) / maximum(abs.(A*B .-  Float64.(big.(A)*big.(B)))))

@benchmark MatMatMulExample.mymul!($MC, $MA, $MB, $(MatMatMulExample.BlockSizes(Fixed(4), Fixed(4), Fixed(2))))
