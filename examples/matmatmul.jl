# For beenchmarking, use: julia -O3

# Note: I'd like to be able to do this with `view`s that result
# in fixed-size subarrays, but at the moment, those are still allocating.
# So passing ranges as separate arguments instead.

module MatMatMulExample

using FixedNumbers
using StaticArrays
using LinearAlgebra

const ftrue = Fixed(true)

"A struct that stores the block sizes used in matmatmul"
struct BlockSizes{TM<:Integer,TN<:Integer,TK<:Integer}
    m::TM
    n::TN
    k::TK
end

const dimerr = DimensionMismatch("Incompatible matrix axes")
const aliaserr = ErrorException("Destination matrix cannot be one of inputs")

"""
Check that ranges are in bounds for `mymul`.
Instead of Julia's `@inbounds` mechanism, we pass `inbounds` explicitly
as a `FixedOrBool` parameter. This works even when functions are not inlined,
and makes it easier to eliminate bounds-checking code when using the @code_llvm
macro.
"""
@inline function checkmulbounds(
                A::AbstractMatrix, B::AbstractMatrix, C::AbstractMatrix,
                mm::AbstractRange, nn::AbstractRange, kk::AbstractRange)
        checkbounds(C, mm, nn)
        checkbounds(A, mm, kk)
        checkbounds(B, kk, nn)
end

"""
C <- A*B + beta*C

If `inbounds` is `true` then no check is performed that matrix sizes match
block specifications. This can lead to memory corruption.
"""
function mymul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
                mnk::BlockSizes, mnks::BlockSizes...)
    (M,N) = size(C)
    K = size(A,2)
    (1:M,1:K) == axes(A) && (1:K,1:N) == axes(B) && (1:M,1:N) == axes(C) || throw(dimerr)
    (C===A || C===B) && throw(aliaserr)

    mymul!(C, A, B, Fixed(false), ftrue,
        M÷mnk.m, N÷mnk.n, K÷mnk.k,
        Fixed(M%mnk.m), Fixed(N%mnk.n), Fixed(K%mnk.k),
        mnk.m, mnk.n, mnk.k,
        mnks...)
end

function mymul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
                beta::Number, inbounds::FixedOrBool,
                tm::Integer, tn::Integer, tk::Integer,
                rm::Integer, rn::Integer, rk::Integer,
                m::Integer, n::Integer, k::Integer)
    if !inbounds
        checkmulbounds(A, B, C, Base.OneTo(tm*m+rm), Base.OneTo(tn*n+rn), Base.OneTo(tk*k+rk))
    end
    for i=0:tm-1
        mm = i*m .+ FixedOneTo(m)
        myrowsmul!(C, A, B, beta, ftrue, mm, tn, tk, rn, rk, n, k)
    end
    if rm>0
        mm = tm*m .+ FixedOneTo(rm)
        myrowsmul!(C, A, B, beta, ftrue, mm, tn, tk, rn, rk, n, k)
    end
end

#C[mm,:] <- A[mm,:]*B + beta*C[mm,:]
@inline function myrowsmul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
        beta::Number, inbounds::FixedOrBool,
        mm::AbstractUnitRange{<:Integer}, tn::Integer, tk::Integer,
        rn::Integer, rk::Integer, n::Integer, k::Integer)
    if !inbounds
        checkmulbounds(inbounds, A, B, C, mm, Base.OneTo(tn*n+rn), Base.OneTo(tk*k+rk))
    end
    for j=0:tn-1
        nn = j*n .+ FixedOneTo(n)
        mysubmatmul!(C, A, B, beta, ftrue, mm, nn, tk, rk, k)
    end
    if rn>0
        nn = tn*n .+ FixedOneTo(rn)
        mysubmatmul!(C, A, B, beta, ftrue, mm, nn, tk, rk, k)
    end
end

# This is allocating unless k and rk are `FixedInteger`s
# C[mm,nn] <- A[mm,:]*B[:,nn] + beta*C[mm,nn]
@inline function mysubmatmul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
        beta::Number, inbounds::FixedOrBool,
        mm::AbstractUnitRange{<:Integer}, nn::AbstractUnitRange{<:Integer},
        tk::Integer, rk::Integer, k::Integer)
    if !inbounds
        checkmulbounds(inbounds, A, B, C, mm, nn, Base.OneTo(tk*k+rk))
    end
    X = beta * load(SMatrix, C, mm, nn, ftrue)
    for h=0:tk-1
        kk = h*k .+ FixedOneTo(k)
        X += load(SMatrix, A, mm, kk, ftrue) * load(SMatrix, B, kk, nn, ftrue)
    end
    if rk>0
        kk = tk*k .+ FixedOneTo(rk)
        X += load(SMatrix, A, mm, kk, ftrue) * load(SMatrix, B, kk, nn, ftrue)
    end
    store!(C, mm, nn, X, ftrue)
    return nothing
end

# @inline function mysubmatmul!b(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
#         mm::AbstractUnitRange{<:Integer}, nn::AbstractUnitRange{<:Integer},
#         tk::Integer, rk::Integer, k::Integer, beta::Number, bs::BlockSizes...)
#     kk = FixedOneTo(k)
#     submatmul!(C, A, B, mm, nn, kk, beta, bs...)
#     for h=1:tk-1
#         kk = h*k .+ FixedOneTo(k)
#         submatmul!(C, A, B, mm, nn, kk, Fixed(1), bs...)
#     end
#     if rk>0
#         kk = tk*k .+ FixedOneTo(rk)
#         submatmul!(C, A, B, mm, nn, kk, Fixed(1), bs...)
#     end
#     return nothing
# end

# Fast, zero-size LinearIndices for Static matrices if we define:
Base.axes(A::StaticArray) = map(FixedOneTo, size(A))

"Read a small subset of a StaticMatrix into a StaticMatrix of given type"
@generated function load(::Type{T}, C::StaticMatrix{M,N},
        mm::FixedUnitRange{Int,IM,FixedInteger{m}},
        nn::FixedUnitRange{Int,IN,FixedInteger{n}},
        inbounds::FixedOrBool) where {T,M,N,IM,IN,m,n}
    a = Vector{Expr}()
    L = LinearIndices((M,N))
    for j=1:n
        for i=1:m
            push!(a, :( C[k+$(L[i,j])] ))
        end
    end
    return quote
        Base.@_inline_meta
        if !inbounds
             checkbounds(C, mm, nn)
        end
        k = M*zeroth(nn) + zeroth(mm)
        @inbounds T{m,n}($(Expr(:tuple, a...)))
    end
end
# Note, reading into a transpose is slow. Probably best to read first,
# then transpose.

"Store a small StaticMatrix into a subset of a StaticMatrix"
@generated function store!(C::StaticMatrix{M,N},
        mm::FixedUnitRange{Int,IM,FixedInteger{m}},
        nn::FixedUnitRange{Int,IN,FixedInteger{n}},
        X::StaticMatrix{m,n}, inbounds::FixedOrBool) where {M,N,IM,IN,m,n}
    a = Vector{Expr}()
    y = Vector{Expr}()
    L = LinearIndices((M,N))
    Ly = LinearIndices((m,n))
    for j=1:n
        for i=1:m
            push!(a, :( C[k+$(L[i,j])] ))
            push!(y, :( X[$(Ly[i,j])] ))
        end
    end
    return quote
        Base.@_inline_meta
        if !inbounds
             checkbounds(C, mm, nn)
        end
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

MA = MMatrix{16,16}(randn(16,16))
MB = MMatrix{16,16}(randn(16,16))
MC = MMatrix{16,16}(zeros(16,16))

MatMatMulExample.mymul!(C, A, B, MatMatMulExample.BlockSizes(Fixed(4), Fixed(4), Fixed(4)))

println("Relative inaccuracy compared to BLAS = ", maximum(abs.(C .-  Float64.(big.(A)*big.(B)))) / maximum(abs.(A*B .-  Float64.(big.(A)*big.(B)))))

println(@benchmark MatMatMulExample.mymul!($C, $A, $B, $(MatMatMulExample.BlockSizes(Fixed(4), Fixed(4), Fixed(2)))))

println(@benchmark MatMatMulExample.mymul!($MC, $MA, $MB, $(MatMatMulExample.BlockSizes(Fixed(4), Fixed(4), Fixed(2)))))
