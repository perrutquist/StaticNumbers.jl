# For beenchmarking, use: julia -O3

# Note: I'd like to be able to do this with `view`s that result
# in fixed-size subarrays, but at the moment, those are still allocating.
# So passing ranges as separate arguments instead.

module MatMatMulExample

using FixedNumbers
using StaticArrays
using LinearAlgebra

set_zero_subnormals(true) # Don't want subnormals to interfer with benchmarking

#Fast, zero-size LinearIndices for Static matrices if we define:
Base.axes(A::StaticArray) = map(FixedOneTo, size(A))

"A struct that stores the block size used in matmatmul"
struct BlockSize{M<:Integer,N<:Integer,K<:Integer}
    m::M
    n::N
    k::K
end

"A struct that stores the matrices and sizes used in matmul"
struct MulArgs{MA<:AbstractArray,MB<:AbstractArray,MC<:AbstractArray,M<:FixedOrInt,N<:FixedOrInt,K<:FixedOrInt}
    A::MA
    B::MB
    C::MC
    m::M
    n::N
    k::K
    function MulArgs(A::MA,B::MB,C::MC,m::M,n::N,k::K) where {MA<:AbstractArray,MB<:AbstractArray,MC<:AbstractArray,M<:Integer,N<:Integer,K<:Integer}
        @boundscheck begin
            (1:m,1:k) == axes(A) && (1:k,1:n) == axes(B) && (1:m,1:n) == axes(C) || throw(dimerr)
        end
        new{MA,MB,MC,M,N,K}(A,B,C,m,n,k)
    end
end
Base.@propagate_inbounds function MulArgs(A,B,C)
    (m,n) = sz(C)
    k = sz(A,2)
    # MulArgs(A,B,C,Fixed(m),Fixed(n),Fixed(k)) # Compiles for every size !
    MulArgs(A,B,C,m,n,k) # slower.
end
function MulArgs(A::StaticMatrix{m,k},B::StaticMatrix{k,n},C::StaticMatrix{m,n}) where {m,n,k}
    @inbounds MulArgs(A,B,C,Fixed(m),Fixed(n),Fixed(k))
end

"A version of size, which gives `FixedInteger`s for `StaticArray`s"
sz(A) = size(A)
sz(A::StaticArray) = Fixed.(size(A))
sz(A, d) = sz(A)[d]

const dimerr = DimensionMismatch("Incompatible matrix axes")
const aliaserr = ErrorException("Destination matrix cannot be one of inputs")
const rerr = DimensionMismatch("Subrange error.")

"""
Check that ranges are in bounds for `mymul`.
"""
@inline function checkmulbounds(ABC::MulArgs, mm::AbstractRange, nn::AbstractRange, kk::AbstractRange)
    #println(mm, ",\t ", nn, ",\t ", kk)
    1 <= first(mm) <= ABC.m &&
    1 <= last(mm) <= ABC.m &&
    1 <= first(nn) <= ABC.n &&
    1 <= last(nn) <= ABC.n &&
    1 <= first(kk) <= ABC.k &&
    1 <= last(kk) <= ABC.k || throw(rerr)
end

"""
C <- A*B + beta*C

If `inbounds` is `true` then no check is performed that matrix sizes match
block specifications. This can lead to memory corruption.
"""
function mymul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix,
                beta::Number,
                mnk::BlockSize, mnks::BlockSize...)
    mymul!(MulArgs(A,B,C), beta::Number,  mnk, mnks...)
end

@inline function mymul!(ABC::MulArgs, beta::Number,
                mnk::BlockSize, mnks::BlockSize...)
    mymul!(ABC, beta,
        Base.OneTo(ABC.m), Base.OneTo(ABC.n), Base.OneTo(ABC.k),
        mnk, mnks...)
end

# TODO: All args to this function end up in an allocation. Can we avoid this?
function mymul!(ABC::MulArgs, beta::Number,
                mm::AbstractUnitRange{<:Integer}, nn::AbstractUnitRange{<:Integer}, kk::AbstractUnitRange{<:Integer},
                mnk::BlockSize)

     mymul!(ABC, beta,
         mm, nn, kk,
         length(mm)÷mnk.m, length(nn)÷mnk.n, length(kk)÷mnk.k,
         fixedmod(length(mm), mnk.m), fixedmod(length(nn), mnk.n), fixedmod(length(kk), mnk.k),
         mnk.m, mnk.n, mnk.k)
end
function mymul!(ABC::MulArgs, beta::Number,
                mm::AbstractUnitRange{<:Integer}, nn::AbstractUnitRange{<:Integer}, kk::AbstractUnitRange{<:Integer},
                mnk::BlockSize, mnk2::BlockSize)

     mymul!(ABC, beta,
         mm, nn, kk,
         length(mm)÷mnk.m, length(nn)÷mnk.n, length(kk)÷mnk.k,
         fixedmod(length(mm), mnk.m), fixedmod(length(nn), mnk.n), fixedmod(length(kk), mnk.k),
         mnk.m, mnk.n, mnk.k,
         mnk2)
end

# C <- A*B
mymul!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix, mnks::BlockSize...) = mymul!(C, A, B, Fixed(false), mnks...)
mymul!(ABC::MulArgs, mnk1::BlockSize) = mymul!(ABC, Fixed(false), mnk1)
mymul!(ABC::MulArgs, mnk1::BlockSize, mnk2::BlockSize) = mymul!(ABC, Fixed(false), mnk1, mnk2)

@inline function mymul!(ABC::MulArgs, beta::Number,
                mm::AbstractUnitRange{<:Integer}, nn::AbstractUnitRange{<:Integer}, kk::AbstractUnitRange{<:Integer},
                tm::Integer, tn::Integer, tk::Integer,
                rm::Integer, rn::Integer, rk::Integer,
                m::Integer, n::Integer, k::Integer,
                mnks::BlockSize...)
    for i=0:tm-1
        mm1 = @inbounds mm[i*m .+ FixedOneTo(m)]
        mymul!(ABC, beta, mm1, nn, kk, tn, tk, rn, rk, n, k, mnks...)
    end
    if rm>0
        mm1 = @inbounds mm[tm*m .+ FixedOneTo(rm)]
        mymul!(ABC, beta, mm1, nn, kk, tn, tk, rn, rk, n, k, mnks...)
    end
end

#C[mm,:] <- A[mm,:]*B + beta*C[mm,:]
function mymul!(ABC::MulArgs, beta::Number,
        mm::AbstractUnitRange{<:Integer}, nn::AbstractUnitRange{<:Integer}, kk::AbstractUnitRange{<:Integer},
        tn::Integer, tk::Integer,
        rn::Integer, rk::Integer,
        n::Integer, k::Integer,
        mnks::BlockSize...)
    for j=0:tn-1
        nn1 = @inbounds nn[j*n .+ FixedOneTo(n)]
        mymul!(ABC, beta, mm, nn1, kk, tk, rk, k, mnks...)
    end
    if rn>0
        nn1 = @inbounds nn[tn*n .+ FixedOneTo(rn)]
        mymul!(ABC, beta, mm, nn1, kk, tk, rk, k, mnks...)
    end
end

# C[mm,nn] <- A[mm,:]*B[:,nn] + beta*C[mm,nn]
function mymul!(ABC::MulArgs, beta::Number,
        mm::AbstractUnitRange{<:Integer}, nn::AbstractUnitRange{<:Integer}, kk::AbstractUnitRange{<:Integer},
        tk::Integer, rk::Integer, k::Integer)
    X = beta * load(SMatrix, ABC.C, ABC.m, mm, nn)
    for h=0:tk-1
        kk1 = @inbounds kk[h*k .+ FixedOneTo(k)]
        X += load(SMatrix, ABC.A, ABC.m, mm, kk1) * load(SMatrix, ABC.B, ABC.k, kk1, nn)
    end
    if rk>0
        kk1 = @inbounds kk[tk*k .+ FixedOneTo(rk)]
        X += load(SMatrix, ABC.A, ABC.m, mm, kk1) * load(SMatrix, ABC.B, ABC.k, kk1, nn)
    end
    store!(ABC.C, ABC.m, mm, nn, X)
    return nothing
end

@inline function mymul!(ABC::MulArgs, beta::Number,
        mm::AbstractUnitRange{<:Integer}, nn::AbstractUnitRange{<:Integer}, kk::AbstractUnitRange{<:Integer},
        tk::Integer, rk::Integer, k::Integer, mnks::BlockSize...)
    kk1 = @inbounds kk[FixedOneTo(k)]
    if tk>0
        mymul!(ABC, beta, mm, nn, kk1, mnks...)
    end
    for h=1:tk-1
        kk1 = @inbounds kk[h*k .+ FixedOneTo(k)]
        mymul!(ABC, Fixed(1), mm, nn, kk1, mnks...)
    end
    if rk>0
        kk1 = @inbounds kk[tk*k .+ FixedOneTo(rk)]
        mymul!(ABC, Fixed(1), mm, nn, kk1, mnks...)
    end
    return nothing
end

@generated function load(::Type{T}, C::AbstractMatrix, M::Integer,
        mm::FixedUnitRange{Int,IM,FixedInteger{m}},
        nn::FixedUnitRange{Int,IN,FixedInteger{n}}) where {T,IM,IN,m,n}
    a = Vector{Expr}()
    for j=1:n
        for i=1:m
            push!(a, :( C[k+$i+$(j-1)*M] ))
        end
    end
    return quote
        Base.@_inline_meta
        k = M*zeroth(nn) + zeroth(mm)
        @inbounds T{m,n}($(Expr(:tuple, a...)))
    end
end

# # Note, reading into a transpose is slow. Probably best to read first,
# then transpose.

@generated function store!(C::AbstractMatrix, M::Integer,
        mm::FixedUnitRange{Int,IM,FixedInteger{m}},
        nn::FixedUnitRange{Int,IN,FixedInteger{n}},
        X::StaticMatrix{m,n}) where {IM,IN,m,n}
    a = Vector{Expr}()
    for j=1:n
        for i=1:m
            push!(a, :( C[k+$i+$(j-1)*M] ))
        end
    end
    return quote
        Base.@_inline_meta
        k = zeroth(mm)+zeroth(nn)*M
        @inbounds $(Expr(:tuple, a...)) = X.data
        nothing
    end
end

# TODO Transpose and Adjoint

end # module

using FixedNumbers
using StaticArrays
using LinearAlgebra
using BenchmarkTools
using Profile

m=17
n=18
k=19

const A = randn(m,k)
const B = randn(k,n)
const C = zeros(m,n)

# const MA = MMatrix{m,k}(A)
# const MB = MMatrix{k,n}(B)
# const MC = zeros(MMatrix{m,n})

const ABC = MatMatMulExample.MulArgs(A,B,C)
# const MABC = MatMatMulExample.MulArgs(MA,MB,MC)

const blk = MatMatMulExample.BlockSize(Fixed(4), Fixed(4), Fixed(2))
MatMatMulExample.mymul!(ABC, blk)
println("1-block. Relative inaccuracy compared to BLAS = ", maximum(abs.(C .-  Float64.(big.(A)*big.(B)))) / maximum(abs.(A*B .-  Float64.(big.(A)*big.(B)))))

const blk1 = MatMatMulExample.BlockSize(Fixed(4), Fixed(4), Fixed(2))
const blk2 = MatMatMulExample.BlockSize(Fixed(8), Fixed(8), Fixed(4))
MatMatMulExample.mymul!(ABC, blk1, blk2)
println("2-block. Relative inaccuracy compared to BLAS = ", maximum(abs.(C .-  Float64.(big.(A)*big.(B)))) / maximum(abs.(A*B .-  Float64.(big.(A)*big.(B)))))
#println(C - A*B)

function profiletarget(ABC, blk, iters)
    for i=1:iters
        MatMatMulExample.mymul!(ABC, blk)
    end
end
profiletarget(ABC, blk, 1)

# @profile profiletarget(ABC, blk, 100000)
# Profile.print(C=true, mincount=5)

println("mul!, BLAS=", BLAS.vendor())
display(@benchmark mul!($C, $A, $B) samples=10 evals=1000)
println()

struct Wrapfloat <: Real
    x::Float64
end
Base.:*(x::Wrapfloat, y::Wrapfloat) = Wrapfloat(x.x*y.x)
Base.:+(x::Wrapfloat, y::Wrapfloat) = Wrapfloat(x.x+y.x)
Base.zero(::Wrapfloat) = Wrapfloat(0.0)
const WA = Wrapfloat.(A)
const WB = Wrapfloat.(B)
const WC = Wrapfloat.(C)
const WABC = MatMatMulExample.MulArgs(WA,WB,WC)
println("mul!, Wrapfloat")
display(@benchmark mul!($WC, $WA, $WB) samples=10 evals=10000)
println()

println("mymul!, Wrapfloat")
display(@benchmark MatMatMulExample.mymul!($WABC, $blk) samples=10 evals=10000)
println()

println("mymul!, 1-block Matrix")
display(@benchmark MatMatMulExample.mymul!($ABC, $blk) samples=10 evals=10000)
println()

println("mymul!, 2-block Matrix")
display(@benchmark MatMatMulExample.mymul!($ABC, $blk1, $blk2) samples=10 evals=10000)
println()

# println("mymul!, MMatrix")
# display(@benchmark MatMatMulExample.mymul!($MABC, $blk) samples=10 evals=10000)
# println()

println("Gather malloc statistics...")
Profile.clear_malloc_data()
#MatMatMulExample.mymul!(ABC, blk)
MatMatMulExample.mymul!(ABC, blk1, blk2)
