# Examples

Here are a couple of examples of how StaticNumbers can be used.

## Fibonacci numbers

We can abuse static numbers to make the compiler compute the 20:th Fibonacci number:

```
julia> using StaticNumbers

julia> fib(n) = n <= 1 ? n : fib(static(n-1)) + fib(static(n-2))
fib (generic function with 1 method)

julia> @code_llvm fib(static(20))

;  @ none:1 within `fib'
define i64 @julia_fib_12331() {
top:
  ret i64 6765
}
```

We see that the compiler has created a function that simply returns the 20th Fibonacci number (6765) without doing any run-time calculation at all. (In the process, the compiler builds a [memoization](https://en.wikipedia.org/wiki/Memoization)
table of the first 20 Fibonacci numbers as part of the dispatch table.)

## The Ackermann function

A similar example is the [Ackermann function](https://en.wikipedia.org/wiki/Ackermann_function).
We can make the first argument, `m`, a static number without causing a large number of mehtods
to be compiled, since `m` is rarely larger@ than 5. (Otherwise the computation would take forever.)

```
using StaticNumbers
function A(m,n)
    if iszero(m)
        n + one(n)
    elseif iszero(n)
        A(@stat(m - one(m)), one(n))
    else
        A(@stat(m - one(m)), A(m, n - one(n)))
    end
end
@time A(4, 1)
@time A(static(4), 1) # much faster
```

The function `A` above also works when `m` is a regular `Int`, but the use of static numbers
makes it significantly faster, even on the first call where compile time is also included.

## Fast multiplication of small matrices.

As a more complicated example, we'll look at how static numbers together with generators and recursive functions to make highly specialized code (without having to use `@generated` functions).

The following example is based on [this blog post](http://kristofferc.github.io/post/intrinsics/) describing how to use [SIMD.jl](https://github.com/eschnett/SIMD.jl) to multiply 3-by-3 matrices even faster than what [StaticArrays.jl](https://github.com/JuliaArrays/StaticArrays.jl) does. We want to generalize that example to arbitrary (but small) matrix sizes.

Note: For matrix sizes other than 3x3, this example currenlty requres Julia with [#34473](https://github.com/JuliaLang/julia/pull/34473) and [#34490](https://github.com/JuliaLang/julia/pull/34490). These are in the latest nightly builds (1.5.0-DEV.206 and later) and hopefully they will also be in 1.4.0 when it is released.

We start by defining the type `Mat`, which we'll use to hold our matrices, plus a few methods are needed to make `Mat` display and convert to other matrix types:
```julia
using StaticNumbers
using SIMD
import Base: size, getindex, IndexStyle

struct Mat{T,M,N} <: AbstractMatrix{T}
    data::NTuple{N,Vec{M,T}}
end

@inline size(::Mat{<:Any,M,N}) where {M,N} = (static(M), static(N))
Base.@propagate_inbounds getindex(A::Mat, i::Integer, j::Integer) = A.data[j][i]
@inline IndexStyle(::Type{<:Mat}) = IndexCartesian()
```
So far, the only use of StaticNumbers is in the `size` function. Because the size is returned as static numbers, the `axes` of a `Mat` will automatically become `StaticOneTo` ranges, wich is a special case of the static-length range types provided by StaticNumbers.

In constructing a `Mat` from another matrix, we use generators over static ranges:
```julia
@inline function (::Type{T})(A::AbstractMatrix) where {T<:Mat}
    T(Tuple(Vec(@inbounds(A[i,j]) for i in static(axes(A,1))) for j in static(axes(A,2))))
end
```

(StaticNumbers provide special constructors for the `Tuple` and `Vec` types that utilize these generators.)

We are using the `@inline` directive on every function that we create, since in this example we want to force the compiler to do as much work as possible. (The user must avoid creating too large matrcies with the `Mat` type, or there will be trouble.)
It is worth pointing out that Julia will go to extreme lengths to honor the `@inline` directive, as long as all type signatures are concrete. This can be used to "unroll loops" by doing tail recursion on `Tuple`s. For example with the following following function to compute the inner product of two tuples, Julia will generate explicit code for each tuple length that is encountered.
```julia
using Base: tail
@inline function inner(A::Tuple{Any}, B::Tuple{Any})
    first(A)*first(B)
end
@inline function inner(A::Tuple{Any,Vararg{Any}}, B::Tuple{Any,Vararg{Any}})
    muladd(first(A), first(B), inner(tail(A), tail(B)))
end
```

With this function in place, the actual matrix multiplicaion is done in just one line:
```julia
@inline function Base.:*(A::Mat, B::AbstractMatrix)
    Mat(Tuple(inner(A.data, Tuple(@inbounds(B[k,j]) for k in axes(A,2))) for j in static(axes(B,2))))
end
```
Again, we are using generators over static ranges to create `Tuple`s in a way that is similar to what we'd get if we wrote a `@generated` function.
(We are leaving out some the size checks here for brevity, but those would otherwise get optimized out by the compiler in the below benchmarks.)

```julia
julia> using BenchmarkTools, StaticArrays

julia> s = Ref(rand(SMatrix{3,3}))

julia> m = Ref(Mat(rand(3,3)))

julia> @btime $(s)[] * $(s)[];
  7.785 ns (0 allocations: 0 bytes)

julia> @btime $(m)[] * $(m)[];
  3.364 ns (0 allocations: 0 bytes)
```
This comparison is a bit unfair, because `SMatrix` and `Mat` have different memory layouts. In a `Mat`, there is padding so that each length-3 column vector is stored in 4 bytes for aligned loading into registeres. In order to make an appples-to-apples comparison, we should start and end with an `SMatrix`.

```julia
julia> @inline mul_as_Mat(A, B) = SMatrix{size(A,1), size(B,2)}(Mat(A) * Mat(B));

julia> @btime mul_as_Mat($(s)[], $(s)[]);
  3.366 ns (0 allocations: 0 bytes)
```
The Julia and LLVM compilers are doing an amazing job here, optimzing the constructors for `Mat` and `SMatrix`, both of which are written to accept an `AbstractArray`. We could start from any other matrix type, as long as the compiler is able to figure out the size statically. For example, let's try a `view` where the length of each axis is static.
```julia
julia> v = view(rand(7,11), range(5, length=static(3)), range(9, length=static(3)));

julia> @btime mul_as_Mat($v, $v);
9.628 ns (0 allocations: 0 bytes)
```
This is a bit slower (probably because loading data from a view involves calculating offsets based on the stride of the underlying matrix) but still this is an impressive result given that we did not write any spacialized code for views.

Footnote: Our example uses the same type of vectorization regardless of the matrix sizes, which is of course not optimal. (A 1x4 matrix is far better represented as one length-4 vector than 4 length-1 vectors, for example.) A more advanced `*` function would use a cost model to select the vectorization that works best for the sizes involved, like [LoopVectorization](https://github.com/chriselrod/LoopVectorization.jl) does.
