# StaticNumbers.jl

[![Build Status](https://travis-ci.org/perrutquist/StaticNumbers.jl.svg?branch=master)](https://travis-ci.org/perrutquist/StaticNumbers.jl)
[![codecov.io](http://codecov.io/github/perrutquist/StaticNumbers.jl/coverage.svg?branch=master)](http://codecov.io/github/perrutquist/StaticNumbers.jl?branch=master)

This package provides `Number` datatypes which store their data in a
type parameter. These are referred to as `StaticNumber`s. (The word
"static" has lots of uses in computer science. Here, it means
that the number is constant at runtime.)

Data that is passed in type parameters is (usually) handled at compile-time,
rather than at run-time. In certain cases this can lead to better performance.
For this reason, some functions accept [value type](https://docs.julialang.org/en/v1/manual/types/index.html#%22Value-types%22-1) arguments, `Val{X}`,
where `X` is an argument that is passed at compile-time.
`Static` is an alternative to `Val` which is specifically
designed to handle numbers.

The difference between `Val` and `Static` is that `Static` types
promote and convert like their type parameters, so they can be used directly in
arithmetic operations. (For example `static(1) + 1` equals `2`.)
This makes it possible to use them with functions that were not specifically
written to accept value arguments, essentially forcing the Julia compiler to do
[constant propagation](https://en.wikipedia.org/wiki/Constant_folding) in
situations where it might not otherwise have done so. For example, we can
abuse it to make the compiler compute the 20:th Fibonacci number:
```julia
julia> using StaticNumbers

julia> fib(n) = n <= 1 ? n : fib(static(n-1)) + fib(static(n-2))
fib (generic function with 1 method)

julia> @code_llvm fib(static(20))

;  @ REPL[2]:1 within `fib'
define i64 @julia_fib_12331() {
top:
  ret i64 6765
}
```
(In the process, the compiler builds a [memoization](https://en.wikipedia.org/wiki/Memoization)
table of the first 20 Fibonacci numbers as part of the dispatch table.)

Under the surface, there are three `Static` datatypes: `StaticInteger`,
`StaticReal`, and `StaticNumber`, subtypes of `Integer`, `Real` and `Number`
respectively. The `Union` type `Static` can be used to refer to them all.
For brevity, all three types are displayed as `static(X)`, and it is also
recommended to create them using this syntax.

Note: At the moment, the type union is named `Static`, while the function
that creates static variables is named `static`.

By default, any operation on a `Static` will result in a non-`Static` type.
For example, `static(2)+static(2)` gives `4`, not `static(4)`.
The function `maybe_static` makes the result of a computation `Static` when
all arguments are static. For example, `maybe_static(+, static(2), static(2))`
will return `static(4)`. (Julia is often able to infer the return type.)

It is of course also possible to overload methods to return `Static` for `Static`
inputs. The `@generate_static_methods` macro can be used on a small set of `Static` numbers
to make certain operations preserve the `Static` type when possible.

When creating `Static` numbers, it is important to consider whether the type
system will be able to work efficiently. For example, `f(static(x), y)` is
likely slower than `f(x, y)` even when called repeatedly with the same `x`.
A specialized method of `f` is created for this value of `x`, and the function
call itself will be faster. But since the type system will not know the type
of `static(x)` in advance, a dynamic dispatch will happen at each function call.

On the other hand, something like `f(x==0 ? static(0) : x, y)` will typically be
fast. The construct `x==0 ? static(0) : x` will belong to `Union{typeof(x), static(0)}`,
and Julia is able to dispatch efficiently on small type unions.
Shorthands for this construct are `f(trystatic(x, 0), y)` and `f(x ⩢ 0, y)`.

It is important not to make the set of `Static` numbers too large,
as [this can lead to a lot of compilation overhead](https://docs.julialang.org/en/v1/manual/performance-tips/index.html#The-dangers-of-abusing-multiple-dispatch-(aka,-more-on-types-with-values-as-parameters)-1).

There is no `StaticRational` datatype, but a `StaticReal` with a
`Rational` type parameter will convert and promote like its parameter.
For example: `static(1//2) + 1 === 3//2`.

`Static` numbers are only fast when fully specified. A `Vector{Static}`
is much slower than a `Vector{Int}`.
(A `Vector{StaticInteger{1}}` is fast and requires very little memory,
but on the other hand it can only store the number one.)
