# Introduction

This package provides number datatypes which store their data in a
type parameter. These are referred to as static numbers. (The word
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
situations where it might not otherwise have done so. 

Under the surface, there are three `Static` datatypes: `StaticInteger`,
`StaticReal`, and `StaticNumber`, subtypes of `Integer`, `Real` and `Number`
respectively. The `Union` type `Static` can be used to refer to them all.
For brevity, all three types are displayed as `static(X)`, and it is also
recommended to create them using this syntax.

Note: At the moment, the type union is named `Static` with a capital S, while the function
that creates static variables is named `static` in lowercase.

By default, any operation on a `Static` will result in a non-`Static` type.
For example, `static(2)+static(2)` gives `4`, not `static(4)`.

The macro `@stat` makes the result of a computation a `Static` when all arguments are static
or literals. For example:
```julia
i = 2
s = static(2)
s + s        # returns 4 - not static(4), even though s is static.
@stat s + s  # returns static(4)
@stat s + 2  # also returns static(4), since 2 is a literal
@stat s + i  # returns 4, because i is not a static number
```

If the `@stat` macro is used with pure (and relatively simple) functions, then the Julia
compiler will be able to infer the return type, resulting in performant code.

Static numbers can be used to create ranges with static length. Such ranges can be used to
create tuples in an efficient manner. For example:
```julia
Tuple(i^2 for i in static(1):static(4)) # computed at compile time
Tuple(i^2 for i in 1:4) # computed at runtime, length of the tuple is not inferred (as of Julia 1.3.1).
```

Indexing into tuples can also be a lot more efficient with static numbers. For example:
```julia
t = (1, 2, 3, 4)
t[@stat 2:end-1] # fast, type stable and non-allocating
t[2:end-1] # much less performant (as of Julia 1.3.1)
```
(Indexing tuples without static numbers will work better in the future, see [pull request 31138](https://github.com/JuliaLang/julia/pull/31138).)

When creating static numbers, it is important to consider whether the type
system will be able to work efficiently. For example, `f(static(x), y)` is
likely slower than `f(x, y)` even when called repeatedly with the same `x`.
A specialized method of `f` is created for this value of `x`, and the function
call itself will be faster. But since the type system will not know the type
of `static(x)` in advance, a dynamic dispatch will happen at each function call.

On the other hand, something like `f(x==0 ? static(0) : x, y)` will typically be
fast. The construct `x==0 ? static(0) : x` will belong to `Union{typeof(x), static(0)}`,
and Julia is able to dispatch efficiently on small type unions.
Shorthands for this construct are `f(trystatic(x, 0), y)` and `f(x ⩢ 0, y)`.

It is important not to make the set of static numbers (i.e. types) too large,
as [this can lead to a lot of compilation overhead](https://docs.julialang.org/en/v1/manual/performance-tips/index.html#The-dangers-of-abusing-multiple-dispatch-(aka,-more-on-types-with-values-as-parameters)-1).
The `@stat` macro can be dangerous when used inside loops or in recursive functions.

There is no `StaticRational` datatype, but a `StaticReal` with a
`Rational` type parameter will convert and promote like its parameter.
For example: `static(1//2) + 1 === 3//2`.

The `Unsigned` datatype currently does not work well with static numbers.
For this reason  the `@stat` macro does turn unsigned numbers into static.
(This is work in progress, and subjec to change.)

Static numbers are only fast when fully specified. A `Vector{Static}`
is much slower than a `Vector{Int}`.
(A `Vector{StaticInteger{1}}` is fast and requires very little memory,
but on the other hand it can only store the number one.)

[Source code on GitHub](https://github.com/perrutquist/StaticNumbers.jl)
