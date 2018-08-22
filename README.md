# FixedNumbers.jl

This package provides `Fixed` datatypes which store their data in a type parameter.

Data that is passed in type parameters is (usually) handled at compile-time,
rather than at run-time. In certain cases this can lead to slightly
better performance.
For this reason, some functions accept "value type" arguments, `Val{X}()`,
where `X` is an argument that is passed at compile-time. (See the Julia
documentation.)

The difference between `Val{X}()` and `Fixed{X}()` is that `Fixed` numbers
promote and convert like their type parameters, so they can be used directly in
arithmetic operations. (For example `Fixed(1) + 1` equals `2`.)
This makes it possible to use them with functions that were not specifically
written to accept value arguments.

Under the surface, there are three `Fixed` datatypes: `FixedInteger`,
`FixedReal`, and `FixedNumber`, subtypes of `Integer`, `Real` and `Number`
respectively. The `Union` type `Fixed{X}` can be used to refer to them all.
For brevity, all three types are displayed as `Fixed(X)`, and it is also
recommended to create them using this syntax.

There is currently no `FixedRational` datatype, but a `FixedReal` with a
`Rational` type parameter will convert and promote its parameter.
For example: `Fixed(1//2) + 1 === 3//2`.

Note: `Fixed` numbers are only fast when fully specified. A `Vector{Fixed}`
is much slower than a `Vector{Int}`.
A `Vector{FixedInteger{1}}` is fast and requires very little memory,
but on the other hand it can only store the number one.