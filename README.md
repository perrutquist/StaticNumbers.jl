# StaticNumbers.jl

[![Build Status](https://travis-ci.org/perrutquist/StaticNumbers.jl.svg?branch=master)](https://travis-ci.org/perrutquist/StaticNumbers.jl)
[![codecov.io](http://codecov.io/github/perrutquist/StaticNumbers.jl/coverage.svg?branch=master)](http://codecov.io/github/perrutquist/StaticNumbers.jl?branch=master)
[![PkgEval](https://juliaci.github.io/NanosoldierReports/pkgeval_badges/S/StaticNumbers.svg)](https://juliaci.github.io/NanosoldierReports/pkgeval_badges/report.html)
[![](https://img.shields.io/badge/docs-latest-blue.svg)](https://perrutquist.github.io/StaticNumbers.jl/dev/index.html)

This package provides number datatypes which store their values in type parameters, making them runtime constants.

Note: The glue code to StaticArrays.jl and SIMD.jl that was loaded automatically (using Requires.jl) in StaticNumbers release 0.3.3 is no longer loaded by default.
It can be loaded with by running `StaticNumbers.@glue_to StaticArrays` and/or `StaticNumbers.@glue_to SIMD`, if needed.

See the [documentation](https://perrutquist.github.io/StaticNumbers.jl/dev/index.html) for more info.
