import SIMD: Vec

@inline Vec(g::Base.Generator{<:LengthRange}) = Vec(Tuple(g))
