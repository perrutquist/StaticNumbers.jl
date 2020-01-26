Base.@deprecate ofstatictype(x, y) (z = oftype(unstatic(x), y); x isa Static ? static(z) : z)
