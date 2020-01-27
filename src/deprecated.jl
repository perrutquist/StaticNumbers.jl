Base.@deprecate ofstatictype(x, y) (z = oftype(unstatic(x), y); x isa Static ? static(z) : z)

macro staticnumbers(args::Expr, funs1::Expr, funs2::Expr, targets::Expr=:(()))
    @warn "The @staticnumbers macro has been renamed @generate_static_methods." # Not the right way to do this.
    args.head == :tuple || error("Expected a Tuple of numbers")
    funs1.head == :tuple || error("Expected a Tuple of 1-arg functions")
    funs2.head == :tuple || error("Expected a Tuple of 2-arg functions")
    targets.head == :tuple || error("Expected a Tuple of target numbers")
    return esc(Expr(:block,vcat(
       genstaticmethods1(funs1.args, args.args, targets.args),
       genstaticmethods2(funs2.args, args.args, targets.args)
       )...))
end
