
"""
Create an expression a method for `Fixed` input to `fun` if the result
is among the `targets`
"""
function genfixedmethod1(fun, arg1, targets)
    r = try
        @eval $fun($arg1)
    catch
        return nothing
    end
    ts = filter(isequal(r), targets)
    if length(ts) == 1
        return :( $fun(::$(typeof(Fixed(arg1)))) = $(Fixed(first(ts))) )
    end
    return nothing
end

"""
Create an expression a method for two `Fixed` inputs to `fun` if the result
is among the `targets`
"""
function genfixedmethod2(fun, arg1, arg2, targets)
    r = try
        @eval $fun($arg1, $arg2)
    catch
        return nothing
    end
    ts = filter(isequal(r), targets)
    if length(ts) == 1
        return :( $fun(::$(typeof(Fixed(arg1))),::$(typeof(Fixed(arg2)))) = $(Fixed(first(ts))) )
    end
    return nothing
end

"""
Create an array of all the methods for `Fixed` 1-argument inputs to `fun`
where the result is either among the possible inputs, `args`, or among
the additional `targets`.
"""
function genfixedmethods1(funs, args, targets)
    m = Vector{Expr}()
    tu = union(args, targets)
    for f in funs
        for a in args
            e = genfixedmethod1(f, a, tu)
            if e isa Expr
                push!(m, e)
            end
        end
    end
    return m
end

"""
Create an array of all the methods for `Fixed` 2-argument inputs to `fun`
where the result is either among the possible inputs, `args`, or among
the additional `targets`.
"""
function genfixedmethods2(funs, args, targets)
    m = Vector{Expr}()
    tu = union(args, targets)
    for f in funs
        for a1 in args
            for a2 in args
                e = genfixedmethod2(f, a1, a2, tu)
                if e isa Expr
                    push!(m, e)
                end
            end
        end
    end
    return m
end

"""
@fixednumbers numbers 1argfuns 2argfuns

This macro creates methods that return `Fixed` numbers when
functions are called with only `Fixed` arguments.

The inputs should be a list of literal numbers that will be tested as
inputs and outputs of all functions.

Optionally a fourth argument can give a list of numbers that will only
be considered as results, but not as inputs.

Example:
```
@fixednumbers (0, 1) (sin, cos) (+, -)
```
will create all the following method definitions:
```
sin(::FixedInteger{0}) = FixedInteger{0}()
cos(::FixedInteger{0}) = FixedInteger{1}()
+(::FixedInteger{0}, ::FixedInteger{0}) = FixedInteger{0}()
+(::FixedInteger{0}, ::FixedInteger{1}) = FixedInteger{1}()
+(::FixedInteger{1}, ::FixedInteger{0}) = FixedInteger{1}()
-(::FixedInteger{0}, ::FixedInteger{0}) = FixedInteger{0}()
-(::FixedInteger{1}, ::FixedInteger{0}) = FixedInteger{0}()
-(::FixedInteger{1}, ::FixedInteger{1}) = FixedInteger{1}()
```
(Note: The macro will run in the local scope. Functions from `Base`
must be imported before they can be extended.)
"""
macro fixednumbers(args::Expr, funs1::Expr, funs2::Expr, targets::Expr=:(()))
    args.head == :tuple || error("Expected a Tuple of numbers")
    funs1.head == :tuple || error("Expected a Tuple of 1-arg functions")
    funs2.head == :tuple || error("Expected a Tuple of 2-arg functions")
    targets.head == :tuple || error("Expected a Tuple of target numbers")
    return esc(Expr(:block,vcat(
       genfixedmethods1(funs1.args, args.args, targets.args),
       genfixedmethods2(funs2.args, args.args, targets.args)
       )))
end
