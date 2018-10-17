
"""
Create an expression a method for `Static` input to `fun` if the result
is among the `targets`
"""
function genstaticmethod1(fun, arg1, targets)
    r = try
        @eval $fun($arg1)
    catch
        return nothing
    end
    ts = filter(isequal(r), targets)
    if length(ts) == 1
        return :( $fun(::$(typeof(static(arg1)))) = $(static(first(ts))) )
    end
    return nothing
end

"""
Create an expression a method for two `Static` inputs to `fun` if the result
is among the `targets`
"""
function genstaticmethod2(fun, arg1, arg2, targets)
    r = try
        @eval $fun($arg1, $arg2)
    catch
        return nothing
    end
    ts = filter(isequal(r), targets)
    if length(ts) == 1
        return :( $fun(::$(typeof(static(arg1))),::$(typeof(static(arg2)))) = $(static(first(ts))) )
    end
    return nothing
end

"""
Create an array of all the methods for `Static` 1-argument inputs to `fun`
where the result is either among the possible inputs, `args`, or among
the additional `targets`.
"""
function genstaticmethods1(funs, args, targets)
    m = Vector{Expr}()
    tu = union(args, targets)
    for f in funs
        for a in args
            e = genstaticmethod1(f, a, tu)
            if e isa Expr
                push!(m, e)
            end
        end
    end
    return m
end

"""
Create an array of all the methods for `Static` 2-argument inputs to `fun`
where the result is either among the possible inputs, `args`, or among
the additional `targets`.
"""
function genstaticmethods2(funs, args, targets)
    m = Vector{Expr}()
    tu = union(args, targets)
    for f in funs
        for a1 in args
            for a2 in args
                e = genstaticmethod2(f, a1, a2, tu)
                if e isa Expr
                    push!(m, e)
                end
            end
        end
    end
    return m
end

"""
@staticnumbers numbers 1argfuns 2argfuns

This macro creates methods that return `Static` numbers when
functions are called with only `Static` arguments.

The inputs should be a list of literal numbers that will be tested as
inputs and outputs of all functions.

Optionally a fourth argument can give a list of numbers that will only
be considered as results, but not as inputs.

Example:
```
@staticnumbers (0, 1) (sin, cos) (+, -)
```
will create all the following method definitions:
```
sin(::StaticInteger{0}) = StaticInteger{0}()
cos(::StaticInteger{0}) = StaticInteger{1}()
+(::StaticInteger{0}, ::StaticInteger{0}) = StaticInteger{0}()
+(::StaticInteger{0}, ::StaticInteger{1}) = StaticInteger{1}()
+(::StaticInteger{1}, ::StaticInteger{0}) = StaticInteger{1}()
-(::StaticInteger{0}, ::StaticInteger{0}) = StaticInteger{0}()
-(::StaticInteger{1}, ::StaticInteger{0}) = StaticInteger{0}()
-(::StaticInteger{1}, ::StaticInteger{1}) = StaticInteger{1}()
```
(Note: The macro will run in the local scope. Functions from `Base`
must be imported before they can be extended.)
"""
macro staticnumbers(args::Expr, funs1::Expr, funs2::Expr, targets::Expr=:(()))
    args.head == :tuple || error("Expected a Tuple of numbers")
    funs1.head == :tuple || error("Expected a Tuple of 1-arg functions")
    funs2.head == :tuple || error("Expected a Tuple of 2-arg functions")
    targets.head == :tuple || error("Expected a Tuple of target numbers")
    return esc(Expr(:block,vcat(
       genstaticmethods1(funs1.args, args.args, targets.args),
       genstaticmethods2(funs2.args, args.args, targets.args)
       )...))
end
