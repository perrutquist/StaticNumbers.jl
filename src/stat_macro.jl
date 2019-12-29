export maybe_static, @stat

"""
    maybe_static(f, args...)

Returns `static(f(args...))`, if all of `args` are `Static` and `f` returns a number.
If any of the args is not `Static`, then `f(args...)` is returned unchanged.

The `@stat` macro applies the `maybe_static` function to all function calls,
and this function can be overloaded to provide special behaviuor for certain
functions under the macro.
"""
@inline maybe_static(f::F, args...) where {F} = f(args...)
@inline function maybe_static(f::F, args::Static...) where {F}
    y = f(args...)
    y isa Number && !(y isa Bool) ? static(y) : y
end

@inline maybe_static(::typeof(nfields), t) = static(nfields(t))

@inline maybe_static(::typeof(firstindex), T::Tuple) = static(firstindex(T))
@inline maybe_static(::typeof(lastindex), T::Tuple) = static(lastindex(T))
@inline maybe_static(::typeof(length), T::Tuple) = static(length(T))

@inline maybe_static(::typeof(axes), T::Tuple) = StaticOneTo(length(T))

"""
Turn all constants in an expression into `static` and all
function calls into `trystatic`.
"""
statify(ex) = ex
statify(x::Number) = :( static($x) )
statify(s::Symbol) = s == :end ? :( static($s) ) : s
function statify(ex::Expr)
    if ex.head == :call
        if first(string(ex.args[1])) == '.'  #for example: .+
            # TODO: We should handle breadcasted maybe_static.
            # Expr(:., :maybe_static, Expr(:tuple, Symbol(string(ex.args[1])[2:end]), map(statify, ex.args[2:end])...))
            Expr(ex.head, map(statify, ex.args)...)
        else
            Expr(ex.head, :maybe_static, map(statify, ex.args)...)
        end
    #elseif ex.head == :.
    #    Expr(:., :maybe_static, Expr(:tuple, ex.args[1], map(statify, ex.args[2].args)...))
    elseif ex.head ∈ (:if, :&&, :||) || ex.head == :(=) && ex.args[1].head == :call
        Expr(ex.head, ex.args[1], map(statify, ex.args[2:end])...)
    else
        Expr(ex.head, map(statify, ex.args)...)
    end
end

# The name @static was taken!
macro stat(ex)
    esc(statify(ex))
end
