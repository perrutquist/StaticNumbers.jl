export maybe_static, @stat

"""
    maybe_static(f, args...)

Returns `static(f(args...))`, if all of args are `Static`. If any of the args
is not `Static`, then `f(args...)` is returned unchanged.

The `@stat` macro applies the `maybe_static` function to all function calls,
and this function can be overloaded to provide special behaviuor for certain
functions under the macro.
"""
@inline maybe_static(f::F, args...) where {F} = f(args...)
@inline maybe_static(f::F, args::Static...) where {F} = static(f(args...))

"""
Turn all constants in an expression into `static` and all
function calls into `trystatic`.
"""
statify(ex) = ex
statify(x::Number) = :( static($x) )
statify(s::Symbol) = s == :end ? :( static($s) ) : s
function statify(ex::Expr)
    if ex.head == :call
        Expr(ex.head, :maybe_static, map(statify, ex.args)...)
    else
        Expr(ex.head, map(statify, ex.args)...)
    end
end

# The name @static was taken!
macro stat(ex)
    esc(statify(ex))
end
