export maybe_static, @stat

"""
    maybe_static(f, args...)

Returns `static(f(args...))`, if all of `args` are `Static` and `f` returns a number.
If any of the args is not `Static`, then `f(args...)` is returned unchanged.

The `@stat` macro applies the `maybe_static` function to all function calls,
and this function can be overloaded to provide special behaviuor for certain
functions under the macro.
"""
@inline function maybe_static(f::F, args...; kwargs...) where {F}
    y = f(args...; kwargs...)
    if y isa Number && !(y isa Bool) && !(y isa Unsigned) &&
            all(map(a->a isa Static, args)) && all(map(a->a isa Static, kwargs.data))
        static(y)
    else
        y
    end
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
statify(x::Unsigned) = x
statify(s::Symbol) = s == :end ? :( static($s) ) : s
function statify(ex::Expr)
    if ex.head == :call
        if first(string(ex.args[1])) == '.'  #for example: .+
            # TODO: We should handle breadcasted maybe_static.
            # Expr(:., :maybe_static, Expr(:tuple, Symbol(string(ex.args[1])[2:end]), map(statify, ex.args[2:end])...))
            Expr(ex.head, map(statify, ex.args)...)
        elseif ex.args[1] ∈ (:oftype, :typeof)
            # Changing the type of the argument(s) makes no sense for these functions
            Expr(ex.head, :maybe_static, ex.args...)
        else
            Expr(ex.head, :maybe_static, map(statify, ex.args)...)
        end
    #elseif ex.head == :.
    #    Expr(:., :maybe_static, Expr(:tuple, ex.args[1], map(statify, ex.args[2].args)...))
    elseif ex.head ∈ (:if, :&&, :||, :(=))
        Expr(ex.head, ex.args[1], map(statify, ex.args[2:end])...)
    elseif ex.head == :ref
        Expr(ex.head, :( StaticNumbers.maybe_wrap($(statify(ex.args[1]))) ), map(statify, ex.args[2:end])...)
    else
        Expr(ex.head, map(statify, ex.args)...)
    end
end

# The name @static was taken!
macro stat(ex)
    esc(statify(ex))
end

"A wrapper type applied by the @stat macro"
struct MaybeStatic{T}
    parent::T
end

@inline Base.getindex(r::MaybeStatic, args...) = getindex(r.parent, args...)
@inline Base.step(r::MaybeStatic) = step(r.parent)
@inline Base.length(r::MaybeStatic) = length(r.parent)
@inline Base.unsafe_length(r::MaybeStatic) = length(r.parent)
@inline Base.first(r::MaybeStatic) = maybe_static(first, r.parent)
@inline Base.last(r::MaybeStatic) = maybe_static(last, r.parent)

maybe_static(::typeof(oftype), x, y) = oftype(x, y)
maybe_static(::typeof(oftype), x, y::Static) = static(oftype(x, y))
maybe_static(::typeof(oftype), ::Static{X}, ::Static{Y}) where {X,Y} = static(oftype(X, Y))
