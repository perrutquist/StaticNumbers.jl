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
    y isa Number && !(y isa Bool) && !(y isa Unsigned) ? static(y) : y
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

maybe_wrap(x) = x
maybe_wrap(x::LengthRange{T,Z,S,L}) where {T,Z<:StaticInteger,S<:StaticInteger,L} = MaybeStatic(x)

@inline Base.getindex(r::MaybeStatic{<:LengthRange}, i::StaticInteger) = static(r.parent[i])
@inline Base.getindex(r::MaybeStatic{<:LengthRange}, i::LengthRange) = maybe_wrap(getindex(r.parent, i))
@inline Base.getindex(r::MaybeStatic, args...) = getindex(r.parent, args...)
@inline Base.step(r::MaybeStatic) = step(r.parent)
@inline Base.length(r::MaybeStatic) = length(r.parent)
@inline Base.unsafe_length(r::MaybeStatic) = length(r.parent)
@inline Base.first(r::MaybeStatic) = maybe_static(first, r.parent)
@inline Base.last(r::MaybeStatic) = maybe_static(last, r.parent)

# Some functions need pass-through of the @stat macro...
@inline maybe_static(::typeof(first), r::LengthRange) = @stat r.zeroth + r.step
@inline maybe_static(::typeof(last), r::LengthRange) = @stat r.zeroth + r.step * r.length
@inline maybe_static(::typeof(first), ::Base.OneTo) = static(1)

@inline function maybe_static(getindex, r::LengthRange, i::StaticInteger)
    @boundscheck checkbounds(r, i)
    @stat r.zeroth + i*r.step
end
