module StructUtils

using Dates, UUIDs

export @noarg, @defaults, @tags, @kwarg, Selectors

"""
    StructUtils.StructStyle

Abstract type that all concrete struct styles must subtype.
Custom struct styles allow fine-grained control over various
StructUtils.jl interface methods like `fieldtags`, `fielddefaults`,
`lift`, `lower`, etc.
"""
abstract type StructStyle end

"""
    StructUtils.DefaultStyle

Default struct style that all StructUtils.jl interface methods
are defined for by default.
"""
struct DefaultStyle <: StructStyle end

include("macros.jl")

"""
  StructUtils.dictlike(x) -> Bool
  StructUtils.dictlike(::StructStyle, x) -> Bool
  StructUtils.dictlike(::StructStyle, ::Type{T}) -> Bool

Returns `true` if `x` or type `T` is dictionary-like, `false` otherwise.
When `StructUtils.make(T, source)` is called, if `dictlike(T)` is `true`,
an instance will be `initialize`d, and then `addkeyval!`ed for each
key-value pair in `source`.
"""
function dictlike end

dictlike(st::StructStyle, x) = dictlike(st, typeof(x))
dictlike(::StructStyle, T::Type) = dictlike(T)
function dictlike(T::Type)
    if T <: AbstractDict
        return true
    elseif T <: AbstractVector{<:Pair}
        return true
    else
        return false
    end
end

"""
  StructUtils.noarg(x) -> Bool
  StructUtils.noarg(::StructStyle, x) -> Bool
  StructUtils.noarg(::StructStyle, ::Type{T}) -> Bool

Signals that `x` or type `T` is a mutable type that can be constructed by calling an empty
constructor, like `t = T()`. Automatically overloaded when structs use the
`@noarg` macro in their struct definition. The default value is `false` unless
explicitly overloaded.
"""
function noarg end

noarg(st::StructStyle, x) = noarg(st, typeof(x))
noarg(::StructStyle, T::Type) = noarg(T)
noarg(T::Type) = false

"""
  StructUtils.kwarg(x) -> Bool
  StructUtils.kwarg(::StructStyle, x) -> Bool
  StructUtils.kwarg(::StructStyle, ::Type{T}) -> Bool

Signals that `x` or type `T` can be constructed by passing struct fields as keyword arguments
to the constructor, like `t = T(field1=a, field2=b, ...)`. Automatically overloaded
when structs use the `StructUtils.@kwarg` macro in their struct definition. The default value
is `false` unless explicitly overloaded.

Note that `StructUtils.@kwarg` is a separate implementation of `Base.@kwdef`, yet should
be a drop-in replacement for it.
"""
function kwarg end

kwarg(st::StructStyle, x) = kwarg(st, typeof(x))
kwarg(::StructStyle, T::Type) = kwarg(T)
kwarg(T::Type) = false

"""
    StructUtils.fieldtagkey(::StructStyle) -> Symbol

Field tags defined on struct fields can be grouped by keys that are associated with
a particular struct style. This function returns the key that should be used to
retrieve field tags for a given struct style. By default, this function returns
`nothing`. An example overload might look like:

```julia
struct MySQLStyle <: StructStyle end

StructUtils.fieldtagkey(::MySQLStyle) = :mysql

@tags struct Foo
    a::Int &(mysql=(name="foo_a",),)
    b::String
end
```

In this example, when `StructUtils.make` is called on `Foo` with the `MySQLStyle` style,
only `(name="foo_a",)` will be retrieved from the field tags for `a` because the
`mysql` key is associated with the `MySQLStyle` struct style. In other words, fieldtag keys
allow custom struct styles to "namespace" field tags so structs can overload specific tags
in multiple ways for different namespaces, i.e. `a::Int &(mysql=(name="foo_a",), json=(name="json_a",))`.
"""
function fieldtagkey end

fieldtagkey(::StructStyle) = nothing

"""
    StructUtils.fieldtags(::StructStyle, ::Type{T}) -> NamedTuple
    StructUtils.fieldtags(::StructStyle, ::Type{T}, fieldname) -> NamedTuple

Returns a `NamedTuple` of field tags for the struct `T`. Field tags can be
added manually by overloading `fieldtags`, or included via convenient syntax
using the StructUtils.jl macros: `@tags`, `@noarg`, `@defaults`, or `@kwarg`.
Note this function returns the tags of *all* fields as a single NamedTuple.
"""
function fieldtags end

fieldtags(::StructStyle, T::Type)::NamedTuple{(),Tuple{}} = (;)

function fieldtags(st::StructStyle, T::Type, field)
    ft = fieldtags(st, T)
    fft = get(() -> (;), ft, field)
    ftk = fieldtagkey(st)
    return ftk === nothing ? fft : get(fft, ftk, fft)
end

"""
    StructUtils.fielddefaults(::StructStyle, ::Type{T}) -> NamedTuple
    StructUtils.fielddefault(::StructStyle, ::Type{T}, fieldname) -> NamedTuple

Returns a `NamedTuple` of field defaults for the struct `T`. Field defaults can be
added manually by overloading `fielddefaults`, or included via convenient syntax
using the StructUtils.jl macros: `@tags`, `@noarg`, `@defaults`, or `@kwarg`.
"""
function fielddefaults end

fielddefaults(::StructStyle, T::Type)::NamedTuple{(),Tuple{}} = (;)
fielddefault(st::StructStyle, T::Type, key) = get(fielddefaults(st, T), key, nothing)
@doc (@doc fielddefaults) fielddefault

"""
    StructUtils.initialize(::StructStyle, T, source) -> T

In `StructUtils.make`, this function is called to initialize a new instance of `T`,
when `T` is `dictlike`, `arraylike`, or `noarg`. The `source` is passed from the call to `make`,
and can be used for initialization if appropriate.
The default implementation of `initialize` is to call `T()` or `T(undef, 0)`
for `<:AbstractArray` types.
"""
function initialize end

initialize(st::StructStyle, T::Type, @nospecialize(source)) =
    arraylike(st, T) ? T(undef, 0) : T()

function initialize(st::StructStyle, ::Type{A}, source) where {A<:AbstractArray}
    if ndims(A) > 1
        dims = discover_dims(st, source)
        return A(undef, dims)
    else
        return A(undef, 0)
    end
end

"""
    StructUtils.addkeyval!(d, k, v)

Add a key-value pair to a dictionary-like object `d`. This function is called
by `StructUtils.make` when `d` is `dictlike`. The default implementation is to
call `d[k] = v` for `AbstractDict`.
"""
function addkeyval! end

addkeyval!(d::AbstractDict, k, v) = d[k] = v
addkeyval!(d::AbstractVector, k, v) = push!(d, k => v)

_keytype(d) = keytype(d)
_keytype(::AbstractVector{Pair{A,B}}) where {A,B} = A
_valtype(d) = valtype(d)
_valtype(::AbstractVector{Pair{A,B}}) where {A,B} = B

"""
  StructUtils.arraylike(x) -> Bool
  StructUtils.arraylike(::StructStyle, x) -> Bool
  StructUtils.arraylike(::StructStyle, ::Type{T}) -> Bool

Returns `true` if `x` or type `T` is array-like, `false` otherwise. This function is
called by `StructUtils.make` to determine if `T` is array-like. The default
implementation returns `true` for `<:AbstractArray`, `<:AbstractSet`, `<:Tuple`,
`<:Base.Generator`, and `<:Core.SimpleVector` types, and `false` for `<:AbstractArray{T,0}`.

Once `initialize` is called, `StructUtils.make` will call `push!` to add values
to the array-like object.
"""
function arraylike end

arraylike(st::StructStyle, x) = arraylike(st, typeof(x))
arraylike(::StructStyle, T::Type) = arraylike(T)
function arraylike(T::Type)
    if T <: AbstractArray && ndims(T) == 0
        return false
    elseif T <: AbstractArray || T <: AbstractSet || T <: Tuple || T <: Base.Generator || T <: Core.SimpleVector
        return true
    else
        return false
    end
end

"""
  StructUtils.structlike(x) -> Bool
  StructUtils.structlike(::StructStyle, x) -> Bool
  StructUtils.structlike(::StructStyle, ::Type{T}) -> Bool

Returns `true` if `x` or type `T` is struct-like, `false` otherwise. This function is
called by `StructUtils.make` to determine if `T` is struct-like. The default
implementation returns `true` for `isstructtype(T)` and `!Base.issingletontype(T)`.

`structlike` structs are expected to be able to be constructed by the default constructor
like `T(field1, field2, ...)`.

Due to how `StructUtils.make` works, `structlike` is often overloaded to `false` by "unit"/"atom" types
where fields should be considered private to the `make` process and should instead attempt to
`lift` the `source` object into the `unit` type.
"""
function structlike end

structlike(st::StructStyle, x) = structlike(st, typeof(x))
structlike(::StructStyle, T::Type) = structlike(T)
function structlike(T::Type)
    if T <: Function || T <: Module || (T <: AbstractArray && ndims(T) == 0) ||
       T <: AbstractChar || T <: AbstractString || T == Symbol || T == Regex || T <: Dates.TimeType ||
       T <: Number || T == Nothing || T == Missing || T == UUID || T == VersionNumber
        return false
    elseif isstructtype(T) && !Base.issingletontype(T)
        return true
    else
        return false
    end
end

"""
  StructUtils.nulllike(x) -> Bool
  StructUtils.nulllike(::StructStyle, x) -> Bool
  StructUtils.nulllike(::StructStyle, ::Type{T}) -> Bool

Returns `true` if `x` or type `T` is null-like, `false` otherwise. This function is
mainly used in the `make!` implementation to determine if a
`Union` type can be narrowed by excluding `nulllike` types like `Nothing` and `Missing`.
"""
function nulllike end

nulllike(st::StructStyle, x) = nulllike(st, typeof(x))
nulllike(::StructStyle, T::Type) = nulllike(T)
nulllike(T::Type) = T === Missing || T === Nothing

"""
  StructUtils.lower(x) -> x
  StructUtils.lower(::StructStyle, x) -> x

Domain value transformation function. This function is called by
`StructUtils.applyeach` on each value in the `source` object before
calling the apply function. By default, `lower` is the identity function.
This allows a domain transformation of values according to the
style used.
"""
function lower end

lower(::StructStyle, x) = lower(x)
lower(x) = x

function lower(st::StructStyle, x, tags)
    # there are a few builtin tags supported
    if x isa Dates.TimeType && haskey(tags, :dateformat)
        return Dates.format(x, tags.dateformat)
    elseif haskey(tags, :lower)
        return tags.lower(x)
    else
        return lower(st, x)
    end
end

"""
  StructUtils.lowerkey(x) -> x
  StructUtils.lowerkey(style::StructUtils.StructStyle, x) -> x

Allows customizing how a value is lowered when used specifically as a key.
By default, calls [`StructUtils.lower`](@ref). Called from [`StructUtils.applyeach`](@ref)
on the key or index before passed to the key-value function.

### Example

```julia
struct Point
    x::Int; y::Int
end

# lower a Point as a single string value
StructUtils.lowerkey(::StructUtils.StructStyle, p::Point) = "\$(p.x)_\$(p.y)"

d = Dict(Point(1, 2) => 99)

StructUtils.make(Dict{String, Dict{String, Point}}, Dict(Point(1, 2) => Dict(Point(3, 4) => Point(5, 6))))
# Dict{String, Dict{String, Point}} with 1 entry:
#   "1_2" => Dict("3_4"=>Point(5, 6))
```

For loss-less round-tripping also provide a [`StructUtils.liftkey`](@ref) overload to "lift" the key back.
"""
lowerkey(::StructStyle, x) = lowerkey(x)
lowerkey(x) = x

"""
  StructUtils.lift(::Type{T}, x) -> T
  StructUtils.lift(::StructStyle, ::Type{T}, x) -> T

Lifts a value `x` to a type `T`. This function is called by `StructUtils.make`
to lift unit/atom values to the appropriate type. The default implementation is
the identity function for most types, but it also includes special cases
for `Symbol`, `Char`, `UUID`, `VersionNumber`, `Regex`, and `TimeType` types to be
constructed from strings.
Allows transforming a "domain value" that may be some primitive representation
into a more complex Julia type.
"""
function lift end

lift(::Type{Symbol}, x) = Symbol(x)
lift(::Type{String}, x::Symbol) = String(x)
lift(::Type{T}, x) where {T} = Base.issingletontype(T) ? T() : convert(T, x)
lift(::Type{>:Missing}, ::Nothing) = missing
lift(::Type{>:Nothing}, ::Nothing) = nothing
lift(::Type{>:Union{Missing,Nothing}}, ::Nothing) = nothing
lift(::Type{>:Union{Missing,Nothing}}, ::Missing) = missing
lift(::Type{Char}, x::AbstractString) = length(x) == 1 ? x[1] : throw(ArgumentError("expected single character, got $x"))
lift(::Type{UUID}, x::AbstractString) = UUID(x)
lift(::Type{VersionNumber}, x::AbstractString) = VersionNumber(x)
lift(::Type{Regex}, x::AbstractString) = Regex(x)
lift(::Type{T}, x::AbstractString) where {T<:Dates.TimeType} = T(x)

function lift(::Type{T}, x::AbstractString) where {T <: Enum}
    sym = Symbol(x)
    for (k, v) in Base.Enums.namemap(T)
        v === sym && return T(k)
    end
    throw(ArgumentError("invalid `$T` string value: \"$sym\""))
end

lift(::StructStyle, ::Type{T}, x) where {T} = lift(T, x)

# bit of an odd case, but support 0-dimensional array lifting from scalar value
function lift(st::StructStyle, ::Type{A}, x) where {A<:AbstractArray{T,0}} where {T}
    m = A(undef)
    m[1] = lift(st, T, x)
    return m
end

function lift(st::StructStyle, ::Type{T}, x, tags) where {T}
    if haskey(tags, :lift)
        return tags.lift(x)
    elseif T <: Dates.TimeType && haskey(tags, :dateformat)
        if tags.dateformat isa String
            return parse(T, x, Dates.DateFormat(tags.dateformat))
        else
            return parse(T, x, tags.dateformat)
        end
    else
        return lift(st, T, x)
    end
end

lift(f, st::StructStyle, ::Type{T}, x, tags) where {T} = f(lift(st, T, x, tags))

"""
  StructUtils.liftkey(::Type{T}, x) -> x
  StructUtils.liftkey(style::StructStyle, ::Type{T}, x) -> x

Allows customizing how a key is lifted before being passed to [`addkeyval!`](@ref)
in `dictlike` construction.

By default, calls [`StructUtils.lift`](@ref).

### Example

```julia
struct Point
    x::Int; y::Int
end

# lift a Point from a string value
StructUtils.liftkey(::StructUtils.StructStyle, x::String) = Point(parse(Int, split(x, "_")[1]), parse(Int, split(x, "_")[2]))

d = Dict("1_2" => 99)
StructUtils.make(Dict{Point, Int}, Dict("1_2" => 99))
# Dict{Point, Int} with 1 entry:
#   Point(1, 2) => 99
```

For loss-less round-tripping also provide a [`StructUtils.lowerkey`](@ref) overload to "lower" the key.
"""
function liftkey end

liftkey(st::StructStyle, ::Type{T}, x) where {T} = liftkey(T, x)
liftkey(::Type{T}, x) where {T} = lift(T, x)
liftkey(f, st::StructStyle, ::Type{T}, x) where {T} = f(liftkey(st, T, x))

"""
    StructUtils.applyeach(style, f, x) -> Union{StructUtils.EarlyReturn, Nothing}

A custom `foreach`-like function that operates specifically on `(key, val)` or `(ind, val)` pairs,
and supports short-circuiting (via `StructUtils.EarlyReturn`). It also supports a `StructStyle` argument
to allow for style-specific behavior for non-owned types.

For each key-value or index-value pair in `x`, call `f(k, v)`.
If `f` returns a `StructUtils.EarlyReturn` instance, `applyeach` should
return the `EarlyReturn` immediately and stop iterating (i.e. short-circuit).
Otherwise, the return value of `f` can be ignored and iteration continues.

Key types are generally expected to be Symbols, Strings, or Integers.

An example overload of `applyeach` for a generic iterable would be:

```julia
function StructUtils.applyeach(style::StructUtils.StructStyle, f, x::MyIterable)
    for (i, v) in enumerate(x)
        ret = f(StructUtils.lowerkey(style, i), StructUtils.lower(style, v))
        # if `f` returns EarlyReturn, return immediately
        ret isa StructUtils.EarlyReturn && return ret
    end
    return
end
```

Note that `applyeach` must include the `style` argument when overloading.

Also note that before applying `f`, the key or index is passed through `StructUtils.lowerkey(style, k)`,
and the value `v` is passed through `StructUtils.lower(style, v)`.

If a value is `#undef` or otherwise not defined, the `f` function should generally be called with `nothing` or skipped.
"""
function applyeach end

"""
    StructUtils.EarlyReturn{T}

A wrapper type that can be used in function arguments to `applyeach`
to short-circuit iteration and return a value from `applyeach`.

Example usage:

```julia
function find_needle_in_haystack(haystack, needle)
    ret = applyeach(haystack) do k, v
        k == needle && return StructUtils.EarlyReturn(v)
    end
    ret isa StructUtils.EarlyReturn && return ret.value
    throw(ArgumentError("needle not found in haystack")
end
````
"""
struct EarlyReturn{T}
    value::T
end

applyeach(f, x) = applyeach(DefaultStyle(), f, x)
applyeach(f, st::StructStyle, x) = applyeach(st, f, x)

function applyeach(st::StructStyle, f, x::AbstractArray)
    for i in eachindex(x)
        ret = if @inbounds(isassigned(x, i))
            f(lowerkey(st, i), lower(st, @inbounds(x[i])))
        else
            f(lowerkey(st, i), lower(st, nothing))
        end
        ret isa EarlyReturn && return ret
    end
    return length(x)
end

# special-case Pair vectors to act like Dicts
function applyeach(st::StructStyle, f, x::AbstractVector{Pair{K,V}}) where {K,V}
    for (k, v) in x
        ret = f(lowerkey(st, k), lower(st, v))
        ret isa EarlyReturn && return ret
    end
    return
end

# appropriate definition for iterables that
# can't have #undef values
function applyeach(st::StructStyle, f, x::Union{AbstractSet,Base.Generator,Core.SimpleVector})
    for (i, v) in enumerate(x)
        ret = f(lowerkey(st, i), lower(st, v))
        ret isa EarlyReturn && return ret
    end
    return
end

# generic definition for Tuple, NamedTuple, structs
function applyeach(st::StructStyle, f, x::T) where {T}
    if @generated
        N = fieldcount(T)
        ex = quote
            defs = fielddefaults(st, T)
        end
        for i = 1:N
            fname = Meta.quot(fieldname(T, i))
            push!(ex.args, quote
                ftags = fieldtags(st, T, $fname)
                if !haskey(ftags, :ignore) || !ftags.ignore
                    fname = get(ftags, :name, $fname)
                    ret = if isdefined(x, $i)
                        f(lowerkey(st, fname), lower(st, getfield(x, $i), ftags))
                    elseif haskey(defs, $fname)
                        # this branch should be really rare because we should
                        # have applied a field default in the struct constructor
                        f(lowerkey(st, fname), lower(st, defs[$fname], ftags))
                    else
                        f(lowerkey(st, fname), lower(st, nothing, ftags))
                    end
                    ret isa EarlyReturn && return ret
                end
            end)
        end
        push!(ex.args, :(return))
        return ex
    else
        defs = fielddefaults(st, T)
        for i = 1:fieldcount(T)
            fname = fieldname(T, i)
            ftags = fieldtags(st, T, fname)
            if !haskey(ftags, :ignore) || !ftags.ignore
                fname = get(ftags, :name, fname)
                ret = if isdefined(x, i)
                    f(lowerkey(st, fname), lower(st, getfield(x, i), ftags))
                elseif haskey(defs, fname)
                    f(lowerkey(st, fname), lower(st, defs[fname], ftags))
                else
                    f(lowerkey(st, fname), lower(st, nothing, ftags))
                end
                ret isa EarlyReturn && return ret
            end
        end
        return
    end
end

function applyeach(st::StructStyle, f, x::AbstractDict)
    for (k, v) in x
        ret = f(lowerkey(st, k), lower(st, v))
        ret isa EarlyReturn && return ret
    end
    return
end

@static if VERSION < v"1.10"
    function _isfieldatomic(t::Type, s::Int)
        t = Base.unwrap_unionall(t)
        # TODO: what to do for `Union`?
        isa(t, DataType) || return false # uncertain
        ismutabletype(t) || return false # immutable structs are never atomic
        1 <= s <= length(t.name.names) || return false # OOB reads are not atomic (they always throw)
        atomicfields = t.name.atomicfields
        atomicfields === C_NULL && return false
        s -= 1
        return unsafe_load(Ptr{UInt32}(atomicfields), 1 + s÷32) & (1 << (s%32)) != 0
    end
else
    const _isfieldatomic = Base.isfieldatomic
end

_setfield!(x, i, v) = setfield!(x, i, v, _isfieldatomic(typeof(x), i) ? :sequentially_consistent : :not_atomic)

keyeq(a::Symbol, b::String) = a === Symbol(b)
keyeq(a::String, b::Symbol) = Symbol(a) == b
keyeq(a, b::String) = string(a) == b
keyeq(a::AbstractString, b::String) = String(a) == b
keyeq(a, b) = isequal(a, b)
keyeq(x) = y -> keyeq(x, y)

@inline function _foreach(f::F, n::Int) where F
    # marked inline since this benefits from constant propagation of `n`
    if n == 1
        f(1) isa EarlyReturn && return
    elseif n == 2
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return
    elseif n == 3
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return; f(3) isa EarlyReturn && return
    elseif n == 4
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return; f(3) isa EarlyReturn && return; f(4) isa EarlyReturn && return
    elseif n == 5
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return; f(3) isa EarlyReturn && return; f(4) isa EarlyReturn && return; f(5) isa EarlyReturn && return
    elseif n == 6
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return; f(3) isa EarlyReturn && return; f(4) isa EarlyReturn && return; f(5) isa EarlyReturn && return; f(6) isa EarlyReturn && return
    elseif n == 7
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return; f(3) isa EarlyReturn && return; f(4) isa EarlyReturn && return; f(5) isa EarlyReturn && return; f(6) isa EarlyReturn && return; f(7) isa EarlyReturn && return
    elseif n == 8
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return; f(3) isa EarlyReturn && return; f(4) isa EarlyReturn && return; f(5) isa EarlyReturn && return; f(6) isa EarlyReturn && return; f(7) isa EarlyReturn && return; f(8) isa EarlyReturn && return
    elseif n == 9
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return; f(3) isa EarlyReturn && return; f(4) isa EarlyReturn && return; f(5) isa EarlyReturn && return; f(6) isa EarlyReturn && return; f(7) isa EarlyReturn && return; f(8) isa EarlyReturn && return; f(9) isa EarlyReturn && return
    elseif n == 10
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return; f(3) isa EarlyReturn && return; f(4) isa EarlyReturn && return; f(5) isa EarlyReturn && return; f(6) isa EarlyReturn && return; f(7) isa EarlyReturn && return; f(8) isa EarlyReturn && return; f(9) isa EarlyReturn && return; f(10) isa EarlyReturn && return
    elseif n == 11
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return; f(3) isa EarlyReturn && return; f(4) isa EarlyReturn && return; f(5) isa EarlyReturn && return; f(6) isa EarlyReturn && return; f(7) isa EarlyReturn && return; f(8) isa EarlyReturn && return; f(9) isa EarlyReturn && return; f(10) isa EarlyReturn && return; f(11) isa EarlyReturn && return
    elseif n == 12
        f(1) isa EarlyReturn && return; f(2) isa EarlyReturn && return; f(3) isa EarlyReturn && return; f(4) isa EarlyReturn && return; f(5) isa EarlyReturn && return; f(6) isa EarlyReturn && return; f(7) isa EarlyReturn && return; f(8) isa EarlyReturn && return; f(9) isa EarlyReturn && return; f(10) isa EarlyReturn && return; f(11) isa EarlyReturn && return; f(12) isa EarlyReturn && return
    else
        foreach(f, 1:n)
    end
    return
end

# helper closure that computes the length of an applyeach source
# note that it should be used sparingly/carefully since it consumes
# the source object and we generally want to do a single pass
if VERSION < v"1.10"
mutable struct LengthClosure
    len::Int
end
(f::LengthClosure)(_, _) = f.len += 1
function applylength(x)
    lc = LengthClosure(0)
    StructUtils.applyeach(lc, x)
    return lc.len
end
else
struct LengthClosure
    len::Ptr{Int}
end

(f::LengthClosure)(_, _) = unsafe_store!(f.len, unsafe_load(f.len) + 1)

function applylength(x)
    ref = Ref(0)
    GC.@preserve ref begin
        lc = LengthClosure(Base.unsafe_convert(Ptr{Int}, ref))
        StructUtils.applyeach(lc, x)
        return unsafe_load(lc.len)
    end
end
end # VERSION < v"1.10"

# recursively build up multidimensional array dimensions
# "[[1.0],[2.0]]" => (1, 2)
# "[[1.0,2.0]]" => (2, 1)
# "[[[1.0]],[[2.0]]]" => (1, 1, 2)
# "[[[1.0],[2.0]]]" => (1, 2, 1)
# "[[[1.0,2.0]]]" => (2, 1, 1)
# length of innermost array is 1st dim
function discover_dims(style, x)
    @assert arraylike(style, x)
    len = applylength(x)
    ret = (applyeach(x) do _, v
        return arraylike(style, v) ? EarlyReturn(discover_dims(style, v)) : EarlyReturn(())
    end)::EarlyReturn
    return (ret.value..., len)
end

struct MultiDimClosure{S,A}
    style::S
    arr::A
    dims::Vector{Int}
    cur_dim::Base.RefValue{Int}
end

function (f::MultiDimClosure{S,A})(i::Int, val) where {S,A}
    f.dims[f.cur_dim[]] = i
    if arraylike(f.style, val)
        f.cur_dim[] -= 1
        st = applyeach(f, f.style, val)
        f.cur_dim[] += 1
    else
        st = make!(MultiDimValFunc(f.style, f.arr, f.dims), f.style, eltype(f.arr), val)
    end
    return st
end

struct MultiDimValFunc{S,A}
    style::S
    arr::A
    dims::Vector{Int}
end

(f::MultiDimValFunc{S,A})(x) where {S,A} = setindex!(f.arr, x, f.dims...)

"""
    StructUtils.make(T, source) -> T
    StructUtils.make(T, source, style) -> T
    StructUtils.make!(f, style, T, source)
    StructUtils.make!(style, x::T, source)

Construct a struct of type `T` from `source` using the given `style`. The `source` can be any
type of object, and the `style` can be any `StructStyle` subtype (default `StructUtils.DefaultStyle()`).

`make` will use any knowledge of `noarg`, `arraylike`, or `dictlike` in order to
determine how to construct an instance of `T`. The fallback for structs is to rely on
the automatic "all argument" constructor that structs have defined by default (e.g. `T(fields...)`).

`make` calls `applyeach` on the `source` object, where the key-value pairs
from `source` will be used in constructing `T`.

The 3rd definition above allows passing in an "applicator" function that is
applied to the constructed struct. This is useful when the initial `T` is
abstract or a union type and a `choosetype` field tag or other `StructUtils.make` definition
is used to determine the concrete runtime type to construct.

The 4th definition allows passing in an already-constructed instance of `T` (`x`),
which must be mutable, and source key-value pairs will be applied as
to `x` as source keys are matched to struct field names.

For structs, `fieldtags` will be accounted for and certain tags can be used
to influence the construction of the struct.
"""
function make end

mutable struct ValueClosure{T}
    value::T
    ValueClosure{T}() where {T} = new{T}()
end

(f::ValueClosure{T})(x) where {T} = setfield!(f, :value, x)

function make(::Type{T}, source, style::StructStyle=DefaultStyle()) where {T}
    out = ValueClosure{T}()
    make!(out, style, T, source)
    return out.value
end

function make!(f, style::StructStyle, T::Type, source, tags=(;))
    if haskey(tags, :choosetype)
        return make!(f, style, tags.choosetype(source), source, _delete(tags, :choosetype))
    end
    # start with some hard-coded Union cases
    if T !== Any
        if T >: Missing && T !== Missing
            if nulllike(style, source)
                return make!(f, style, Missing, source, tags)
            else
                return make!(f, style, nonmissingtype(T), source, tags)
            end
        elseif T >: Nothing && T !== Nothing
            if nulllike(style, source)
                return make!(f, style, Nothing, source, tags)
            else
                return make!(f, style, Base.nonnothingtype(T), source, tags)
            end
        end
    end
    if T <: Tuple
        return maketuple!(f, style, T, source)
    elseif dictlike(style, T)
        return makedict!(f, style, initialize(style, T, source), source)
    elseif arraylike(style, T)
        return makearray!(f, style, initialize(style, T, source), source)
    elseif noarg(style, T)
        return makenoarg!(f, style, initialize(style, T, source), source)
    elseif structlike(style, T)
        return makestruct!(f, style, T, source)
    else
        return lift(f, style, T, source, tags)
    end
end

if VERSION < v"1.11"
    mem(n) = Vector{Any}(undef, n)
else
    mem(n) = Memory{Any}(undef, n)
end

function maketuple!(f, style, ::Type{T}, source) where {T}
    vals = mem(fieldcount(T))
    for i = 1:fieldcount(T)
        @inbounds vals[i] = nothing
    end
    i = Ref{Int}(0)
    st = applyeach(style, source) do _, v
        j = i[] += 1
        if j <= fieldcount(T)
            FT = fieldtype(T, j)
            return make!(x -> setindex!(vals, x, j), style, FT, v)
        end
        return nothing
    end
    f(Tuple(vals))
    return st
end

struct DictSetClosure{T, K}
    dict::T
    key::K
end

function (f::DictSetClosure{T, K})(v) where {T, K}
    addkeyval!(f.dict, f.key, v)
end

struct DictClosure{T, S}
    dict::T
    style::S
end

function (f::DictClosure{T, S})(k, v) where {T, S}
    key = liftkey(f.style, _keytype(f.dict), k)
    st = make!(DictSetClosure(f.dict, key), f.style, _valtype(f.dict), v)
    return st
end

@noinline function makedict!(f, style, dict, source)
    st = applyeach(style, DictClosure(dict, style), source)
    f(dict)
    return st
end

struct ArraySetClosure{T, A}
    arr::T
    style::A
end

function (f::ArraySetClosure{T, A})(v) where {T, A}
    push!(f.arr, v)
end

struct ArrayClosure{T, A}
    arr::T
    style::A
end

function (f::ArrayClosure{T, A})(_, v) where {T, A}
    st = make!(ArraySetClosure(f.arr, f.style), f.style, eltype(f.arr), v)
    return st
end

@noinline function makearray!(f, style, x::T, source) where {T}
    if ndims(T) > 1
        # multidimensional arrays
        n = ndims(T)
        st = applyeach(style, MultiDimClosure(style, x, ones(Int, n), Ref(n)), source)
        f(x)
        return st
    else
        st = applyeach(style, ArrayClosure(x, style), source)
        f(x)
        return st
    end
end

@noinline function makenoarg!(f, style, y::T, source) where {T}
    st = applyeach(style, source) do k, v
        ret = findfield(style, T, y, k, v)
        ret isa EarlyReturn && return ret.value
        return nothing
    end
    f(y)
    return st
end

function makestruct!(f, style, ::Type{T}, source) where {T}
    fields = mem(fieldcount(T))
    for i = 1:fieldcount(T)
        @inbounds fields[i] = fielddefault(style, T, fieldname(T, i))
    end
    st = applyeach(style, source) do k, v
        ret = findfield(style, T, fields, k, v)
        ret isa EarlyReturn && return ret.value
        return nothing
    end
    if T <: NamedTuple
        f(T(Tuple(fields)))
    else
        f(T(fields...))
    end
    return st
end

# x is mutable struct of type T or Memory{Any} for struct
# findfield tries to find the field of a struct that key matches
function findfield(style, ::Type{T}, x, key::Int, val) where {T}
    _foreach(fieldcount(T)) do i
        if key == i
            field = fieldname(T, i)
            ftags = fieldtags(style, T, field)
            return EarlyReturn(applyfield!(style, T, x, i, ftags, val))
        end
    end
end

function findfield(style, ::Type{T}, x, key::Symbol, val) where {T}
    _foreach(fieldcount(T)) do i
        field = fieldname(T, i)
        ftags = fieldtags(style, T, field)
        field = get(ftags, :name, field)
        if key == field
            return EarlyReturn(applyfield!(style, T, x, i, ftags, val))
        end
    end
end

function findfield(style, ::Type{T}, x, key, val) where {T}
    # generated use here to avoid the String(f) allocation at runtime
    # if someone knows of a better way to compile away getting the field name as a string, please tell
    if @generated
        ex = Expr(:block)
        for i = 1:fieldcount(T)
            f = fieldname(T, i)
            fstr = String(f)
            push!(ex.args, quote
                ftags = fieldtags(style, T, $(Meta.quot(f)))
                field = get(ftags, :name, $fstr)
                if keyeq(key, field)
                    return EarlyReturn(applyfield!(style, T, x, $(i), ftags, val))
                end
            end)
        end
        return ex
    else
        _foreach(fieldcount(T)) do i
            f = fieldname(T, i)
            ftags = fieldtags(style, T, f)
            field = get(() -> String(f), ftags, :name)
            if keyeq(key, field)
                return EarlyReturn(applyfield!(style, T, x, i, ftags, val))
            end
        end
    end
end

struct FieldError <: Exception
    T::Type
    field::Symbol
    source::Any
end

Base.showerror(io::IO, e::FieldError) = print(io, "StructUtils.FieldError: error while making field `$(e.field)::$(fieldtype(e.T, e.field))` of type `$(e.T)` from `$(e.source)`")

# we matched a field, so store the value/set the field with val
function applyfield!(style, ::Type{T}, x, i::Int, ftags, val) where {T}
    FT = fieldtype(T, i)
    try
        return make!(style, FT, val, ftags) do v
            if noarg(style, T)
                _setfield!(x, i, v)
            else
                x[i] = v
            end
        end
    catch
        throw(FieldError(T, fieldname(T, i), val))
    end
end

if isdefined(Base, :delete) && applicable(Base.delete, (a=1,), :a)
    const _delete = Base.delete
else
    Base.@constprop :aggressive function delete(a::NamedTuple{an}, field::Symbol) where {an}
        names = Base.diff_names(an, (field,))
        NamedTuple{names}(a)
    end
    const _delete = delete
end

make!(x::T, source; style::StructStyle=DefaultStyle()) where {T} = make!(style, x, source)
make!(::Type{T}, source; style::StructStyle=DefaultStyle()) where {T} = make!(style, T, source)

function make!(style::StructStyle, x::T, source) where {T}
    if dictlike(style, x)
        return makedict!(identity, style, x, source)
    elseif arraylike(style, x)
        return makearray!(identity, style, x, source)
    elseif noarg(style, x)
        return makenoarg!(identity, style, x, source)
    else
        throw(ArgumentError("Type `$T` does not support in-place `make!`"))
    end
end

function make!(style::StructStyle, ::Type{T}, source) where {T}
    x = initialize(style, T, source)
    make!(style, x, source)
    return x
end

@doc (@doc make) make!

"""
  StructUtils.reset!(x::T)

If `T` was defined with default values via `@defaults`, `@tags`, `@kwarg`, or `@noarg`,
`reset!` will reset the fields of `x` to their default values.
`T` must be a mutable struct type.
"""
function reset!(x::T; style::StructStyle=DefaultStyle()) where {T}
    if @generated
        N = fieldcount(T)
        ex = quote
            defs = fielddefaults(style, T)
        end
        for i = 1:N
            fname = Meta.quot(fieldname(T, i))
            push!(ex.args, quote
                if haskey(defs, $fname)
                    _setfield!(x, $i, defs[$fname])
                end
            end)
        end
        push!(ex.args, :(return x))
        return ex
    else
        defs = fielddefaults(style, T)
        for i = 1:fieldcount(T)
            fname = fieldname(T, i)
            if haskey(defs, fname)
                _setfield!(x, i, defs[fname])
            end
        end
        return x
    end
end

include("selectors.jl")

"""
    StructUtils.@choosetype T func
    StructUtils.@choosetype style T func

Convenience macro for defining a `StructUtils.make!` overload for an abstract type `T` where
`func` is a function that "chooses" a concrete type `S` at runtime. `func` can be one of two forms:
  * `source -> S`
  * `(source, tags) -> S)`

That is, it either takes just the `source` object that is passed to `make` and must choose a concrete
type `S`, or it can take both the `source` and a set of fieldtags that may be present for the field
of a type being "made".

The 2nd definition also takes a `style` argument, allowing for overloads of non-owned types `T`.

Example:

```julia
abstract type Vehicle end

struct Car <: Vehicle
    make::String
    model::String
    seatingCapacity::Int
    topSpeed::Float64
end

struct Truck <: Vehicle
    make::String
    model::String
    payloadCapacity::Float64
end

StructUtils.@choosetype Vehicle x -> x["type"] == "car" ? Car : x["type"] == "truck" ? Truck : throw(ArgumentError("Unknown vehicle type: \$(x["type"])"))

x = StructUtils.make(Vehicle, Dict("type" => "car", "make" => "Toyota", "model" => "Corolla", "seatingCapacity" => 4, "topSpeed" => 120.5))
@test x == Car("Toyota", "Corolla", 4, 120.5)
```
"""
macro choosetype(T, ex)
    esc(quote
        function StructUtils.make!(f, st::StructUtils.StructStyle, ::Type{$T}, source, tags)
            func = $(ex)
            StructUtils.make!(f, st, applicable(func, source, tags) ? func(source, tags) : func(source), source, tags)
        end
    end)
end

macro choosetype(style, T, ex)
    esc(quote
        function StructUtils.make!(f, st::$(style), ::Type{$T}, source, tags)
            func = $(ex)
            StructUtils.make!(f, st, applicable(func, source, tags) ? func(source, tags) : func(source), source, tags)
        end
    end)
end

end
