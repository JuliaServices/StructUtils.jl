module StructUtils

using Dates, UUIDs

export @noarg, @defaults, @tags, @kwarg, @nonstruct, Selectors

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
dictlike(::Type{<:AbstractDict}) = true
dictlike(::Type{<:AbstractVector{<:Pair}}) = true
dictlike(@nospecialize(T)) = false

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
noarg(@nospecialize(T)) = false

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
kwarg(@nospecialize(T)) = false

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
    StructUtils.defaultstate(::StructStyle) -> Any

Returns the default state for a given struct style. This is used to initialize
the state of a struct when no state is provided. The default implementation
returns `nothing`.
"""
defaultstate(::StructStyle) = nothing

"""
    StructUtils.unknownfield(::StructStyle, ::Type{T}, key, value)

Called from [`StructUtils.make`](@ref) and [`StructUtils.make!`](@ref) when a
source key or index does not match any field or positional slot in the target
type `T`.

The default implementation ignores the extra input by returning
[`StructUtils.defaultstate`](@ref). Custom struct styles can overload this to
throw, return [`StructUtils.EarlyReturn`](@ref), or otherwise customize the
behavior for unknown fields.
"""
function unknownfield end

unknownfield(st::StructStyle, ::Type{T}, key, value) where {T} = defaultstate(st)

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
    return _fieldtag(st, fieldtags(st, T), field)
end

function _fieldtag(st::StructStyle, ft, field)
    isempty(ft) && return (;)
    fft = get(ft, field, (;))
    ftk = fieldtagkey(st)
    return ftk === nothing ? fft : get(fft, ftk, fft)
end

@generated function _fieldtagtuple(st::StructStyle, ::Type{T}, fsyms) where {T}
    t = Expr(:tuple)
    for i = 1:fieldcount(T)
        push!(t.args, :(_fieldtag(st, ft, $(QuoteNode(fieldname(T, i))))))
    end
    return quote
        Base.@_inline_meta
        ft = fieldtags(st, T)
        if isempty(ft)
            return _fieldtagtuple_public(st, T, fsyms)
        else
            return $t
        end
    end
end

@generated function _fieldtagtuple_public(st::StructStyle, ::Type{T}, fsyms) where {T}
    t = Expr(:tuple)
    for i = 1:fieldcount(T)
        push!(t.args, :(fieldtags(st, T, $(QuoteNode(fieldname(T, i))))))
    end
    return Expr(:block, :(Base.@_inline_meta), :(return $t))
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
fielddefaults(st::StructStyle, T::Type, vals) = fielddefaults(st, T)
fielddefault(st::StructStyle, T::Type, key) = get(fielddefaults(st, T), key, nothing)

"See [`fielddefaults`](@ref)."
fielddefault

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
        dims = discover_dims(st, source, ndims(A))
        return A(undef, dims)
    else
        return A(undef, 0)
    end
end

initialize(::StructStyle, ::Type{T}, source) where {T<:AbstractSet} = T()

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
arraylike(::Type{<:AbstractArray{T,0}}) where {T} = false
arraylike(::Type{<:AbstractArray}) = true
arraylike(::Type{<:AbstractSet}) = true
arraylike(::Type{<:Tuple}) = true
arraylike(::Type{<:Base.Generator}) = true
arraylike(::Type{<:Core.SimpleVector}) = true
arraylike(@nospecialize(::Type)) = false
arraylike(@nospecialize(x)) = arraylike(typeof(x))

"""
    StructUtils.fixedsizearray(::Type{T}) -> Bool
    StructUtils.fixedsizearray(::StructStyle, ::Type{T}) -> Bool

Returns `true` if `T` is a fixed-size array type that should be pre-allocated
and filled via `setindex!` rather than grown via `push!`. The default
implementation returns `true` for multidimensional `<:AbstractArray` types
(ndims > 1) and `false` for everything else.

Override this for custom array types that have a fixed, known size but
are not growable (e.g. `StaticArrays.StaticArray`).
"""
function fixedsizearray end

fixedsizearray(::Type) = false
fixedsizearray(::Type{<:AbstractArray{T,N}}) where {T,N} = N > 1
fixedsizearray(::Type{<:AbstractSet}) = false
fixedsizearray(st::StructStyle, ::Type{T}) where {T} = fixedsizearray(T)

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
structlike(::Type{<:Function}) = false
structlike(::Type{<:Module}) = false
structlike(::Type{<:AbstractArray{T,0}}) where {T} = false
structlike(::Type{<:AbstractChar}) = false
structlike(::Type{<:AbstractString}) = false
structlike(::Type{Symbol}) = false
structlike(::Type{Regex}) = false
structlike(::Type{<:Dates.TimeType}) = false
structlike(::Type{Number}) = false
structlike(::Type{BigInt}) = false
structlike(::Type{BigFloat}) = false
structlike(::Type{Nothing}) = false
structlike(::Type{Missing}) = false
structlike(::Type{UUID}) = false
structlike(::Type{VersionNumber}) = false
structlike(::Type{MIME}) = false
structlike(::Type{<:NamedTuple}) = true
structlike(@nospecialize(x)) = structlike(typeof(x))
structlike(@nospecialize(T::Type)) = isstructtype(T) && !Base.issingletontype(T)

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
nulllike(@nospecialize(T)) = T === Missing || T === Nothing

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
    StructUtils.lift(::StructStyle, ::Type{T}, x) -> Tuple{T, Any}

Lifts a value `x` to a type `T`. This function is called by `StructUtils.make`
to lift unit/atom values to the appropriate type. The default implementation is
the identity function for most types, but it also includes special cases
for `Symbol`, `Char`, `UUID`, `VersionNumber`, `MIME`, `Regex`, and `TimeType` types to be
constructed from strings.
Allows transforming a "domain value" that may be some primitive representation
into a more complex Julia type.

The method with a `StructStyle` argument should return a tuple of the lifted value and any side-effect state
derived from lifting the value.
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
lift(::Type{MIME}, x::AbstractString) = MIME(x)
lift(::Type{Regex}, x::AbstractString) = Regex(x)
lift(::Type{T}, x::AbstractString) where {T<:Dates.TimeType} = T(x)
lift(::Type{Dates.Date}, x::AbstractString) = _liftdate(String(x))
lift(::Type{Dates.DateTime}, x::AbstractString) = _liftdatetime(String(x))
lift(::Type{Dates.Time}, x::AbstractString) = _lifttime(String(x))

# ISO 8601 parsers for the three core Dates types. The `Date(str)` family
# routes through the DateFormat machinery, whose token handling and
# diagnostics are too dynamic for static compilation (`juliac --trim`); these
# accept exactly the grammar the default formats do — variable-width numeric
# fields, an optional year sign, progressively optional smaller fields, and a
# 1-3 digit fraction — and construct through the validating constructors.
@inline function _isodigits(s::String, i::Int, maxwidth::Int)
    n = ncodeunits(s)
    value = 0
    width = 0
    while i <= n && width < maxwidth
        b = codeunit(s, i)
        (UInt8('0') <= b <= UInt8('9')) || break
        value = 10 * value + Int(b - UInt8('0'))
        i += 1
        width += 1
    end
    return width == 0 ? -1 : value, i
end

@inline _isochar(s::String, i::Int, c::Char) =
    i <= ncodeunits(s) && codeunit(s, i) == UInt8(c)

# The default formats treat input as the token sequence
# `y - m - d T H : M : S . s` (dates stop after `d`, times start at `H`):
# numeric fields are variable-width, the year may carry a sign, delimiters
# must match in exact order, and input may end after any complete token —
# remaining fields default. A missing numeric field, a wrong delimiter, or
# trailing content is an error.
macro _isofield(var, maxwidth)
    esc(quote
        i > n && @goto done
        $var, i = _isodigits(s, i, $maxwidth)
        $var == -1 && throw(ArgumentError(errmsg))
    end)
end

macro _isodelim(c)
    esc(quote
        i > n && @goto done
        codeunit(s, i) == UInt8($c) || throw(ArgumentError(errmsg))
        i += 1
    end)
end

function _isoparse(s::String, withdate::Bool, withtime::Bool)
    errmsg = withtime ? (withdate ? "invalid ISO 8601 date-time" : "invalid ISO 8601 time") : "invalid ISO 8601 date"
    n = ncodeunits(s)
    i = 1
    y = 0
    m = 1
    d = 1
    h = 0
    mi = 0
    sec = 0
    ms = 0
    if withdate
        negative = _isochar(s, i, '-')
        (negative || _isochar(s, i, '+')) && (i += 1)
        y, i = _isodigits(s, i, 18)
        y == -1 && throw(ArgumentError(errmsg))
        negative && (y = -y)
        @_isodelim '-'
        @_isofield m 2
        @_isodelim '-'
        @_isofield d 2
        withtime || @goto done
        @_isodelim 'T'
        @_isofield h 2
    else
        # the leading field is required: time-only input may not be empty
        h, i = _isodigits(s, i, 2)
        h == -1 && throw(ArgumentError(errmsg))
    end
    @_isodelim ':'
    @_isofield mi 2
    @_isodelim ':'
    @_isofield sec 2
    @_isodelim '.'
    i > n && @goto done
    fraction_start = i
    ms, i = _isodigits(s, i, 3)
    ms == -1 && throw(ArgumentError(errmsg))
    # the fraction is at most three digits (milliseconds), scaled as if
    # right-padded: ".4" is 400 milliseconds
    for _ = 1:(3 - (i - fraction_start))
        ms *= 10
    end
    @label done
    i > n || throw(ArgumentError(errmsg))
    return y, m, d, h, mi, sec, ms
end

function _liftdate(s::String)
    y, m, d, _, _, _, _ = _isoparse(s, true, false)
    return Dates.Date(y, m, d)
end

function _liftdatetime(s::String)
    # `DateTime(str)` rejects time-zone designators, but RFC 3339 date-times
    # with an offset — `2026-08-07T15:00:00Z`, `…+02:00` — are what most JSON
    # producers emit. Accept them here and normalize to UTC; without an
    # offset the value is taken as-is, exactly like the constructor.
    n = ncodeunits(s)
    offsetminutes = 0
    body = s
    if n > 1 && codeunit(s, n) == UInt8('Z') &&
       any(i -> codeunit(s, i) == UInt8('T'), 1:(n - 1))
        body = String(view(codeunits(s), 1:(n - 1)))
    elseif n >= 6 &&
           (codeunit(s, n - 5) == UInt8('+') || codeunit(s, n - 5) == UInt8('-')) &&
           codeunit(s, n - 2) == UInt8(':') &&
           # require a 'T' before the sign so a date's own '-' never matches
           any(i -> codeunit(s, i) == UInt8('T'), 1:(n - 6))
        all(i -> UInt8('0') <= codeunit(s, i) <= UInt8('9'), (n - 4, n - 3, n - 1, n)) ||
            throw(ArgumentError("invalid ISO 8601 date-time"))
        hours = 10 * Int(codeunit(s, n - 4) - UInt8('0')) +
                Int(codeunit(s, n - 3) - UInt8('0'))
        minutes = 10 * Int(codeunit(s, n - 1) - UInt8('0')) +
                  Int(codeunit(s, n) - UInt8('0'))
        offsetminutes = (codeunit(s, n - 5) == UInt8('+') ? -1 : 1) *
                        (60 * hours + minutes)
        body = String(view(codeunits(s), 1:(n - 6)))
    end
    y, m, d, h, mi, sec, ms = _isoparse(body, true, true)
    value = Dates.DateTime(y, m, d, h, mi, sec, ms)
    return offsetminutes == 0 ? value : value + Dates.Minute(offsetminutes)
end

function _lifttime(s::String)
    _, _, _, h, mi, sec, ms = _isoparse(s, false, true)
    return Dates.Time(h, mi, sec, ms)
end

function lift(::Type{T}, x::AbstractString) where {T<:Enum}
    sym = Symbol(x)
    for (k, v) in Base.Enums.namemap(T)
        v === sym && return T(k)
    end
    throw(ArgumentError("invalid `$T` string value: \"$sym\""))
end

lift(st::StructStyle, ::Type{T}, x) where {T} = lift(T, x), defaultstate(st)

# bit of an odd case, but support 0-dimensional array lifting from scalar value
function lift(st::StructStyle, ::Type{A}, x) where {A<:AbstractArray{T,0}} where {T}
    m = A(undef)
    m[1] = lift(st, T, x)
    return m, defaultstate(st)
end

function lift(st::StructStyle, ::Type{T}, x, tags) where {T}
    if haskey(tags, :lift)
        return tags.lift(x), defaultstate(st)
    elseif T <: Dates.TimeType && haskey(tags, :dateformat)
        if tags.dateformat isa String
            return parse(T, x, Dates.DateFormat(tags.dateformat)), defaultstate(st)
        else
            return parse(T, x, tags.dateformat), defaultstate(st)
        end
    else
        return lift(st, T, x)
    end
end

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

liftkey(::StructStyle, ::Type{T}, x) where {T} = liftkey(T, x)
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
    throw(ArgumentError("needle not found in haystack"))
end
```
"""
struct EarlyReturn{T}
    value::T
end

struct _MatchedState{T}
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
    return defaultstate(st)
end

# special-case Pair vectors to act like Dicts
function applyeach(st::StructStyle, f, x::AbstractVector{Pair{K,V}}) where {K,V}
    for (k, v) in x
        ret = f(lowerkey(st, k), lower(st, v))
        ret isa EarlyReturn && return ret
    end
    return defaultstate(st)
end

# special-case Pair to act like key-value object
function applyeach(st::StructStyle, f, x::Pair)
    ret = f(lowerkey(st, x.first), lower(st, x.second))
    ret isa EarlyReturn && return ret
    return defaultstate(st)
end

# appropriate definition for iterables that
# can't have #undef values
function applyeach(st::StructStyle, f, x::Union{AbstractSet,Base.Generator,Core.SimpleVector})
    for (i, v) in enumerate(x)
        ret = f(lowerkey(st, i), lower(st, v))
        ret isa EarlyReturn && return ret
    end
    return defaultstate(st)
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
        push!(ex.args, :(return defaultstate(st)))
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
        return defaultstate(st)
    end
end

function applyeach(st::StructStyle, f, x::AbstractDict)
    for (k, v) in x
        ret = f(lowerkey(st, k), lower(st, v))
        ret isa EarlyReturn && return ret
    end
    return defaultstate(st)
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
        return unsafe_load(Ptr{UInt32}(atomicfields), 1 + s ÷ 32) & (1 << (s % 32)) != 0
    end
else
    const _isfieldatomic = Base.isfieldatomic
end

_setfield!(x, i, v) = setfield!(x, i, v, _isfieldatomic(typeof(x), i) ? :sequentially_consistent : :not_atomic)

keyeq(a::Symbol, b::String) = a === Symbol(b)
keyeq(a::String, b::Symbol) = Symbol(a) == b
keyeq(a, b::String) = string(a) == b
keyeq(a::AbstractString, b::String) = String(a) == b
keyeq(a, b::Tuple) = any(keyeq(a), b)
keyeq(a, b) = isequal(a, b)
keyeq(x) = y -> keyeq(x, y)

macro _f(i)
    esc(:(x = f($i); x isa EarlyReturn && return x.value))
end

@generated function _foreach(f, ::Type{T}) where {T}
    # marked inline since this benefits from constant propagation of `n`
    n = fieldcount(T)
    ex = Expr(:block)
    push!(ex.args, :(Base.@_inline_meta))
    for i = 1:n
        push!(ex.args, :(@_f($i)))
    end
    return ex
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
function discover_dims(style, x, ndims::Int)
    @assert arraylike(style, x)
    len = applylength(x)
    if ndims == 1
        return (len,)
    end

    ret = (
        applyeach(x) do _, v
            return arraylike(style, v) ? EarlyReturn(discover_dims(style, v, ndims - 1)) : EarlyReturn(())
        end
    )::EarlyReturn
    return (ret.value..., len)
end

"""
    StructUtils.discover_dims(style, ::Type{T}, source) -> Tuple

Discover the dimensions for a fixed-size array type `T`. By default,
delegates to `discover_dims(style, source, ndims(T))` to scan the source object.
Override for types where dimensions are encoded in the type itself
(e.g. `StaticArrays.StaticArray`), avoiding the need to scan the source.
"""
discover_dims(style, ::Type{T}, source) where {T} = discover_dims(style, source, ndims(T))

"""
    StructUtils.arrayfromdata(::Type{T}, mem, dims::Tuple) -> T

Convert a filled data buffer `mem` with shape `dims` into the target array
type `T`. Called at the end of `makearray` for `fixedsizearray` types.
"""
function arrayfromdata end

arrayfromdata(::Type{T}, buf::Vector, dims::Tuple) where {T<:AbstractArray} =
    reshape(buf, dims)

if VERSION >= v"1.11"
    arrayfromdata(::Type{T}, mem::Memory, dims::Tuple) where {T<:AbstractArray} =
        Base.wrap(Array, Base.memoryref(mem), dims)
end

struct MultiDimClosure{S,A}
    style::S
    arr::A
    dims::Vector{Int}
    cur_dim::Base.RefValue{Int}
end

function (f::MultiDimClosure{S,A})(i::Int, val) where {S,A}
    f.dims[f.cur_dim[]] = i
    if arraylike(f.style, val) && f.cur_dim[] > 1
        f.cur_dim[] -= 1
        st = applyeach(f, f.style, val)
        f.cur_dim[] += 1
    else
        val, st = make(f.style, eltype(f.arr), val)
        setindex!(f.arr, val, f.dims...)
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
    StructUtils.make(style, T, source) -> Tuple{T, Any}
    StructUtils.make!(style, x::T, source)

Construct a struct of type `T` from `source` using the given `style`. The `source` can be any
type of object, and the `style` can be any `StructStyle` subtype (default `StructUtils.DefaultStyle()`).

`make` will use any knowledge of `noarg`, `arraylike`, or `dictlike` in order to
determine how to construct an instance of `T`. The fallback for structs is to rely on
the automatic "all argument" constructor that structs have defined by default (e.g. `T(fields...)`).

`make` calls `applyeach` on the `source` object, where the key-value pairs
from `source` will be used in constructing `T`.

The 3rd definition takes a `style` argument, allowing for overloads of non-owned types `T`.
The main difference between this and the 2nd definition is that the 3rd definition allows for
the `make` function to return a tuple of the constructed struct and any side-effect state
derived from making the struct.

The 4th definition allows passing in an already-constructed instance of `T` (`x`),
which must be mutable, and source key-value pairs will be applied as
to `x` as source keys are matched to struct field names.

For structs, `fieldtags` will be accounted for and certain tags can be used
to influence the construction of the struct.
"""
function make end

function make(::Type{T}, source, style::StructStyle=DefaultStyle()) where {T}
    x, _ = make(style, T, source)
    return x
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

# Abstract collection targets are not constructible, so when the incoming value
# already satisfies the abstract type we must preserve it instead of rebuilding.
@inline abstractcollectionpassthrough(style::StructStyle, ::Type{T}, source) where {T} =
    isabstracttype(T) && source isa T && (dictlike(style, T) || arraylike(style, T))

# Keep normal `make` dispatch at the public boundary so exact custom methods
# and `@choosetype` methods win first. The concrete `Val{T}` token then gives
# the default implementation a specialized signature even when `T` is a
# Union, which avoids duplicating Union behavior in generated field code.
function make(style::StructStyle, ::Type{T}, source, tags) where {T}
    return _make(style, Val{T}(), source, tags)
end

function _make(style::StructStyle, ::Val{T}, source, tags) where {T}
    if haskey(tags, :choosetype)
        return make(style, tags.choosetype(source), source, _delete(tags, :choosetype))
    end
    if T !== Any
        if T >: Missing && T !== Missing
            if nulllike(style, source)
                return make(style, Missing, source, tags)
            else
                return make(style, nonmissingtype(T), source, tags)
            end
        elseif T >: Nothing && T !== Nothing
            if nulllike(style, source)
                return make(style, Nothing, source, tags)
            else
                return make(style, Base.nonnothingtype(T), source, tags)
            end
        end
        # for Union types like Union{T, Vector{T}} (after Nothing/Missing have been peeled),
        # we can disambiguate by checking if source is arraylike;
        # only applies when there's exactly one arraylike and one non-arraylike member
        if T isa Union
            types = Base.uniontypes(T)
            arr_type = nothing
            scalar_type = nothing
            ambiguous = false
            for t in types
                if arraylike(style, t)
                    # more than one arraylike type means we can't disambiguate
                    if arr_type !== nothing
                        ambiguous = true
                        break
                    end
                    arr_type = t
                else
                    if scalar_type !== nothing
                        ambiguous = true
                        break
                    end
                    scalar_type = t
                end
            end
            if !ambiguous && arr_type !== nothing && scalar_type !== nothing
                if arraylike(style, source)
                    return make(style, arr_type, source, tags)
                else
                    return make(style, scalar_type, source, tags)
                end
            end
        end
    end
    if T <: Tuple || dictlike(style, T) || arraylike(style, T) || noarg(style, T) || structlike(style, T)
        return make(style, T, source)
    else
        return lift(style, T, source, tags)
    end
end

function make(style::StructStyle, ::Type{T}, source) where {T}
    if abstractcollectionpassthrough(style, T, source)
        return source, defaultstate(style)
    end
    # start with some hard-coded Union cases
    if T !== Any
        if T >: Missing && T !== Missing
            if nulllike(style, source)
                return make(style, Missing, source)
            else
                return make(style, nonmissingtype(T), source)
            end
        elseif T >: Nothing && T !== Nothing
            if nulllike(style, source)
                return make(style, Nothing, source)
            else
                return make(style, Base.nonnothingtype(T), source)
            end
        end
        # for Union types like Union{T, Vector{T}} (after Nothing/Missing have been peeled),
        # we can disambiguate by checking if source is arraylike;
        # only applies when there's exactly one arraylike and one non-arraylike member
        if T isa Union
            types = Base.uniontypes(T)
            arr_type = nothing
            scalar_type = nothing
            ambiguous = false
            for t in types
                if arraylike(style, t)
                    # more than one arraylike type means we can't disambiguate
                    if arr_type !== nothing
                        ambiguous = true
                        break
                    end
                    arr_type = t
                else
                    if scalar_type !== nothing
                        ambiguous = true
                        break
                    end
                    scalar_type = t
                end
            end
            if !ambiguous && arr_type !== nothing && scalar_type !== nothing
                if arraylike(style, source)
                    return make(style, arr_type, source)
                else
                    return make(style, scalar_type, source)
                end
            end
        end
    end
    if T <: Tuple
        return maketuple(style, T, lower(style, source))
    elseif dictlike(style, T)
        lowered = lower(style, source)
        if abstractcollectionpassthrough(style, T, lowered)
            return lowered, defaultstate(style)
        end
        return makedict(style, T, lowered)
    elseif arraylike(style, T)
        lowered = lower(style, source)
        if abstractcollectionpassthrough(style, T, lowered)
            return lowered, defaultstate(style)
        end
        return makearray(style, T, lowered)
    elseif noarg(style, T)
        return makenoarg(style, T, lower(style, source))
    elseif structlike(style, T)
        return makestruct(style, T, lower(style, source))
    else
        return lift(style, T, source)
    end
end

if VERSION < v"1.11"
    mem(n) = Vector{Any}(undef, n)
else
    mem(n) = Memory{Any}(undef, n)
end

macro _t(i)
    esc(:(isassigned(vals, $i) ? @inbounds(vals[$i])::fieldtype(T, $i) : fielddefault(style, T, $i)::fieldtype(T, $i)))
end

@generated function _tuple(::Type{T}, vals, style) where {T}
    t = Expr(:tuple)
    for i = 1:fieldcount(T)
        push!(t.args, :(@_t($i)))
    end
    return Expr(:block, :(Base.@_inline_meta), t)
end

struct TupleClosure{T,A,S}
    vals::A
    style::S
    i::Ptr{Int}
end

function (f::TupleClosure{T,A,S})(k, v) where {T,A,S}
    st = _foreach(T) do i
        if typeof(k) == Int
            if k == i
                intval, intst = make(f.style, fieldtype(T, i), v)
                @inbounds f.vals[i] = intval
                return EarlyReturn(_MatchedState(intst))
            end
        else
            j = unsafe_load(f.i)
            if j == i
                unsafe_store!(f.i, i + 1)
                elseval, elsest = make(f.style, fieldtype(T, i), v)
                @inbounds f.vals[i] = elseval
                return EarlyReturn(_MatchedState(elsest))
            end
        end
    end
    return st isa _MatchedState ? st.value : unknownfield(f.style, T, k, v)
end

function maketuple(style, ::Type{T}, source) where {T}
    vals = mem(fieldcount(T))
    ref = Ref(1)
    GC.@preserve ref begin
        i = Base.unsafe_convert(Ptr{Int}, ref)
        st = applyeach(style, TupleClosure{T,typeof(vals),typeof(style)}(vals, style, i), source)
        return _tuple(T, vals, style), st
    end
end

struct DictClosure{T,S}
    dict::T
    style::S
end

function (f::DictClosure{T,S})(k, v) where {T,S}
    val, st = make(f.style, _valtype(f.dict), v)
    addkeyval!(f.dict, liftkey(f.style, _keytype(f.dict), k), val)
    return st
end

makedict(style, ::Type{T}, source) where {T} = makedict(style, initialize(style, T, source), source)

function makedict(style, dict::T, source) where {T}
    st = applyeach(style, DictClosure(dict, style), source)
    return dict, st
end

struct ArrayClosure{T,S}
    arr::T
    style::S
end

function (f::ArrayClosure{T,S})(_, v) where {T,S}
    val, st = make(f.style, eltype(f.arr), v)
    push!(f.arr, val)
    return st
end

struct FixedArrayClosure{A,S}
    arr::A
    style::S
    idx::Base.RefValue{Int}
end

function (f::FixedArrayClosure{A,S})(_, v) where {A,S}
    val, st = make(f.style, eltype(f.arr), v)
    i = f.idx[]
    @inbounds f.arr[i] = val
    f.idx[] = i + 1
    return st
end

function makearray(style, ::Type{T}, source) where {T}
    if fixedsizearray(style, T)
        ET = eltype(T)
        dims = discover_dims(style, T, source)
        L = prod(dims)
        if VERSION >= v"1.11"
            data = Memory{ET}(undef, L)
        else
            data = Vector{ET}(undef, L)
        end
        N = length(dims)
        if N > 1
            buf = reshape(data, dims)
            st = applyeach(style, MultiDimClosure(style, buf, ones(Int, N), Ref(N)), source)
        else
            st = applyeach(style, FixedArrayClosure(data, style, Ref(1)), source)
        end
        return arrayfromdata(T, data, dims), st
    else
        return @inline makearray(style, initialize(style, T, source), source)
    end
end

function makearray(style, x::T, source) where {T}
    if !(T <: AbstractSet) && ndims(T) > 1
        # multidimensional arrays
        n = ndims(T)
        st = applyeach(style, MultiDimClosure(style, x, ones(Int, n), Ref(n)), source)
        return x, st
    else
        st = applyeach(style, ArrayClosure(x, style), source)
        return x, st
    end
end

# NOTE for all @generated functions in this file: generator bodies avoid
# comprehensions/generators that capture `T` — each such closure type is
# specific to `Type{T}`, so running the generator would trigger fresh
# inference of `collect(Generator{...})` for every target type (measured at
# ~5-10ms per closure per type)
@generated function fieldnamestrings(::Type{T}) where {T}
    t = Expr(:tuple)
    for i = 1:fieldcount(T)
        push!(t.args, String(fieldname(T, i)))
    end
    return t
end

@generated function fieldnamesymbols(::Type{T}) where {T}
    t = Expr(:tuple)
    for i = 1:fieldcount(T)
        push!(t.args, QuoteNode(fieldname(T, i)))
    end
    return t
end

if VERSION < v"1.11"
    setval!(vals::Vector{Any}, x, i) = @inbounds vals[i] = x
else
    setval!(vals::Memory{Any}, x, i) = @inbounds vals[i] = x
end

setval!(vals::T, x, i) where {T} = _setfield!(vals, i, x)

# Struct-shaped targets are filled by a FieldSink: a source key is matched to
# a field index (see `cursorhit`/`matchscan` below), then the generated
# `applyfield!` ladder dispatches the index to a `make` call on that field's
# concrete type.

struct NoFieldMetadata end

struct FieldMetadata{FT}
    tags::FT
end

struct NoCursor end

@inline fieldmetadata(tags::Tuple{Vararg{@NamedTuple{}}}) = NoFieldMetadata()
@inline fieldmetadata(tags) = FieldMetadata(tags)

@inline sinktag(::NoFieldMetadata, ::Int) = (;)
@inline sinktag(metadata::FieldMetadata, i::Int) = @inbounds metadata.tags[i]

@inline ignoredfield(tags::NamedTuple{names}) where {names} =
    :ignore in names && tags.ignore

"""
    StructUtils.orderedfields(::StructStyle) -> Bool

Return `true` only when a style consumes source keys in field order and its
owned key type implements [`StructUtils.orderedfieldmatch`](@ref). This is an
internal, experimental integration hook. The source integration must own the
only key type for which `orderedfieldmatch` can return `true`; generic sources
must keep declaration-order matching.
"""
orderedfields(::StructStyle) = false

@inline fieldcursor(style, ::NoFieldMetadata) =
    orderedfields(style) ? Ref(1) : NoCursor()
@inline fieldcursor(style, ::FieldMetadata) = NoCursor()

struct FieldSink{T,S,V,M,C}
    vals::V           # Memory{Any} (immutable/NamedTuple targets) or the instance itself (noarg)
    style::S
    metadata::M       # empty marker or per-field tag NamedTuples, fetched once per `make`
    cursor::C         # NoCursor, or source-owned ordered-key cursor storage
end

FieldSink{T}(vals::V, style::S, metadata::M, cursor::C) where {T,S,V,M,C} =
    FieldSink{T,S,V,M,C}(vals, style, metadata, cursor)

# Key matching is two-phase. Phase 1 (`cursorhit`) is an internal opt-in for
# source-owned key types that can prove the next raw field-name match without
# changing `keyeq` semantics. Generic sources skip it: a custom key may match
# several fields and must always select the first one in declaration order.
# Phase 2 (`matchscan`) is a per-type generated scan with field-name literals.
@inline function matchone(k, ::NoFieldMetadata, i, fn, fstr)
    _ = i
    if typeof(k) == Symbol
        return keyeq(k, fn)
    else
        return keyeq(k, fstr)
    end
end

@inline function matchone(k, metadata::FieldMetadata, i, fn, fstr)
    tags = sinktag(metadata, i)
    if typeof(k) == Symbol
        name = get(tags, :name, fn)
        return keyeq(k, name) || keyeq(k, fn)
    else
        return keyeq(k, get(tags, :name, fstr))
    end
end

"""
    StructUtils.orderedfieldmatch(key, field::String) -> Bool

Return `true` when an integration-owned source-key type proves an exact raw
field-name match. This is an internal, experimental hook. Styles must also opt
in through [`StructUtils.orderedfields`](@ref), and the generic fallback must
remain `false`.
"""
@inline orderedfieldmatch(key, field::String) = false
@inline cursorhit(k, ::NoCursor, metadata, fstrs) = 0
@inline advancecursor!(::NoCursor, i::Int, n::Int) = nothing
@inline advancecursor!(cursor::Base.RefValue{Int}, i::Int, n::Int) =
    cursor[] = i == n ? 1 : i + 1

function cursorhit(k, cursor::Base.RefValue{Int}, metadata, fstrs)
    N = length(fstrs)
    # Tagged names can overlap. Preserve first-field scan order by using the
    # cursor only when every field has the default empty metadata.
    metadata isa NoFieldMetadata || return 0
    N == 0 && return 0
    i = cursor[]
    i > N && (i = 1)
    if orderedfieldmatch(k, @inbounds(fstrs[i]))
        cursor[] = i == N ? 1 : i + 1
        return i
    end
    return 0
end

@generated function matchscan(::Type{T}, k, metadata) where {T}
    ex = Expr(:block)
    for i = 1:fieldcount(T)
        fn = QuoteNode(fieldname(T, i))
        fstr = String(fieldname(T, i))
        push!(ex.args, :(matchone(k, metadata, $i, $fn, $fstr) && return $i))
    end
    push!(ex.args, :(return 0))
    return ex
end

# Splice each field type as a literal. Normal `make` dispatch remains visible,
# including exact custom methods; its default path uses the concrete Val token
# above for Union targets.
function _fieldmake(j::Int, @nospecialize(ft))
    return :(make(f.style, $ft, v, sinktag(f.metadata, $j)))
end

@generated function applyfield!(f::FieldSink{T}, i::Int, v) where {T}
    ex = Expr(:block)
    for j = 1:fieldcount(T)
        push!(ex.args, quote
            if i == $j
                ignoredfield(sinktag(f.metadata, $j)) && return defaultstate(f.style)
                val, st = $(_fieldmake(j, fieldtype(T, j)))
                setval!(f.vals, val, $j)
                return st
            end
        end)
    end
    push!(ex.args, :(return defaultstate(f.style)))
    return ex
end

function (f::FieldSink{T,S,V,M,C})(k, v) where {T,S,V,M,C}
    N = fieldcount(T)
    i = typeof(k) == Int ? ((1 <= k <= N) ? k : 0) :
        cursorhit(k, f.cursor, f.metadata, fieldnamestrings(T))
    if i == 0
        typeof(k) == Int && return unknownfield(f.style, T, k, v)
        i = matchscan(T, k, f.metadata)
        i == 0 && return unknownfield(f.style, T, k, v)
        advancecursor!(f.cursor, i, N)
    end
    return applyfield!(f, i, v)
end

# Build the sink for one make of `T` (tags fetched exactly once per make) and
# run the source through it. Generic sources use an allocation-free NoCursor;
# source integrations can provide private cursor storage for owned key types.
function fillfields!(style::StructStyle, ::Type{T}, vals, source) where {T}
    tags = _fieldtagtuple(style, T, fieldnamesymbols(T))
    metadata = fieldmetadata(tags)
    cursor = fieldcursor(style, metadata)
    return applyeach(style, FieldSink{T}(vals, style, metadata, cursor), source)
end

makenoarg(style, ::Type{T}, source) where {T} = makenoarg(style, initialize(style, T, source), source)

function makenoarg(style, y::T, source) where {T}
    st = fillfields!(style, T, y, source)
    return y, st
end

macro _v(i)
    esc(:(isassigned(vals, $i) ? @inbounds(vals[$i])::fieldtype(T, $i) : get(defs, @inbounds(fsyms[$i]), nothing)::fieldtype(T, $i)))
end

@generated function _construct(::Type{T}, vals, style, fsyms) where {T}
    n = fieldcount(T)
    # fast path: all fields assigned, skip fielddefaults entirely
    all_assigned = n == 0 ? true : :(isassigned(vals, 1))
    for i = 2:n
        all_assigned = Expr(:&&, all_assigned, :(isassigned(vals, $i)))
    end
    fast = Expr(:call, :T)
    slowcall = Expr(:call, :T)
    for i = 1:n
        push!(fast.args, :(@inbounds(vals[$i])::fieldtype(T, $i)))
        push!(slowcall.args, :(@_v($i)))
    end
    slow = Expr(:block, :(defs = fielddefaults(style, T, vals)), slowcall)
    return Expr(:block, :(Base.@_inline_meta), Expr(:if, all_assigned, fast, slow))
end

function makestruct(style, ::Type{T}, source) where {T}
    vals = mem(fieldcount(T))
    st = fillfields!(style, T, vals, source)
    if T <: NamedTuple
        return T(_tuple(T, vals, style)), st
    else
        return _construct(T, vals, style, fieldnamesymbols(T)), st
    end
end

make!(x::T, source; style::StructStyle=DefaultStyle()) where {T} = make!(style, x, source)
make!(::Type{T}, source; style::StructStyle=DefaultStyle()) where {T} = make!(style, T, source)

function make!(style::StructStyle, x::T, source) where {T}
    if dictlike(style, x)
        _, st = makedict(style, x, source)
    elseif arraylike(style, x)
        _, st = makearray(style, x, source)
    elseif noarg(style, x)
        _, st = makenoarg(style, x, source)
    else
        throw(ArgumentError("Type `$T` does not support in-place `make!`"))
    end
    return st
end

function make!(style::StructStyle, ::Type{T}, source) where {T}
    if abstractcollectionpassthrough(style, T, source)
        return source
    end
    x = initialize(style, T, source)
    make!(style, x, source)
    return x
end

"See [`make`](@ref)."
make!

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
        function StructUtils.make(st::StructUtils.StructStyle, ::Type{$T}, source, tags)
            func = $(ex)
            StructUtils.make(st, applicable(func, source, tags) ? func(source, tags) : func(source), source, tags)
        end
        function StructUtils.make(st::StructUtils.StructStyle, ::Type{$T}, source)
            func = $(ex)
            StructUtils.make(st, func(source), source)
        end
    end)
end

macro choosetype(style, T, ex)
    esc(quote
        function StructUtils.make(st::$(style), ::Type{$T}, source, tags)
            func = $(ex)
            StructUtils.make(st, applicable(func, source, tags) ? func(source, tags) : func(source), source, tags)
        end
        function StructUtils.make(st::$(style), ::Type{$T}, source)
            func = $(ex)
            StructUtils.make(st, func(source), source)
        end
    end)
end

end
