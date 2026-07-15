# Tier-0 "interpreter" make path: a non-specializing, field-table-driven
# construction engine for struct targets from tree-like sources (AbstractDict,
# Vector{<:Pair}). One compiled instance per (style, source-type) — never per
# target type — so first-use compile cost for a new struct type is table-build
# only (microseconds), and the whole engine precompiles into pkgimages.
#
# Construction goes through dispatch-free runtime primitives (the same kernel
# Serialization uses): jl_new_structv from boxed slots, jl_alloc_array_1d +
# memoryref builtins for typed vectors. All per-field behavior is driven by a
# closed set of kind tags computed once per type into a FieldTable.
#
# Trim story: with the `trim_build` compile-time preference set, the JIT-only
# arms (per-type fielddefaults/fieldtags consultation, dynamic `lift` for
# custom leaf types, defaults-thunk re-evaluation, exotic key/source shapes)
# are compile-time pruned, leaving a graph `juliac --trim=safe` can fully
# verify. Types registered via the struct macros (or an explicit
# `register_fieldtable!`) carry their defaults/tags as plain data and parse
# through the interpreter in trimmed binaries with no per-type dispatch.

# closed kind universe; anything else is KIND_CUSTOM (dynamic lift, JIT-only)
const KIND_UNSUPPORTED = Int8(0)
const KIND_STRING = Int8(1)
const KIND_INT64 = Int8(2)
const KIND_INT32 = Int8(3)
const KIND_INT16 = Int8(4)
const KIND_INT8 = Int8(5)
const KIND_INT128 = Int8(6)
const KIND_UINT64 = Int8(7)
const KIND_UINT32 = Int8(8)
const KIND_UINT16 = Int8(9)
const KIND_UINT8 = Int8(10)
const KIND_UINT128 = Int8(11)
const KIND_FLOAT64 = Int8(12)
const KIND_FLOAT32 = Int8(13)
const KIND_FLOAT16 = Int8(14)
const KIND_BOOL = Int8(15)
const KIND_DATE = Int8(16)
const KIND_DATETIME = Int8(17)
const KIND_TIME = Int8(18)
const KIND_UUID = Int8(19)
const KIND_SYMBOL = Int8(20)
const KIND_CHAR = Int8(21)
const KIND_ANY = Int8(22)
const KIND_STRUCT = Int8(23)
const KIND_VECTOR = Int8(24)
const KIND_CUSTOM = Int8(25)

function scalarkind(@nospecialize(ft))
    ft === String && return KIND_STRING
    ft === Int64 && return KIND_INT64
    ft === Int32 && return KIND_INT32
    ft === Int16 && return KIND_INT16
    ft === Int8 && return KIND_INT8
    ft === Int128 && return KIND_INT128
    ft === UInt64 && return KIND_UINT64
    ft === UInt32 && return KIND_UINT32
    ft === UInt16 && return KIND_UINT16
    ft === UInt8 && return KIND_UINT8
    ft === UInt128 && return KIND_UINT128
    ft === Float64 && return KIND_FLOAT64
    ft === Float32 && return KIND_FLOAT32
    ft === Float16 && return KIND_FLOAT16
    ft === Bool && return KIND_BOOL
    ft === Dates.Date && return KIND_DATE
    ft === Dates.DateTime && return KIND_DATETIME
    ft === Dates.Time && return KIND_TIME
    ft === UUID && return KIND_UUID
    ft === Symbol && return KIND_SYMBOL
    ft === Char && return KIND_CHAR
    ft === Any && return KIND_ANY
    return KIND_UNSUPPORTED
end

# sentinel states for the per-field default slot
struct NoDefault end
const NODEFAULT = NoDefault()
struct FreshEmpty end          # empty vector default: re-materialize per construct
const FRESHEMPTY = FreshEmpty()
struct ThunkDefault end        # mutable/aliasing-unsafe default: re-eval thunk (JIT only)
const THUNKDEFAULT = ThunkDefault()

struct FieldSpec
    name::String       # match name (post fieldtags rename)
    namesym::Symbol    # same, as Symbol, for Symbol-keyed sources
    fieldsym::Symbol   # original field name (defaults NamedTuples key on this)
    kind::Int8
    nullable::Bool     # Union{..., Nothing}
    missingable::Bool  # Union{..., Missing}
    ft::Any            # non-Nothing/Missing field type
    elkind::Int8       # KIND_VECTOR: element kind
    elft::Any          # KIND_VECTOR: element type
    default::Any       # shared boxed default, or NODEFAULT/FRESHEMPTY/THUNKDEFAULT
end

struct FieldTable
    T::DataType
    specs::Vector{FieldSpec}
    eligible::Bool         # interpreter can construct this type at all
    anythunk::Bool         # some field needs defaults-thunk re-evaluation (JIT only)
    defaultsthunk::Any     # () -> NamedTuple, or nothing
    valsdefaults::Bool     # defaults reference parsed fields: use the 3-arg
                           # fielddefaults(style, T, vals) semantics (JIT only)
end

# raw per-type metadata as registered by the struct macros: plain data, so
# table resolution never needs per-type method dispatch (the trim requirement)
struct RawMeta
    defaults::Any        # NamedTuple of defaults evaluated at registration, or nothing
    defaultsthunk::Any   # () -> NamedTuple for aliasing-unsafe re-evaluation, or nothing
    tags::Any            # NamedTuple of fieldtags, or nothing
    nonstruct::Bool      # @nonstruct types lift as leaves, never field-parse
    tagsmethod::Any      # Method resolving fieldtags at registration time
    defaultsmethod::Any  # Method resolving fielddefaults at registration time
    valsdependent::Bool  # defaults reference other (parsed) fields
end

# registry snapshots are immutable-after-publish; readers take one atomic
# load and never contend with the write lock (the eligibility gate runs on
# the hot path of every structlike `make`)
struct MetaSnap
    raw::IdDict{Any,RawMeta}
    # (target type) => [(style type) => resolved table]; per style TYPE
    # because fieldtagkey namespacing and metadata-method resolution are
    # style-dependent
    tables::IdDict{Any,Vector{Pair{DataType,FieldTable}}}
end

mutable struct MetaStore
    @atomic snap::MetaSnap
end

const METASTORE = MetaStore(MetaSnap(IdDict{Any,RawMeta}(),
    IdDict{Any,Vector{Pair{DataType,FieldTable}}}()))
const META_LOCK = ReentrantLock()

"""
    StructUtils.register_fieldtable!(T; defaults=nothing, tags=nothing)

Register tier-0 interpreter metadata for struct type `T`: `defaults` is a
zero-arg function returning a `NamedTuple` of field defaults (evaluated in
field order), `tags` a `NamedTuple` of fieldtags. The `@noarg`, `@kwarg`,
`@defaults`, and `@tags` macros emit a call to this automatically; call it
manually for types with hand-written `fielddefaults`/`fieldtags` overloads
that must parse through the interpreter in `juliac --trim` binaries (where
per-type method consultation is unavailable).
"""
function register_fieldtable!(@nospecialize(T::Type); defaults=nothing, tags=nothing,
                              nonstruct::Bool=false, valsdependent::Bool=false)
    nt = nothing
    if defaults !== nothing && !valsdependent
        # evaluate once, eagerly: the values become plain data usable in
        # trimmed binaries; parametric defaults that need bound type
        # parameters may throw here — keep the thunk, resolve per-instantiation
        nt = try
            defaults()
        catch
            nothing
        end
    end
    # capture which methods resolve this type's metadata right now (the
    # macro-emitted ones, or the generic fallbacks): if a more specific or
    # per-style overload appears later, table resolution detects the mismatch
    # and routes that (style, T) to the classic path instead
    tm = _whichmeta(fieldtags, T)
    dm = _whichmeta(fielddefaults, T)
    # explicit lock/unlock: a `lock() do` closure capturing @nospecialize
    # arguments is not verifier-resolvable
    lock(META_LOCK)
    try
        old = @atomic METASTORE.snap
        raw = copy(old.raw)
        raw[T] = RawMeta(nt, defaults, tags, nonstruct, tm, dm, valsdependent)
        tables = copy(old.tables)
        empty!(tables) # invalidate resolved tables (nested kinds may change)
        @atomic METASTORE.snap = MetaSnap(raw, tables)
    finally
        unlock(META_LOCK)
    end
    return nothing
end

_whichmeta(@nospecialize(f), @nospecialize(T)) = try
    which(f, Tuple{StructStyle, Type{T}})
catch
    nothing
end

_whichmeta_style(@nospecialize(f), @nospecialize(style), @nospecialize(T)) = try
    which(f, Tuple{typeof(style), Type{T}})
catch
    nothing
end

# ---------------- table resolution ----------------

# builtin-only union peel: strips Nothing/Missing from up to 3-component
# unions; a wider union or non-concrete remainder ends up KIND_UNSUPPORTED
function peel_nullmissing(@nospecialize(ft0))
    nullable = false
    missingable = false
    ft = ft0
    for _ = 1:2
        ft isa Union || break
        a = getfield(ft, :a)
        b = getfield(ft, :b)
        if a === Nothing
            nullable = true
            ft = b
        elseif b === Nothing
            nullable = true
            ft = a
        elseif a === Missing
            missingable = true
            ft = b
        elseif b === Missing
            missingable = true
            ft = a
        else
            break
        end
    end
    return nullable, missingable, ft
end

_isvectortype(@nospecialize(ft)) =
    ft isa DataType && getfield(getfield(ft, :name), :wrapper) === Array &&
    length(getfield(ft, :parameters)::Core.SimpleVector) == 2 &&
    (getfield(ft, :parameters)::Core.SimpleVector)[2] === 1

_vector_eltype(@nospecialize(ft)) = (getfield(ft, :parameters)::Core.SimpleVector)[1]

_vectype(@nospecialize(E)) = Core.apply_type(Vector, E)

function kindfor(@nospecialize(ft))
    k = scalarkind(ft)
    k != KIND_UNSUPPORTED && return k
    ft isa DataType || return KIND_UNSUPPORTED
    _isvectortype(ft) && return KIND_VECTOR
    if isstructtype(ft) && !ismutabletype(ft) && !Base.issingletontype(ft) &&
       isconcretetype(ft) && !(ft <: AbstractDict) && !(ft <: AbstractArray) &&
       !(ft <: AbstractSet) && !(ft <: AbstractString) && !(ft <: Tuple) &&
       !(ft <: NamedTuple) && !(ft <: Function)
        return KIND_STRUCT
    end
    return KIND_UNSUPPORTED
end

# classify one evaluated default value for aliasing safety. FRESHEMPTY only
# applies to empty Arrays (matching the KIND_VECTOR construct arm); the
# narrow isa keeps the length call verifier-resolvable
function classify_default(@nospecialize(v))
    # size read via the field: generic length on an abstract Array does not
    # devirtualize for the trim verifier
    v isa Vector && (getfield(v, :size)::Tuple{Int})[1] == 0 && return FRESHEMPTY
    (isbits(v) || v isa String || v isa Symbol) && return v
    return THUNKDEFAULT
end

_rawmeta(snap::MetaSnap, @nospecialize(T)) = begin
    m = get(snap.raw, T, nothing)
    if m === nothing && T isa DataType
        # parametric structs register under their UnionAll wrapper
        m = get(snap.raw, getfield(getfield(T, :name), :wrapper), nothing)
    end
    m
end

function buildtable(@nospecialize(T::Type), style::StructStyle)
    T isa DataType ||
        return FieldTable(Nothing, FieldSpec[], false, false, nothing, false)
    tagkey = fieldtagkey(style)
    snap = @atomic METASTORE.snap
    meta = _rawmeta(snap, T)
    metaok = meta !== nothing
    valsdefaults = metaok && (meta::RawMeta).valsdependent
    if metaok && !TRIM_BUILD
        # the registered data is only valid while it is still what
        # fieldtags/fielddefaults would answer for this style: a per-style or
        # more specific overload (possibly stateful) routes to the classic
        # path, preserving exact semantics
        m = meta::RawMeta
        metaok = _whichmeta_style(fieldtags, style, T) === m.tagsmethod &&
                 _whichmeta_style(fielddefaults, style, T) === m.defaultsmethod
    end
    local defaults, tags, thunk
    if metaok
        m = meta::RawMeta
        thunk = m.defaultsthunk
        defaults = valsdefaults ? (;) :
                   m.defaults !== nothing ? m.defaults :
                   (!TRIM_BUILD && thunk !== nothing ? _callthunk(thunk) : (;))
        tags = m.tags === nothing ? (;) : m.tags
    elseif TRIM_BUILD
        # trimmed binaries cannot consult per-type fielddefaults/fieldtags
        # methods (open dynamic dispatch); unregistered types parse with
        # reflection only (no defaults, no tag renames)
        defaults = (;)
        tags = (;)
        thunk = nothing
    else
        # unregistered (or overridden) type under JIT: the classic path owns
        # it — exact semantics for manual/stateful metadata overloads
        return FieldTable(T, FieldSpec[], false, false, nothing, false)
    end
    names = getfield(getfield(T, :name), :names)::Core.SimpleVector
    n = fieldcount(T)
    specs = Vector{FieldSpec}(undef, n)
    eligible = isstructtype(T) && !ismutabletype(T) && !Base.issingletontype(T) &&
               isconcretetype(T) && n > 0 && !(T <: Tuple) && !(T <: NamedTuple) &&
               !(T <: AbstractDict) && !(T <: AbstractArray) && !(T <: AbstractSet) &&
               !(T <: AbstractString) && !(T <: Function) && !(T <: Type)
    anythunk = false
    for i = 1:n
        fn = names[i]::Symbol
        ftags = _fieldtag_nt(tags, fn, tagkey)
        ignored = _tagbool(ftags, :ignore)
        nm = _tagname(ftags, fn)
        nm === nothing && (eligible = false; nm = fn)
        ft0 = fieldtype(T, i)
        nullable, missingable, ft = peel_nullmissing(ft0)
        k = ignored ? KIND_UNSUPPORTED : kindfor(ft)
        if k == KIND_STRUCT
            fm = _rawmeta(snap, ft)
            fm !== nothing && (fm::RawMeta).nonstruct && (k = KIND_CUSTOM)
        end
        # lift/choosetype/dateformat tags force the dynamic arm
        if k != KIND_UNSUPPORTED && ftags isa NamedTuple &&
           (haskey(ftags, :lift) || haskey(ftags, :choosetype) || haskey(ftags, :dateformat))
            k = KIND_CUSTOM
        end
        if k == KIND_UNSUPPORTED && !ignored
            # a field the interpreter can't produce directly: leaf-ish types
            # get the dynamic-lift arm; containers/exotics make T ineligible
            if ft isa DataType && !(ft <: AbstractDict) && !(ft <: AbstractArray) &&
               !(ft <: AbstractSet) && !(ft <: Tuple) && !(ft <: NamedTuple)
                k = KIND_CUSTOM
            else
                eligible = false
            end
        end
        elk = KIND_UNSUPPORTED
        elft = nothing
        if k == KIND_VECTOR
            elft = _vector_eltype(ft)
            elk = elft isa Type ? kindfor(elft) : KIND_UNSUPPORTED
            if elk == KIND_STRUCT
                em = _rawmeta(snap, elft)
                em !== nothing && (em::RawMeta).nonstruct && (elk = KIND_CUSTOM)
            end
            if elk == KIND_UNSUPPORTED || elk == KIND_VECTOR || elk == KIND_CUSTOM
                # Vector{exotic}/nested vectors keep today's specialized path;
                # KIND_CUSTOM elements stay old-path too so per-element lift
                # tags/choosetype semantics are byte-identical
                eligible = false
            end
        end
        rawdef = _defaultfor(defaults, fn)
        def = rawdef isa NoDefault ? NODEFAULT : classify_default(rawdef)
        if def === THUNKDEFAULT
            anythunk = true
        elseif !(rawdef isa NoDefault) && def !== FRESHEMPTY
            def = rawdef
        end
        specs[i] = FieldSpec(String(nm), nm, fn, k, nullable, missingable, ft, elk, elft, def)
    end
    if anythunk
        thunk === nothing && (eligible = false)
    end
    valsdefaults && TRIM_BUILD && (eligible = false)
    return FieldTable(T, specs, eligible, anythunk && !valsdefaults,
        (anythunk && !valsdefaults) ? thunk : nothing, valsdefaults)
end

_callthunk(@nospecialize(f)) = try
    f()
catch
    (;)
end

# tag helpers on runtime NamedTuples: getfield/haskey on a NamedTuple with a
# runtime Symbol are builtin-backed, no dispatch
function _fieldtag_nt(@nospecialize(tags), fn::Symbol, tagkey::Union{Nothing,Symbol})
    tags isa NamedTuple || return nothing
    haskey(tags, fn) || return nothing
    ft = getfield(tags, fn)
    ft isa NamedTuple || return nothing
    if tagkey !== nothing && haskey(ft, tagkey)
        sub = getfield(ft, tagkey)
        sub isa NamedTuple && return sub
    end
    return ft
end

_tagbool(@nospecialize(ftags), k::Symbol) =
    ftags isa NamedTuple && haskey(ftags, k) && getfield(ftags, k) === true

function _tagname(@nospecialize(ftags), fn::Symbol)
    ftags isa NamedTuple || return fn
    haskey(ftags, :name) || return fn
    nm = getfield(ftags, :name)
    nm isa Symbol && return nm
    nm isa String && return Symbol(nm)
    return nothing # exotic name tags (e.g. Tuple aliases) keep the classic path
end

function _defaultfor(@nospecialize(defaults), fn::Symbol)
    defaults isa NamedTuple || return NODEFAULT
    haskey(defaults, fn) || return NODEFAULT
    return getfield(defaults, fn)
end

function fieldtable(@nospecialize(T::Type), style::StructStyle)
    styletype = typeof(style)
    snap = @atomic METASTORE.snap
    entry = get(snap.tables, T, nothing)
    if entry !== nothing
        for (st, tbl) in entry
            st === styletype && return tbl
        end
    end
    tbl = buildtable(T, style)
    lock(META_LOCK)
    try
        old = @atomic METASTORE.snap
        tables = copy(old.tables)
        v = get(tables, T, nothing)
        v = v === nothing ? Vector{Pair{DataType,FieldTable}}() : copy(v)
        found = false
        for (st, _) in v
            st === styletype && (found = true; break)
        end
        if !found
            push!(v, styletype => tbl)
            tables[T] = v
            @atomic METASTORE.snap = MetaSnap(old.raw, tables)
        end
    finally
        unlock(META_LOCK)
    end
    return tbl
end

# ---------------- dispatch-free construction primitives ----------------

_alloc_vector(@nospecialize(E), n::Int) =
    ccall(:jl_alloc_array_1d, Any, (Any, Csize_t), Core.apply_type(Vector, E), n)

function _setvec!(@nospecialize(arr), i::Int, @nospecialize(v))
    ref = getfield(arr, :ref)
    r = Core.memoryrefnew(ref, i, true)
    Core.memoryrefset!(r, v, :not_atomic, false)
    return nothing
end

@noinline _missingfield(name::String, T::DataType) =
    throw(ArgumentError(string("missing required field '", name, "' constructing ", String(nameof(T)))))

@noinline _liftfail(kind::Int8, name::String) =
    throw(ArgumentError(string("tier-0 interpreter cannot lift value for field '", name,
        "' (kind ", Int(kind), "); define the struct with `:hot` or register a supported kind")))

function _construct_interp(style::StructStyle, tbl::FieldTable, slots::Vector{Any})
    specs = tbl.specs
    n = length(specs)
    defs = if !TRIM_BUILD && tbl.valsdefaults
        # defaults referencing parsed fields: evaluate against the slots,
        # exactly like the classic _construct slow path
        fielddefaults(style, tbl.T, slots)
    elseif !TRIM_BUILD && tbl.anythunk
        _callthunk(tbl.defaultsthunk)
    else
        nothing
    end
    for i = 1:n
        isassigned(slots, i) && continue
        sp = @inbounds specs[i]
        d = tbl.valsdefaults ? THUNKDEFAULT : sp.default
        if d === NODEFAULT
            if sp.nullable
                slots[i] = nothing
            elseif sp.missingable
                slots[i] = missing
            else
                _missingfield(sp.name, tbl.T)
            end
        elseif d === FRESHEMPTY
            slots[i] = _alloc_vector(sp.elft, 0)
        elseif d === THUNKDEFAULT
            if TRIM_BUILD
                _missingfield(sp.name, tbl.T)
            else
                d2 = _defaultfor(defs, sp.fieldsym)
                if d2 isa NoDefault
                    if sp.nullable
                        slots[i] = nothing
                    elseif sp.missingable
                        slots[i] = missing
                    else
                        _missingfield(sp.name, tbl.T)
                    end
                else
                    slots[i] = d2
                end
            end
        else
            slots[i] = d
        end
    end
    GC.@preserve slots begin
        return ccall(:jl_new_structv, Any, (Any, Ptr{Any}, UInt32), tbl.T, pointer(slots), n % UInt32)
    end
end

# ---------------- leaf lifting (closed kinds; exact-typed results) ----------------

# ISO 8601 fast paths: Dates' DateFormat machinery is both slower and not
# trim-verifiable in its error paths
@inline function _dig(cs::String, i::Int, name::String)
    b = codeunit(cs, i)
    (UInt8('0') <= b <= UInt8('9')) || _liftfail(KIND_DATE, name)
    return (b - UInt8('0')) % Int
end

function _parse_iso_date(s::String, name::String)
    (ncodeunits(s) == 10 && codeunit(s, 5) == UInt8('-') && codeunit(s, 8) == UInt8('-')) ||
        _liftfail(KIND_DATE, name)
    y = _dig(s, 1, name) * 1000 + _dig(s, 2, name) * 100 + _dig(s, 3, name) * 10 + _dig(s, 4, name)
    m = _dig(s, 6, name) * 10 + _dig(s, 7, name)
    d = _dig(s, 9, name) * 10 + _dig(s, 10, name)
    return Dates.Date(y, m, d)
end

function _parse_iso_timeparts(s::String, off::Int, name::String)
    n = ncodeunits(s)
    (n >= off + 7 && codeunit(s, off + 2) == UInt8(':') && codeunit(s, off + 5) == UInt8(':')) ||
        _liftfail(KIND_TIME, name)
    h = _dig(s, off, name) * 10 + _dig(s, off + 1, name)
    mi = _dig(s, off + 3, name) * 10 + _dig(s, off + 4, name)
    sec = _dig(s, off + 6, name) * 10 + _dig(s, off + 7, name)
    ms = 0
    if n >= off + 9 && codeunit(s, off + 8) == UInt8('.')
        mult = 100
        i = off + 9
        while i <= n && mult > 0
            b = codeunit(s, i)
            (UInt8('0') <= b <= UInt8('9')) || break
            ms += ((b - UInt8('0')) % Int) * mult
            mult = div(mult, 10)
            i += 1
        end
    end
    return h, mi, sec, ms
end

function _parse_iso_datetime(s::String, name::String)
    n = ncodeunits(s)
    (n >= 19 && codeunit(s, 5) == UInt8('-') && codeunit(s, 8) == UInt8('-') &&
     (codeunit(s, 11) == UInt8('T') || codeunit(s, 11) == UInt8(' '))) ||
        _liftfail(KIND_DATETIME, name)
    y = _dig(s, 1, name) * 1000 + _dig(s, 2, name) * 100 + _dig(s, 3, name) * 10 + _dig(s, 4, name)
    m = _dig(s, 6, name) * 10 + _dig(s, 7, name)
    d = _dig(s, 9, name) * 10 + _dig(s, 10, name)
    h, mi, sec, ms = _parse_iso_timeparts(s, 12, name)
    return Dates.DateTime(y, m, d, h, mi, sec, ms)
end

# lift an already-materialized tree scalar to the spec's exact type; `v` is
# never `nothing` here (nulls are handled by the caller)
function _liftleaf(style::StructStyle, kind::Int8, @nospecialize(ft), @nospecialize(v), name::String)
    if kind == KIND_STRING
        v isa String && return v
        v isa Symbol && return String(v)
    elseif kind == KIND_INT64
        v isa Int64 && return v
        v isa Float64 && return Int64(v)
    elseif kind == KIND_FLOAT64
        v isa Float64 && return v
        v isa Int64 && return Float64(v)
    elseif kind == KIND_BOOL
        v isa Bool && return v
    elseif kind == KIND_DATE
        v isa Dates.Date && return v
        v isa String && return _parse_iso_date(v, name)
    elseif kind == KIND_DATETIME
        v isa Dates.DateTime && return v
        v isa String && return _parse_iso_datetime(v, name)
    elseif kind == KIND_TIME
        v isa Dates.Time && return v
        if v isa String
            h, mi, sec, ms = _parse_iso_timeparts(v, 1, name)
            return Dates.Time(h, mi, sec, ms)
        end
    elseif kind == KIND_UUID
        v isa UUID && return v
        v isa String && return UUID(v)
    elseif kind == KIND_SYMBOL
        v isa Symbol && return v
        v isa String && return Symbol(v)
    elseif kind == KIND_CHAR
        v isa Char && return v
        if v isa String
            length(v) == 1 || _liftfail(kind, name)
            return v[1]
        end
    elseif kind == KIND_INT32
        v isa Int32 && return v
        v isa Int64 && return Int32(v)
        v isa Float64 && return Int32(v)
    elseif kind == KIND_INT16
        v isa Int16 && return v
        v isa Int64 && return Int16(v)
    elseif kind == KIND_INT8
        v isa Int8 && return v
        v isa Int64 && return Int8(v)
    elseif kind == KIND_INT128
        v isa Int128 && return v
        v isa Int64 && return Int128(v)
    elseif kind == KIND_UINT64
        v isa UInt64 && return v
        v isa Int64 && return UInt64(v)
    elseif kind == KIND_UINT32
        v isa UInt32 && return v
        v isa Int64 && return UInt32(v)
    elseif kind == KIND_UINT16
        v isa UInt16 && return v
        v isa Int64 && return UInt16(v)
    elseif kind == KIND_UINT8
        v isa UInt8 && return v
        v isa Int64 && return UInt8(v)
    elseif kind == KIND_UINT128
        v isa UInt128 && return v
        v isa Int64 && return UInt128(v)
    elseif kind == KIND_FLOAT32
        v isa Float32 && return v
        v isa Float64 && return Float32(v)
        v isa Int64 && return Float32(v)
    elseif kind == KIND_FLOAT16
        v isa Float16 && return v
        v isa Float64 && return Float16(v)
        v isa Int64 && return Float16(v)
    end
    if TRIM_BUILD
        _liftfail(kind, name)
    else
        # JIT fallback: same semantics `make` has always had for leaves
        x, _ = lift(style, ft, v)
        return x
    end
end

# ---------------- the interpreter ----------------

# per-(style, source) closure; deliberately NOT parameterized on the target
# type, so applyeach specializes once per source shape, never per struct
struct InterpClosure{S<:StructStyle}
    style::S
    tbl::FieldTable
    slots::Vector{Any}
end

function _findspec(specs::Vector{FieldSpec}, @nospecialize(k))
    if k isa Symbol
        for i = 1:length(specs)
            @inbounds(specs[i]).namesym === k && return i
        end
    elseif k isa String
        for i = 1:length(specs)
            @inbounds(specs[i]).name == k && return i
        end
    elseif !TRIM_BUILD
        # exotic key types (lowerkey overloads): JIT-only generic match
        for i = 1:length(specs)
            keyeq(k, @inbounds(specs[i]).name) && return i
        end
    end
    return 0
end

function (f::InterpClosure{S})(@nospecialize(k), @nospecialize(v)) where {S}
    specs = f.tbl.specs
    i = _findspec(specs, k)
    i == 0 && return unknownfield(f.style, f.tbl.T, k, v)
    sp = @inbounds specs[i]
    if v === nothing && sp.kind != KIND_ANY
        if sp.nullable
            f.slots[i] = nothing
        elseif sp.missingable
            f.slots[i] = missing
        elseif TRIM_BUILD
            _liftfail(sp.kind, sp.name)
        else
            x, _ = lift(f.style, sp.ft, nothing)
            f.slots[i] = x
        end
        return defaultstate(f.style)
    end
    f.slots[i] = _interp_value(f.style, sp, v)
    return defaultstate(f.style)
end

function _interp_value(style::StructStyle, sp::FieldSpec, @nospecialize(v))
    k = sp.kind
    if k == KIND_STRUCT
        if TRIM_BUILD
            # direct recursion with a closed nested-source ladder: concrete
            # types keep the applyeach drive statically resolvable (formats
            # with their own tree types route their sources at their own
            # entry, e.g. JSON drives Object roots through `make` directly)
            if v isa Dict{String,Any}
                x, _ = _interp_make(style, sp.ft, v)
                return x
            elseif v isa Dict{Symbol,Any}
                x, _ = _interp_make(style, sp.ft, v)
                return x
            else
                _liftfail(k, sp.name)
            end
        else
            # JIT: route through the dispatcher so per-style trait overloads
            # (dictlike/arraylike/noarg/structlike) keep exact semantics; it
            # re-enters the interpreter through the gate when appropriate
            x, _ = make(style, sp.ft::Type, v)
            return x
        end
    elseif k == KIND_VECTOR
        return _interp_vector(style, sp, v)
    elseif k == KIND_ANY
        return v
    elseif k == KIND_CUSTOM
        if TRIM_BUILD
            _liftfail(k, sp.name)
        else
            x, _ = make(style, sp.ft, v)
            return x
        end
    else
        return _liftleaf(style, k, sp.ft, v, sp.name)
    end
end

function _interp_vecel(style::StructStyle, elk::Int8, @nospecialize(elft), @nospecialize(ev), name::String)
    if elk == KIND_STRUCT
        if TRIM_BUILD
            if ev isa Dict{String,Any}
                x, _ = _interp_make(style, elft, ev)
                return x
            elseif ev isa Dict{Symbol,Any}
                x, _ = _interp_make(style, elft, ev)
                return x
            else
                _liftfail(elk, name)
            end
        else
            x, _ = make(style, elft::Type, ev)
            return x
        end
    elseif elk == KIND_ANY
        return ev
    else
        return _liftleaf(style, elk, elft, ev, name)
    end
end

function _interp_vector(style::StructStyle, sp::FieldSpec, @nospecialize(v))
    elk = sp.elkind
    elft = sp.elft
    if v isa Vector{Any}
        n = length(v)
        arr = _alloc_vector(elft, n)
        for j = 1:n
            _setvec!(arr, j, _interp_vecel(style, elk, elft, @inbounds(v[j]), sp.name))
        end
        return arr
    elseif !TRIM_BUILD && v isa AbstractVector
        # generic source vectors (e.g. a Vector{Int} value inside a user Dict)
        n = length(v)
        arr = _alloc_vector(elft, n)
        j = 1
        for ev in v
            _setvec!(arr, j, _interp_vecel(style, elk, elft, ev, sp.name))
            j += 1
        end
        return arr
    end
    if TRIM_BUILD
        _liftfail(elk, sp.name)
    else
        # non-vector source for a vector field: today's generic path decides
        x, _ = make(style, _vectype(elft), v)
        return x
    end
end

# true when `source` is a tree shape the interpreter drives directly; under
# trim only AbstractDict (the Vector{Pair} applyeach dispatch is ambiguous
# between the Pair-vector and AbstractArray methods for the verifier)
_interpsource(@nospecialize(source)) =
    TRIM_BUILD ? source isa AbstractDict :
    (source isa AbstractDict || source isa AbstractVector{<:Pair})

# entry: returns (value, state) like every `make`. Both the target type and
# the source are inference-erased (one instance per style, ever — the
# engine's whole point); the drive ladder gives the trim verifier concrete
# applyeach targets, with a dynamic-dispatch arm for arbitrary tree shapes
# under JIT (specializing on source here invites invalidation-recompile
# blowups when extensions load mid-session)
Base.@nospecializeinfer function _interp_make(style::StructStyle, @nospecialize(T::Type), @nospecialize(source))
    tbl = fieldtable(T, style)
    if !tbl.eligible
        # trim recursion into an ineligible type fails loudly at parse time;
        # under JIT the dispatcher gate prevents reaching here, but fall back
        # to the generic path for robustness
        TRIM_BUILD && _liftfail(KIND_STRUCT, String(nameof(tbl.T)))
        return make(style, T, source)
    end
    slots = Vector{Any}(undef, length(tbl.specs))
    f = InterpClosure(style, tbl, slots)
    st = if source isa Dict{String,Any}
        applyeach(style, f, source)
    elseif source isa Dict{Symbol,Any}
        applyeach(style, f, source)
    elseif !TRIM_BUILD
        applyeach(style, f, source)
    else
        _liftfail(KIND_STRUCT, String(nameof(tbl.T)))
    end
    # match makestruct: EarlyReturn from a custom unknownfield flows through
    # as state; construction still happens
    return _construct_interp(style, tbl, slots), st
end

# gate consulted from `make`'s structlike arm: interpret when the target has
# an eligible table and the source is tree-shaped
function _interpready(style::StructStyle, @nospecialize(T::Type), @nospecialize(source))
    T isa DataType || return false
    _interpsource(source) || return false
    return fieldtable(T, style).eligible
end
