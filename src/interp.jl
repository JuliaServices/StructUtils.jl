# The interpreter: the default way `make` builds structs. Instead of
# compiling code for each target type, it describes each type once as data —
# a FieldTable of (name, kind tag, default, tags) per field — and one shared
# engine walks any source against that table. The engine specializes on the
# style and the source container type, never on the target, so a brand-new
# struct type costs a table build (microseconds), and the engine itself is
# already compiled into the package image.
#
# Values become fields in two steps: a small closed set of "kind" tags
# (below) says how to convert each leaf — string, the integer widths, dates,
# UUIDs, nested struct, vector, dict, ... — and construction then goes
# through the same runtime primitives Serialization uses (jl_new_structv
# from a slot buffer; array builtins for typed vectors), which involve no
# dispatch on the target type.
#
# In a `trim_build = true` build (a `juliac --trim` binary), the arms that
# need runtime method lookup — consulting per-type fieldtags/fielddefaults
# methods, the generic `lift` fallback for custom leaf types, re-running
# default thunks — are compiled out, since a trimmed binary has no JIT to
# run them. Types registered by the struct macros carry their defaults and
# tags as plain data, so they parse through the interpreter in trimmed
# binaries with nothing left to look up.

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
const KIND_DICT = Int8(26)
const KIND_SET = Int8(27)
const KIND_UNION2 = Int8(28)
const KIND_TUPLE = Int8(29)
const KIND_FIXEDARRAY = Int8(30)
const KIND_SETLIKE = Int8(31)

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

# recursive description of what to produce at one JSON value position:
# vector elements, dict values, and two-component-union arms nest as child
# specs, so arbitrarily nested containers stay table-driven
struct ValueSpec
    kind::Int8
    nullable::Bool     # Union{..., Nothing} at this position
    missingable::Bool  # Union{..., Missing} at this position
    declft::Any        # declared (pre-peel) type: allocation type for containers
    ft::Any            # peeled target type at this position
    keykind::Int8      # KIND_DICT: key kind (String/Symbol/int widths)
    child::Any         # ::Union{Nothing,ValueSpec}: vector/set element, dict value,
                       # or the arraylike arm of a two-component union
    child2::Any        # ::Union{Nothing,ValueSpec}: the non-arraylike union arm
end

struct FieldSpec
    name::String       # match name (post fieldtags rename)
    namesym::Symbol    # same, as Symbol, for Symbol-keyed sources
    fieldsym::Symbol   # original field name (defaults NamedTuples key on this)
    aliases::Any       # nothing, or (Vector{String}, Vector{Symbol}) extra
                       # match names from tuple-alias name tags
    spec::ValueSpec
    default::Any       # shared boxed default, or NODEFAULT/FRESHEMPTY/THUNKDEFAULT
    tags::Any          # resolved per-field tag NamedTuple (KIND_CUSTOM passes
                       # it through the 4-arg make: lift/choosetype/dateformat)
end

struct FieldTable
    T::DataType
    mutable::Bool          # @noarg target: construct T() / uninit + setfield!
    specs::Vector{FieldSpec}
    eligible::Bool         # interpreter can construct this type at all
    anythunk::Bool         # some field needs defaults-thunk re-evaluation (JIT only)
    defaultsthunk::Any     # () -> NamedTuple, or nothing
    valsdefaults::Bool     # defaults reference parsed fields: use the 3-arg
                           # fielddefaults(style, T, vals) semantics (JIT only)
    volatile::Bool         # metadata comes from live per-type methods
                           # (unregistered types, or overloads more specific
                           # than the registration): rebuild per make so
                           # stateful fieldtags/fielddefaults keep their
                           # call-per-make semantics; never cached
end

# raw per-type metadata as registered by the struct macros: plain data, so
# table resolution never needs per-type method dispatch (the trim requirement)
struct RawMeta
    defaults::Any        # NamedTuple of defaults evaluated at registration, or nothing
    defaultsthunk::Any   # () -> NamedTuple for aliasing-unsafe re-evaluation, or nothing
    tags::Any            # NamedTuple of fieldtags, or nothing
    nonstruct::Bool      # @nonstruct types lift as leaves, never field-parse
    tagsmethods::Any     # Methods applicable to fieldtags(::StructStyle, ::Type{<:T})
                         # at registration (the macro's emissions, incl.
                         # parameterized forms, or the generic fallback)
    defaultsmethods::Any # same, for fielddefaults
    valsdependent::Bool  # defaults reference other (parsed) fields
end

# registry snapshots are immutable-after-publish; readers take one atomic
# load and never contend with the write lock (the eligibility gate runs on
# the hot path of every structlike `make`)
struct MetaSnap
    raw::IdDict{Any,RawMeta}
    # (target type) => [(style type) => root ValueSpec] for non-struct
    # targets (containers, tuples) driven through the interpreter directly
    roots::IdDict{Any,Vector{Pair{DataType,Any}}}
    # (target type) => [(style type) => resolved table]; per style TYPE
    # because fieldtagkey namespacing and metadata-method resolution are
    # style-dependent
    tables::IdDict{Any,Vector{Pair{DataType,FieldTable}}}
end

mutable struct MetaStore
    @atomic snap::MetaSnap
end

const METASTORE = MetaStore(MetaSnap(IdDict{Any,RawMeta}(),
    IdDict{Any,Vector{Pair{DataType,Any}}}(),
    IdDict{Any,Vector{Pair{DataType,FieldTable}}}()))
const META_LOCK = ReentrantLock()

"""
    StructUtils.register_fieldtable!(T; defaults=nothing, tags=nothing,
                                     nonstruct=false, valsdependent=false)

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
    # and rebuilds the table from the live methods instead
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
        roots = copy(old.roots)
        empty!(roots)
        @atomic METASTORE.snap = MetaSnap(raw, roots, tables)
    finally
        unlock(META_LOCK)
    end
    return nothing
end

_whichmeta(@nospecialize(f), @nospecialize(T)) = try
    collect(methods(f, Tuple{StructStyle, Type{<:T}}))
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
    if ft isa Union
        # Nothing/Missing were already peeled: a remaining 2-union is
        # decided by source shape (array-like source -> the array-like
        # member, anything else -> the other member)
        return KIND_UNION2
    end
    ft isa DataType || return KIND_UNSUPPORTED
    _isvectortype(ft) && return KIND_VECTOR
    if !TRIM_BUILD
        ft <: Tuple && isconcretetype(ft) && return KIND_TUPLE
        ft <: AbstractSet && isconcretetype(ft) && return KIND_SETLIKE
        ft <: AbstractArray && isconcretetype(ft) && !(ft <: AbstractVector) &&
            return KIND_FIXEDARRAY
    end
    if getfield(getfield(ft, :name), :wrapper) === Dict &&
       length(getfield(ft, :parameters)::Core.SimpleVector) == 2
        return TRIM_BUILD ? KIND_UNSUPPORTED : KIND_DICT
    end
    if isstructtype(ft) && !Base.issingletontype(ft) && isconcretetype(ft) &&
       !(ft <: AbstractDict) && !(ft <: AbstractArray) && !(ft <: AbstractSet) &&
       !(ft <: AbstractString) && !(ft <: Tuple) && !(ft <: Function)
        # NamedTuples and (registered) mutables construct through their own
        # tables like any struct
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
        return FieldTable(Nothing, false, FieldSpec[], false, false, nothing, false, false)
    tagkey = fieldtagkey(style)
    snap = @atomic METASTORE.snap
    meta = _rawmeta(snap, T)
    metaok = meta !== nothing
    volatile = false
    valsdefaults = metaok && (meta::RawMeta).valsdependent
    if metaok && !TRIM_BUILD
        # the registered data is only valid while it is still what
        # fieldtags/fielddefaults would answer for this style: if a per-style
        # or more specific overload exists (possibly stateful), ignore the
        # registration and consult the live methods every time
        m = meta::RawMeta
        tm = _whichmeta_style(fieldtags, style, T)
        dm = _whichmeta_style(fielddefaults, style, T)
        metaok = m.tagsmethods isa Vector && m.defaultsmethods isa Vector &&
                 tm in m.tagsmethods && dm in m.defaultsmethods
    end
    local defaults, tags, thunk
    if metaok
        m = meta::RawMeta
        thunk = m.defaultsthunk
        defaults = valsdefaults ? (;) :
                   m.defaults !== nothing ? m.defaults :
                   (!TRIM_BUILD && thunk !== nothing ? _callthunk(thunk) : (;))
        if defaults === nothing
            # the thunk threw: defaults need bound type parameters (e.g.
            # `ntuple(_ -> 0, N)`), so evaluate per-construct via the
            # parameterized 3-arg fielddefaults, like vals-dependent defaults
            valsdefaults = true
            defaults = (;)
        end
        tags = m.tags === nothing ? (;) : m.tags
    elseif TRIM_BUILD
        # trimmed binaries cannot consult per-type fielddefaults/fieldtags
        # methods (open dynamic dispatch); unregistered types parse with
        # reflection only (no defaults, no tag renames)
        defaults = (;)
        tags = (;)
        thunk = nothing
    elseif T <: NamedTuple
        # NamedTuples carry no macro metadata; reflection is complete
        defaults = (;)
        tags = (;)
        thunk = nothing
    else
        # unregistered (or overridden) type under JIT: consult the live
        # per-type metadata methods, and mark the table volatile — it is
        # rebuilt on every make, so stateful or later-redefined
        # fieldtags/fielddefaults overloads keep their exact semantics
        volatile = true
        defaults = fielddefaults(style, T)
        tags = _live_tags(style, T)
        thunk = () -> fielddefaults(style, T)
    end
    isnt = T <: NamedTuple
    names = isnt ? (T.parameters[1]::Tuple) :
            getfield(getfield(T, :name), :names)::Core.SimpleVector
    n = fieldcount(T)
    specs = Vector{FieldSpec}(undef, n)
    ismut = ismutabletype(T)
    eligible = isstructtype(T) && !Base.issingletontype(T) && isconcretetype(T) &&
               n > 0 && !(T <: Tuple) && !(T <: AbstractDict) && !(T <: AbstractArray) &&
               !(T <: AbstractSet) && !(T <: AbstractString) && !(T <: Function) && !(T <: Type)
    # mutable targets must be @noarg-registered: their empty constructor is
    # what applies defaults
    ismut && !metaok && (eligible = false)
    anythunk = false
    for i = 1:n
        fn = names[i]::Symbol
        ftags = _fieldtag_nt(tags, fn, tagkey)
        if ftags === nothing && volatile && tags isa NamedTuple && isempty(tags)
            # public per-field form: fieldtags(style, T, field) — consulted
            # only when the whole-type form answers empty (mirroring
            # _fieldtagtuple); the table is already volatile, so stateful
            # per-field hooks keep their call-per-make semantics
            pf = _live_pertags(style, T, fn)
            pf isa NamedTuple && !isempty(pf) && (ftags = pf)
        end
        ignored = _tagbool(ftags, :ignore)
        nm, aliases = _tagname_aliases(ftags, fn)
        # raw-key matching is a JIT facility; trim builds keep such types out
        aliases isa RawKey && TRIM_BUILD && (eligible = false)
        ft0 = fieldtype(T, i)
        vs = valuespec(snap, style, ft0, ignored, ftags)
        rawdef = _defaultfor(defaults, fn)
        def = rawdef isa NoDefault ? NODEFAULT : classify_default(rawdef)
        # an empty-vector default is only re-materializable when the spec
        # knows the element type; otherwise it needs the thunk
        def === FRESHEMPTY && vs.kind != KIND_VECTOR && (def = THUNKDEFAULT)
        if def === THUNKDEFAULT
            anythunk = true
        elseif !(rawdef isa NoDefault) && def !== FRESHEMPTY
            def = rawdef
        end
        specs[i] = FieldSpec(String(nm), nm, fn, aliases, vs, def,
            ftags isa NamedTuple ? ftags : (;))
    end
    if anythunk
        thunk === nothing && (eligible = false)
    end
    valsdefaults && TRIM_BUILD && (eligible = false)
    return FieldTable(T, ismut, specs, eligible, anythunk && !valsdefaults,
        (anythunk && !valsdefaults) ? thunk : nothing, valsdefaults, volatile)
end

# Build the recursive ValueSpec for one declared field type. Shapes the
# interpreter has no dedicated handling for degrade to KIND_CUSTOM, which
# hands each such value to the generic 4-arg `make` (one dynamic call per
# value) rather than making the whole type ineligible. Trait consultation
# happens only in JIT sessions; trim builds classify from reflection alone.
function valuespec(snap::MetaSnap, style::StructStyle, @nospecialize(ft0), ignored::Bool,
                   @nospecialize(ftags))
    nullable, missingable, ft = peel_nullmissing(ft0)
    k = ignored ? KIND_CUSTOM : kindfor(ft)
    # pair-element vectors are dictlike by the trait ladder (built via
    # addkeyval! push): the retained container machinery owns them
    k == KIND_VECTOR && ft isa Type && ft <: AbstractVector{<:Pair} && (k = KIND_CUSTOM)
    if k == KIND_STRUCT
        fm = _rawmeta(snap, ft)
        fm !== nothing && (fm::RawMeta).nonstruct && (k = KIND_CUSTOM)
        # @nonstruct also emits a structlike=false trait method, which
        # persists in the defining package's image even when the registry
        # mutation from its precompile session does not: honor it live
        # (JIT-only: under juliac the single compile session keeps the
        # registry authoritative)
        !TRIM_BUILD && k == KIND_STRUCT && !structlike(style, ft) && (k = KIND_CUSTOM)
        # types with their own `make` overloads (e.g. JSON's raw-capture
        # JSONText) must stay on the generic arm — driving their fields
        # through the interpreter would silently bypass user semantics
        !TRIM_BUILD && k == KIND_STRUCT && _has_custom_make(ft) && (k = KIND_CUSTOM)
        # style-level trait overrides outrank the structural classification:
        # a struct the style declares dictlike/arraylike belongs to the
        # dispatcher's container arms, not the field-table drive
        !TRIM_BUILD && k == KIND_STRUCT && (dictlike(style, ft) || arraylike(style, ft)) && (k = KIND_CUSTOM)
        # mutable nested structs construct through their own table
        !TRIM_BUILD && ft isa DataType && ismutabletype(ft) && !(fm !== nothing) && (k = KIND_CUSTOM)
    end
    # lift/choosetype/dateformat tags force the generic arm (field level only)
    if k != KIND_UNSUPPORTED && ftags isa NamedTuple &&
       (haskey(ftags, :lift) || haskey(ftags, :choosetype) || haskey(ftags, :dateformat))
        k = KIND_CUSTOM
    end
    keykind = KIND_UNSUPPORTED
    child = nothing
    child2 = nothing
    if k == KIND_VECTOR
        child = valuespec(snap, style, _vector_eltype(ft), false, nothing)
        # nested unsupported elements degrade the vector itself to CUSTOM
        child.kind == KIND_UNSUPPORTED && (k = KIND_CUSTOM; child = nothing)
    elseif k == KIND_DICT
        keykind = scalarkind(_dict_keytype(ft))
        keykind in (KIND_STRING, KIND_SYMBOL, KIND_INT64, KIND_INT32, KIND_INT16,
            KIND_UINT64, KIND_UINT32, KIND_UINT16, KIND_UINT8) || (k = KIND_CUSTOM)
        if k == KIND_DICT
            child = valuespec(snap, style, _dict_valtype(ft), false, nothing)
            child.kind == KIND_UNSUPPORTED && (k = KIND_CUSTOM; child = nothing)
        end
    elseif k == KIND_TUPLE
        members = StructUtils.ValueSpec[valuespec(snap, style, fieldtype(ft, i), false, nothing)
                                        for i = 1:fieldcount(ft)]
        any(m -> m.kind == KIND_UNSUPPORTED, members) ? (k = KIND_CUSTOM) : (child = members)
    elseif k == KIND_SETLIKE || k == KIND_FIXEDARRAY
        if TRIM_BUILD
            # JIT-only kinds: the generic eltype below is not verifier-
            # resolvable; trim keeps sets and fixed-size arrays on the
            # generic arm (their pre-expansion behavior)
            k = KIND_CUSTOM
        else
            elt = eltype(ft)
            child = valuespec(snap, style, elt, false, nothing)
            child.kind == KIND_UNSUPPORTED && (k = KIND_CUSTOM; child = nothing)
        end
    elseif k == KIND_UNION2
        a = getfield(ft, :a)
        b = getfield(ft, :b)
        # source-shape disambiguation only works when exactly ONE member is
        # array-like (arrays and sets, per the trait defaults) — with two
        # array-like members the choice would be ambiguous, so the union
        # degrades to the generic arm (which errors, deliberately)
        arr_a = _shape_arraylike(a)
        arr_b = _shape_arraylike(b)
        if arr_a == arr_b
            k = KIND_CUSTOM
        else
            sa = valuespec(snap, style, a, false, nothing)
            sb = valuespec(snap, style, b, false, nothing)
            child = arr_a ? sa : sb   # the arraylike arm
            child2 = arr_a ? sb : sa  # the scalar arm
        end
    end
    if k == KIND_UNSUPPORTED
        # anything else — Sets, Matrices, exotic containers, abstract leaves,
        # UnionAlls — goes through the generic arm rather than poisoning the
        # containing type. Under trim these fail loudly at parse time.
        k = KIND_CUSTOM
    end
    return ValueSpec(k, nullable, missingable, ft0, ft, keykind, child, child2)
end

_shape_arraylike(@nospecialize(ft)) =
    ft isa DataType && (ft <: AbstractArray || ft <: AbstractSet)

_dict_keytype(@nospecialize(ft)) = (getfield(ft, :parameters)::Core.SimpleVector)[1]
_dict_valtype(@nospecialize(ft)) = (getfield(ft, :parameters)::Core.SimpleVector)[2]

# name tag: Symbol/String rename, or a Tuple of aliases (all become match
# candidates; first is the canonical write name)
function _tagname_aliases(@nospecialize(ftags), fn::Symbol)
    ftags isa NamedTuple || return fn, nothing
    haskey(ftags, :name) || return fn, nothing
    nm = getfield(ftags, :name)
    nm isa Symbol && return nm, nothing
    nm isa String && return Symbol(nm), nothing
    if nm isa Tuple && !isempty(nm)
        strs = String[]
        for x in nm
            if x isa String
                push!(strs, x)
            elseif x isa Symbol
                push!(strs, String(x))
            else
                return fn, RawKey(nm) # exotic entry: generic keyeq match
            end
        end
        syms = Symbol[Symbol(x) for x in strs]
        return syms[1], (strs, syms)
    end
    return fn, RawKey(nm) # exotic name tag: generic keyeq match
end

# wrapper for a name tag the fast matchers can't turn into strings/symbols
# ahead of time; _findspec compares these with `keyeq` at match time (JIT
# only)
struct RawKey
    x::Any
end

# `nothing` on throw: a defaults thunk that cannot evaluate standalone
# (type-parameter-dependent expressions) signals vals-defaults mode, where
# construction consults the parameterized 3-arg fielddefaults instead
_callthunk(@nospecialize(f)) = try
    f()
catch
    nothing
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
    return nothing # exotic name tags (e.g. Tuple aliases) match via _findspec
end

function _defaultfor(@nospecialize(defaults), fn::Symbol)
    defaults isa NamedTuple || return NODEFAULT
    haskey(defaults, fn) || return NODEFAULT
    return getfield(defaults, fn)
end

_live_tags(style::StructStyle, @nospecialize(T)) = try
    fieldtags(style, T)
catch
    (;)
end

# a `make` method more specific than the generic dispatcher exists for T:
# checked once per table build (cached with the table; volatile tables are
# already the deliberate slow path). Queried with the CONCRETE Type{T}
# singleton — a `Type{<:T}` query would intersect every emitted
# `make(::StructStyle, ::Type{S}, ::Any) where S<:Other` method through the
# bottom type (S = Union{} inhabits both bounds)
_has_custom_make(@nospecialize(T)) = try
    generic = which(make, Tuple{StructStyle,Type,Any})
    any(m -> m !== generic, methods(make, Tuple{StructStyle,Type{T},Any}))
catch
    false
end

# Per-type memo of which FIELDS have their own `make` methods, consulted by
# the hot closures. The lookup happens at first use rather than inside the
# generated closure code: generated functions expand against the method
# table as it existed when StructUtils loaded, so they can never see methods
# a user (or another package) defines later. JIT-only; trim builds never
# consult it.
mutable struct _CustomFieldsMemo
    @atomic table::Dict{Type,Any} # UnionAll targets key here too (#54)
end
const _CUSTOM_FIELDS = _CustomFieldsMemo(Dict{Type,Any}())
const _CUSTOM_FIELDS_LOCK = ReentrantLock()

function _custom_make_fields(::Type{T}) where {T}
    tbl = @atomic _CUSTOM_FIELDS.table
    r = get(tbl, T, nothing)
    r === nothing || return r::NTuple{fieldcount(T),Bool}
    v = ntuple(i -> _has_custom_make(fieldtype(T, i)), fieldcount(T))
    lock(_CUSTOM_FIELDS_LOCK)
    try
        old = @atomic _CUSTOM_FIELDS.table
        if !haskey(old, T)
            new = copy(old)
            new[T] = v
            @atomic _CUSTOM_FIELDS.table = new
        end
    finally
        unlock(_CUSTOM_FIELDS_LOCK)
    end
    return v
end

_live_pertags(style::StructStyle, @nospecialize(T), fn::Symbol) = try
    fieldtags(style, T, fn)
catch
    (;)
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
    tbl.volatile && return tbl # live-metadata tables are never cached
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
            @atomic METASTORE.snap = MetaSnap(old.raw, old.roots, tables)
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

const KINDNAMES = ("String", "Int64", "Int32", "Int16", "Int8", "Int128",
    "UInt64", "UInt32", "UInt16", "UInt8", "UInt128", "Float64", "Float32",
    "Float16", "Bool", "Date", "DateTime", "Time", "UUID", "Symbol", "Char",
    "Any", "struct", "vector", "custom")

_kindname(kind::Int8) = 1 <= kind <= length(KINDNAMES) ? KINDNAMES[kind] : "unsupported"

@noinline _liftfail(kind::Int8, name::String) =
    throw(ArgumentError(string("tier-0 interpreter cannot produce a ", _kindname(kind),
        " value for field '", name, "'; annotate the struct `:hot` for the specialized path")))

function _construct_interp(style::StructStyle, tbl::FieldTable, slots::Vector{Any}, @nospecialize(source))
    tbl.mutable && return _construct_mutable(style, tbl, slots, source)
    specs = tbl.specs
    n = length(specs)
    defs = if !TRIM_BUILD && tbl.valsdefaults
        # defaults that reference other parsed fields (or bound type
        # parameters) can only be computed now, against the filled slots
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
            if sp.spec.nullable
                slots[i] = nothing
            elseif sp.spec.missingable
                slots[i] = missing
            else
                _missingfield(sp.name, tbl.T)
            end
        elseif d === FRESHEMPTY
            slots[i] = _alloc_vector((sp.spec.child::ValueSpec).declft, 0)
        elseif d === THUNKDEFAULT
            if TRIM_BUILD
                _missingfield(sp.name, tbl.T)
            else
                d2 = _defaultfor(defs, sp.fieldsym)
                if d2 isa NoDefault
                    if sp.spec.nullable
                        slots[i] = nothing
                    elseif sp.spec.missingable
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

# @noarg mutables: the empty constructor applies defaults (honoring
# per-style `initialize` overloads under JIT), parsed slots overwrite via
# _setfield! (which honors @atomic field declarations). Trim builds
# construct uninitialized and apply the table defaults instead —
# `initialize` overloads are a JIT-only nicety.
function _construct_mutable(style::StructStyle, tbl::FieldTable, slots::Vector{Any}, @nospecialize(source))
    specs = tbl.specs
    n = length(specs)
    local obj
    if TRIM_BUILD
        obj = ccall(:jl_new_struct_uninit, Any, (Any,), tbl.T)
        for i = 1:n
            isassigned(slots, i) && continue
            sp = @inbounds specs[i]
            d = sp.default
            if d === NODEFAULT
                sp.spec.nullable ? _setfield!(obj, i, nothing) :
                sp.spec.missingable ? _setfield!(obj, i, missing) : nothing
            elseif d === FRESHEMPTY
                _setfield!(obj, i, _alloc_vector((sp.spec.child::ValueSpec).declft, 0))
            elseif d !== THUNKDEFAULT
                _setfield!(obj, i, d)
            end
        end
    else
        obj = initialize(style, tbl.T, source)
    end
    for i = 1:n
        isassigned(slots, i) || continue
        _setfield!(obj, i, @inbounds slots[i])
    end
    return obj
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

# the default string lifts for the stdlib date types use these hand-rolled
# parsers everywhere (not just inside the interpreter): the ISO defaults are
# what `Date(::String)` accepts anyway, they are faster than the DateFormat
# machinery, and — decisively — DateFormat's error paths are not
# trim-verifiable, which poisoned every typed date parse under `--trim`.
# Custom formats still route through the `dateformat` fieldtag.
lift(::Type{Dates.Date}, x::AbstractString) = _parse_iso_date(String(x), "Date")
lift(::Type{Dates.DateTime}, x::AbstractString) = _parse_iso_datetime(String(x), "DateTime")
function lift(::Type{Dates.Time}, x::AbstractString)
    h, mi, sec, ms = _parse_iso_timeparts(String(x), 1, "Time")
    return Dates.Time(h, mi, sec, ms)
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
    if k isa Int
        # positional sources (Vector/Tuple elements via applyeach): the
        # element index IS the field index
        return 1 <= k <= length(specs) ? k : 0
    elseif k isa Symbol
        for i = 1:length(specs)
            sp = @inbounds specs[i]
            (sp.namesym === k || sp.fieldsym === k) && return i
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
        return 0
    end
    # rare path: alias tuples and raw name tags register extra candidates
    for i = 1:length(specs)
        al = @inbounds(specs[i]).aliases
        al === nothing && continue
        if al isa RawKey
            !TRIM_BUILD && keyeq(k, al.x) && return i
            continue
        end
        strs, syms = al::Tuple{Vector{String},Vector{Symbol}}
        if k isa Symbol
            for a in syms
                a === k && return i
            end
        elseif k isa String
            for a in strs
                a == k && return i
            end
        end
    end
    return 0
end

function (f::InterpClosure{S})(@nospecialize(k), @nospecialize(v)) where {S}
    specs = f.tbl.specs
    i = _findspec(specs, k)
    i == 0 && return unknownfield(f.style, f.tbl.T, k, v)
    sp = @inbounds specs[i]
    vs = sp.spec
    if v === nothing && vs.kind != KIND_ANY && vs.kind != KIND_CUSTOM
        # a null-like source value fills a field that admits both Missing
        # and Nothing with `missing` (same order @_peel resolves unions)
        if vs.missingable
            f.slots[i] = missing
        elseif vs.nullable
            f.slots[i] = nothing
        elseif TRIM_BUILD
            _liftfail(vs.kind, sp.name)
        else
            x, _ = lift(f.style, vs.ft, nothing)
            f.slots[i] = x
        end
        return defaultstate(f.style)
    elseif v === missing && vs.kind != KIND_ANY && vs.kind != KIND_CUSTOM
        # `missing` fills the Missing arm first, like `nothing` above.
        # Kept as its own branch so each lift call below has a constant
        # third argument — a Union-typed argument would make the lift
        # dispatch ambiguous in trimmed binaries.
        if vs.missingable
            f.slots[i] = missing
        elseif vs.nullable
            f.slots[i] = nothing
        elseif TRIM_BUILD
            _liftfail(vs.kind, sp.name)
        else
            x, _ = lift(f.style, vs.ft, missing)
            f.slots[i] = x
        end
        return defaultstate(f.style)
    end
    f.slots[i] = _interp_value(f.style, sp, v)
    return defaultstate(f.style)
end

# field-level entry: CUSTOM here carries the field's tags; everything else
# recurses through the spec tree
function _interp_value(style::StructStyle, sp::FieldSpec, @nospecialize(v))
    vs = sp.spec
    if vs.kind == KIND_CUSTOM
        if TRIM_BUILD
            _liftfail(vs.kind, sp.name)
        else
            # the 4-arg make with the field's resolved tags, so lift/
            # choosetype/dateformat tags behave exactly as documented
            x, _ = make(style, vs.declft::Type, v, sp.tags)
            return x
        end
    end
    return _spec_value(style, vs, v, sp.name)
end

# position-level recursion over the ValueSpec tree
function _spec_value(style::StructStyle, vs::ValueSpec, @nospecialize(v), name::String)
    k = vs.kind
    if k == KIND_STRUCT
        if TRIM_BUILD
            # direct recursion with a closed nested-source ladder: concrete
            # types keep the applyeach drive statically resolvable (formats
            # with their own tree types route their sources at their own
            # entry, e.g. JSON drives Object roots through `make` directly)
            if v isa Dict{String,Any}
                x, _ = _interp_make(style, vs.ft, v)
                return x
            elseif v isa Dict{Symbol,Any}
                x, _ = _interp_make(style, vs.ft, v)
                return x
            else
                _liftfail(k, name)
            end
        else
            # JIT: route through the dispatcher so per-style trait overloads
            # (dictlike/arraylike/noarg/structlike) keep exact semantics; it
            # re-enters the interpreter through the gate when appropriate
            x, _ = make(style, vs.ft::Type, v)
            return x
        end
    elseif k == KIND_VECTOR
        return _spec_vector(style, vs, v, name)
    elseif k == KIND_DICT
        # JIT-only (kindfor never yields KIND_DICT in trim builds, but the
        # arm must be compile-time dead or its dynamic innards stay
        # verifier-reachable)
        if TRIM_BUILD
            _liftfail(k, name)
        else
            return _spec_dict(style, vs, v, name)
        end
    elseif k == KIND_UNION2
        # 2-union: an array-like source picks the array-like member
        arm = (v isa AbstractArray || v isa AbstractSet) ? (vs.child::ValueSpec) :
                                                           (vs.child2::ValueSpec)
        return _spec_nullwrap(style, arm, v, name)
    elseif k == KIND_TUPLE
        if TRIM_BUILD
            _liftfail(k, name)
        else
            return _spec_tuple(style, vs, v, name)
        end
    elseif k == KIND_SETLIKE
        if TRIM_BUILD
            _liftfail(k, name)
        else
            set = initialize(style, vs.ft, v)
            el = vs.child::ValueSpec
            if v isa AbstractVector || v isa AbstractSet
                for ev in v
                    push!(set, _spec_nullwrap(style, el, ev, name))
                end
                return set
            end
            TRIM_BUILD && _liftfail(vs.kind, name)
            x, _ = makearray(style, vs.declft, v)
            return x
        end
    elseif k == KIND_FIXEDARRAY
        if TRIM_BUILD
            _liftfail(k, name)
        else
            return _spec_fixedarray(style, vs, v, name)
        end
    elseif k == KIND_ANY
        return v
    elseif k == KIND_CUSTOM
        if TRIM_BUILD
            _liftfail(k, name)
        else
            # element/value position: hand the value to the tagless
            # 3-arg make
            x, _ = make(style, vs.declft::Type, v)
            return x
        end
    else
        return _liftleaf(style, k, vs.ft, v, name)
    end
end

# nested positions can themselves be nullable (e.g. Vector{Union{T,Nothing}}
# elements, union arms)
function _spec_nullwrap(style::StructStyle, vs::ValueSpec, @nospecialize(v), name::String)
    if v === nothing && vs.kind != KIND_ANY && vs.kind != KIND_CUSTOM
        # null-like values fill the Missing arm first when both are admitted
        # (the same order @_peel resolves unions); two branches so each lift
        # call has a constant third argument (see the closure above)
        vs.missingable && return missing
        vs.nullable && return nothing
        TRIM_BUILD && _liftfail(vs.kind, name)
        x, _ = lift(style, vs.ft, nothing)
        return x
    elseif v === missing && vs.kind != KIND_ANY && vs.kind != KIND_CUSTOM
        vs.missingable && return missing
        vs.nullable && return nothing
        TRIM_BUILD && _liftfail(vs.kind, name)
        x, _ = lift(style, vs.ft, missing)
        return x
    end
    return _spec_value(style, vs, v, name)
end

function _spec_vector(style::StructStyle, vs::ValueSpec, @nospecialize(v), name::String)
    el = vs.child::ValueSpec
    if v isa Vector{Any}
        n = length(v)
        arr = _alloc_vector(el.declft, n)
        for j = 1:n
            _setvec!(arr, j, _spec_nullwrap(style, el, @inbounds(v[j]), name))
        end
        return arr
    elseif !TRIM_BUILD && v isa AbstractVector && !(v isa AbstractVector{<:Pair})
        # generic source vectors (e.g. a Vector{Int} value inside a user Dict);
        # pair-element vectors are keyed sources and fall to makearray below
        n = length(v)
        arr = _alloc_vector(el.declft, n)
        j = 1
        for ev in v
            _setvec!(arr, j, _spec_nullwrap(style, el, ev, name))
            j += 1
        end
        return arr
    end
    if TRIM_BUILD
        _liftfail(vs.kind, name)
    else
        # non-vector source: the retained container machinery owns it
        # (calling `make` would re-enter _interp_root on the same spec);
        # trim fails loudly on shapes its folded path can't drive
        TRIM_BUILD && _liftfail(vs.kind, name)
        x, _ = makearray(style, vs.declft, v)
        return x
    end
end

# Dict-typed fields (JIT only; kindfor never yields KIND_DICT under trim):
# `initialize` honors per-style sizehints/overloads, keys lift per keykind,
# values recurse through the child spec
function _spec_dict(style::StructStyle, vs::ValueSpec, @nospecialize(v), name::String)
    el = vs.child::ValueSpec
    dict = initialize(style, vs.ft, v)
    if v isa AbstractDict
        for (dk, dv) in v
            addkeyval!(dict, _liftdictkey(style, vs, dk, name), _spec_nullwrap(style, el, dv, name))
        end
    elseif v isa AbstractVector{<:Pair}
        for (dk, dv) in v
            addkeyval!(dict, _liftdictkey(style, vs, dk, name), _spec_nullwrap(style, el, dv, name))
        end
    else
        TRIM_BUILD && _liftfail(vs.kind, name)
        x, _ = makedict(style, vs.declft, v)
        return x
    end
    return dict
end

# tuple targets: positional fill — array sources by index, keyed/iterable
# sources in encounter order; members lift through their own specs, and
# construction uses the same boxed-slot ccall as structs (tuple types are
# struct-shaped for jl_new_structv)
function _spec_tuple(style::StructStyle, vs::ValueSpec, @nospecialize(v), name::String)
    members = vs.child::Vector{ValueSpec}
    n = length(members)
    slots = Vector{Any}(undef, n)
    i = 0
    if v isa AbstractDict
        for (_, ev) in v
            i >= n && break
            i += 1
            slots[i] = _spec_nullwrap(style, @inbounds(members[i]), ev, name)
        end
    elseif (v isa AbstractVector && !(v isa AbstractVector{<:Pair})) || v isa Tuple
        k = 0
        for ev in v
            k += 1
            if i >= n
                # surplus positional elements go to the style's unknownfield
                # hook (which ignores them by default)
                unknownfield(style, vs.ft::DataType, k, ev)
                continue
            end
            i += 1
            slots[i] = _spec_nullwrap(style, @inbounds(members[i]), ev, name)
        end
    else
        # exotic source: the retained container machinery owns it (calling
        # `make` here would re-enter _interp_root on the same spec — a loop);
        # trim fails loudly — its folded container path can't drive this shape
        TRIM_BUILD && _liftfail(vs.kind, name)
        x, _ = maketuple(style, vs.declft, v)
        return x
    end
    i == n || _missingfield(name, vs.ft::DataType)
    GC.@preserve slots begin
        return ccall(:jl_new_structv, Any, (Any, Ptr{Any}, UInt32), vs.ft, pointer(slots), n % UInt32)
    end
end

# multidim / fixed-size arrays from nested source vectors: discover the
# dimensions from nesting depth and lengths (innermost length is dim 1),
# flatten in column-major order, and build through the style's
# arrayfromdata hook (which the StaticArrays extension also implements)
function _spec_fixedarray(style::StructStyle, vs::ValueSpec, @nospecialize(v), name::String)
    v isa AbstractVector || begin
        TRIM_BUILD && _liftfail(vs.kind, name)
        x, _ = makearray(style, vs.declft, v)
        return x
    end
    dims = Int[]
    probe = v
    while probe isa AbstractVector
        pushfirst!(dims, length(probe))
        probe = isempty(probe) ? nothing : probe[1]
    end
    el = vs.child::ValueSpec
    data = Vector{Any}()
    _flatten_colmajor!(data, v, length(dims))
    lifted = _alloc_vector(el.declft, length(data))
    for j = 1:length(data)
        _setvec!(lifted, j, _spec_nullwrap(style, el, @inbounds(data[j]), name))
    end
    return arrayfromdata(vs.ft, lifted, Tuple(dims))
end

function _flatten_colmajor!(data::Vector{Any}, @nospecialize(v), depth::Int)
    if depth <= 0 || !(v isa AbstractVector)
        push!(data, v)
        return nothing
    end
    for ev in v
        _flatten_colmajor!(data, ev, depth - 1)
    end
    return nothing
end

function _liftdictkey(style::StructStyle, vs::ValueSpec, @nospecialize(dk), name::String)
    kk = vs.keykind
    if kk == KIND_STRING
        dk isa String && return dk
        dk isa Symbol && return String(dk)
    elseif kk == KIND_SYMBOL
        dk isa Symbol && return dk
        dk isa String && return Symbol(dk)
    elseif dk isa String && (kk == KIND_INT64 || kk == KIND_INT32 || kk == KIND_INT16 ||
           kk == KIND_UINT64 || kk == KIND_UINT32 || kk == KIND_UINT16 || kk == KIND_UINT8)
        return _liftleaf(style, kk, _dict_keytype(vs.ft), Base.parse(Int64, dk), name)
    end
    # odd pairings: the style's liftkey hook decides (JIT)
    return liftkey(style, _dict_keytype(vs.ft), dk)
end

# true when `source` is a tree shape the interpreter drives directly; under
# trim only AbstractDict (the Vector{Pair} applyeach dispatch is ambiguous
# between the Pair-vector and AbstractArray methods for the verifier)
"""
    StructUtils.interpsource(source) -> Bool

`true` (the default) when `source` may be driven through the tier-0
interpreter. Formats with lazy source types that carry their own `lift`
protocols (e.g. JSON's `LazyValue`) overload this to `false`, keeping those
sources on their format-owned descent.
"""
interpsource(@nospecialize(source)) = true

# JIT: the interpreter drives any source applyeach understands unless the
# source type opts out (trees get concrete fast arms in _interp_make;
# everything else takes the dynamic applyeach arm). Trim keeps the
# concrete-dict contract; non-tree sources route to the hot descent at the
# dispatcher.
_interpsource(@nospecialize(source)) =
    TRIM_BUILD ? source isa AbstractDict : interpsource(source)

# entry: returns (value, state) like every `make`. Both the target type and
# the source are inference-erased (one instance per style, ever — the
# engine's whole point); the drive ladder gives the trim verifier concrete
# applyeach targets, with a dynamic-dispatch arm for arbitrary tree shapes
# under JIT (specializing on source here invites invalidation-recompile
# blowups when extensions load mid-session)
Base.@nospecializeinfer function _interp_make(style::StructStyle, @nospecialize(T::Type), @nospecialize(source))
    # Targets the interpreter can't table — incomplete parametric types,
    # mutables without an @noarg registration, defaults it can't re-evaluate
    # — take the per-type hot path instead (calling back into `make` here
    # would loop). The Tuple assert keeps this function's return type
    # concrete enough for callers to destructure statically. Trim builds
    # fail loudly: the hot path with a runtime-only type isn't compiled.
    if !(T isa DataType && (tbl = fieldtable(T, style)).eligible)
        TRIM_BUILD && _liftfail(KIND_STRUCT, string(T))
        return _hot_make3(style, T, source)::Tuple{Any,Any}
    end
    return _interp_make(style, tbl, source)
end

# table-based core: the drive + construction (one instance per style)
Base.@nospecializeinfer function _interp_make(style::StructStyle, tbl::FieldTable, @nospecialize(source))
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
    # EarlyReturn from a custom unknownfield flows through
    # as state; construction still happens
    return _construct_interp(style, tbl, slots, source), st
end

# root ValueSpec for a non-struct target (Vector, Dict, Set, Tuple, Matrix,
# or unions thereof): built once per (target, style type), same store
function rootspec(@nospecialize(T::Type), style::StructStyle)
    styletype = typeof(style)
    snap = @atomic METASTORE.snap
    entry = get(snap.roots, T, nothing)
    if entry !== nothing
        for (st, vs) in entry
            st === styletype && return vs::ValueSpec
        end
    end
    vs = valuespec(snap, style, T, false, nothing)
    lock(META_LOCK)
    try
        old = @atomic METASTORE.snap
        roots = copy(old.roots)
        v = get(roots, T, nothing)
        v = v === nothing ? Vector{Pair{DataType,Any}}() : copy(v)
        any(p -> first(p) === styletype, v) || push!(v, styletype => vs)
        roots[T] = v
        @atomic METASTORE.snap = MetaSnap(old.raw, roots, old.tables)
    finally
        unlock(META_LOCK)
    end
    return vs
end

# entry for non-struct targets: run the spec tree over the source and wrap
# in the (value, state) contract. The un-specialized arguments keep the
# source check below a runtime decision: if the compiler could prove its
# answer at a call site it would delete this arm, and compiling the
# mutually recursive make functions with arms deleted has hung the
# compiler outright
Base.@nospecializeinfer function _interp_root(style::StructStyle, @nospecialize(T::Type), @nospecialize(source))
    # lazy/positional format sources carry their own (value, pos) state
    # contract: the retained container machinery owns them
    _interpsource(source) || return nothing
    vs = rootspec(T, style)
    if vs.kind == KIND_CUSTOM || vs.kind == KIND_UNSUPPORTED
        # a target the spec tree can't describe (custom AbstractDict
        # subtypes, exotic arraylikes): decline — the dispatcher's container
        # arms own it
        return nothing
    end
    return _spec_nullwrap(style, vs, source, "root"), defaultstate(style)
end

# gate consulted from `make`'s structlike arm under JIT: returns the built
# field table when the target is eligible and the source tree-shaped, else
# nothing. Returning the table (rather than a Bool) lets the dispatcher hand
# it straight to the interpreter — volatile tables rebuild on every
# `fieldtable` call, and a second build would double-fire stateful
# fieldtags/fielddefaults hooks
function _interptable(style::StructStyle, @nospecialize(T::Type), @nospecialize(source))
    T isa DataType || return nothing
    _interpsource(source) || return nothing
    tbl = fieldtable(T, style)
    return tbl.eligible ? tbl : nothing
end

"""
    StructUtils.interpready(style, T) -> Bool

`true` when the tier-0 interpreter has an eligible field table for `T` under
`style` — i.e. a tree-shaped `make` will construct through the interpreter.
"""
interpready(style::StructStyle, @nospecialize(T::Type)) =
    T isa DataType && fieldtable(T, style).eligible

