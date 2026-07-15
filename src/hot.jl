# The :hot tier: a fully-specialized `make` family, reachable only through
# methods emitted by the struct macros' `:hot` option (or the standalone
# `@hot`). Every dispatcher here carries `::Type{T} where T` — the forced
# per-type specialization PR #62 gated globally behind a preference — but
# scoped to annotated types, so nobody else pays the compile time. The
# emitted entry tree-gates first: tree-shaped sources always take the tier-0
# interpreter (faster at that size, and interpretable in trimmed binaries),
# while the hot descent owns non-tree sources (format lazy values,
# NamedTuples, structs) with a statically-resolvable call graph — no
# preference required for `juliac --trim`.

"""
    StructUtils.ishot(T) -> Bool

`true` when `T` was annotated `:hot` (e.g. `@kwarg :hot struct ...`) or via
`StructUtils.@hot`. Hot types get fully-specialized `make` methods compiled
at package-precompile time through the hot-hook registry.
"""
ishot(@nospecialize(T)) = false

"""
    StructUtils.interpready(style, T) -> Bool

`true` when the tier-0 interpreter has an eligible field table for `T` under
`style` — i.e. a tree-shaped `make` will construct through the interpreter.
"""
interpready(style::StructStyle, @nospecialize(T::Type)) =
    T isa DataType && fieldtable(T, style).eligible

# ---------------- hot-hook registry ----------------

const HOT_HOOKS = Any[]

"""
    StructUtils.register_hot_hook!(f)

Register a format-side precompile hook. During downstream package
precompilation, each `:hot`-annotated struct definition calls
`f(T, samples::Tuple)` inside a newly-inferred-tagging block, so everything
the hook compiles (e.g. a typed JSON parse of `T`) is cached in the defining
package's image. Register from `__init__` (the registry is runtime state).
Hooks must swallow their own errors.
"""
register_hot_hook!(@nospecialize(f)) = (push!(HOT_HOOKS, f); nothing)

function _hot_precompile!(@nospecialize(T::Type), samples::Tuple=(); force::Bool=false)
    if force
        # test path: run the hooks without precompile tagging
        _hot_workload(T, samples)
        return nothing
    end
    # workloads exist to seed pkgimages; in a `juliac --trim` compile session
    # reachable-code compilation is juliac's job, and executing the workload
    # would bake JIT-only instances into the image for the verifier to reject.
    # The driver session's own JLOptions may not carry the trim flag, so we
    # also honor the ecosystem env convention juliac builds already use.
    Base.JLOptions().trim != 0 && return nothing
    get(ENV, "JULIAC_DISABLE_PRECOMPILE_WORKLOADS", "0") == "1" && return nothing
    Base.generating_output() || return nothing
    # the workload wrapper tags newly-inferred instances so the *downstream*
    # package's pkgimage retains them (bare execution during precompile
    # drops external method instances)
    @compile_workload begin
        _hot_workload(T, samples)
    end
    return nothing
end

function _hot_workload(@nospecialize(T::Type), samples::Tuple)
    try
        # baseline: compile the tree descent; a required-field error is
        # fine — matching and construction compile before the throw
        make(T, Dict{String,Any}())
    catch
    end
    for h in HOT_HOOKS
        try
            h(T, samples)
        catch
        end
    end
    return nothing
end

# ---------------- specialized dispatchers (per-T `where T` throughout) ----------------

# 2-component-union disambiguation with literal component types (a runtime
# Base.uniontypes loop makes the recursive calls dynamic)
@generated function _hot_unionmake(style::StructStyle, ::Type{T}, source, tags) where {T}
    types = Base.uniontypes(T)
    length(types) == 2 || return :(return nothing)
    A, B = types
    return quote
        a_arr = arraylike(style, $A)
        b_arr = arraylike(style, $B)
        if a_arr && !b_arr
            return arraylike(style, source) ? _hot_field(style, $A, source, tags) : _hot_field(style, $B, source, tags)
        elseif b_arr && !a_arr
            return arraylike(style, source) ? _hot_field(style, $B, source, tags) : _hot_field(style, $A, source, tags)
        end
        return nothing
    end
end

@generated function _hot_unionmake(style::StructStyle, ::Type{T}, source) where {T}
    types = Base.uniontypes(T)
    length(types) == 2 || return :(return nothing)
    A, B = types
    return quote
        a_arr = arraylike(style, $A)
        b_arr = arraylike(style, $B)
        if a_arr && !b_arr
            return arraylike(style, source) ? _hot_make3(style, $A, source) : _hot_make3(style, $B, source)
        elseif b_arr && !a_arr
            return arraylike(style, source) ? _hot_make3(style, $B, source) : _hot_make3(style, $A, source)
        end
        return nothing
    end
end

# field-level entry: the 4-arg `make` semantics with a specializing signature
function _hot_field(style::StructStyle, ::Type{T}, source, tags) where {T}
    if haskey(tags, :choosetype)
        # runtime-chosen types re-enter the generic machinery
        return make(style, tags.choosetype(source), source, _delete(tags, :choosetype))
    end
    if T !== Any
        if T >: Missing && T !== Missing
            if nulllike(style, source)
                return _hot_field(style, Missing, source, tags)
            else
                return _hot_field(style, nonmissingtype(T), source, tags)
            end
        elseif T >: Nothing && T !== Nothing
            if nulllike(style, source)
                return _hot_field(style, Nothing, source, tags)
            else
                return _hot_field(style, Base.nonnothingtype(T), source, tags)
            end
        end
        if T isa Union
            r = _hot_unionmake(style, T, source, tags)
            r !== nothing && return r
        end
    end
    if T <: Tuple || dictlike(style, T) || arraylike(style, T) || noarg(style, T) || structlike(style, T)
        return _hot_make3(style, T, source)
    else
        return lift(style, T, source, tags)
    end
end

function _hot_make3(style::StructStyle, ::Type{T}, source) where {T}
    if abstractcollectionpassthrough(style, T, source)
        return source, defaultstate(style)
    end
    if T !== Any
        if T >: Missing && T !== Missing
            if nulllike(style, source)
                return _hot_make3(style, Missing, source)
            else
                return _hot_make3(style, nonmissingtype(T), source)
            end
        elseif T >: Nothing && T !== Nothing
            if nulllike(style, source)
                return _hot_make3(style, Nothing, source)
            else
                return _hot_make3(style, Base.nonnothingtype(T), source)
            end
        end
        if T isa Union
            r = _hot_unionmake(style, T, source)
            r !== nothing && return r
        end
    end
    if T <: Tuple
        # tuple targets reuse the existing generated machinery
        return maketuple(style, T, source)
    elseif dictlike(style, T)
        return _hot_makedict(style, T, source)
    elseif arraylike(style, T)
        if fixedsizearray(style, T)
            # multidim/fixed-size arrays reuse the existing machinery
            return makearray(style, T, source)
        end
        return _hot_makearray(style, initialize(style, T, source), source)
    elseif noarg(style, T)
        return _hot_makenoarg(style, initialize(style, T, source), source)
    elseif structlike(style, T)
        return _hot_makestruct(style, T, source)
    else
        return lift(style, T, source)
    end
end

# ---------------- specialized closures ----------------

struct HotStructClosure{T,A,S,FS,FSS,FT}
    vals::A # Memory{Any} for structs, the instance for noarg mutables
    style::S
    fsyms::FS
    fstrs::FSS
    ftags::FT
end

HotStructClosure{T}(vals::A, style::S, fsyms::FS, fstrs::FSS) where {T,A,S,FS,FSS} =
    (ftags = _fieldtagtuple(style, T, fsyms);
     HotStructClosure{T,A,S,FS,FSS,typeof(ftags)}(vals, style, fsyms, fstrs, ftags))

# literal field indices and field types: the runtime-`i` closure form makes
# `fieldtype(T, i)` abstract, funneling every recursive call through the
# single generic instance — unresolvable under --trim (PR #62's findfield)
@generated function _hot_findfield(::Type{T}, k, v, f) where {T}
    ex = Expr(:block)
    push!(ex.args, :(Base.@_inline_meta))
    for i = 1:fieldcount(T)
        ft = fieldtype(T, i)
        push!(ex.args, quote
            if typeof(k) == Symbol
                let fn = f.fsyms[$i], ftags = f.ftags[$i]
                    field = get(ftags, :name, fn)
                    if keyeq(k, field) || keyeq(k, fn)
                        symval, symst = _hot_field(f.style, $ft, v, ftags)
                        setval!(f.vals, symval, $i)
                        return symst
                    end
                end
            elseif typeof(k) == Int
                if k == $i
                    let ftags = f.ftags[$i]
                        intval, intst = _hot_field(f.style, $ft, v, ftags)
                        setval!(f.vals, intval, $i)
                        return intst
                    end
                end
            else
                let fstr = f.fstrs[$i], ftags = f.ftags[$i]
                    field = get(ftags, :name, fstr)
                    if keyeq(k, field)
                        strval, strst = _hot_field(f.style, $ft, v, ftags)
                        setval!(f.vals, strval, $i)
                        return strst
                    end
                end
            end
        end)
    end
    push!(ex.args, :(return unknownfield(f.style, T, k, v)))
    return ex
end

(f::HotStructClosure{T,A,S,FS,FSS,FT})(k, v) where {T,A,S,FS,FSS,FT} = _hot_findfield(T, k, v, f)

struct HotArrayClosure{T,S}
    arr::T
    style::S
end

function (f::HotArrayClosure{T,S})(_, v) where {T,S}
    val, st = _hot_make3(f.style, eltype(f.arr), v)::NTuple{2,Any}
    push!(f.arr, val)
    return st
end

struct HotDictClosure{T,S}
    dict::T
    style::S
end

function (f::HotDictClosure{T,S})(k, v) where {T,S}
    val, st = _hot_make3(f.style, _valtype(f.dict), v)::NTuple{2,Any}
    addkeyval!(f.dict, liftkey(f.style, _keytype(f.dict), k), val)
    return st
end

function _hot_makestruct(style::StructStyle, ::Type{T}, source) where {T}
    vals = mem(fieldcount(T))
    fsyms = fieldnamesymbols(T)
    fstrs = fieldnamestrings(T)
    st = applyeach(style, HotStructClosure{T}(vals, style, fsyms, fstrs), source)
    if T <: NamedTuple
        return T(_tuple(T, vals, style)), st
    else
        return _construct(T, vals, style, fsyms), st
    end
end

function _hot_makenoarg(style::StructStyle, y::T, source) where {T}
    fsyms = fieldnamesymbols(T)
    fstrs = fieldnamestrings(T)
    st = applyeach(style, HotStructClosure{T}(y, style, fsyms, fstrs), source)
    return y, st
end

function _hot_makearray(style::StructStyle, x::T, source) where {T}
    st = applyeach(style, HotArrayClosure(x, style), source)
    return x, st
end

_hot_makedict(style::StructStyle, ::Type{T}, source) where {T} =
    _hot_makedict(style, initialize(style, T, source), source)

function _hot_makedict(style::StructStyle, dict::T, source) where {T}
    st = applyeach(style, HotDictClosure(dict, style), source)
    return dict, st
end

# entries targeted by the macro-emitted per-type `make` methods: eligible
# trees take the interpreter (fast at that size, trim-clean via its own
# story); the hot descent owns everything else with a static graph. The
# eligibility check must live HERE — the interpreter's ineligible fallback
# re-enters `make`, which would redispatch to the hot method forever.
function _hot_entry(style::StructStyle, ::Type{T}, source) where {T}
    if _interpsource(source) && interpready(style, T)
        return _interp_make(style, T, source)
    end
    return _hot_make3(style, T, source)
end

function _hot_entry(style::StructStyle, ::Type{T}, source, tags) where {T}
    if haskey(tags, :choosetype)
        return make(style, tags.choosetype(source), source, _delete(tags, :choosetype))
    end
    if _interpsource(source) && interpready(style, T)
        return _interp_make(style, T, source)
    end
    return _hot_make3(style, T, source)
end

"""
    StructUtils.@hot T
    StructUtils.@hot T "sample" ...

Standalone `:hot` annotation for an existing struct type (including types
you don't own — vendored API types being the typical case). Equivalent to
defining the struct with `@kwarg :hot struct ...`: emits fully-specialized
`make` methods for `T` and triggers precompile-time compilation through the
hot-hook registry. Optional sample strings are handed to format hooks (e.g.
JSON parses each sample against `T` during precompilation).
"""
macro hot(T, samples...)
    esc(quote
        StructUtils.ishot(::Type{<:$T}) = true
        function StructUtils.make(style::StructUtils.StructStyle, ::Type{S}, source) where {S<:$T}
            StructUtils._hot_entry(style, S, source)
        end
        function StructUtils.make(style::StructUtils.StructStyle, ::Type{S}, source, tags) where {S<:$T}
            StructUtils._hot_entry(style, S, source, tags)
        end
        # reflection-only field table so eligible tree sources still take the
        # interpreter (types defined via the struct macros register richer
        # metadata at their own definition site)
        StructUtils.register_fieldtable!($T)
        StructUtils._hot_precompile!($T, ($(samples...),))
        $T
    end)
end

"""
    StructUtils.interptreesafe(style, T) -> Bool

`true` when `T`'s field table — including nested struct and vector-element
tables — contains no choosetype-tagged fields or abstract CUSTOM leaves.
Those receive the raw source value in user functions, so formats that
materialize an alternate tree representation for the interpreter (e.g. JSON
parsing to `Object` instead of handing out lazy values) must keep such types
on their classic path.
"""
interptreesafe(style::StructStyle, @nospecialize(T::Type)) =
    _treesafe(style, T, Base.IdSet{Any}())

function _treesafe(style::StructStyle, @nospecialize(T), seen::Base.IdSet{Any})::Bool
    T in seen && return true
    push!(seen, T)
    T isa DataType || return true
    tbl = fieldtable(T, style)
    tbl.eligible || return true # routed classic anyway
    tbl.treesafe || return false
    for sp in tbl.specs
        if sp.kind == KIND_STRUCT
            _treesafe(style, sp.ft, seen) || return false
        elseif sp.kind == KIND_VECTOR && sp.elkind == KIND_STRUCT
            _treesafe(style, sp.elft, seen) || return false
        end
    end
    return true
end
