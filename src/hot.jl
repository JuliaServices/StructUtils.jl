# The :hot tier: a fully-specialized `make` family, reachable only through
# methods emitted by the struct macros' `:hot` option (or the standalone
# `@hot`). Every dispatcher here carries `::Type{T} where T` — the forced
# per-type specialization PR #62 gated globally behind a preference — but
# scoped to annotated types, so nobody else pays the compile time. The
# emitted entry tree-gates first: eligible tree-shaped sources take the
# tier-0 interpreter (faster at request sizes, and interpretable in trimmed
# binaries), while the hot descent owns everything else (format lazy values,
# NamedTuples, structs) with a statically-resolvable call graph — no
# preference required for `juliac --trim`. Under `trim_build` the make
# dispatcher also routes *unannotated* non-tree structlike targets here; see
# the architecture note at the top of StructUtils.jl.

"""
    StructUtils.ishot(T) -> Bool

`true` when `T` was annotated `:hot` (e.g. `@kwarg :hot struct ...`) or via
`StructUtils.@hot`. Hot types get fully-specialized `make` methods compiled
at package-precompile time through the hot-hook registry.
"""
ishot(@nospecialize(T)) = false


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


# field-level entry: the 4-arg `make` semantics with a specializing signature
function _hot_field(style::StructStyle, ::Type{T}, source, tags) where {T}
    if haskey(tags, :choosetype)
        # runtime-chosen types re-enter the generic machinery
        return make(style, tags.choosetype(source), source, _delete(tags, :choosetype))
    end
    @_peel _hot_field tags
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
    @_peel _hot_make3
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

struct HotStructClosure{T,A,S,FS,FSS,FT,CMF}
    vals::A # Memory{Any} for structs, the instance for noarg mutables
    style::S
    fsyms::FS
    fstrs::FSS
    ftags::FT
    cmf::CMF # per-field: the type carries its own make methods (JIT only)
end

HotStructClosure{T}(vals::A, style::S, fsyms::FS, fstrs::FSS) where {T,A,S,FS,FSS} =
    (ftags = _fieldtagtuple(style, T, fsyms);
     cmf = TRIM_BUILD ? nothing : _custom_make_fields(T);
     HotStructClosure{T,A,S,FS,FSS,typeof(ftags),typeof(cmf)}(vals, style, fsyms, fstrs, ftags, cmf))

# literal field indices and field types: the runtime-`i` closure form makes
# `fieldtype(T, i)` abstract, funneling every recursive call through the
# single generic instance — unresolvable under --trim (PR #62's lesson).
# Under JIT, fields whose type carries its OWN `make` methods (raw-capture
# types like JSON's JSONText, @choosetype-emitted union overrides) dispatch
# through the 4-arg `make` so those hooks fire before any union peel — the
# classic closures' behavior. The verdict lives on the closure (f.cmf,
# memoized per type in the runtime world — a generator's frozen world could
# never see late-defined hooks). Trim builds emit the plain hot field: the
# runtime methods() reflection behind the verdict is not verifier-safe.
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
                        symval, symst = TRIM_BUILD ? _hot_field(f.style, $ft, v, ftags) :
                            (f.cmf[$i] ? _make_override(f.style, $ft, v, ftags) :
                                         _hot_field(f.style, $ft, v, ftags))
                        setval!(f.vals, symval, $i)
                        return symst
                    end
                end
            elseif typeof(k) == Int
                if k == $i
                    let ftags = f.ftags[$i]
                        intval, intst = TRIM_BUILD ? _hot_field(f.style, $ft, v, ftags) :
                            (f.cmf[$i] ? _make_override(f.style, $ft, v, ftags) :
                                         _hot_field(f.style, $ft, v, ftags))
                        setval!(f.vals, intval, $i)
                        return intst
                    end
                end
            else
                let fstr = f.fstrs[$i], ftags = f.ftags[$i]
                    field = get(ftags, :name, fstr)
                    if keyeq(k, field)
                        strval, strst = TRIM_BUILD ? _hot_field(f.style, $ft, v, ftags) :
                            (f.cmf[$i] ? _make_override(f.style, $ft, v, ftags) :
                                         _hot_field(f.style, $ft, v, ftags))
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

# inference barrier for the custom-make field arm: called with a CONST field
# type from the generated closure, a raw `make` invocation invites the
# constprop/irinterp spiral through the recursive make family (the
# interpsource lesson); the widened shared instance dispatches at runtime,
# where the user's override wins by ordinary specificity
Base.@nospecializeinfer @noinline _make_override(style::StructStyle, @nospecialize(T::Type), @nospecialize(source), @nospecialize(tags)) =
    make(style, T, source, tags)

(f::HotStructClosure{T,A,S,FS,FSS,FT,CMF})(k, v) where {T,A,S,FS,FSS,FT,CMF} = _hot_findfield(T, k, v, f)

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
    ex = Expr(:block,
        # reflection-only field table so eligible tree sources still take the
        # interpreter (macro-defined types register richer metadata at their
        # own definition site)
        :(StructUtils.register_fieldtable!($T)),
        _hot_exprs(T, samples...)...,
        T)
    return esc(ex)
end

