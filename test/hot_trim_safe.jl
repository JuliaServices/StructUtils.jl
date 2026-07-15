# :hot tier trim workload: compiled with `juliac --trim=safe` in the plain
# (no-preference) env — the emitted specialized descent is statically
# resolvable on its own for non-tree sources (NamedTuples here; format lazy
# values in practice).
#
# Field/value shapes are limited to the cross-product-proven set (Int,
# String, NamedTuple, Vector): the hot findfield keeps every (field ×
# value-type) arm alive for runtime-keyed sources, so scalar types whose
# wrong-type lifts have interior machinery (dates) belong to format-side
# workloads where the source value type is uniform. Tree-shaped sources are
# exercised by interp_trim_safe.jl under the trim_build preference instead.
using StructUtils

@kwarg :hot struct HTTier
    name::String
    amount::Int = 0
    currency::String = "usd"
end

@kwarg :hot struct HTEvent
    name::String
    cap::Union{Int,Nothing} = nothing
    venue::Union{HTTier,Nothing} = nothing
    tiers::Vector{HTTier} = HTTier[]
end

function run_hot_trim_sample()
    ev = StructUtils.make(HTEvent, (name="e", cap=4,
        venue=(name="v", amount=1), tiers=[(name="a", amount=0), (name="b", amount=5)]))
    ev isa HTEvent || error("type")
    e = ev::HTEvent
    e.name == "e" || error("name")
    e.cap == 4 || error("cap")
    v = e.venue
    v isa HTTier || error("venue")
    (v::HTTier).amount == 1 || error("venue amount")
    length(e.tiers) == 2 || error("tiers")
    e.tiers[1].currency == "usd" || error("cur default")
    e.tiers[2].amount == 5 || error("amount")
    ev2 = StructUtils.make(HTEvent, (name="x",))
    (ev2::HTEvent).cap === nothing || error("cap nothing")
    isempty((ev2::HTEvent).tiers) || error("tiers default")
    return nothing
end

function @main(args::Vector{String})::Cint
    _ = args
    run_hot_trim_sample()
    Core.println("HOT_TRIM_OK")
    return 0
end

Base.Experimental.entrypoint(main, (Vector{String},))
