# tier-0 interpreter trim workload: compiled by trim_compile_tests.jl with
# `juliac --trim=safe` in an env whose LocalPreferences sets
# [StructUtils] trim_build = true. Exercises the closed kind universe,
# registered metadata (macro-emitted), reflection-only tables, defaults
# policies, and error paths — all through the non-specializing interpreter.
using StructUtils, Dates, UUIDs

@kwarg struct TTier
    name::String
    amount::Int = 0
    currency::String = "usd"
end

@kwarg struct TLoc
    label::String = ""
    lat::Union{Float64,Nothing} = nothing
    tags::Vector{Symbol} = Symbol[]
end

@kwarg struct TEvent
    name::String
    day::Date = Date(0)
    at::Union{DateTime,Nothing} = nothing
    uid::Union{UUID,Nothing} = nothing
    cap::Union{Int,Nothing} = nothing
    kind::Symbol = :none
    venue::Union{TLoc,Nothing} = nothing
    tiers::Vector{TTier} = TTier[]
    locs::Vector{TLoc} = TLoc[]
    score::Union{Float64,Missing} = missing
end

struct TPlain
    x::Int
    y::String
end

struct TInts
    a::Int
    b::Int
end

function run_interp_trim_sample()
    # trees are built with per-key setindex! (concrete key/value types):
    # heterogeneous Dict pair-splat constructors are themselves not
    # trim-verifiable, and real trimmed apps receive trees from a parser
    venue = Dict{String,Any}()
    venue["label"] = "Gym"
    venue["lat"] = 40.1
    venue["tags"] = Any["indoor"]
    tier1 = Dict{String,Any}()
    tier1["name"] = "Early"
    tier1["amount"] = 2500
    tier1["currency"] = "eur"
    tier2 = Dict{String,Any}()
    tier2["name"] = "Late"
    src = Dict{String,Any}()
    src["name"] = "Kickoff"
    src["day"] = "2026-08-01"
    src["at"] = "2026-07-25T23:59:59"
    src["uid"] = "c8b1cf79-de6a-54ab-a142-682c06a0de6a"
    src["cap"] = 64
    src["kind"] = "league"
    src["venue"] = venue
    src["tiers"] = Any[tier1, tier2]
    src["unknown_extra"] = Any[1, 2, 3]
    ev = StructUtils.make(TEvent, src)
    ev isa TEvent || error("TEvent type")
    e = ev::TEvent
    e.name == "Kickoff" || error("name")
    e.day == Date(2026, 8, 1) || error("day")
    e.at == DateTime(2026, 7, 25, 23, 59, 59) || error("at")
    e.uid == UUID("c8b1cf79-de6a-54ab-a142-682c06a0de6a") || error("uid")
    e.cap == 64 || error("cap")
    e.kind === :league || error("kind")
    v = e.venue
    v isa TLoc || error("venue")
    (v::TLoc).lat == 40.1 || error("lat")
    (v::TLoc).tags == [:indoor] || error("tags")
    length(e.tiers) == 2 || error("tiers")
    e.tiers[1].currency == "eur" || error("cur1")
    e.tiers[2].currency == "usd" || error("cur2")
    isempty(e.locs) || error("locs")
    e.score === missing || error("score")
    # fresh-empty defaults never alias
    minimal = Dict{String,Any}()
    minimal["name"] = "b"
    e2 = StructUtils.make(TEvent, minimal)
    (e2::TEvent).tiers === e.tiers && error("alias")
    # Symbol-keyed source
    symsrc = Dict{Symbol,Any}()
    symsrc[:name] = "s"
    symsrc[:amount] = 5
    t1 = StructUtils.make(TTier, symsrc)
    (t1::TTier).amount == 5 || error("symkey")
    # Symbol-keyed source into an unregistered struct (the case
    # make_trim_safe.jl previously exercised): reflection-only table
    intsrc = Dict{Symbol,Any}()
    intsrc[:a] = 1
    intsrc[:b] = 2
    ti = StructUtils.make(TInts, intsrc)
    ti isa TInts || error("tints type")
    (ti::TInts).a == 1 || error("tints a")
    (ti::TInts).b == 2 || error("tints b")
    # unregistered plain struct with mixed field types
    plainsrc = Dict{String,Any}()
    plainsrc["x"] = 1
    plainsrc["y"] = "z"
    p = StructUtils.make(TPlain, plainsrc)
    p isa TPlain || error("plain type")
    (p::TPlain).x == 1 || error("plain x")
    (p::TPlain).y == "z" || error("plain y")
    # required-field error path stays trim-clean and reachable
    badsrc = Dict{String,Any}()
    badsrc["amount"] = 1
    threw = false
    try
        StructUtils.make(TTier, badsrc)
    catch
        threw = true
    end
    threw || error("required")
    return nothing
end

function @main(args::Vector{String})::Cint
    _ = args
    run_interp_trim_sample()
    Core.println("INTERP_TRIM_OK")
    return 0
end

Base.Experimental.entrypoint(main, (Vector{String},))
