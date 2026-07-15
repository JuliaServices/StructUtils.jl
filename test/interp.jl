using Test, Dates, UUIDs, StructUtils

# tier-0 interpreter unit tests: table resolution, closed-kind lifting,
# defaults policies, and routing between the interpreter and the classic path

struct ITagStyle <: StructUtils.StructStyle end
StructUtils.fieldtagkey(::ITagStyle) = :itag

mutable struct ICountStyle <: StructUtils.StructStyle
    calls::Int
end

struct IStrictStyle <: StructUtils.StructStyle end
struct IUnknownFieldError <: Exception
    key::Any
end
StructUtils.unknownfield(::IStrictStyle, ::Type{T}, key, value) where {T} =
    throw(IUnknownFieldError(key))

@kwarg struct ITier
    name::String
    amount::Int = 0
    currency::String = "usd"
end

@kwarg struct ILoc
    label::String = ""
    lat::Union{Float64,Nothing} = nothing
    tags::Vector{Symbol} = Symbol[]
end

@kwarg struct IEvent
    name::String
    day::Date = Date(0)
    at::Union{DateTime,Nothing} = nothing
    uid::Union{UUID,Nothing} = nothing
    cap::Union{Int,Nothing} = nothing
    kind::Symbol = :none &(itag=(name="event_kind",),)
    venue::Union{ILoc,Nothing} = nothing
    tiers::Vector{ITier} = ITier[]
    seed::Vector{Int} = [1, 2, 3]
    note::Any = nothing
    score::Union{Float64,Missing} = missing
end

@kwarg struct IComputed
    a::Int
    b::Int = a + 10
end

@nonstruct struct IPoint
    x::Int
    y::Int
end
StructUtils.lift(::Type{IPoint}, s::String) =
    (p = split(s, ','); IPoint(parse(Int, p[1]), parse(Int, p[2])))

@kwarg struct IHasPoint
    p::IPoint = IPoint(0, 0)
end

struct IPlain
    x::Int
    y::String
end

struct IManual
    m::Int
end
StructUtils.fielddefaults(::StructUtils.StructStyle, ::Type{IManual}) = (m=42,)

struct ICounted
    c::Int
end
function StructUtils.fieldtags(st::ICountStyle, ::Type{ICounted})
    st.calls += 1
    return (c=(name="c",),)
end

@kwarg struct IAliased
    v::Int = 0 &(name=("v", "value"),)
end

const IEVENT_SRC = Dict{String,Any}(
    "name" => "Kickoff",
    "day" => "2026-08-01",
    "at" => "2026-07-25T23:59:59",
    "uid" => "c8b1cf79-de6a-54ab-a142-682c06a0de6a",
    "cap" => 64,
    "kind" => "league",
    "venue" => Dict{String,Any}("label" => "Gym", "lat" => 40.1, "tags" => Any["indoor"]),
    "tiers" => Any[
        Dict{String,Any}("name" => "Early", "amount" => 2500, "currency" => "eur"),
        Dict{String,Any}("name" => "Late"),
    ],
    "note" => Dict{String,Any}("k" => "v"),
    "score" => nothing,
    "unknown_extra" => Any[1, 2, 3],
)

@testset "tier-0 interpreter" begin
    @testset "closed-kind correctness" begin
        ev = StructUtils.make(IEvent, IEVENT_SRC)
        @test ev.name == "Kickoff"
        @test ev.day == Date(2026, 8, 1)
        @test ev.at == DateTime(2026, 7, 25, 23, 59, 59)
        @test ev.uid == UUID("c8b1cf79-de6a-54ab-a142-682c06a0de6a")
        @test ev.cap == 64
        @test ev.kind === :league
        @test ev.venue isa ILoc && ev.venue.lat == 40.1 && ev.venue.tags == [:indoor]
        @test length(ev.tiers) == 2
        @test ev.tiers[1].currency == "eur"
        @test ev.tiers[2].currency == "usd" # per-field default
        @test ev.note isa Dict{String,Any}
        @test ev.score === missing
        @test StructUtils.fieldtable(IEvent, StructUtils.DefaultStyle()).eligible
        # Symbol keys and Vector{Pair} sources
        @test StructUtils.make(ITier, Dict(:name => "s", :amount => 5)).amount == 5
        @test StructUtils.make(ITier, ["name" => "vp"]).currency == "usd"
    end

    @testset "defaults policies" begin
        a = StructUtils.make(IEvent, Dict{String,Any}("name" => "a"))
        b = StructUtils.make(IEvent, Dict{String,Any}("name" => "b"))
        @test a.tiers !== b.tiers          # FRESHEMPTY re-materialized
        @test a.seed == [1, 2, 3] && a.seed !== b.seed  # thunk default not aliased
        @test a.venue === nothing && a.cap === nothing && a.score === missing
        # vals-dependent defaults compute against parsed values
        @test StructUtils.make(IComputed, Dict("a" => 5)).b == 15
        @test StructUtils.make(IComputed, Dict("a" => 5, "b" => 1)).b == 1
        # missing required field
        @test_throws ArgumentError StructUtils.make(ITier, Dict{String,Any}("amount" => 1))
    end

    @testset "routing to the classic path" begin
        # unregistered plain struct: classic path, still works from a Dict
        @test StructUtils.make(IPlain, Dict("x" => 1, "y" => "z")) == IPlain(1, "z")
        @test !StructUtils.fieldtable(IPlain, StructUtils.DefaultStyle()).eligible
        # manual fielddefaults overload (no macro): classic path honors it
        @test StructUtils.make(IManual, Dict{String,Any}()).m == 42
        @test !StructUtils.fieldtable(IManual, StructUtils.DefaultStyle()).eligible
        # stateful per-style fieldtags: classic path, called once per make
        cst = ICountStyle(0)
        StructUtils.make(cst, ICounted, Dict("c" => 1))
        StructUtils.make(cst, ICounted, Dict("c" => 2))
        @test cst.calls >= 2
        @test !StructUtils.fieldtable(ICounted, cst).eligible
        # tuple alias name tags: classic path, both aliases match
        @test StructUtils.make(IAliased, Dict("value" => 7)).v == 7
        @test StructUtils.make(IAliased, Dict("v" => 8)).v == 8
        @test !StructUtils.fieldtable(IAliased, StructUtils.DefaultStyle()).eligible
        # @nonstruct nested field lifts, never field-parses
        @test StructUtils.make(IHasPoint, Dict("p" => "3,4")).p == IPoint(3, 4)
    end

    @testset "tagkey namespacing and styles" begin
        # ITagStyle resolves the :itag namespace: rename applies
        ev = StructUtils.make(ITagStyle(), IEvent, Dict{String,Any}("name" => "x", "event_kind" => "k"))
        @test ev[1].kind === :k
        # DefaultStyle has no tagkey: the :itag-namespaced rename does NOT
        # apply, so "event_kind" is an unknown key and the default holds
        ev2 = StructUtils.make(IEvent, Dict{String,Any}("name" => "x", "event_kind" => "k2"))
        @test ev2.kind === :none
        # unknownfield hook fires for unmatched keys
        @test_throws IUnknownFieldError StructUtils.make(IStrictStyle(), ITier,
            Dict{String,Any}("name" => "t", "bogus" => 1))
    end
end
