using Test, Dates, UUIDs, StructUtils

# :hot tier: emitted specialized methods, tree-gate routing, hooks

@kwarg struct HTier
    name::String
    amount::Int = 0
    currency::String = "usd"
end

@kwarg :hot struct HotTier
    name::String
    amount::Int = 0
    currency::String = "usd"
end

@kwarg :hot struct HotEvent
    name::String
    day::Date = Date(0)
    cap::Union{Int,Nothing} = nothing
    venue::Union{HotTier,Nothing} = nothing
    tiers::Vector{HotTier} = HotTier[]
end

@noarg :hot mutable struct HotMut
    a::Int = 0
    b::String = ""
end

struct HPlain
    x::Int
end
StructUtils.@hot HPlain

@testset ":hot tier" begin
    @test StructUtils.ishot(HotTier)
    @test !StructUtils.ishot(HTier)
    @test StructUtils.ishot(HPlain)

    # differential: hot vs non-hot twin from identical tree sources
    srcd = Dict{String,Any}("name" => "a", "amount" => 2)
    hot = StructUtils.make(HotTier, srcd)
    plain = StructUtils.make(HTier, srcd)
    @test hot.name == plain.name == "a"
    @test hot.amount == plain.amount == 2
    @test hot.currency == plain.currency == "usd"

    # NamedTuple source takes the hot descent
    nt = StructUtils.make(HotTier, (name="n", amount=3, currency="eur"))
    @test nt.name == "n" && nt.amount == 3 && nt.currency == "eur"

    # nested structs + vectors from NamedTuple sources
    ev = StructUtils.make(HotEvent, (name="e", day=Date(2026, 1, 2), venue=(name="v",),
        tiers=[(name="t1",), (name="t2", amount=5)]))
    @test ev.venue isa HotTier && ev.venue.name == "v" && ev.venue.currency == "usd"
    @test length(ev.tiers) == 2 && ev.tiers[2].amount == 5
    @test ev.day == Date(2026, 1, 2)
    @test ev.cap === nothing

    # tree sources route through the interpreter with identical results
    ev2 = StructUtils.make(HotEvent, Dict{String,Any}("name" => "e2",
        "tiers" => Any[Dict{String,Any}("name" => "x")]))
    @test ev2.tiers[1].currency == "usd"
    @test ev2.venue === nothing

    # @noarg :hot mutable
    hm = StructUtils.make(HotMut, (a=1, b="z"))
    @test hm.a == 1 && hm.b == "z"

    # standalone @hot on a plain (non-macro) struct
    @test StructUtils.make(HPlain, (x=7,)).x == 7
    @test StructUtils.make(HPlain, Dict{String,Any}("x" => 8)).x == 8

    # hook registry fires per annotated type under force
    fired = Ref(0)
    hook = (T, samples) -> (fired[] += 1; nothing)
    StructUtils.register_hot_hook!(hook)
    try
        StructUtils._hot_precompile!(HotTier; force=true)
        @test fired[] == 1
        StructUtils._hot_precompile!(HotEvent, ("{}",); force=true)
        @test fired[] == 2
    finally
        pop!(StructUtils.HOT_HOOKS)
    end

    # :nonstruct rejects :hot
    @test_throws LoadError @eval @nonstruct :hot struct HotNon
        x::Int
    end
end
