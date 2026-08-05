using Test, StructUtils

mutable struct InboundAuditStyle <: StructUtils.StructStyle
    unknowns::Vector{Any}
end

StructUtils.fieldtagkey(::InboundAuditStyle) = :wire
StructUtils.defaultstate(::InboundAuditStyle) = :audit_state
function StructUtils.unknownfield(
    style::InboundAuditStyle,
    ::Type{T},
    key,
    value,
) where {T}
    push!(style.unknowns, (T, key, value))
    return :unknown_state
end

struct StrictInboundStyle <: StructUtils.StructStyle end
StructUtils.fieldtagkey(::StrictInboundStyle) = :wire
StructUtils.unknownfield(::StrictInboundStyle, ::Type{T}, key, value) where {T} =
    throw(ArgumentError("unknown $(repr(key)) for $T"))

@defaults struct PlainIgnored
    visible::Int = 1
    ignored::Int = 99 &(ignore=true,)
end

@defaults struct WireIgnored
    visible::Int = 1 &(wire=(name="shown",),)
    ignored::Int = 99 &(wire=(name="secret", ignore=true),)
    notignored::Int = 7 &(wire=(ignore=false,),)
end

@noarg mutable struct MutableWireIgnored
    visible::Int = 1 &(wire=(name="shown",),)
    ignored::Int = 99 &(wire=(name="secret", ignore=true),)
end

@testset "ignore=true on inbound make" begin
    @test StructUtils.make(PlainIgnored, (visible=2, ignored=200)) ==
        PlainIgnored(2, 99)
    @test StructUtils.make(PlainIgnored, Dict("visible" => 2, "ignored" => 200)) ==
        PlainIgnored(2, 99)
    @test StructUtils.make(PlainIgnored, [2, 200]) == PlainIgnored(2, 99)

    for source in (
        Dict{String,Int}("shown" => 2, "secret" => 200, "notignored" => 8),
        Dict{Symbol,Int}(:shown => 2, :ignored => 200, :notignored => 8),
        [2, 200, 8],
    )
        style = InboundAuditStyle(Any[])
        value, state = StructUtils.make(style, WireIgnored, source)
        @test value == WireIgnored(2, 99, 8)
        @test state === :audit_state
        @test isempty(style.unknowns)
    end

    style = InboundAuditStyle(Any[])
    value, state = StructUtils.make(
        style,
        WireIgnored,
        ["shown" => 3, "secret" => 300, "extra" => 4],
    )
    @test value == WireIgnored(3, 99, 7)
    @test state === :audit_state
    @test style.unknowns == Any[(WireIgnored, "extra", 4)]

    @test StructUtils.make(WireIgnored, (secret=300,), StrictInboundStyle()) ==
        WireIgnored(1, 99, 7)
    @test_throws ArgumentError StructUtils.make(
        WireIgnored,
        (extra=4,),
        StrictInboundStyle(),
    )

    for source in (
        Dict{String,Int}("shown" => 2, "secret" => 200),
        Dict{Symbol,Int}(:shown => 2, :ignored => 200),
        [2, 200],
    )
        style = InboundAuditStyle(Any[])
        value = MutableWireIgnored()
        value.ignored = 55
        state = StructUtils.make!(style, value, source)
        @test value.visible == 2
        @test value.ignored == 55
        @test state === :audit_state
        @test isempty(style.unknowns)
    end
end
