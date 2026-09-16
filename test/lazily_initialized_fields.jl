using Test
using StructUtils
using LazilyInitializedFields

struct LazyLeaf{T}
    value::T
end

@lazy struct LazyFields{T}
    required::T
    @lazy scalar::Int
    @lazy vector::Vector{T}
    @lazy child::LazyLeaf{T}
    @lazy optional::Union{Nothing,LazyLeaf{T}}
    @lazy anyvalue::Any
end

struct LazyHolder{T}
    child::LazyFields{T}
end

@lazy struct LazyCache{T}
    id::Int
    @lazy entry::LazyLeaf{T}
end

@lazy struct LazyNothing
    @lazy value::Nothing
end

@lazy struct LazyMissing
    @lazy value::Missing
end

@lazy struct LazyDefaults
    required::Int
    @lazy explicit::Int
    @lazy implicit::String
end

StructUtils.fielddefaults(::StructUtils.StructStyle, ::Type{LazyDefaults}) =
    (explicit=41,)

struct LazyDefaultStyle <: StructUtils.StructStyle end
StructUtils.fielddefaults(::LazyDefaultStyle, ::Type{LazyDefaults}) =
    (explicit=42,)

@testset "LazilyInitializedFields extension" begin
    @testset "omitted fields use uninit" begin
        value = StructUtils.make(LazyFields{Int}, Dict("required" => 1))

        @test value.required == 1
        for field in (:scalar, :vector, :child, :optional, :anyvalue)
            @test LazilyInitializedFields.islazyfield(typeof(value), field)
            @test getfield(value, field) === LazilyInitializedFields.uninit
            @test !LazilyInitializedFields.isinit(value, field)
        end
    end

    @testset "present fields use their logical types" begin
        value = StructUtils.make(
            LazyFields{Int},
            Dict{String,Any}(
                "required" => 1,
                "scalar" => 2,
                "vector" => Any[3, 4],
                "child" => Dict("value" => 5),
                "optional" => Dict("value" => 6),
                "anyvalue" => Dict("untouched" => 7),
            ),
        )

        @test value.required == 1
        @test value.scalar == 2
        @test value.vector == [3, 4]
        @test value.child == LazyLeaf{Int}(5)
        @test value.optional == LazyLeaf{Int}(6)
        @test value.anyvalue == Dict("untouched" => 7)

        @test (@inferred StructUtils.make(
            LazyFields{Int},
            (required=1, child=(value=2,), optional=nothing),
        )) isa LazyFields{Int}

        optional_nothing = StructUtils.make(
            LazyFields{Int},
            Dict("required" => 1, "optional" => nothing),
        )
        @test optional_nothing.optional === nothing

        explicit_uninit = StructUtils.make(
            LazyFields{Int},
            Dict("required" => 1, "child" => LazilyInitializedFields.uninit),
        )
        @test getfield(explicit_uninit, :child) === LazilyInitializedFields.uninit
    end

    @testset "nested and direct union construction" begin
        holder = StructUtils.make(
            LazyHolder{Int},
            Dict("child" => Dict("required" => 1)),
        )
        @test holder.child.required == 1
        @test getfield(holder.child, :child) === LazilyInitializedFields.uninit

        target = Union{typeof(LazilyInitializedFields.uninit),LazyLeaf{Int}}
        @test StructUtils.make(target, Dict("value" => 2)) == LazyLeaf{Int}(2)
        @test StructUtils.make(target, LazilyInitializedFields.uninit) ===
              LazilyInitializedFields.uninit
    end

    @testset "unparameterized parent infers its parameter" begin
        value = StructUtils.make(
            LazyCache,
            Dict("id" => 1, "entry" => Dict("value" => 2)),
        )
        @test value isa LazyCache{Int}
        @test value.entry == LazyLeaf{Int}(2)
    end

    @testset "nullable-only fields reject non-null values" begin
        @test StructUtils.make(LazyNothing, Dict("value" => nothing)).value === nothing
        @test StructUtils.make(LazyMissing, Dict("value" => nothing)).value === missing
        @test_throws ArgumentError StructUtils.make(LazyNothing, Dict("value" => 1))
        @test_throws ArgumentError StructUtils.make(LazyMissing, Dict("value" => 1))
    end

    @testset "explicit defaults take precedence" begin
        default = StructUtils.make(LazyDefaults, Dict("required" => 1))
        @test default.explicit == 41
        @test getfield(default, :implicit) === LazilyInitializedFields.uninit

        styled = StructUtils.make(
            LazyDefaults,
            Dict("required" => 1),
            LazyDefaultStyle(),
        )
        @test styled.explicit == 42
        @test getfield(styled, :implicit) === LazilyInitializedFields.uninit
    end

end
