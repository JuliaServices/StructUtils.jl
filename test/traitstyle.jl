module TraitStyleTests
using StructUtils, Test

struct InnerStyle <: StructUtils.StructStyle end
struct Wrapper{S} <: StructUtils.StructStyle
    inner::S
end
StructUtils.traitstyle(st::Wrapper) = st.inner
StructUtils.defaultstate(::Wrapper) = :outer

struct CustomDict
    values::Dict{String,Int}
end
Base.keytype(::CustomDict) = String
Base.valtype(::CustomDict) = Int
StructUtils.dictlike(::StructUtils.StructStyle, ::Type{CustomDict}) = false
StructUtils.dictlike(::InnerStyle, ::Type{CustomDict}) = true
StructUtils.initialize(::Wrapper, ::Type{CustomDict}, source) = CustomDict(Dict("initial"=>0))
StructUtils.addkeyval!(x::CustomDict, k, v) = (x.values[k] = v)
StructUtils.liftkey(::Wrapper, ::Type{String}, x::String) = uppercase(x)

struct NullSource end
StructUtils.nulllike(::StructUtils.StructStyle, ::Type{NullSource}) = false
StructUtils.nulllike(::InnerStyle, ::Type{NullSource}) = true
StructUtils.lift(::Wrapper, ::Type{Nothing}, ::NullSource) = (nothing, :outer)

struct Scalar
    value::Int
end
StructUtils.structlike(::StructUtils.StructStyle, ::Type{Scalar}) = true
StructUtils.structlike(::InnerStyle, ::Type{Scalar}) = false
StructUtils.lift(::Wrapper, ::Type{Scalar}, value::Int) = (Scalar(value), :outer)

mutable struct MutableTarget
    value::Int
end
StructUtils.noarg(::StructUtils.StructStyle, ::Type{MutableTarget}) = false
StructUtils.noarg(::InnerStyle, ::Type{MutableTarget}) = true
StructUtils.initialize(::Wrapper, ::Type{MutableTarget}, source) = MutableTarget(0)

@testset "Trait style preserves construction context" begin
    style = Wrapper(InnerStyle())
    @test StructUtils.traitstyle(StructUtils.DefaultStyle()) isa StructUtils.DefaultStyle
    value, state = StructUtils.make(style, CustomDict, Dict("key"=>1))
    @test value.values == Dict("initial"=>0, "KEY"=>1)
    @test state === :outer
    StructUtils.make!(style, value, Dict("next"=>2))
    @test value.values["NEXT"] == 2
    @test StructUtils.make(style, Union{Nothing,Int}, NullSource()) === (nothing, :outer)
    @test StructUtils.make(style, Union{Nothing,Int}, NullSource(), (;)) === (nothing, :outer)
    scalar, state = StructUtils.make(style, Scalar, 3)
    @test scalar.value == 3
    @test state === :outer
    mutable_value, state = StructUtils.make(style, MutableTarget, (value=4,))
    @test mutable_value.value == 4
    @test state === :outer
    StructUtils.make!(style, mutable_value, (value=5,))
    @test mutable_value.value == 5
    @test first(StructUtils.make(style, Matrix{Int}, [[1,2],[3,4]])) == [1 3; 2 4]
end
end
