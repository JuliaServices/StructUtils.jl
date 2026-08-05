# Focused regression tests for struct-shaped construction (`makestruct`/`makenoarg`
# internals). These pin behavior that the field-matching/construction machinery
# must preserve regardless of how it is implemented.
using Test, StructUtils

# --- inner constructors must run (no construction bypass) ---
struct Positive
    x::Int
    function Positive(x)
        x > 0 || throw(ArgumentError("x must be positive"))
        return new(x)
    end
end

mutable struct CtorCounter
    n::Int
end
const CTOR_COUNTER = CtorCounter(0)
struct CountedCtor
    a::Int
    function CountedCtor(a)
        CTOR_COUNTER.n += 1
        return new(a)
    end
end

# --- stateful styles: metadata call cadence ---
mutable struct CadenceWholeStyle <: StructUtils.StructStyle
    calls::Int
end
mutable struct CadencePerFieldStyle <: StructUtils.StructStyle
    calls::Int
end

struct CadenceTagged
    a::Int
    b::Int
end

function StructUtils.fieldtags(style::CadenceWholeStyle, ::Type{CadenceTagged})
    style.calls += 1
    return (a=(name="A",), b=(name="B",))
end

function StructUtils.fieldtags(style::CadencePerFieldStyle, ::Type{CadenceTagged}, field::Symbol)
    style.calls += 1
    return (;)
end

# --- alias tuples and rename asymmetry ---
@tags struct AliasTupleT
    id::Int &(name=("ident", :idx),)
    code::Int
end

@tags struct RenamedT
    id::Int &(name="identifier",)
    code::Int
end

@tags struct CollidingName
    a::Int &(name="b",)
    b::Int
end
StructUtils.fielddefaults(::StructUtils.StructStyle, ::Type{CollidingName}) =
    (a=-1, b=-2)

@tags mutable struct MutableCollidingName
    a::Int &(name="b",)
    b::Int
    MutableCollidingName() = new(-1, -2)
end
StructUtils.noarg(::StructUtils.StructStyle, ::Type{MutableCollidingName}) = true

@tags struct OverlappingNames
    a::Int &(name="x",)
    b::Int &(name="x",)
end
StructUtils.fielddefaults(::StructUtils.StructStyle, ::Type{OverlappingNames}) =
    (a=-1, b=-2)

@tags struct ExplicitNothingName
    a::Int &(name=nothing,)
end
StructUtils.fielddefaults(::StructUtils.StructStyle, ::Type{ExplicitNothingName}) =
    (a=99,)

# --- nullable fields must retain normal custom make dispatch ---
abstract type NullableChoice end
struct ChosenValue <: NullableChoice
    x::Int
end
const OptionalChoice = Union{Nothing,NullableChoice}
StructUtils.@choosetype OptionalChoice source ->
    source === nothing ? Nothing : ChosenValue
struct ChoiceHolder
    value::OptionalChoice
end

struct CustomOptionalValue
    x::Int
end
const CustomOptional = Union{Nothing,CustomOptionalValue}
function StructUtils.make(
    style::StructUtils.StructStyle,
    ::Type{CustomOptional},
    source,
    tags,
)
    value = source === nothing ? nothing : CustomOptionalValue(source.x + 1)
    return value, StructUtils.defaultstate(style)
end
struct CustomOptionalHolder
    value::CustomOptional
end

struct WildcardKey end
StructUtils.keyeq(::WildcardKey, ::String) = true
struct WildcardTarget
    a::Int
    b::Int
end
StructUtils.fielddefaults(::StructUtils.StructStyle, ::Type{WildcardTarget}) =
    (a=-1, b=-2)

# --- shuffled key order ---
struct Ten
    f1::Int; f2::Int; f3::Int; f4::Int; f5::Int
    f6::Int; f7::Int; f8::Int; f9::Int; f10::Int
end

# --- wide struct (64 fields) ---
let fields = join(("g$i::Int" for i in 1:64), "\n")
    eval(Meta.parse("struct Wide64\n$fields\nend"))
end

@testset "struct construction regressions" begin
    @testset "inner constructor executes" begin
        @test StructUtils.make(Positive, (x=3,)) == Positive(3)
        @test_throws ArgumentError StructUtils.make(Positive, (x=-1,))
        @test_throws ArgumentError StructUtils.make(Positive, Dict("x" => -1))
        CTOR_COUNTER.n = 0
        @test StructUtils.make(CountedCtor, (a=7,)).a == 7
        @test CTOR_COUNTER.n == 1
    end

    @testset "stateful style call cadence" begin
        # whole-type fieldtags: exactly one call per make, per style instance
        s1 = CadenceWholeStyle(0)
        @test StructUtils.make(CadenceTagged, (A=1, B=2), s1) == CadenceTagged(1, 2)
        @test s1.calls == 1
        @test StructUtils.make(CadenceTagged, (A=3, B=4), s1) == CadenceTagged(3, 4)
        @test s1.calls == 2  # no cross-make caching
        s2 = CadenceWholeStyle(0)
        @test StructUtils.make(CadenceTagged, (A=5, B=6), s2) == CadenceTagged(5, 6)
        @test s2.calls == 1  # instances independent
        @test s1.calls == 2

        # per-field public fieldtags: one call per field per make
        p1 = CadencePerFieldStyle(0)
        @test StructUtils.make(CadenceTagged, (a=1, b=2), p1) == CadenceTagged(1, 2)
        @test p1.calls == 2
        @test StructUtils.make(CadenceTagged, (a=3, b=4), p1) == CadenceTagged(3, 4)
        @test p1.calls == 4
    end

    @testset "alias tuples and rename asymmetry" begin
        # every alias matches, from String- and Symbol-keyed sources
        @test StructUtils.make(AliasTupleT, Dict("ident" => 1, "code" => 2)) == AliasTupleT(1, 2)
        @test StructUtils.make(AliasTupleT, Dict("idx" => 1, "code" => 2)) == AliasTupleT(1, 2)
        @test StructUtils.make(AliasTupleT, Dict(:ident => 1, :code => 2)) == AliasTupleT(1, 2)
        @test StructUtils.make(AliasTupleT, Dict(:idx => 1, :code => 2)) == AliasTupleT(1, 2)
        # Symbol keys also match the original field name; String keys do not
        @test StructUtils.make(AliasTupleT, Dict(:id => 1, :code => 2)) == AliasTupleT(1, 2)
        @test_throws Exception StructUtils.make(AliasTupleT, Dict("id" => 1, "code" => 2))
        @test StructUtils.make(RenamedT, Dict(:id => 1, :code => 2)) == RenamedT(1, 2)
        @test StructUtils.make(RenamedT, Dict(:identifier => 1, :code => 2)) == RenamedT(1, 2)
        @test StructUtils.make(RenamedT, Dict("identifier" => 1, "code" => 2)) == RenamedT(1, 2)
        @test_throws Exception StructUtils.make(RenamedT, Dict("id" => 1, "code" => 2))
    end

    @testset "overlapping and explicit names" begin
        # Field matching is first-to-last for every source key. A cursor must
        # not let a later raw field bypass an earlier alias with the same name.
        @test StructUtils.make(CollidingName, [:a => 10, :b => 20]) ==
            CollidingName(20, -2)
        mutable_collision = StructUtils.make(MutableCollidingName, [:a => 10, :b => 20])
        @test (mutable_collision.a, mutable_collision.b) == (20, -2)
        @test StructUtils.make(OverlappingNames, ["x" => 1, "x" => 2]) ==
            OverlappingNames(2, -2)

        # An explicit `name=nothing` differs from an absent name for string
        # sources. Symbol sources still match the raw Julia field name.
        @test StructUtils.make(ExplicitNothingName, ["a" => 1]) ==
            ExplicitNothingName(99)
        @test StructUtils.make(ExplicitNothingName, [:a => 1]) ==
            ExplicitNothingName(1)
    end

    @testset "custom nullable dispatch" begin
        @test StructUtils.make(ChoiceHolder, (value=(x=2,),)) ==
            ChoiceHolder(ChosenValue(2))
        @test StructUtils.make(ChoiceHolder, (value=nothing,)) ==
            ChoiceHolder(nothing)
        @test StructUtils.make(CustomOptionalHolder, (value=(x=2,),)) ==
            CustomOptionalHolder(CustomOptionalValue(3))
        @test StructUtils.make(CustomOptionalHolder, (value=nothing,)) ==
            CustomOptionalHolder(nothing)
    end

    @testset "custom key precedence" begin
        @test StructUtils.make(
            WildcardTarget,
            [WildcardKey() => 1, WildcardKey() => 2],
        ) == WildcardTarget(2, -2)
    end

    @testset "shuffled key order" begin
        ordered = [Symbol("f$i") => i for i in 1:10]
        expected = StructUtils.make(Ten, ordered)
        @test expected == Ten((1:10)...)
        # reversed, interleaved, and rotated orders all produce the same value
        @test StructUtils.make(Ten, reverse(ordered)) == expected
        shuffled = [ordered[i] for i in [7, 2, 10, 4, 1, 9, 3, 6, 5, 8]]
        @test StructUtils.make(Ten, shuffled) == expected
        rotated = vcat(ordered[4:end], ordered[1:3])
        @test StructUtils.make(Ten, rotated) == expected
        # String keys, shuffled, with unknown keys interleaved (ignored by default)
        strkeys = vcat(["zzz" => 99], ["f$i" => i for i in 10:-1:1], ["extra" => -1])
        @test StructUtils.make(Ten, strkeys) == expected
    end

    @testset "wide struct" begin
        ordered = ["g$i" => i for i in 1:64]
        w = StructUtils.make(Wide64, ordered)
        @test all(getfield(w, i) == i for i in 1:64)
        # deterministic full-cycle permutation of key order
        perm = [(i * 37) % 64 + 1 for i in 0:63]
        @test length(unique(perm)) == 64
        shuffled = ["g$(p)" => p for p in perm]
        w2 = StructUtils.make(Wide64, shuffled)
        @test all(getfield(w2, i) == i for i in 1:64)
        @test w2 == w
    end
end
