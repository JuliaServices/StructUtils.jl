# A source shaped like JSON.jl's lazy values, with separate object and array loops.
struct NestedSource
    kind::Symbol
    keys::Vector{String}
    vals::Vector{NestedSource}
    leaf::String
end
nested_obj(pairs...) = NestedSource(:object, String[first(p) for p in pairs], NestedSource[last(p) for p in pairs], "")
nested_arr(vals...) = NestedSource(:array, String[], NestedSource[vals...], "")
nested_leaf(s) = NestedSource(:leaf, String[], NestedSource[], s)

function StructUtils.applyeach(st::StructUtils.StructStyle, f, x::NestedSource)
    if x.kind === :object
        for i in eachindex(x.vals)
            ret = f(x.keys[i], x.vals[i])
            ret isa StructUtils.EarlyReturn && return ret
        end
    else
        for i in eachindex(x.vals)
            ret = f(i, x.vals[i])
            ret isa StructUtils.EarlyReturn && return ret
        end
    end
    return StructUtils.defaultstate(st)
end
StructUtils.structlike(::StructUtils.StructStyle, x::NestedSource) = x.kind === :object
StructUtils.arraylike(::StructUtils.StructStyle, x::NestedSource) = x.kind === :array
StructUtils.lift(::Type{String}, x::NestedSource) = x.leaf
StructUtils.lift(::Type{Int}, x::NestedSource) = parse(Int, x.leaf)

# Levels alternate between untagged and tagged, because a tagged struct inside an untagged one
# is what widens `applyeach` without the sink wrapper. Each level holds a vector of the level
# below next to a Dict, the shape from #98.
struct Nested0; s::String; end
@tags struct Nested1; x::Vector{Nested0} &(name="xs",); m::Dict{String,Int}; end
struct Nested2; x::Vector{Nested1}; m::Dict{String,Int}; end
@tags struct Nested3; x::Vector{Nested2} &(name="xs",); m::Dict{String,Int}; end
struct Nested4; x::Vector{Nested3}; m::Dict{String,Int}; end

@testset "nested struct targets infer without widening" begin
    src = nested_obj(
        "x" => nested_arr(nested_obj("xs" => nested_arr(), "m" => nested_obj())),
        "m" => nested_obj("a" => nested_leaf("1")))
    x = StructUtils.make(Nested4, src)
    @test x.m == Dict("a" => 1) && length(x.x) == 1 && isempty(x.x[1].x)
    # On Julia 1.11 and later, a widened call (a bounded `Type{<:X}` target, or a `FieldSink`
    # without its struct type) makes the first `make` of a nested target exponential in its
    # depth. On 1.10 StructUtils calls `make` and `applyeach` directly (see `_MAXHEIGHT`) and
    # compile time stays linear, so there the test checks only the result.
    if VERSION >= v"1.11"
        makes = sinks = 0
        widened = Any[]
        for m in methods(StructUtils.make), mi in Base.specializations(m)
            params = Base.unwrap_unionall(mi.specTypes).parameters
            NestedSource in params || continue
            makes += 1
            for p in params
                p isa UnionAll && p <: Type && p.var.ub !== Any && push!(widened, mi.specTypes)
            end
        end
        for m in methods(StructUtils.applyeach), mi in Base.specializations(m)
            mi.specTypes <: Tuple{Any,Any,StructUtils.FieldSink,NestedSource} || continue
            sinks += 1
            # A sink that keeps its struct type but has an abstract style comes from unrelated
            # abstract calls into other tests' methods (seen on Julia 1.14), not from this widening.
            sink = Base.unwrap_unionall(mi.specTypes).parameters[3]
            (sink isa TypeVar || Base.unwrap_unionall(sink).parameters[1] isa TypeVar) &&
                push!(widened, mi.specTypes)
        end
        @test makes > 0 && sinks > 0
        @test isempty(widened)
    end
end
