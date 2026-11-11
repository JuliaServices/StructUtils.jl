# A source shaped like a lazily parsed JSON value: objects and arrays are walked by separate
# loops that pass String and Int keys, so `make` reaches each nested target by several call paths.
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

# Untagged and tagged levels alternate; each holds a vector of the level below next to a Dict.
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
    # When inference takes a nested field type for unbounded recursion, it widens the call
    # to `make` (e.g. to `Type{<:Dict}`) and caches no frame between the two nesting levels,
    # so the first `make` of a deeply nested target takes time exponential in its depth.
    if VERSION >= v"1.10"
        widened = Any[]
        for m in methods(StructUtils.make), mi in Base.specializations(m)
            params = Base.unwrap_unionall(mi.specTypes).parameters
            NestedSource in params || continue
            for p in params
                p isa UnionAll && p <: Type && p.var.ub !== Any && push!(widened, mi.specTypes)
            end
        end
        @test isempty(widened)
    end
end
