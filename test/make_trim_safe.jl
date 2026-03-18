using StructUtils

# Plain struct (no defaults)
struct TrimA
    a::Int
    b::Int
    c::Int
    d::Int
end

# Struct with simple defaults
@defaults struct TrimB
    a::Int
    b::Int
    c::Int = 0
    d::Int = 0
end

# Struct with computed/dependent defaults
@defaults struct TrimC
    a::Int
    b::String = string(a)
end

# @kwarg struct with chained defaults
@kwarg struct TrimD
    a::Int = 1
    b::Int = a + 10
    c::Int = a + b
end

# @noarg mutable struct
@noarg mutable struct TrimE
    a::Int = 0
    b::Int = 0
end

function run_make_trim_sample()::Nothing
    # 1. Plain struct from NamedTuple
    a = StructUtils.make(TrimA, (a=1, b=2, c=3, d=4))
    a.a == 1 || error("TrimA.a")
    a.d == 4 || error("TrimA.d")

    # 2. Plain struct from Dict{Symbol,Int} (homogeneous value type)
    a2 = StructUtils.make(TrimA, Dict{Symbol,Int}(:a => 1, :b => 2, :c => 3, :d => 4))
    a2.a == 1 || error("TrimA Dict")

    # 3. Struct with defaults — all provided
    b1 = StructUtils.make(TrimB, (a=1, b=2, c=3, d=4))
    b1.c == 3 || error("TrimB all")

    # 4. Struct with defaults — defaults used
    b2 = StructUtils.make(TrimB, (a=1, b=2))
    b2.c == 0 || error("TrimB defaults")

    # 5. Computed defaults — default used
    c1 = StructUtils.make(TrimC, (a=42,))
    c1.b == "42" || error("TrimC computed")

    # 6. Computed defaults — all provided
    c2 = StructUtils.make(TrimC, (a=42, b="hello"))
    c2.b == "hello" || error("TrimC provided")

    # 7. Chained kwarg defaults
    d1 = StructUtils.make(TrimD, (a=5,))
    d1.b == 15 || error("TrimD.b")
    d1.c == 20 || error("TrimD.c")

    # 8. @noarg mutable struct
    e = StructUtils.make(TrimE, (a=10, b=20))
    e.a == 10 || error("TrimE.a")

    # 9. NamedTuple target
    nt = StructUtils.make(@NamedTuple{a::Int, b::Int}, (a=1, b=2))
    nt.a == 1 || error("NamedTuple")

    # 10. applyeach (serialization direction)
    count = Ref(0)
    StructUtils.applyeach(StructUtils.DefaultStyle(), (k, v) -> (count[] += 1; nothing), a)
    count[] == 4 || error("applyeach")

    return nothing
end

function @main(args::Vector{String})::Cint
    _ = args
    run_make_trim_sample()
    return 0
end

Base.Experimental.entrypoint(main, (Vector{String},))
