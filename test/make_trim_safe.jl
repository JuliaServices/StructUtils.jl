using Dates, StructUtils
using StructUtils.Selectors: @selectors

struct TrimTemporal
    day::Date
    stamp::DateTime
    tick::Time
end

struct TrimA
    a::Int
    b::Int
    c::Int
    d::Int
end

@defaults struct TrimB
    a::Int
    b::Int
    c::Int = 0
    d::Int = 0
end

@defaults struct TrimC
    a::Int
    b::String = string(a)
end

@kwarg struct TrimD
    a::Int = 1
    b::Int = a + 10
    c::Int = a + b
end

@noarg mutable struct TrimE
    a::Int = 0
    b::Int = 0
end

@defaults struct TrimSimpleDefaults
    a::Int = 1
    b::String = "ok"
end

struct TrimStyle <: StructUtils.StructStyle end

StructUtils.fieldtagkey(::TrimStyle) = :trim
StructUtils.defaultstate(::TrimStyle) = :trim_state

@nonstruct struct TrimPoint
    x::Int
    y::Int
end

function _trim_point(s::AbstractString)::TrimPoint
    parts = split(s, ',')
    return TrimPoint(parse(Int, parts[1]), parse(Int, parts[2]))
end

Base.convert(::Type{TrimPoint}, s::AbstractString) = _trim_point(s)
StructUtils.lowerkey(::TrimStyle, p::TrimPoint) = "$(p.x),$(p.y)"
StructUtils.liftkey(::TrimStyle, ::Type{TrimPoint}, s::AbstractString) = _trim_point(s)

@tags struct TrimTagged
    id::Int &(trim=(name="identifier",),)
    code::Int
    ignored::Int = 99 &(trim=(ignore=true,),)
end

abstract type TrimVehicle end

struct TrimCarSource
    seats::Int
    wheels::Int
end

struct TrimTruckSource
    payload::Int
    axles::Int
end

struct TrimCar <: TrimVehicle
    seats::Int
    wheels::Int
end

struct TrimTruck <: TrimVehicle
    payload::Int
    axles::Int
end

trim_vehicle_type(::TrimCarSource) = TrimCar
trim_vehicle_type(::TrimTruckSource) = TrimTruck

StructUtils.@choosetype TrimVehicle trim_vehicle_type

struct TrimSelectorRecord
    key::Int
    value::Int
end

@selectors TrimSelectorRecord

function _assert_trim_public_traits()::Nothing
    StructUtils.dictlike(Dict{Symbol,Int}) || error("dictlike")
    StructUtils.arraylike(Vector{Int}) || error("arraylike vector")
    StructUtils.arraylike((1, 2)) || error("arraylike tuple")
    StructUtils.fixedsizearray(Matrix{Int}) || error("fixedsizearray")
    StructUtils.structlike(StructUtils.DefaultStyle(), TrimTagged) || error("structlike")
    StructUtils.noarg(StructUtils.DefaultStyle(), TrimE) || error("noarg")
    StructUtils.kwarg(StructUtils.DefaultStyle(), TrimD) || error("kwarg")
    StructUtils.nulllike(Nothing) || error("nulllike")
    StructUtils.keyeq(:id, "id") || error("keyeq")
    # temporal lifts hand-parse ISO 8601; the Dates constructors are not
    # statically compilable
    StructUtils.make(TrimTemporal, Dict(
        "day" => "2026-08-07",
        "stamp" => "2026-08-07T15:00:00.076",
        "tick" => "12:30:15.25",
    )) == TrimTemporal(
        Date(2026, 8, 7),
        DateTime(2026, 8, 7, 15, 0, 0, 76),
        Time(12, 30, 15, 250),
    ) || error("temporal make")
    StructUtils.unknownfield(StructUtils.DefaultStyle(), TrimTagged, :extra, 1) === nothing || error("unknownfield")
    StructUtils.fielddefault(TrimStyle(), TrimSimpleDefaults, :a) == 1 || error("fielddefault")
    StructUtils.fielddefaults(TrimStyle(), TrimSimpleDefaults).b == "ok" || error("fielddefaults")
    StructUtils.fieldtags(TrimStyle(), TrimTagged, :id).name == "identifier" || error("fieldtags")
    StructUtils.discover_dims(StructUtils.DefaultStyle(), [1, 2, 3], 1) == (3,) || error("discover_dims")
    return nothing
end

function _assert_trim_applyeach(tagged::TrimTagged)::Nothing
    count = Ref(0)
    saw_id = Ref(false)
    saw_code = Ref(false)
    ret = StructUtils.applyeach(TrimStyle(), tagged) do k, v
        count[] += 1
        if k == "identifier"
            v == 1 || error("applyeach id")
            saw_id[] = true
        elseif k == :code
            v == 7 || error("applyeach code")
            saw_code[] = true
        end
        return nothing
    end
    ret === :trim_state || error("applyeach state")
    count[] == 2 || error("applyeach count")
    saw_id[] || error("applyeach id seen")
    saw_code[] || error("applyeach code seen")

    early = StructUtils.applyeach(TrimStyle(), tagged) do k, v
        k == :code && return StructUtils.EarlyReturn(v)
        return nothing
    end
    early isa StructUtils.EarlyReturn || error("EarlyReturn")
    early.value == 7 || error("EarlyReturn value")
    return nothing
end

function _assert_trim_selectors(tagged::TrimTagged)::Nothing
    _ = tagged
    record = TrimSelectorRecord(3, 7)
    propertynames(record) == [:key, :value] || error("selector propertynames")
    record.key == 3 || error("selector property")
    record[:value] == 7 || error("selector getindex")
    return nothing
end

function run_make_trim_sample()::Nothing
    _assert_trim_public_traits()

    a = StructUtils.make(TrimA, (a=1, b=2, c=3, d=4))
    a.a == 1 || error("TrimA.a")
    a.d == 4 || error("TrimA.d")

    a2 = StructUtils.make(TrimA, Dict{Symbol,Int}(:a => 1, :b => 2, :c => 3, :d => 4))
    a2.a == 1 || error("TrimA Dict")

    b1 = StructUtils.make(TrimB, (a=1, b=2, c=3, d=4))
    b1.c == 3 || error("TrimB all")

    b2 = StructUtils.make(TrimB, (a=1, b=2))
    b2.c == 0 || error("TrimB defaults")

    c1 = StructUtils.make(TrimC, (a=42,))
    c1.b == "42" || error("TrimC computed")

    c2 = StructUtils.make(TrimC, (a=42, b="hello"))
    c2.b == "hello" || error("TrimC provided")

    d1 = StructUtils.make(TrimD, (a=5,))
    d1.b == 15 || error("TrimD.b")
    d1.c == 20 || error("TrimD.c")

    e = StructUtils.make(TrimE, (a=10, b=20))
    e.a == 10 || error("TrimE.a")
    StructUtils.make!(e, (a=11, b=21))
    e.b == 21 || error("make!")
    StructUtils.reset!(e)
    e.a == 0 || error("reset!")

    nt = StructUtils.make(@NamedTuple{a::Int, b::Int}, (a=1, b=2))
    nt.a == 1 || error("NamedTuple")

    vector = StructUtils.make(Vector{Int}, (1, 2, 3))
    vector == [1, 2, 3] || error("Vector")

    point_dict = StructUtils.make(Dict{TrimPoint, Int}, Dict("1,2" => 12), TrimStyle())
    point_dict[TrimPoint(1, 2)] == 12 || error("liftkey")

    tagged = StructUtils.make(
        TrimTagged,
        (identifier=1, code=7),
        TrimStyle(),
    )
    tagged.id == 1 || error("TrimTagged id")
    tagged.ignored == 99 || error("TrimTagged default")

    _assert_trim_applyeach(tagged)
    _assert_trim_selectors(tagged)

    car = StructUtils.make(TrimVehicle, TrimCarSource(4, 4))
    car == TrimCar(4, 4) || error("@choosetype car")

    truck = StructUtils.make(TrimVehicle, TrimTruckSource(10, 3))
    truck == TrimTruck(10, 3) || error("@choosetype truck")

    scalar = StructUtils.make(TrimPoint, "8,9")
    scalar == TrimPoint(8, 9) || error("@nonstruct lift")

    return nothing
end

function @main(args::Vector{String})::Cint
    _ = args
    run_make_trim_sample()
    return 0
end

Base.Experimental.entrypoint(main, (Vector{String},))
