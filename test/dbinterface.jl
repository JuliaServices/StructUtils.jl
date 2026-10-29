using StructUtils, DBInterface, Tables, Test

# Database rows are `Tables.AbstractRow`s that are only valid until the cursor advances.
struct DBTestRow <: Tables.AbstractRow
    values::NamedTuple
end
Tables.columnnames(r::DBTestRow) = keys(getfield(r, :values))
Tables.getcolumn(r::DBTestRow, nm::Symbol) = getfield(r, :values)[nm]
Tables.getcolumn(r::DBTestRow, i::Int) = getfield(r, :values)[i]

# A row cursor that, like a database result stream, can be iterated only once.
mutable struct OnePassCursor <: DBInterface.Cursor
    rows::Vector{NamedTuple}
    started::Bool
end
OnePassCursor(rows) = OnePassCursor(rows, false)
Tables.isrowtable(::Type{OnePassCursor}) = true
Base.IteratorSize(::Type{OnePassCursor}) = Base.SizeUnknown()
function Base.iterate(c::OnePassCursor, i=1)
    if i == 1
        c.started && error("cursor already iterated")
        c.started = true
    end
    return i > length(c.rows) ? nothing : (DBTestRow(c.rows[i]), i + 1)
end

# A cursor with only column access, like ODBC.jl's default cursor.
struct ColumnCursor <: DBInterface.Cursor
    columns::NamedTuple
end
Tables.istable(::Type{ColumnCursor}) = true
Tables.columnaccess(::Type{ColumnCursor}) = true
Tables.columns(c::ColumnCursor) = c.columns

struct DBTestUser
    id::Int
    name::String
end

struct DBTestConnection <: DBInterface.Connection
    password::String
end
struct DBTestStatement <: DBInterface.Statement
    conn::DBTestConnection
end

@testset "DBInterface extension" begin
    rows = NamedTuple[(id=1, name="a"), (id=2, name="b")]
    @test StructUtils.arraylike(OnePassCursor(rows))
    @test StructUtils.make(Vector{DBTestUser}, OnePassCursor(rows)) == [DBTestUser(1, "a"), DBTestUser(2, "b")]
    @test StructUtils.make(Vector{Dict{String,Any}}, OnePassCursor(rows)) ==
        [Dict("id" => 1, "name" => "a"), Dict("id" => 2, "name" => "b")]
    @test StructUtils.make(Vector{DBTestUser}, OnePassCursor(NamedTuple[])) == DBTestUser[]
    @test StructUtils.make(Vector{DBTestUser}, ColumnCursor((id=[1, 2], name=["a", "b"]))) ==
        [DBTestUser(1, "a"), DBTestUser(2, "b")]
    # keys are row indices, and an early return stops before the cursor reads further
    seen = Any[]
    ret = StructUtils.applyeach(OnePassCursor(rows)) do i, row
        push!(seen, (i, Tables.getcolumn(row, :id)))
        return StructUtils.EarlyReturn(i)
    end
    @test ret == StructUtils.EarlyReturn(1)
    @test seen == [(1, 1)]
    # connections and statements are not traversed as structs
    @test !StructUtils.structlike(DBTestConnection("secret"))
    @test !StructUtils.structlike(DBTestStatement(DBTestConnection("secret")))
end
