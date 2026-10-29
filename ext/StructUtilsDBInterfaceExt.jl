module StructUtilsDBInterfaceExt

using StructUtils, DBInterface, Tables

# A database cursor is an array of its rows, not its internal fields:
# `StructUtils.make(Vector{T}, cursor)` makes one `T` per row, and JSON.jl writes
# an array of row objects. `applyeach` iterates the cursor once and passes each
# row on before advancing, since a row may only be valid until then.
StructUtils.arraylike(::Type{<:DBInterface.Cursor}) = true

function StructUtils.applyeach(st::StructUtils.StructStyle, f, x::DBInterface.Cursor)
    # `Tables.rows` also gives rows for a cursor with only column access.
    for (i, row) in enumerate(Tables.rows(x))
        ret = f(StructUtils.lowerkey(st, i), StructUtils.lower(st, row))
        ret isa StructUtils.EarlyReturn && return ret
    end
    return StructUtils.defaultstate(st)
end

# Connections and prepared statements are not data. Their fields hold sockets
# and connection options, so they are not traversed; JSON.jl writes `string(x)`.
StructUtils.structlike(::Type{<:DBInterface.Connection}) = false
StructUtils.structlike(::Type{<:DBInterface.Statement}) = false

end # module
