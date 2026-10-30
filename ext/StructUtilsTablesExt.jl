module StructUtilsTablesExt

using StructUtils, Tables

function StructUtils.applyeach(st::StructUtils.StructStyle, f, x::T) where {T <: Tables.AbstractRow}
    for nm in Tables.columnnames(x)
        ret = f(StructUtils.lowerkey(st, nm), StructUtils.lower(st, Tables.getcolumn(x, nm)))
        ret isa StructUtils.EarlyReturn && return ret
    end
    return StructUtils.defaultstate(st)
end

# A column table, such as an `Arrow.Table` or `Tables.Columns(x)`, is its column
# name => column pairs (like a `DataFrame` or `Tables.columntable`), not its
# internal fields.
function StructUtils.applyeach(st::StructUtils.StructStyle, f, x::Tables.AbstractColumns)
    for nm in Tables.columnnames(x)
        ret = f(StructUtils.lowerkey(st, nm), StructUtils.lower(st, Tables.getcolumn(x, nm)))
        ret isa StructUtils.EarlyReturn && return ret
    end
    return StructUtils.defaultstate(st)
end

end # module
