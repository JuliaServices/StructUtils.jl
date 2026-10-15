module StructUtilsDataFramesExt

using StructUtils, DataFrames

# A row is its column name => value pairs and a data frame its column name =>
# column pairs (like `Tables.columntable`), rather than their internal fields.
function StructUtils.applyeach(st::StructUtils.StructStyle, f, x::DataFrameRow)
    for (k, v) in pairs(x)
        ret = f(StructUtils.lowerkey(st, k), StructUtils.lower(st, v))
        ret isa StructUtils.EarlyReturn && return ret
    end
    return StructUtils.defaultstate(st)
end

function StructUtils.applyeach(st::StructUtils.StructStyle, f, x::AbstractDataFrame)
    for (k, v) in pairs(eachcol(x))
        ret = f(StructUtils.lowerkey(st, k), StructUtils.lower(st, v))
        ret isa StructUtils.EarlyReturn && return ret
    end
    return StructUtils.defaultstate(st)
end

end # module
