module StructUtilsLazilyInitializedFieldsExt

using StructUtils
using LazilyInitializedFields: islazyfield, uninit

const Uninitialized = typeof(uninit)

@inline function StructUtils._unionmember(
    style::StructUtils.StructStyle,
    ::Type{T},
    ::Type{Uninitialized},
    @nospecialize(source),
) where {T}
    source isa Uninitialized && return Uninitialized

    logical = Base.typesplit(Base.unwrap_unionall(T), Uninitialized)
    if logical <: Union{Missing,Nothing} && !StructUtils.nulllike(style, source)
        throw(ArgumentError("cannot construct lazy field type `$T` from non-null source"))
    end
    return logical
end

@inline function StructUtils._missingfield(
    ::StructUtils.StructStyle,
    T::Type,
    key::Symbol,
    defaults::NamedTuple,
)
    haskey(defaults, key) && return defaults[key]
    if applicable(islazyfield, T, key) && islazyfield(T, key)
        return uninit
    end
    return nothing
end

end # module
