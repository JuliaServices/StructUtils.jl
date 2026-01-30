module StructUtilsStaticArraysCoreExt

using StructUtils
using StaticArraysCore: StaticArray, size_to_tuple

StructUtils.fixedsizearray(::Type{<:StaticArray}) = true

StructUtils.discover_dims(style, ::Type{<:StaticArray{S}}, source) where {S<:Tuple} =
    size_to_tuple(S)

StructUtils.arrayfromdata(::Type{T}, mem::Memory, dims::Tuple) where {T<:StaticArray} =
    T(Tuple(mem))

end # module
