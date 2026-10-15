using StructUtils, DataFrames, Test

@testset "DataFrames extension" begin
    df = DataFrame(a=[1, 2], b=["x", "y"])
    @test StructUtils.make(Dict{String,Any}, df[2, :]) == Dict("a" => 2, "b" => "y")
    @test StructUtils.make(@NamedTuple{a::Int, b::String}, df[1, :]) == (a=1, b="x")
    @test StructUtils.make(Dict{String,Any}, df) == Dict("a" => [1, 2], "b" => ["x", "y"])
    @test StructUtils.make(Dict{String,Any}, view(df, 2:2, [:b])) == Dict("b" => ["y"])
end
