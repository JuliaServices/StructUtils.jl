using StructUtils, Tables, Test

@testset "tables.jl" begin
    tbl = (a=[1, 2, 3], b=[4, 5, 6])
    tbl2 = [StructUtils.make(@NamedTuple{a::Int, b::Int}, row) for row in Tables.rows(tbl)]
    @test length(tbl2) == 3
    @test tbl2[1].a == 1
    @test tbl2[1].b == 4
    @test tbl2[2].a == 2
    @test tbl2[2].b == 5
    @test tbl2[3].a == 3
    @test tbl2[3].b == 6
    # a column table is its columns, not its fields
    cols = Tables.Columns((a=[1, 2], b=["x", "y"]))
    @test StructUtils.make(Dict{String,Any}, cols) == Dict("a" => [1, 2], "b" => ["x", "y"])
    @test StructUtils.make(@NamedTuple{a::Vector{Int}, b::Vector{Int}}, Tables.dictcolumntable(tbl)) ==
        (a=[1, 2, 3], b=[4, 5, 6])
    @test StructUtils.make(Dict{String,Any}, Tables.table([1 2; 3 4]; header=[:p, :q])) ==
        Dict("p" => [1, 3], "q" => [2, 4])
end
