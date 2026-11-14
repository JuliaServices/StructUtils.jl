struct ShapeStyle <: StructUtils.StructStyle
    lifted::Vector{String}
end
StructUtils.lift(style::ShapeStyle, ::Type{Int}, x::String) = (push!(style.lifted, x); (parse(Int, x), :lifted))

struct StopShapeStyle <: StructUtils.StructStyle
    stop::Int
end
StructUtils.lift(style::StopShapeStyle, ::Type{Int}, x::Int) =
    (x, x == style.stop ? StructUtils.EarlyReturn(:stopped) : nothing)

mutable struct ShapeSource
    values::Vector{Any}
    visits::Int
end
ShapeSource(values) = ShapeSource(Any[values...], 0)
StructUtils.arraylike(::Type{ShapeSource}) = true
function StructUtils.applyeach(style::ShapeStyle, f, source::ShapeSource)
    source.visits += 1
    source.visits == 1 || error("source traversed twice")
    for (i, value) in enumerate(source.values)
        result = f(StructUtils.lowerkey(style, i), StructUtils.lower(style, value))
        result isa StructUtils.EarlyReturn && return result
    end
    return :shape_source
end

@testset "multidimensional input shape" begin
    for E in (Int, String)
        a, b, c, d, extra = E === Int ? (1, 2, 3, 4, 5) : ("a", "b", "c", "d", "e")
        for T in (Matrix{E}, SMatrix{2,2,E}, MMatrix{2,2,E})
            @test StructUtils.make(T, [[a, b], [c, d]]) == [a c; b d]
            @test_throws DimensionMismatch StructUtils.make(T, [[a, b], [c]])
            @test_throws DimensionMismatch StructUtils.make(T, [[a, b], E[]])
            @test_throws DimensionMismatch StructUtils.make(T, [[a, b], [c, d, extra]])
        end
        for T in (SMatrix{2,2,E}, MMatrix{2,2,E})
            @test_throws DimensionMismatch StructUtils.make(T, [[a, b]])
            @test_throws DimensionMismatch StructUtils.make(T, [[a, b], [c, d], [a, b]])
            @test_throws DimensionMismatch StructUtils.make(T, [a, b])
        end
        @test_throws DimensionMismatch StructUtils.make(Array{E,3}, [[[a, b], [c, d]], [[a, b]]])
        @test_throws DimensionMismatch StructUtils.make(Array{E,3}, [[[a, b]], [[c]]])
    end

    # Leaf vectors are values, so only their enclosing matrix must be rectangular.
    leaves = [[[1, 2], [3]], [[4, 5, 6], [7, 8]]]
    @test StructUtils.make(Matrix{Vector{Int}}, leaves) == reshape([[1, 2], [3], [4, 5, 6], [7, 8]], 2, 2)
    @test StructUtils.make(SMatrix{1,2,Int}, [1, 2]) == SMatrix{1,2,Int}(1, 2)
    @test StructUtils.make(SMatrix{1,2,Int}, Any[[1], 2]) == SMatrix{1,2,Int}(1, 2)
    @test StructUtils.make(SArray{Tuple{1,1,2},Int,3,2}, [1, 2])[:] == [1, 2]
    @test StructUtils.make(SMatrix{0,2,Int}, [Int[], Int[]]) == SMatrix{0,2,Int}()
    @test StructUtils.make(SMatrix{2,0,Int}, Any[]) == SMatrix{2,0,Int}()
    @test_throws DimensionMismatch StructUtils.make(SMatrix{0,2,Int}, [1, 2])
    @test_throws DimensionMismatch StructUtils.make(SMatrix{0,2,Int}, [Int[], Int[], Int[]])
    @test size(StructUtils.make(Matrix{Int}, [Int[], Int[]])) == (0, 2)
    @test StructUtils.make(Matrix{Int}, [1, 2]) == [1, 2]
    @test StructUtils.make(Array{Int,3}, [[1, 2], [3, 4]]) == [1 3; 2 4]

    first = ShapeSource(["1", "2"])
    second = ShapeSource(["3", "4"])
    source = ShapeSource([first, second])
    style = ShapeStyle(String[])
    value, state = StructUtils.make(style, SMatrix{2,2,Int}, source)
    @test value == [1 3; 2 4]
    @test state === :shape_source
    @test source.visits == first.visits == second.visits == 1
    @test style.lifted == ["1", "2", "3", "4"]
    @test_throws DimensionMismatch StructUtils.make(ShapeStyle(String[]), SMatrix{2,2,Int}, ShapeSource([ShapeSource(["1", "2"]), ShapeSource(["3"])]))
    @test_throws DimensionMismatch StructUtils.make(SMatrix{2,2,Int}, [1 => [1, 2], 1 => [3, 4]])
    @test StructUtils.make(SMatrix{2,2,Int}, [2 => [3, 4], 1 => [1, 2]]) == [1 3; 2 4]
    @test StructUtils.make(SMatrix{2,2,Int}, [1 => [1, 2], 2 => [3, 4], 1 => [5, 6]]) == [5 3; 6 4]
    @test StructUtils.make(SArray{Tuple{0,2,2},Int,3,0}, [[Int[], Int[]], [Int[], Int[]]]) == SArray{Tuple{0,2,2},Int,3,0}()

    throwing_style = ShapeStyle(String[])
    @test_throws ArgumentError StructUtils.make(throwing_style, SMatrix{2,2,Int}, [["1", "bad"], ["3", "4"]])
    @test throwing_style.lifted == ["1", "bad"]
    @test StructUtils.make(throwing_style, SMatrix{2,2,Int}, [["1", "2"], ["3", "4"]])[1] == [1 3; 2 4]
    @test throwing_style.lifted == ["1", "bad", "1", "2", "3", "4"]
    stopped, state = StructUtils.make(StopShapeStyle(4), SMatrix{2,2,Int}, [[1, 2], [3, 4]])
    @test stopped == [1 3; 2 4]
    @test state isa StructUtils.EarlyReturn && state.value === :stopped
    @test_throws DimensionMismatch StructUtils.make(StopShapeStyle(2), SMatrix{2,2,Int}, [[1, 2], [3, 4]])

    # Updating an existing matrix remains a partial update.
    target = fill(9, 2, 2)
    StructUtils.make!(target, [[1]])
    @test target == [1 9; 9 9]
end
