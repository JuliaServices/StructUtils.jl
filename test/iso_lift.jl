# The default lifts for Date, DateTime, and Time hand-parse ISO 8601 instead
# of calling the Dates constructors: the DateFormat machinery those route
# through is too dynamic for static compilation (`juliac --trim`). These
# tests pin the hand-rolled parsers to the exact grammar and error behavior
# of `Date(str)`, `DateTime(str)`, and `Time(str)`.
@testset "ISO 8601 default lifts match the Dates constructors" begin
    datetime_cases = [
        "2020", "2020-01", "2020-1", "2020-01-01", "2020-1-1",
        "2020-01-01T10", "2020-01-01T10:20", "2020-01-01T10:20:30",
        "2020-01-01T10:20:30.4", "2020-01-01T10:20:30.45",
        "2020-01-01T10:20:30.456", "2020-01-01T10:20:30.4567",
        "-0001-02-03T04:05:06.789", "12345-12-31T23:59:59.999",
        "+2020-01-01", "2020-01-01 10:20:30", "2020-01-01T10:20:30Z",
        "garbage", "", "2020-13-01", "2020-01-32", "2020-", "2020-01-",
        "2020-01T", "2020-01-01T", "2020-01-01T10:", "2020-01-01T10:20:",
        "2020-01-01T10:20:30.", "2020-01-01T.", "-", "+", ".",
    ]
    date_cases = [
        "2020-01-01", "2020-1-1", "2020", "12345-01-01", "-0001-02-03",
        "2020-01-01T10:20:30", "garbage", "2020-02-30", "", "2020-", "2020-01-",
    ]
    time_cases = [
        "10:20:30", "10:20", "10", "10:20:30.4", "10:20:30.456",
        "10:20:30.4567", "24:00:00", "10:61:00", "", "10:", "10:20:",
        "10:20:30.", "10.", "10:20:30. ",
    ]
    outcome(f, x) = try (:ok, f(x)) catch e; (:err, typeof(e)) end
    for c in datetime_cases
        @test outcome(x -> StructUtils.lift(DateTime, x), c) == outcome(DateTime, c)
    end
    for c in date_cases
        @test outcome(x -> StructUtils.lift(Date, x), c) == outcome(Date, c)
    end
    for c in time_cases
        @test outcome(x -> StructUtils.lift(Time, x), c) == outcome(Time, c)
    end

    # `string` output round-trips exactly for every field combination.
    for dt in (DateTime(2026, 8, 7, 15, 0, 0, 76), DateTime(2020, 1, 1), DateTime(-1, 2, 3, 4, 5, 6, 789))
        @test StructUtils.lift(DateTime, string(dt)) == dt
    end
    for d in (Date(2026, 8, 7), Date(-1, 2, 3), Date(12345, 1, 1))
        @test StructUtils.lift(Date, string(d)) == d
    end
    for t in (Time(1, 2, 3), Time(1, 2, 3, 10), Time(23, 59), Time(0))
        @test StructUtils.lift(Time, string(t)) == t
    end

    # SubStrings lift like Strings.
    @test StructUtils.lift(DateTime, SubString("x2020-01-01x", 2, 11)) == DateTime(2020, 1, 1)

    # And the struct path uses the same lifts.
    three = StructUtils.make(ThreeDates, Dict(
        "date" => "2026-08-07",
        "datetime" => "2026-08-07T15:00:00.076",
        "time" => "12:30:15.25",
    ))
    @test three == ThreeDates(
        Date(2026, 8, 7),
        DateTime(2026, 8, 7, 15, 0, 0, 76),
        Time(12, 30, 15, 250),
    )
end
