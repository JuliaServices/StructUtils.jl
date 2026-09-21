using InteractiveUtils, Pkg

versioninfo(verbose=true)
root = dirname(@__DIR__)
out = mkpath(joinpath(root, "trim-diagnosis"))
project = mkpath(joinpath(out, "project"))
Pkg.activate(project)
Pkg.develop(path=root)
Pkg.add("JuliaC")
Pkg.status(; mode=Pkg.PKGMODE_MANIFEST)
using JuliaC
run(`$(JuliaC.get_compiler_cmd()) --version`)

# Reuse the timeout and compilation helpers without running their test set.
harness = read(joinpath(@__DIR__, "trim_compile_tests.jl"), String)
include_string(Main, first(split(harness, "@testset \"Trim compile\" begin")), joinpath(@__DIR__, "trim_compile_tests.jl"))

minimal = joinpath(out, "minimal.jl")
write(minimal, """
function @main(args::Vector{String})::Cint
    ccall(:puts, Cint, (Cstring,), "ENTERED_MAIN")
    return 0
end
Base.Experimental.entrypoint(main, (Vector{String},))
""")
failures = String[]

# Run the package harness with its original command, environment, and output name.
original = read(joinpath(@__DIR__, "trim_compile_tests.jl"), String)
instrumented = replace(original,
    "println(\"[trim] temp environment ready\")" => "println(output); println(\"[trim] temp environment ready\")",
    "println(\"---- trim executable output (\$(script_file)) ----\")" => """
        capture = mktempdir($(repr(out)); prefix="failure-", cleanup=false)
        cp(bundle_dir, joinpath(capture, "bundle"))
        write(joinpath(capture, "compile.log"), output)
        if haskey(ENV, "TRIM_CDB")
            debugger_commands = "sxe -c \\\".exr -1; .ecxr; k; lm; r; u @rip-20 @rip+20; q\\\" av; g"
            _, debug_output, _ = _run_command_with_timeout(`\$(ENV["TRIM_CDB"]) -G -c \$debugger_commands \$(abspath(run_path))`; timeout_s=120.0, log_label="debugger")
            write(joinpath(capture, "debugger.log"), debug_output)
            println(debug_output)
        end
        println("---- trim executable output (\$(script_file)) ----")
        """,
    "for (script_file, output_name) in trim_workloads" => "for repetition in 1:10, (script_file, output_name) in trim_workloads")
write(joinpath(@__DIR__, "trim_compile_tests.jl"), instrumented)
try
    Pkg.test("StructUtils"; julia_args=["--startup-file=no"])
catch err
    push!(failures, "original package harness")
    showerror(stdout, err)
finally
    write(joinpath(@__DIR__, "trim_compile_tests.jl"), original)
end

for (label, ref) in [("minimal", nothing), ("release", "2a2f3e8839b944d1b47744728e1cc617270292c0"), ("main", "56601dbdcf654311813581c71cc893d4bee7e49b")]
    script = minimal
    if ref !== nothing
        checkout = joinpath(out, label * "-source")
        run(`git worktree add --detach $checkout $ref`)
        Pkg.develop(path=checkout)
        script = joinpath(checkout, "test", "make_trim_safe.jl")
    end
    for build in 1:3
        dir = mkpath(joinpath(out, "$label-$build"))
        cd(dir) do
            bundle = joinpath(dir, "bundle")
            code, output, timeout = _run_trim_compile(project, script, "probe"; bundle_dir=bundle)
            write("compile.log", output)
            println("DIAG compile label=$label build=$build exit=$code timeout=$timeout")
            if code != 0 || timeout
                push!(failures, "$label-$build compile")
                return
            end
            for attempt in 1:30
                code, output, timeout = _run_command_with_timeout(`$(joinpath(bundle, "bin", "probe.exe"))`; timeout_s=30.0, log_label="probe")
                write("run-$attempt.log", output)
                println("DIAG run label=$label build=$build attempt=$attempt exit=$code timeout=$timeout output=$(repr(output))")
                if code != 0 || timeout
                    push!(failures, "$label-$build run-$attempt")
                end
            end
            if any(startswith("$label-$build run"), failures) && haskey(ENV, "TRIM_CDB")
                commands = "sxe -c \".exr -1; .ecxr; k; lm; r; u @rip-20 @rip+20; q\" av; g"
                code, output, timeout = _run_command_with_timeout(`$(ENV["TRIM_CDB"]) -G -c $commands $(joinpath(bundle, "bin", "probe.exe"))`; timeout_s=120.0, log_label="debugger")
                write("debugger.log", output)
                println("DIAG debugger label=$label build=$build exit=$code timeout=$timeout\n$output")
            end
        end
    end
end
println("DIAG failures: ", failures)
isempty(failures) || error("Windows trim failures; see preserved artifacts")
