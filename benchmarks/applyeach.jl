# Run with an environment containing StructUtils and Chairmarks.
# Compare pinned base/head versions in separate processes. Output is TSV:
# workload, median seconds, allocations, bytes.
using StructUtils, Chairmarks, Statistics
mutable struct SumSink
 total::Float64
end
(s::SumSink)(k,v) = (s.total+=v;nothing)
const sink=SumSink(0.)
function scan(v)
 sink.total=0.
 StructUtils.applyeach(StructUtils.DefaultStyle(),sink,v)
 sink.total
end
function measure(name, f)
 b=median(@be f() seconds=1)
 println(join((name,b.time,b.allocs,b.bytes), '\t'));flush(stdout)
end
for (name,v) in [("Int",collect(1:100000)),("AnyInt",Any[i for i=1:100000]),("AnyMixed",repeat(Any[1,2.5,true],33333)),("Real",Real[i for i=1:100000]),("Union",repeat(Union{Int,Float64,Bool}[1,2.5,true],33333))]
 @assert scan(v)==sum(v)
 measure("scan_"*name,()->scan(v))
end
