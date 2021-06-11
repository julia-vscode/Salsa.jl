module Inspect
using ..Salsa
using ..Salsa: AbstractSalsaStorage

"""
    Salsa.Inspect.all_inputs_and_derived_vals(st::SalsaStorage)

Should be implemented by salsa storage backends, to support generating a dependency graph
from the current state of a Salsa Runtime.

Returns two dicts (inputs: K=>V and derived_vals: K=>(V, K[deps...]))
"""
function all_inputs_and_derived_vals end


# Crude code to dump a .dot (graphviz) version of the Salsa Node tree.
# For debugging / illustration purposes - not intended to be reachable from production
# code.
function dump_graph(rt::Runtime; module_boxes=false)
    println(build_graph(rt; module_boxes=module_boxes))
end

# There is no IdSet in julia, so we build our own from IdDict.
# We need an IdSet to prevent comparing all the contents of the various maps, which is
# expensive. We just want to compare via `===` which is what an IdDict does.
# (Used the name _IdSet to indicate that this is internal only, and not a full-featured set)
struct _IdSet{T}
    d::IdDict{T,Nothing}  # Only using the Keys
    _IdSet{T}() where T = new(IdDict{T,Nothing}())
end
Base.in(k, s::_IdSet) = haskey(s.d, k)
Base.push!(s::_IdSet, k) = s.d[k] = nothing

function build_graph(rt::Salsa.Runtime; module_boxes=false)
    io = IOBuffer()
    println(io, """digraph G {""")
    println(io, """edge [dir="back"];""")
    seen = _IdSet{Any}()
    modules_map = Dict{Module,Set}()
    edges = Dict{Pair, Int}()
    inputs = Dict{Any, String}()  # Note, there might be duplicate strings, Any must be the key

    _build_graph(io, rt.storage, seen, modules_map, edges, inputs)

    if module_boxes
        for (m,keys) in modules_map
            mname = nameof(m)
            println(io, """subgraph cluster_$mname {""")
            #println(io, """node [style=filled];""")
            # First print non-input keys
            println(io, """ $(join((repr(vertex_name(k)) for k in keys
                                   if !haskey(inputs, k)), " ")) """);
            begin  # Then print input keys all on the bottom
                println(io, "{")
                println(io, "rank=sink;")
                println(io, """ $(join((repr(vertex_name(k)) for k in keys
                                    if haskey(inputs, k)), " ")) """);
                println(io, "}")
            end
            println(io, """ label = "Module `$mname`"; """)
            println(io, """ fontsize = 25; """)
            println(io, """ color=blue; """)
            println(io, """}""")
        end
    end

    if !module_boxes
        println(io, "{")
        println(io, "rank=sink;")
    end
    for (input_key, name) in inputs
        println(io, """$(vertex_name(input_key)) [label=$(repr(name))]""")
    end
    if !module_boxes
        println(io, "}")
    end

    max_count = length(edges) == 0 ? 0 : maximum(values(edges))
    maxwidth = 10
    for ((a,b), count) in edges
        normwidth = 1 + maxwidth * (count / max_count)
        println(io, """ "$(vertex_name(a))" -> "$(vertex_name(b))"  [penwidth=$normwidth, weight=$normwidth]""")
    end
    @show max_count
    println(io, "}")
    return String(take!(io))
end

function _build_graph(io::IO, st::AbstractSalsaStorage, seen::_IdSet, modules_map::Dict, out_edges::Dict, out_inputs::Dict)
    inputs, derived_vals = all_inputs_and_derived_vals(st)
    for (k,v) in inputs
        F = key_func_type(k)
        m = F.name.module
        out_inputs[k] = "@input $(nameof(F.instance))"
        push!(get!(modules_map, m, Set([])), k)
    end

    seen_key_types = Set{Any}()
    for (k,(v, deps)) in derived_vals
        K = key_identifier(k)
        if !(K ∈ seen_key_types)
            push!(seen_key_types, K)
            key_str = _derived_key_as_call_str(k)
            println(io, "$(vertex_name(k)) [shape=rect,label=$(repr(key_str))]")
            F = key_func_type(k)
            m = F.name.module
            push!(get!(modules_map, m, Set([])), k)
        end
        _build_graph(io, st, k, v, deps, seen, modules_map, out_edges)
    end
end

key_func_type(::Union{Salsa.InputKey{F},Salsa.DerivedKey{F}}) where F = F
# TODO: Change this to use key values, not just types, for unrolling the graph.
key_identifier(k::Union{Salsa.InputKey,Salsa.DerivedKey}) = typeof(k)

# TODO: Change these to use key values, not just types, for unrolling the graph.
function vertex_name(k::Salsa.InputKey)
    return vertex_name(typeof(k))
end
function vertex_name(k::Salsa.DerivedKey)
    return vertex_name(typeof(k))
end
function vertex_name(x::Any)::String
    return "v$(objectid(x))"
end

function _build_graph(io, st::AbstractSalsaStorage,
        derived_key, v, deps::Vector,  # Vector of salsa keys
        seen::_IdSet{Any}, modules_map::Dict{Module,Set}, edges::Dict{Pair, Int}) where {F,TT}
    in(derived_key, seen) && return
    push!(seen, derived_key)
    for d in deps
        edge = key_identifier(derived_key) => key_identifier(d)
        count = get!(edges, edge, 0) + 1
        edges[edge] = count
    end
    #_build_graph(io, s.leaf_node, seen)
end

function _derived_key_as_call_str(key::Salsa.DerivedKey{F,TT})::String where {F,TT}
    f = isdefined(F, :instance) ? nameof(F.instance) : nameof(F)
    argsexprs = [
        :rt,
        (Expr(:(::), fieldtype(TT, i)) for i = 1:fieldcount(TT))...,
    ]
    # Display f in a copy/paste executable way, by exploiting Expr printing.
    f_str = string(:(($f,)))[2:end-2]  # (wraps f name in var"" if needed)
    return "$f_str($(join(argsexprs, ", ")))"
end

end # module
