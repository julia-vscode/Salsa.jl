module Inspect
using ..Salsa
using .Salsa._DefaultSalsaStorage: DefaultStorage

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
    for (input_key_func, name) in inputs
        println(io, """$(vertex_name(input_key_func)) [label="$name"]""")
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

function _build_graph(io::IO, st::DefaultStorage, seen::_IdSet, modules_map::Dict, out_edges::Dict, out_inputs::Dict)
    for (input_key,v) in st.inputs_map
        F = key_function(input_key)
        m = F.name.module
        out_inputs[F] = "@input $(nameof(F.instance))"
        push!(get!(modules_map, m, Set([])), F)
    end

    for (derived_key_t,derived_map) in st.derived_function_maps
        _build_graph(io, st, derived_key_t, derived_map, seen, modules_map, out_edges)
    end
end

key_function(::Union{Salsa.InputKey{F}, Salsa.DerivedKey{F}}) where F = F
function vertex_name(::Salsa.InputKey{F}) where F
    return vertex_name(F)
end
function vertex_name(x::Any)::String
    return "v$(objectid(x))"
end

function _build_graph(io, st::DefaultStorage, derived_key_t::Type{<:Salsa.DerivedKey{F,TT}}, derived_map::Dict,
     seen::_IdSet{Any}, modules_map::Dict{Module,Set}, edges::Dict{Pair, Int}) where {F,TT}
    in(derived_key_t, seen) && return
    push!(seen, derived_key_t)
    m = methods(F.instance).mt.module
    push!(get!(modules_map, m, Set([])), F)
    key_str = _derived_key_as_call_str(derived_key_t)
    println(io, "$(vertex_name(F)) [shape=rect,label=\"$key_str\"]")
    for (k,v) in derived_map
        for d in v.dependencies
            edge = (F) => (key_function(d))
            count = get!(edges, edge, 0) + 1
            edges[edge] = count
        end
    end
    #_build_graph(io, s.leaf_node, seen)
end

function _derived_key_as_call_str(::Type{<:Salsa.DerivedKey{F,TT}})::String where {F,TT}
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
