module InspectSalsa
using Salsa

# Crude code to dump a .dot (graphviz) version of the Arroyo Node tree.
# For debugging / illustration purposes - not intended to be reachable from production
# code.
function dump_graph(c::Salsa.AbstractComponent; module_boxes=false)
    println(build_graph(c; module_boxes=module_boxes))
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

function build_graph(c::Salsa.AbstractComponent; module_boxes=false)
    rt = Salsa.get_runtime(c)
    io = IOBuffer()
    println(io, """digraph G {""")
    println(io, """edge [dir="back"];""")
    seen = _IdSet{Any}()
    modules_map = Dict{Module,Set}()
    edges = Dict{Pair, Int}()
    inputs = Dict{Any, String}()  # Note, there might be duplicate strings, Any must be the key

    _build_graph(io, c, seen, modules_map, edges, inputs)

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
        println(io, """$(vertex_name(input_key)) [label="$name"]""")
    end
    if !module_boxes
        println(io, "}")
    end

    max_count = maximum(values(edges))
    maxwidth = 10
    for ((a,b), count) in edges
        normwidth = 1 + maxwidth * (count / max_count)
        println(io, """ "$(vertex_name(a))" -> "$(vertex_name(b))"  [penwidth=$normwidth, weight=$normwidth]""")
    end
    @show max_count
    println(io, "}")
    return String(take!(io))
end

function _build_graph(io::IO, c::Salsa.AbstractComponent, seen::_IdSet, modules_map::Dict, edges::Dict, inputs::Dict)
    rt = c.runtime
    m = typeof(c).name.module
    for fieldname in fieldnames(typeof(c))
        f = getfield(c, fieldname)
        if f in seen
            continue
        else
            push!(seen, f)
        end
        if f isa Salsa.AbstractComponent
            @show typeof(f)
            _build_graph(io, f, seen, modules_map, edges, inputs)
        elseif f isa Salsa.InputTypes
            key = Salsa.InputKey(f)
            inputs[key] = "@input: $(fieldname)"
            push!(get!(modules_map, m, Set([])), key)
        end
    end

    for (derived_key,derived_map) in rt.derived_function_maps
        _build_graph(io, rt, derived_key, derived_map, seen, modules_map, edges)
    end
end

function vertex_name(c::Any)::String
    return "v$(objectid(c))"
end

function _build_graph(io, rt::Salsa.Runtime, derived_key::Salsa.DerivedKey{F,TT}, derived_map::Dict,
     seen::_IdSet{Any}, modules_map::Dict{Module,Set}, edges::Dict{Pair, Int}) where {F,TT}
    in(derived_key, seen) && return
    push!(seen, derived_key)
    m = methods(F.instance).mt.module
    push!(get!(modules_map, m, Set([])), derived_key)
    println(io, "$(vertex_name(derived_key)) [shape=rect,label=\"$(derived_key)\"]")
    #println(io, "$(vertex_name(derived_key)) [label=\"$(derived_key)\"]")
    for (k,v) in derived_map
        for d in v.dependencies
            edge = (derived_key) => (d.key) 
            count = get!(edges, edge, 0) + 1
            edges[edge] = count
        end
    end
    #_build_graph(io, s.leaf_node, seen)
end

end # module