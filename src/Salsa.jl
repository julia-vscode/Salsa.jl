module Salsa

# Document this Module via the README.md file.
@doc let path = joinpath(dirname(@__DIR__), "README.md")
    include_dependency(path)
    replace(read(path, String), "```julia" => "```jldoctest")
end Salsa

# This is the entirety of the high-level Salsa API: Users create inputs and derived
# functions, and access them through a Runtime instance.
export @derived, @declare_input, Runtime, DerivedFunctionException

import MacroTools
# For registering DerivedFunctionException as a wrapped exception.
import ExceptionUnwrapping

using Base: @lock

include("Debug.jl")
using .Debug

# Computational results and demand are identified via dependency keys.
include("dependency_keys.jl")

# Things go wrong, it happens.
include("exceptions.jl")

# For debugging, Salsa can keep its own stack trace with additional information. This is
# also used to detect dependency cycles.
include("stack.jl")

# A trace data structure is used to keep track of dependencies as a derived function
# executes. Traces are somewhat heavy to allocate, so we keep a pre-allocated pool of them.
include("trace.jl")
include("trace_pool.jl")

# Salsa supports pluggable storage backends. These are responsible for storing the inputs,
# derived values, and dependency information discovered by Salsa.
include("storage_api.jl")

# The runtime API is Salsa's internal API. Different runtime types exist for different
# purposes.
include("runtime_api.jl")
include("runtime_generic.jl")
include("runtime_top_level.jl")
include("runtime_tracing.jl")

# --- Macro utils -----

# Return an array of the Symbol names of every argument in a function definition. To ensure
# every argument is named, for unnamed arguments (e.g. `_` or `::Int`), we _generate_ a name
# via `gensym`.
function _argnames(args)
    [
        name === nothing || name === :_ ? gensym("_$i") : name
        for (i, name) in enumerate(first.(map(MacroTools.splitarg, args)))
    ]
end

# Return an array of the Types of every argument in a function definition.
function _argtypes(args)
    getindex.(map(MacroTools.splitarg, args), Ref(2))
end

# ---------------

# Salsa's high-level API consists of two macros, one for declaring derived functions, the
# other for declaring inputs.
include("derived_macro.jl")
include("declare_input_macro.jl")


# Provide Storage-major view on Runtime types for ease of implementing Storage backends.
const RuntimeWithStorage{ST,CT} = Runtime{CT,ST}
const _TopLevelRuntimeWithStorage{ST,CT} = _TopLevelRuntime{CT,ST}
const _TracingRuntimeWithStorage{ST,CT} = _TracingRuntime{CT,ST}

# Default context, manual storage
RuntimeWithStorage{ST}(st = ST()) where {ST} = Runtime{EmptyContext,ST}(EmptyContext(), st)

include("inspect.jl")

# The Salsa package provides a default storage layer to back Salsa Runtimes, which
# implements a simple key-value store using in-memory dictionaries.
include("default_storage.jl")
using ._DefaultSalsaStorage: DefaultStorage, DefaultRuntime

# Simpler print function for default Runtime implementation.
function Base.show(io::IO, rt::_TopLevelRuntime{EmptyContext,DefaultStorage})
    print(io, "Salsa.Runtime($(rt.storage))")
end

function __init__()
    # Init the freelists at runtime based on the number of threads julia is configured with.
    _init_thread_local_pools_and_freelists()
end

end  # module
