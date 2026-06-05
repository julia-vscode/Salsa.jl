module Salsa

# This is the entirety of the high-level Salsa API: Users create inputs and derived
# functions, and access them through a Runtime instance.
export @derived, @declare_input, Runtime, DerivedFunctionException

import MacroTools
# For registering DerivedFunctionException as a wrapped exception.
import ExceptionUnwrapping

# Per-derived-function tracing spans are emitted via the bundled TraceLogging submodule when
# tracing is enabled. This is currently shipped inside Salsa for convenience; it may be split
# out into a standalone package once it has matured.
include("TraceLogging.jl")

include("packagedef.jl")

end  # module
