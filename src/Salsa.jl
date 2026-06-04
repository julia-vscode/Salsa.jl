module Salsa

# This is the entirety of the high-level Salsa API: Users create inputs and derived
# functions, and access them through a Runtime instance.
export @derived, @declare_input, Runtime, DerivedFunctionException

import MacroTools
# For registering DerivedFunctionException as a wrapped exception.
import ExceptionUnwrapping
# Per-derived-function tracing spans are emitted via TraceLogging when tracing is enabled.
import TraceLogging

include("packagedef.jl")

end  # module
