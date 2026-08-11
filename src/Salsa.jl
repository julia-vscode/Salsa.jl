module Salsa

# This is the entirety of the high-level Salsa API: Users create inputs and derived
# functions, and access them through a Runtime instance.
export @derived, @declare_input, Runtime, DerivedFunctionException

import MacroTools
# For registering DerivedFunctionException as a wrapped exception.
import ExceptionUnwrapping
# For first-class cancellation support (see cancellation.jl).
import CancellationTokens
using CancellationTokens:
    CancellationToken, OperationCanceledException, is_cancellation_requested

include("packagedef.jl")

end  # module
