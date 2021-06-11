"""
    DerivedFunctionException(exc, salsa_trace)

Exceptions thrown from within any Salsa @derived function are wrapped in a
DerivedFunctionException(), which also captures the "Salsa Trace", which is the stack trace
of the Salsa functions (derived functions and input functions) that were being executed
when the exception was thrown.

The Salsa Trace is richer than a normal stacktrace: it includes the _values_ of all function
arguments (salsa keys). It is displayed in reverse order, so that the stack trace can be
read from the inside-out, together with the normal julia stacktrace.
"""
struct DerivedFunctionException{T} <: Base.Exception
    captured_exception::T
    salsa_trace::Vector{DependencyKey}
end
DerivedFunctionException(exc::T, trace) where {T} = DerivedFunctionException{T}(exc, trace)

function ExceptionUnwrapping.unwrap_exception(exc::DerivedFunctionException)
    exc.captured_exception
end

function Base.showerror(io::IO, exc::DerivedFunctionException)
    print(io, nameof(typeof(exc)))
    println(io, ": Error encountered while executing derived function:")
    Base.showerror(io, exc.captured_exception)
    println(io, "\n\n------ Salsa Trace -----------------")
    for (idx, dependency_key) in Iterators.reverse(enumerate(exc.salsa_trace))
        println(io, "[$idx] ", dependency_key)  # Uses pretty-printing for Traces defined below
    end
    println(io, "------------------------------------")
end

"""
    DependencyCycleException(problematic_depkey)

Exception thrown when a derived function stacktrace includes a cycle. The exception itself
only contains the problematic call that closed the cycle, the stacktrace itself is needed
for debugging and will get attached to the surrounding `DerivedFunctionException`.
"""
struct DependencyCycleException <: Base.Exception
    problematic_depkey
end

function Base.showerror(io::IO, exc::DependencyCycleException)
    print(io, nameof(typeof(exc)))
    println(io, ": Cycle in derived function stacktrace calling $(exc.problematic_depkey).")
end
