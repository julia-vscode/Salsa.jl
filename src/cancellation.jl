# First-class cancellation support, built on CancellationTokens.jl.
#
# The cancellation token rides on the top-level Runtime, just like the user context: it is
# per-call metadata that does not affect Salsa invalidation and is excluded from
# memoization keys. The Salsa machinery polls the token on every graph edge (in
# `memoized_lookup`) and during cache-validation walks, and user code inside derived
# functions can poll it via `throw_if_cancellation_requested(rt)`.

"""
    Salsa.with_cancellation(rt::Runtime, token::CancellationTokens.CancellationToken)

Return a new top-level `Runtime` sharing this runtime's storage and context, whose
derived-function calls observe `token`. Salsa polls the token between derived-function
calls (and during cache validation) and throws
`CancellationTokens.OperationCanceledException` — unwrapped, never inside a
`DerivedFunctionException` — once cancellation is requested. A cancelled call caches
nothing; the storage remains fully usable afterwards.

Safe for concurrent use: multiple `with_cancellation` runtimes with different tokens may
run top-level calls against the same storage simultaneously.

NOTE: An `OperationCanceledException` that crosses a task boundary inside a derived
function (e.g. via a `TaskFailedException` from `@spawn`) is still wrapped in
`DerivedFunctionException`; use `ExceptionUnwrapping.has_wrapped_exception` to detect it.
"""
function with_cancellation(
    rt::_TopLevelRuntime{CT,ST}, token::CancellationToken
) where {CT,ST}
    return _TopLevelRuntime{CT,ST}(rt.context, rt.storage, token)
end
function with_cancellation(::_TracingRuntime, ::CancellationToken)
    error("`with_cancellation` may not be called from inside a derived function.")
end

"""
    Salsa.with_cancellation(f, rt::Runtime, token)

Do-block form: call `f` with a runtime carrying `token`.

```julia
Salsa.with_cancellation(rt, token) do rt
    my_derived_function(rt, x)
end
```
"""
function with_cancellation(f, rt::Runtime, token::CancellationToken)
    return f(with_cancellation(rt, token))
end

"""
    Salsa.@cancellable token f(rt, args...)

Annotate an existing derived-function call so it observes `token`: rewrites the call to
`f(Salsa.with_cancellation(rt, token), args...)`.
"""
macro cancellable(token, call)
    if !(call isa Expr && call.head === :call && length(call.args) >= 2)
        error(
            "@cancellable expects a function call with at least one argument " *
            "(the Runtime), got: $call",
        )
    end
    new_call = Expr(
        :call,
        esc(call.args[1]),
        :($with_cancellation($(esc(call.args[2])), $(esc(token)))),
        map(esc, call.args[3:end])...,
    )
    return new_call
end

"""
    Salsa.cancellation_token(rt::Runtime)::Union{Nothing,CancellationTokens.CancellationToken}

The cancellation token attached to this runtime (via [`with_cancellation`](@ref)), or
`nothing` if the runtime is not cancellable. Works both on top-level runtimes and on the
runtime passed to derived functions.
"""
cancellation_token(rt::_TopLevelRuntime) = rt.cancellation_token
cancellation_token(rt::_TracingRuntime) = rt.tl_runtime.cancellation_token

"""
    Salsa.throw_if_cancellation_requested(rt::Runtime)

Throw `CancellationTokens.OperationCanceledException` if this runtime carries a token
whose cancellation has been requested; no-op otherwise. Call this from long-running user
code inside `@derived` functions to make them responsive to cancellation between Salsa
calls (the Salsa machinery already polls the token around every derived/input lookup).
"""
@inline function throw_if_cancellation_requested(rt::Runtime)
    token = cancellation_token(rt)
    if token !== nothing && is_cancellation_requested(token)
        throw(OperationCanceledException(token))
    end
    return nothing
end
