# NOTE: We put ContextType first, above, since that's the paramater that users are more
# likely to tweak, whereas the Storage type is more of an internal detail.

# Users call this `Runtime()` function, and we generate them a `_TopLevelRuntime()`.
function Runtime{CT,ST}(ctx::CT = CT(), st::ST = ST()) where {CT,ST<:AbstractSalsaStorage}
    return _TopLevelRuntime{CT,ST}(ctx, st)
end

# By default, Runtime() use the DefaultStorage provided by Salsa in default_storage.jl, and
# does not specify any custom context.
Runtime{CT}(ctx::CT = CT()) where {CT} = Runtime{CT,DefaultStorage}(ctx, DefaultStorage())
Runtime(st) = Runtime{EmptyContext,DefaultStorage}(EmptyContext(), st)
Runtime() = Runtime(DefaultStorage())  # Equivalent to DefaultRuntime()

# This is the top-level Runtime object which users will instantiate for using Salsa. We mark
# the _TopLevelRuntime as mutable, so that we can take its pointer_from_objref(). It will
# very likely be not isbits anyway, since the users' storage objects are unlikely to be
# isbits. It's a bit annoying that this is mutable, because its contents really musn't ever
# change, but here we are.
mutable struct _TopLevelRuntime{CT,ST<:AbstractSalsaStorage} <: Runtime{CT,ST}
    # Store the user-provided context object, where they can store per-runtime meta-data
    # that does not affect Salsa invalidation / re-evaluation.
    context::CT

    # The storage is where all the tracking of state and invalidation happens.
    storage::ST
end

########## Implementation of Runtime API

context(rt::_TopLevelRuntime) = rt.context

storage(rt::_TopLevelRuntime) = rt.storage

# Top-level runtimes do not support tracing. Explicit overloads for safety here.
trace(::_TopLevelRuntime) = error("Attempted to call `trace` on a top-level runtime.")
collect_call_stack(::_TopLevelRuntime) = error("Attempted to call `collect_call_stack` on a top-level runtime.")
collect_trace(::_TopLevelRuntime) = error("Attempted to call `collect_trace` on a top-level runtime.")
destruct_trace!(::_TopLevelRuntime) = error("Attempted to call `destruct_trace!` on a top-level runtime.")

function unmemoized_input_lookup(rt::_TopLevelRuntime, key)
    return _unwrap_salsa_value(rt, _unmemoized_input_lookup_internal(rt, key))
end

# Safety overload.
function previous_output(::_TopLevelRuntime)
    error("`previous_output(rt)` may only be called from inside a derived function.")
end

