module _DefaultSalsaStorage

import ..Salsa
using ..Salsa:
    Runtime, AbstractSalsaStorage, memoized_lookup, get_user_function, collect_trace
using ..Salsa: DependencyKey, DerivedKey, InputKey, _storage, RuntimeWithStorage,
    _TopLevelRuntimeWithStorage, _TracingRuntimeWithStorage
using Base.Threads: Atomic, atomic_add!, atomic_sub!
using Base: @lock

import ..Salsa.Debug: @debug_mode, @dbg_log_trace

# --------------------
# TODO:
# - Investigate @nospecialize on user arguments for compiler performance?
#    - We tried this, but saw perf regressions, so we maybe don't understand the
#      specializations that are going on.
# --------------------


const Revision = Int

struct InputValue{T}
    value::T
    changed_at::Revision
    # Allow converting from abitrary values (e.g. creating a Vector{String} from []).
    InputValue{T}(v, changed_at) where {T} = new{T}(v, changed_at)
end
InputValue(v::T, changed_at) where {T} = InputValue{T}(v, changed_at)

_changed_at(v::InputValue)::Revision = v.changed_at

# This struct is mutable so that we can edit the Revisions in-place without having to
# reallocate a new DerivedValue. This is a performance optimization to avoid allocations.
# Since this struct contains a Vector, it will not be isbits, so it will be heap-allocated
# anyway (though this may change in the future, with Julia's stack allocation patch in 1.5;
# consider re-evaluating this decision then).
mutable struct DerivedValue{T}
    value::T
    # A list of all the computations that were accessed directly from this derived function
    # (not all recursive dependencies) when computing this derived value.
    dependencies::Vector{DependencyKey}
    # These Revisions are used to determine whether we need to re-compute this value or not.
    # We need both of them in order to correctly implement the Early-Exit Optimization.
    changed_at::Revision
    verified_at::Revision
end

_changed_at(v::DerivedValue)::Revision = v.changed_at



const InputMapType = IdDict{InputKey,Dict}
const DerivedFunctionMapType = IdDict{Type{<:DerivedKey},Dict}

mutable struct DefaultStorage <: AbstractSalsaStorage
    # The entire Salsa storage is protected by this lock. All accesses and
    # modifications to the storage should be made within this lock.
    lock::Base.ReentrantLock

    # This is bumped every time the storage changes at all, in `set_input!` and
    # `delete_input!`. Whenever this is bumped, we have to check every derived funciton the
    # first time it is called to see if it is still valid.
    # NOTE: This is guaranteed not to change while any derived functions are currently
    # running anywhere on any threads, which we ensure by tracking the count of active
    # derived functions via `derived_functions_active`, below.
    current_revision::Int64

    # We use one big dictionary for the inputs, storing them all together, to reduce
    # allocating a new dictionary for every input.
    # TODO: Do more performance investigation for the tradeoff between sharing a dictionary
    # vs having separate dicts per method. It seems like the current decision is exactly
    # opposite of what would be best: Since DerivedValues are not isbits, they will always
    # be heap allocated, so there's no reason to strongly type their dict. But inputs can
    # be isbits, so it's probably worth specailizing them.
    inputs_map::Dict{InputKey,InputValue}
    derived_function_maps::DerivedFunctionMapType

    # Tracks whether there are any derived functions currently active. It is an error to
    # modify any inputs while derived functions are active, on the current Task or any Task.
    derived_functions_active::Atomic{Int}

    function DefaultStorage()
        new(Base.ReentrantLock(), 0, InputMapType(), DerivedFunctionMapType(), Atomic{Int}(0))
    end
end

const DefaultRuntime = Salsa.Runtime{Salsa.EmptyContext,DefaultStorage}

function Base.show(io::IO, storage::DefaultStorage)
    current_revision = @lock storage.lock begin
        storage.current_revision
    end
    print(io, "Salsa.DefaultStorage($current_revision, ...)")
end


# NOTE: This implements the dynamic behavior for Salsa Components, allowing users to define
# input/derived function dynamically, by attaching new Dicts for them to the storage at
# runtime.
function get_map_for_key(
    storage::DefaultStorage, ::KT, ::Type{RT}
) where {TT, KT<:DerivedKey{<:Any, TT}, RT}
    @lock storage.lock begin
        return get!(storage.derived_function_maps, KT) do
            # PERFORMANCE NOTE: Only construct key inside this do-block to
            # ensure expensive constructor only called once, the first time.

            # TODO: Use the macro's returntype to strongly type the value.
            #       We'll have to generate this function from within the macro, like we used to
            #       in the existing open-source Salsa.
            # NOTE: Except actually after https://github.com/RelationalAI-oss/Salsa.jl/issues/11
            #       maybe we won't do this anymore, and we'll just use one big dictionary!
            Dict{TT,DerivedValue{RT}}()
        # NOTE: Somehow, julia has trouble deducing this return value!
        end::Dict{TT,DerivedValue{RT}}  # This type assertion reduces allocations by 2!!
    end
end
function get_map_for_key(storage::DefaultStorage, ::InputKey)
    # We use one big dictionary for the inputs, storing them all together, so any input key
    # would return the same value here. :)
    return storage.inputs_map
end


function Salsa._previous_output_internal(
    runtime::Salsa._TracingRuntimeWithStorage{DefaultStorage},
    key::DerivedKey,
)
    storage = _storage(runtime)
    derived_key, args = key, key.args

    previous_output = nothing

    @lock storage.lock begin
        cache = get_map_for_key(storage, derived_key)
        if haskey(cache, args)
            previous_output = getindex(cache, args)
        end
    end

    return previous_output
end


function Salsa._memoized_lookup_internal(
    runtime::Salsa._TracingRuntimeWithStorage{DefaultStorage},
    key::DerivedKey{F,TT},
)  where {F,TT}
    storage = _storage(runtime)
    try  # For storage.derived_functions_active
        atomic_add!(storage.derived_functions_active, 1)

        local existing_value, value
        found_existing = false
        should_run_user_func = true

        derived_key, args = key, key.args

        user_func = get_user_function(runtime, derived_key)
        # Always just box all the results in the same structure for max type stability,
        # since we don't gain anything by knowing the type here anyway. We just have
        # to rely on julia to deduce the return type at the very end.
        RT = Any

        # NOTE: We currently make no attempts to prevent two Tasks from simultaneously
        # computing the same derived function for the same key. For cheap derived functions
        # this may be a more optimal behavior than the overhead caused by coordination.
        # However for expensive functions, this is not ideal. We take this approach for now
        # because it is simpler.

        # The locking here is a bit involved, to prevent the lock from being held during
        # user computation.
        # We minimally lock only around the accesses to the fields of storage.
        # NOTE: It is okay to release and then reacquire the lock inside this function
        # while computing the result because we are guaranteed that inputs cannot be
        # modified while this function is running, so we do not need to fear concurrency
        # violations, e.g. overwriting newer values with outdated results.
        lock_held::Bool = false
        local cache
        trace = Salsa.get_trace(runtime.immediate_dependencies_id) :: Salsa.TraceOfDependencyKeys
        try
            lock(storage.lock)
            lock_held = true

            cache = get_map_for_key(storage, derived_key, RT)

            if haskey(cache, args)
                # Optimization: Skip tracing dependencies when check still_valid on values
                #   - There's no reason to be tracing the Salsa functions during
                #     the `still_valid` check, since we're not going to use them. We _do_
                #     still want to keep the stack trace though for cycle detection and
                #     error messages.
                #   - So we set this toggle on the Trace object itself, to allow us to skip
                #     recording the deps for this phase.
                trace.should_trace = false

                existing_value = getindex(cache, args)::DerivedValue{RT}
                found_existing = true
                unlock(storage.lock)
                lock_held = false

                # storage.current_revision should be monotonically increasing.
                @debug_mode @assert (storage.current_revision >= existing_value.verified_at)

                # NOTE: There is no race condition possible here, despite that the storage
                # isn't locked, because all code that might bump `current_revision`
                # (`set_input!` and `delete_input!`) first asserts that there are no active
                # derived functions (`derived_functions_active == 0`). Meaning that this
                # value will be stable across the lifetime of this function.
                if existing_value.verified_at == storage.current_revision
                    value = existing_value
                    should_run_user_func = false
                    # NOTE: still_valid() will recursively call memoized_lookup, potentially
                    #       recomputing all our recursive dependencies.
                elseif still_valid(runtime, existing_value)
                    # Update the verified_at field, but otherwise use the existing value.
                    # NOTE: As above, current_revision is safe to read during this function
                    # without a lock, due to asserts on derived_functions_active.
                    existing_value.verified_at = storage.current_revision
                    value = existing_value
                    should_run_user_func = false
                end
            end
        finally
            if lock_held
                unlock(storage.lock)
                lock_held = false
            end
            trace.should_trace = true
        end

        # At this point (value == nothing) if (and only if) the args are not
        # in the cache, OR if they are in the cache, but they are no longer valid.
        if should_run_user_func
            @dbg_log_trace @info "invoking $key"
            if found_existing
                # Dependency array swap Optimization:
                #   If we've already got an `existing_value` object here, we can avoid an
                #   allocation and a copy by _swapping_ the `trace`'s `ordered_dependencies`
                #   with `existing_value.dependencies`, so that the deps are written
                #   in-place directly into their final destination! :)
                trace = Salsa.get_trace(runtime.immediate_dependencies_id)
                # Temporarily swap the dependency vectors while running user_func so the
                # deps are recorded in-place. Note that we must swap them back at the end.
                existing_value.dependencies, trace.ordered_deps =
                    trace.ordered_deps, existing_value.dependencies
                try
                    v = user_func(runtime, key.args...)
                finally
                    # Swap back the dependency vectors so the vector isn't modified by
                    # future traces.
                    existing_value.dependencies, trace.ordered_deps =
                        trace.ordered_deps, existing_value.dependencies
                end
            else
                v = user_func(runtime, key.args...)
            end
            # NOTE: We use `isequal` for the Early Exit Optimization, since values are
            # required to be purely immutable (but not necessarily julia `immutable
            # structs`).
            @dbg_log_trace @info "Returning from $key."
            if found_existing && isequal(existing_value.value, v)
                # Early Exit Optimization Part 2: (for Part 1 see `set_input!`, below).
                # If a derived function computes the exact same value, we can terminate
                # early and "backdate" the changed_at field to say this value has _not_
                # changed.
                # NOTE: As above, current_revision is safe to read during this function
                #       without a lock, due to asserts on derived_functions_active.
                existing_value.verified_at = storage.current_revision
                # Note that just because it computed the same value, it doesn't mean it
                # computed it in the same way, so we need to update the list of
                # dependencies as well.
                # HOWEVER, we can actually skip this, thanks to the swap-optimization above.
                # existing_value.dependencies = collect_trace(runtime)

                # We keep the old computed `.value` rather than the new value to help catch
                # bugs with users' over-permissive `isequal()` functions earlier.
                value = existing_value
            elseif found_existing
                # Reuse as much of the existing_value's structure to avoid allocations
                existing_value.value = v
                # We skip this thanks to the swap-optimization, above.
                # existing_value.dependencies = collect_trace(runtime)
                existing_value.changed_at = storage.current_revision
                existing_value.verified_at = storage.current_revision
                value = existing_value
            else
                @dbg_log_trace @info "Computed new derived value for $key."
                # The user function computed a new value, which we must now store.
                # NOTE: We set the computed RT here, which might be more abstract than the
                # actual type of the value, `v`.
                # The other option is to change the dicts to be Dict{K, DerivedValue{<:RT}},
                # but that causes extra allocations.
                value = DerivedValue{RT}(
                    v,
                    collect_trace(runtime),
                    storage.current_revision,
                    storage.current_revision,
                )
                @lock storage.lock begin
                    cache[args] = value
                end
            end # existing_value
        end # if value === nothing

        return value
    finally
        atomic_sub!(storage.derived_functions_active, 1)
    end
end # _memoized_lookup_internal

function Salsa._unwrap_salsa_value(
    runtime::RuntimeWithStorage{DefaultStorage},
    # Note: the presence of a `where T` on this function causes allocations.
    v #=::Union{DerivedValue,InputValue} =#
)
    return v.value
end

# A `value` is still valid if none of its dependencies have changed.
function still_valid(runtime, value)
    for depkey in value.dependencies
        dep_changed_at = key_changed_at(runtime, depkey)
        if dep_changed_at > value.verified_at
            return false
        end
    end # for
    return true
end

function key_changed_at(runtime, key::DependencyKey)
    return _changed_at(memoized_lookup(runtime, key))
end

# =============================================================================

# --- Inputs --------------------------------------------------------------------------

function Salsa._memoized_lookup_internal(
    runtime::Salsa._TracingRuntimeWithStorage{DefaultStorage},
    key::InputKey,
)
    storage = _storage(runtime)
    cache = get_map_for_key(storage, key)
    @lock storage.lock begin
        return cache[key]
    end
end

function Salsa.set_input!(
    runtime::_TopLevelRuntimeWithStorage{DefaultStorage},
    key::InputKey,
    value,
)
    storage = _storage(runtime)

    @lock storage.lock begin
        cache = get_map_for_key(storage, key)

        if haskey(cache, key) && _value_isequal_to_cached(cache[key], value)
            # Early Exit Optimization Part 1: Don't dirty anything if setting exactly the
            # same value for an input.
            return
        end

        # It is an error to modify any inputs while derived functions are active, even
        # concurrently on other threads.
        @assert storage.derived_functions_active[] == 0

        @dbg_log_trace @info "Setting input $key => $value"
        storage.current_revision += 1

        cache[key] = InputValue(value, storage.current_revision)
        return nothing
    end
end
# This function barrier exists to allow specializing the `.value` on the type of
# the cached InputValue. (It doesn't seem to have any effect on performance though?)
function _value_isequal_to_cached(cached::InputValue, value)
    # NOTE: We use `isequal` for the Early Exit Optimization, since we compare values _by
    # value_, not by identity. (That is, `[] == []`, despite `[] !== []`.) And we prefer
    # isequal over `==` since we want to preserve float diffs, just like a Dict would.
    return isequal(cached.value, value)
end

function Salsa.delete_input!(
    runtime::_TopLevelRuntimeWithStorage{DefaultStorage},
    key::InputKey,
)
    @dbg_log_trace @info "Deleting input $key"
    storage = _storage(runtime)
    cache = get_map_for_key(storage, key)

    @lock storage.lock begin
        # It is an error to modify any inputs while derived functions are active, even
        # concurrently on other threads.
        @assert storage.derived_functions_active[] == 0

        storage.current_revision += 1
        delete!(cache, key)
        return nothing
    end
end

function Salsa.new_epoch!(runtime::Salsa.RuntimeWithStorage{DefaultStorage})
end

end  # module
