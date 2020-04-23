module _DefaultSalsaStorage

import ..Salsa
using ..Salsa: Runtime, AbstractSalsaStorage, memoized_lookup, invoke_user_function,
               collect_trace
using ..Salsa: DependencyKey, DerivedKey, InputKey


const Revision = Int

struct InputValue{T}
    value::T
    changed_at::Revision
    # Allow converting from abitrary values (e.g. creating a Vector{String} from []).
    InputValue{T}(v, changed_at) where T = new{T}(v, changed_at)
end
InputValue(v::T, changed_at) where T = InputValue{T}(v,changed_at)

_changed_at(v::InputValue)::Revision = v.changed_at

Base.@kwdef struct DerivedValue{T}
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



const InputMapType = IdDict{InputKey, Dict}
const DerivedFunctionMapType = IdDict{DerivedKey, Dict}

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

    input_maps::InputMapType
    derived_function_maps::DerivedFunctionMapType

    # Tracks whether there are any derived functions currently active. It is an error to
    # modify any inputs while derived functions are active, on the current Task or any Task.
    derived_functions_active::Int

    function DefaultStorage()
        new(Base.ReentrantLock(), 0, InputMapType(), DerivedFunctionMapType(), false)
    end
end

function Base.show(io::IO, storage::DefaultStorage)
    lock(storage.lock) do
        print(io, "Salsa.DefaultStorage($(storage.current_revision), ...)")
    end
end


# NOTE: This implements the dynamic behavior for Salsa Components, allowing users to define
# input/derived function dynamically, by attaching new Dicts for them to the storage at
# runtime.
function get_map_for_key(storage::DefaultStorage, key::DerivedKey)
    lock(storage.lock) do
        _get_or_build_map_for_key!(storage.derived_function_maps, key)
    end
end
function get_map_for_key(storage::DefaultStorage, key::InputKey)
    lock(storage.lock) do
        _get_or_build_map_for_key!(storage.input_maps, key)
    end
end
function _get_or_build_map_for_key!(
    all_maps::IdDict,
    key::Union{InputKey{<:Any,TT},DerivedKey{<:Any,TT}},
)::Dict where {TT}
    return get!(all_maps, key) do
        # PERFORMANCE NOTE: Only construct key inside this do-block to
        # ensure expensive constructor only called once, the first time.

        # TODO: Use the macro's returntype to strongly type the value.
        #       We'll have to generate this function from within the macro, like we used to
        #       in the existing open-source Salsa.
        # NOTE: Except actually after https://github.com/RelationalAI-oss/Salsa.jl/issues/11
        #       maybe we won't do this anymore, and we'll just use one big dictionary!
        Dict{TT, Any}()
    end
end


# TODO: I think we can @nospecialize the arguments for compiler performance?
# TODO: It doesn't seem like this @nospecialize is working... It still seems to be compiling
# a nospecialization for every argument type. :(
function Salsa._memoized_lookup_internal(
    runtime::Runtime{DefaultStorage},
    @nospecialize(key::DependencyKey{<:DerivedKey}),
)::Any
    storage = runtime.storage
    try  # For storage.derived_functions_active
        lock(storage.lock) do
            storage.derived_functions_active += 1
        end

        existing_value = nothing
        value = nothing

        derived_key, args = key.key, key.args

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
        try
            lock(storage.lock); lock_held = true

            cache = get_map_for_key(storage, derived_key)

            if haskey(cache, args)
                existing_value = getindex(cache, args)
                unlock(storage.lock); lock_held = false

                # NOTE: There is no race condition possible here, despite that the storage
                # isn't locked, because all code that might bump `current_revision`
                # (`set_input!` and `delete_input!`) first asserts that there are no active
                # derived functions (`derived_functions_active == 0`). Meaning that this
                # value will be stable across the lifetime of this function.
                if existing_value.verified_at == storage.current_revision
                    value = existing_value
                # NOTE: still_valid() will recursively call memoized_lookup, potentially
                #       recomputing all our recursive dependencies.
                elseif still_valid(runtime, existing_value)
                    # Update the verified_at field, but otherwise use the existing value.
                    value = DerivedValue(
                        # As above, current_revision is safe to read during this function
                        # without a lock, due to asserts on derived_functions_active.
                        verified_at = storage.current_revision,
                        # Unchanged:
                        changed_at = existing_value.changed_at,
                        value = existing_value.value,
                        dependencies = existing_value.dependencies,
                    )
                end
            end
        finally
            if lock_held
                unlock(storage.lock); lock_held = false
            end
        end

        # At this point (value == nothing) if (and only if) the args are not
        # in the cache, OR if they are in the cache, but they are no longer valid.
        if value === nothing    # N.B., do not use `isnothing`
            if get(ENV, "SALSA_TRACE", "0") != "0"
                @info "invoking $key"
            end
            v = invoke_user_function(runtime, key.key, key.args)
            # NOTE: We use `isequal` for the Early Exit Optimization, since values are
            # required to be purely immutable (but not necessarily julia `immutable
            # structs`).
            if get(ENV, "SALSA_TRACE", "0") != "0"
                @info "Returning from $key."
            end
            if existing_value !== nothing && isequal(existing_value.value, v)
                value = DerivedValue(
                    # Early Exit Optimization Part 2: (for Part 1 see methods for:
                    # Base.setindex(::Input*,value) If a derived function computes the
                    # exact same value, we can terminate early and "backdate" the
                    # changed_at field to say this value has _not_ changed.
                    changed_at = existing_value.changed_at,
                    # NOTE: As above, current_revision is safe to read during this function
                    #       without a lock, due to asserts on derived_functions_active.
                    verified_at = storage.current_revision,
                    # We keep the old value rather than the new value to help catch bugs
                    # with users' over-permissive `isequal()` functions earlier.
                    value = existing_value.value,
                    # Note that just because it computed the same value, it doesn't mean it
                    # computed it in the same way, so we need to update the list of
                    # dependencies as well.
                    dependencies = collect_trace(runtime),
                )
                lock(storage.lock) do
                    cache[args] = value
                end
            else
                if get(ENV, "SALSA_TRACE", "0") != "0"
                    @info "Computed new derived value for $key."
                end
                # The user function computed a new value, which we must now store.
                value = DerivedValue(
                    value = v,
                    dependencies = collect_trace(runtime),
                    changed_at = storage.current_revision,
                    verified_at = storage.current_revision,
                )
                lock(storage.lock) do
                    cache[args] = value
                end
            end # existing_value
        end # if value === nothing

        return value
    finally
        lock(storage.lock) do
            storage.derived_functions_active -= 1
        end
    end
end # _memoized_lookup_internal

function Salsa._unwrap_salsa_value(
    runtime::Runtime{DefaultStorage},
    v::Union{DerivedValue{T},InputValue{T}},
)::T where T
    return v.value
end

# A `value` is still valid if none of its dependencies have changed.
function still_valid(runtime, value)
    for depkey in value.dependencies
        dep_changed_at = key_changed_at(runtime, depkey)
        if dep_changed_at > value.verified_at; return false end
    end # for
    true
end

function key_changed_at(runtime, key::DependencyKey)
    _changed_at(memoized_lookup(runtime, key))
end

# =============================================================================

# --- Inputs --------------------------------------------------------------------------

# TODO: I think we can @nospecialize the arguments for compiler performance?
function Salsa._memoized_lookup_internal(
    runtime::Runtime{DefaultStorage},
    # TODO: It doesn't look like this nospecialize is actually doing anything...
    @nospecialize(key::DependencyKey{<:InputKey}),
)::Any
    typedkey, call_args = key.key, key.args
    cache = get_map_for_key(runtime.storage, typedkey)
    return cache[call_args]
end

function Salsa.set_input!(
    runtime::Salsa.Runtime{DefaultStorage},
    @nospecialize(key::DependencyKey),
    @nospecialize(value),
)
    storage = runtime.storage
    typedkey, call_args = key.key, key.args

    lock(storage.lock) do
        cache = get_map_for_key(storage, typedkey)

        # NOTE: We use `isequal` for the Early Exit Optimization, since values are required
        # to be purely immutable (but not necessarily julia `immutable structs`).
        if haskey(cache, key) && isequal(cache[key].value, value)
            # Early Exit Optimization Part 1: Don't dirty anything if setting exactly the
            # same value for an input.
            return
        end

        # It is an error to modify any inputs while derived functions are active, even
        # concurrently on other threads.
        @assert storage.derived_functions_active == 0

        if get(ENV, "SALSA_TRACE", "0") != "0"
            @info "Setting input $key."
        end
        storage.current_revision += 1

        T = valtype(cache)
        cache[call_args] = InputValue{T}(value, storage.current_revision)
        return nothing
    end
end
function Salsa.delete_input!(
    runtime::Salsa.Runtime{DefaultStorage},
    @nospecialize(dependency_key::DependencyKey),
)
    if get(ENV, "SALSA_TRACE", "0") != "0"
        @info "Deleting input $key"
    end
    storage = runtime.storage
    typedkey, call_args = dependency_key.key, dependency_key.args
    lock(storage.lock) do
        # It is an error to modify any inputs while derived functions are active, even
        # concurrently on other threads.
        @assert storage.derived_functions_active == 0

        storage.current_revision += 1
        cache = get_map_for_key(storage, typedkey)
        delete!(cache, call_args)
        return nothing
    end
end

end  # module
