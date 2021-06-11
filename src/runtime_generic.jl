########## Lookups

function memoized_lookup(rt::Runtime, dependency_key::DependencyKey)
    # NOTE: It is important that the tracing happens around all internal computations for
    # derived functions and input functions, as we want to be sure we record _all_
    # dependencies, even those where the result is already cached.
    #
    # You may at first worry that internal checks such as the "Early Exit Optimization"
    # could cause dependencies on _old_ dependency_keys, which may change once our function
    # is invalidated, but in fact it will not. This is because as soon as an old
    # dependency_key returns a new value, we will stop checking old dependencies, and will
    # switch to a full evaluation, which will necessarily include the dependency_key that
    # returned a new value (it will be the first dependency to change its return value).
    rt = new_trace_runtime!(rt, dependency_key)

    # From now on, we're using the newly branched runtime, unique to that function. We also
    # take advantage of this higher-level stack trace to print nicer stack traces whenever a
    # user function throws an exception within Salsa.
    try
        return _memoized_lookup_internal(rt, dependency_key)
    catch e
        # Wrap all caught exceptions in a Salsa exception, so that we can print a summarized
        # trace when errors are handled. Note that the `DerivedFunctionException`s can be
        # handled and unwrapped via the ExceptionUnwrapping.jl package.
        #
        # NOTE: We use isa here, not has_wrapped_exception() because we want to ensure we
        # throw a DerivedFunctionException for the _current_ salsa stack trace. So even if,
        # e.g., this is a TaskFailedException wrapping a DerivedFunctionException, we still
        # want to wrap that one more time to ensure we pretty print the whole stack. :)
        if !(e isa DerivedFunctionException)
            # Include the current summarized Salsa trace in the exception for improved error
            # reporting.
            rethrow(DerivedFunctionException(e, collect_call_stack(rt)))
        else
            rethrow()
        end
    finally
        destruct_trace!(rt)
    end
end

function memoized_lookup_unwrapped(rt::Runtime, dependency_key::DependencyKey)
    return _unwrap_salsa_value(rt, memoized_lookup(rt, dependency_key))
end

# Delegation when called without a fully assembled `InputKey`.
function unmemoized_input_lookup(rt, input, args...)
    return unmemoized_input_lookup(rt, InputKey{typeof(input)}((args...,)))
end
