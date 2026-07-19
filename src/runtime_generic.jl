########## Lookups

function memoized_lookup(rt::Runtime, dependency_key::DependencyKey)
    # Derived functions recurse through user code, so a deep chain of derived-function
    # calls consumes native stack proportional to its depth and would eventually crash
    # with an unrecoverable StackOverflowError. To support arbitrarily deep chains, we
    # hop onto a fresh task stack every STACK_SEGMENT_DEPTH nested calls, so the
    # recursion is bounded by heap size rather than stack size.
    if _needs_fresh_stack(rt, dependency_key)
        return _memoized_lookup_on_fresh_stack(rt, dependency_key)
    end
    return _memoized_lookup_impl(rt, dependency_key)
end

# Fallback: top-level calls and input lookups never need a fresh stack. The method for
# nested derived-function calls is defined in runtime_tracing.jl.
_needs_fresh_stack(::Runtime, ::DependencyKey) = false

@noinline function _memoized_lookup_on_fresh_stack(rt::Runtime, dependency_key::DependencyKey)
    # `fetch` on the hop task infers `Any`, which would poison `memoized_lookup`'s
    # return type for every lookup (shallow, non-hopping chains included) — assert the
    # result back to exactly what the inline `_memoized_lookup_impl` call would have
    # returned. `promote_op` is inference-based and constant-folds at compile time.
    T = Base.promote_op(_memoized_lookup_impl, typeof(rt), typeof(dependency_key))
    return _call_on_fresh_stack(() -> _memoized_lookup_impl(rt, dependency_key))::T
end

# Run `f()` on a freshly scheduled task and return its result, so that `f`'s recursion
# continues from an empty native stack. Used for the stack-segment hops in both
# `memoized_lookup` and the verification fast path (`_derived_changed_at`).
# Two intentional, documented differences vs. plain recursion:
#   - Native backtraces (`catch_backtrace()`, `current_exceptions()`) are truncated at
#     segment boundaries and gain a TaskFailedException "caused by" entry; the Salsa
#     trace carried inside DerivedFunctionException is complete and unaffected.
#   - If the caller is interrupted while blocked in `fetch` (e.g. InterruptException),
#     the child segment keeps running detached until it finishes: it releases its
#     traces safely, but holds `derived_functions_active` up until then, so an
#     immediate subsequent `set_input!` can fail its no-active-deriveds assertion.
#     (Pre-existing related hazard: the child's closure also captures the isbits
#     runtime's raw pointer to the parent Runtime, which must stay alive until the
#     child finishes.)
@noinline function _call_on_fresh_stack(f)
    ct = current_task()
    parent_was_sticky = ct.sticky
    # Pin the calling task too, not just the child: blocking in `fetch` below is a
    # scheduling point, and Julia may resume a non-sticky task (e.g. one started via
    # `Threads.@spawn`) on a *different* thread afterwards. The caller holds trace ids
    # acquired from this thread's pool, which must be released on this same thread
    # (see `release_trace_id`). Restored on exit so we don't change the caller's
    # scheduling behavior beyond the hop.
    ct.sticky = true
    try
        t = Task(f)
        # The child task must stay on this thread: traces are pooled per-thread, and
        # `release_trace_id` returns a trace to the *current* thread's freelist.
        t.sticky = true
        schedule(t)
        try
            return fetch(t)
        catch e
            if e isa TaskFailedException
                # Salsa exceptions arrive at segment boundaries already wrapped in a
                # DerivedFunctionException (with the complete Salsa trace) by
                # `_memoized_lookup_impl`'s catch block, so rethrow the child's
                # exception directly: exceptions must surface identically whether or
                # not the chain happened to cross a stack-segment boundary.
                throw(ExceptionUnwrapping.unwrap_exception(e))
            end
            rethrow()
        end
    finally
        ct.sticky = parent_was_sticky
    end
end

# The segment counter for hop accounting: how many counted levels (derived-function
# calls + verification levels) sit on the current task chain. Zero for runtimes that
# haven't entered a derived function. The `_TracingRuntime` method lives in
# runtime_tracing.jl.
_segment_depth(::Runtime) = Int32(0)

# Identity fallback; the depth-carrying method for `_TracingRuntime` lives in
# runtime_tracing.jl.
_with_segment_depth(rt::Runtime, ::Int32) = rt

function _memoized_lookup_impl(rt::Runtime, dependency_key::DependencyKey)
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
