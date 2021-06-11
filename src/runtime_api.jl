########## Types

"""
    Salsa.Runtime()
    Salsa.Runtime{ContextType}(context::ContextType)
    Salsa.Runtime{ContextType,ST<:AbstractSalsaStorage}(ctx::ContextType, storage::ST)

A Salsa Runtime instance contains all the state for Salsa derived functions and inputs. The
values for inputs and computed derived values are stored in a storage backend, and the
Runtime keeps track of the dependencies of a derived function, which it then communicates to
the storage backend.

Users should create a Salsa.Runtime() to store all the incremental state for a program, and
then pass it to their various `@input`s and `@derived` functions.

Inputs and Derived functions take a Runtime() instance as their first parameter.

By providing a `context` instance of `ContextType`, you can attach extra state to a Runtime,
which you can access from within your derived functions without having any affect on derived
function invalidation. (This should be used very careful because it is essentially an escape
hatch that allows you to violate the requirement that derived functions are pure
functions. The intended use is for situations where you need this context in order to
produce some results [e.g. a factory of some kind], but where the context plays no part in
determining the _value_ of the results.)
"""
abstract type Runtime{ContextType,ST<:AbstractSalsaStorage} end

struct EmptyContext end

########## Accessors

"""
    context(runtime::Runtime{CT})::CT

Return the context object that this runtime was constructed with.
"""
function context end

"""
    storage(runtime::Runtime{CT,ST})::ST

Return the storage object that this runtime was constructed with.
"""
function storage end

########## Tracing

# All runtimes except top-level ones are responsible for tracking dependencies throughout a
# region of the dataflow. Storage implementations use the following methods to access
# dependency information from a runtime.

"""
    trace(runtime::Runtime)

Return the trace object associated with this runtime.
"""
function trace end

"""
    collect_call_stack(runtime::Runtime)::Vector{DependencyKey}

Returns the stack trace of Salsa functions called on the runtime. Stack traces are only
tracked in debug mode. When debug mode is disabled, this function always returns an empty
vector.
"""
function collect_call_stack end

"""
    collect_trace(runtime::Runtime)::Vector{DependencyKey}

Returns demand (deduplicated but in-order) incurred over the lifetime of the runtime.
"""
function collect_trace end

"""
    destruct_trace!(runtime::Runtime)

Free up resources associated with the runtime. This is called by Salsa automatically when
the demand associated with that runtime is resolved (i.e. right before a result is returned
to the user).
"""
function destruct_trace! end

########## Lookups

"""
    memoized_lookup(runtime::Runtime, key::InputKey{F,TT})
    memoized_lookup(runtime::Runtime, key::DerivedKey{F,TT})

Look up `key` in the storage associated with that `runtime`, and return the (cached) value
if it exists.

For derived keys, if no valid cached value is found, compute the value from scratch by
running the derived function `F` indicated in the `DerivedKey{F}` type with the function
arguments extracted from `key.args`. The result is then cached again.
"""
function memoized_lookup end

"""
    unmemoized_input_lookup(runtime::Runtime, input, args...)
    unmemoized_input_lookup(runtime::Runtime, input_key::InputKey)

Sometimes we need to verify a new input value against the current one in user code (for
example to gracefully handle an over-deletion. This check can inherently not be memoized
(because it would lead to a cyclic dependency).
"""
function unmemoized_input_lookup end

# Branching constructor for any runtime capable of tracing. Responsible for adding the
# provided key to the trace.
function new_trace_runtime! end

########## State

"""
    new_epoch!(runtime::Runtime)

Salsa supports running with versioned storage engines. Versioned storage works with epochs
within which the dependency order must be respected. I.e. it is not allowed to assign the
same input twice in the same epoch, or to read a derived value then assign an input on which
that derived value depends.

This function allows Salsa users to start a new epoch in such situations. There are costs
associated with creating new epochs, so this functionality should be used intentionally and
sparingly.

Salsa's DefaultStorage does not support versioning. Calls to this function have no effect
when running with DefaultStorage.
"""
function new_epoch! end

"""
    previous_output(runtime::Runtime)

Must be called from within a `@derived` function. Returns the value that is currently cached
for that function and its arguments (if any). This can be used to implement incremental
logic, that reuses the previously computed value to compute the next value more efficiently.
"""
function previous_output end
