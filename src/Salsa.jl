module Salsa

# Document this Module via the README.md file.
@doc let path = joinpath(dirname(@__DIR__), "README.md")
    include_dependency(path)
    replace(read(path, String), "```julia" => "```jldoctest")
end Salsa

# This is the entirety of the Salsa API: Users create inputs and derived functions, and
# access them through a Runtime instance.
export @derived, @declare_input, Runtime, SalsaWrappedException
import MacroTools

include("Debug.jl")
using .Debug

# For each Salsa call that is defined, there will be a unique instance of `AbstractKey` that
# identifies requests for the cached value (ie accessing inputs or calling derived
# functions). Instances of these key types are used to identify _which methods_ the
# remaining arguments (as a CallArgs tuple) are the key to.
# e.g.  foo(rt, 2,3,5) -> DerivedKey{Foo, (Int,Int,Int)}()
abstract type AbstractKey end
# Dependencies between Salsa computations are represented via instances of DependencyKey,
# which specify everything needed to rerun the computation: (key, function-args)
# The `key` is the `AbstractKey` instance that specifies _which computation_ was performed
# (above) and the `args` is a Tuple of the user-provided arguments to that call.
# The stored arguments do not include the Salsa Runtime object itself, since this is always
# present, and changes as computations are performed.
# Given:
#   @declare_input input_str(::Int, ::Int) :: String
#   @derived function foo(rt, x::Int, y::Any, z::Number) ... end
#   @derived function foo(rt, x,y,z) ... end
# Examples:
#   foo(rt,1,2,3)  -> DependencyKey(key=DerivedKey{typeof(foo), Tuple{Int,Any,Number}}(),
#                                   args=(1, 2, 3))
#   input_str(1,2) -> DependencyKey(key=InputKey{typeof(input_str), Tuple{Int,Int}}(),
#                                   args=(1, 2))
Base.@kwdef struct DependencyKey{KT<:AbstractKey,ARG_T<:Tuple}
    key::KT
    args::ARG_T  # NOTE: After profiling, typing this _does_ reduce allocations ✔︎
end
# TODO(NHD): Use AutoHashEquals.jl here instead to autogenerate these.
# Note that floats should be compared for equality, not NaN-ness
function Base.:(==)(x1::DependencyKey, x2::DependencyKey)
    return isequal(x1.key, x2.key) && isequal(x1.args, x2.args)
end
function Base.isless(x1::DependencyKey, x2::DependencyKey)
    return isequal(x1.key, x2.key) ? isless(x1.args, x2.args) : isless(x1.key, x2.key)
end
Base.hash(x::DependencyKey, h::UInt) = hash(x.key, hash(x.args, hash(:DependencyKey, h)))

# NOTE: After several iterations, the InputKeys are now essentially identical to the
# DerivedKeys. They only differ to allow distinguishing them for dispatch. We might want to
# do some refactoring to share more code below.
struct InputKey{F<:Function,TT<:Tuple{Vararg{Any}}} <: AbstractKey end

# A DerivedKey{F, TT} is stored in the dependencies of a Salsa derived function, in order to
# represent a call to another derived function.
# E.g. Given `@derived foo(::MyComponent,::Int,::Int)`, then calling `foo(component,2,3)`
# would store a dependency as this _DependencyKey_ (defined above):
#   `DependencyKey(key=DerivedKey{foo, (MyComponent,Int,Int)}(), args=(component,2,3))`
# TV: used to specify which method for a function with multiple methods.
struct DerivedKey{F<:Function,TT<:Tuple{Vararg{Any}}} <: AbstractKey end

# Override `Base.show` to minimize redundant printing (skip module name).
function Base.show(io::IO, key::InputKey{F,TT}) where {F,TT}
    print(io, "InputKey{$F,$TT}()")
end
function Base.show(io::IO, key::DerivedKey{F,TT}) where {F,TT}
    print(io, "DerivedKey{$F,$TT}()")
end
# Don't print the type on the DependencyKey, since it's recovered by the fields.
function Base.show(io::IO, dep::DependencyKey)
    print(io, "DependencyKey(key=$(repr(dep.key)), args=$(dep.args))")
end

# Pretty-print a DependencyKey for tracing and printing in SalsaWrappedExceptions:
# @input foo(::Runtime, 1::Int, 2::Int)
# @derived foo(::Runtime, 1::Int, 2::Any, 3::Number)
function _print_dep_key_as_call(
    io::IO,
    dependency::DependencyKey{<:Union{InputKey{F,TT},DerivedKey{F,TT}}},
) where {F,TT}
    call_args = dependency.args
    f = isdefined(F, :instance) ? nameof(F.instance) : nameof(F)
    argsexprs = [
        Expr(:(::), :Runtime),
        (Expr(:(::), call_args[i], fieldtype(TT, i)) for i = 1:length(call_args))...,
    ]
    # Display f in a copy/paste executable way, by exploiting Expr printing.
    f_str = string(:(($f,)))[2:end-2]  # (wraps f name in var"" if needed)
    print(io, "$f_str($(join(argsexprs, ", ")))")
end
function Base.print(io::IO, dependency::DependencyKey{<:InputKey})
    print(io, "@input ")
    _print_dep_key_as_call(io, dependency)
end
function Base.print(io::IO, dependency::DependencyKey{<:DerivedKey})
    print(io, "@derived ")
    _print_dep_key_as_call(io, dependency)
end

abstract type AbstractSalsaStorage end
"""
    Salsa.Runtime()
    Salsa.Runtime{ContextType}(context::ContextType)
    Salsa.Runtime{ContextType,ST<:AbstractSalsaStorage}(ctx::ContextType, storage::ST)

A Salsa Runtime instance contains all the state for Salsa derived functions and inputs.
The values for inputs and computed derived values are stored in a storage backend, and
the Runtime keeps track of the dependencies of a derived function, which it then
communicates to the storage backend.

Users should create a Salsa.Runtime() to store all the incremental state for a program,
and then pass it to their various `@input`s and `@derived` functions.

Inputs and Derived functions take a Runtime() instance as their first parameter.

By providing a `context` instance of `ContextType`, you can attach extra state to a
Runtime, which you can access from within your derived functions without having any affect
on derived function invalidation. (This should be used very careful because it is
essentially an escape hatch that allows you to violate the requirement that derived
functions are pure functions. The intended use is for situations where you need this
context in order to produce some results [e.g. a factory of some kind], but where the
context plays no part in determining the _value_ of the results.)
"""
abstract type Runtime{ContextType,ST<:AbstractSalsaStorage} end

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

struct EmptyContext end

# This is the top-level Runtime object which users will instantiate for using Salsa.
# We mark the _TopLevelRuntime as mutable, so that we can take its pointer_from_objref().
# It will very likely be not isbits anyway, since the users' storage objects are unlikely
# to be isbits. It's a bit annoying that this is mutable, because its contents really
# musn't ever change, but here we are.
mutable struct _TopLevelRuntime{CT,ST<:AbstractSalsaStorage} <: Runtime{CT,ST}
    # Store the user-provided context object, where they can store per-runtime meta-data
    # that does not affect Salsa invalidation / re-evaluation.
    context::CT

    # The storage is where all the tracking of state and invalidation happens.
    storage::ST
end

# A linked list node used to implement a stack trace within Salsa. This trace is used for
# debug tracing (to understand incrementality) and is included in Salsa wrapped Exceptions
# for improved error information.
# We use a linked list for the stack since it is immutable and so will be thread-safe, and
# because the linked list can be cheaply branched (becoming a "tree") for derived functions
# called concurrently with the same parent.
struct SalsaStackFrame
    dp::DependencyKey
    next::Union{Nothing,SalsaStackFrame}
end
# For cycle detection in Debug-mode:
# NOTE: The quadratic performance hazard here; this is to be disabled in production.
stack_has_key(::Nothing, _) = false
function stack_has_key(stack::SalsaStackFrame, key)
    return stack.dp == key || stack_has_key(stack.next, key)
end
function Base.collect(stack::SalsaStackFrame)
    out = DependencyKey[stack.dp]
    while stack.next !== nothing
        push!(out, stack.dp)
        stack = stack.next
    end
    return out
end
Base.length(stack::SalsaStackFrame) = stack.next === nothing ? 1 : length(stack.next) + 1


# We create one trace per derived function, which tracks all the salsa computations
# called by the current derived function, so that they can be recorded as input
# dependencies. We create one instance per derived function to limit contention in
# multithreaded code, and protect insertions with a lock to allow concurrency within the
# body of the function.
# TODO: For best parallel performance, we might consider using Thread-local storage (not
# to be confused with Task-local storage) to collect dependencies, so that we can remove
# the lock entirely: By storing one vector per OS-thread, and aggregating at the end. The
# potential downside is losing some ordering information if part of the function is serial
# and part is parallel.
# NOTE: This is a mutable struct to allow us to modify the call_stack. This is fine because
# it's not going to be isbits anyway due to all of the Vectors, Sets, and Locks, it
# contains, and we are going to keep these in the trace pool (see explanation below).
mutable struct TraceOfDependencyKeys
    # Performance Optimization: De-duplicating Derived Function Traces:
    #   We want to maintain the order in which the dependencies were encountered (to
    #   preserve correctness when checking dependencies for invalidation), but we also
    #   don't want to track exact duplicate dependencies, so we use the seen_deps to filter.
    # TODO: We may want to consider using a linked list instead of a vector to prevent
    # array growth events from causing contention when spawning parallel derived functions.
    ordered_deps::Vector{DependencyKey}
    seen_deps::Set{DependencyKey}

    # We lock around modifications to the list of dependencies, since a derived function
    # may spawn multiple threads, which may themselves call derived functions. This doesn't
    # add too much contention, because it's only blocking around the spawning of the
    # immediate children of node.
    lock::Base.ReentrantLock

    # The trace of Salsa functions that represents the current computation in progress.
    # Since derived functions may spawn threads, this represents a pure stack trace back
    # to the root call into Salsa, which potentially traverses across threads.
    # It's an immutable structure (linked list) to make it thread-safe.
    call_stack::Union{Nothing,SalsaStackFrame}

    # We always create a new, empty TraceOfDependencyKeys for each derived function,
    # since we're only tracing the immediate dependencies of that function.
    function TraceOfDependencyKeys()
        # Pre-allocate all traces to be non-empty, to minimize allocations at runtime.
        # TODO: Tune this. Too big wastes RAM (though, it's fixed cost up front).
        #       Current size, 30, adds about 1MiB, which seems not bad.
        N = 30
        return new(
            sizehint!(Vector{DependencyKey}(), N),
            sizehint!(Set{DependencyKey}(), N),
            Base.ReentrantLock(),
            nothing)
    end
end

function push_key!(trace::TraceOfDependencyKeys, depkey)
    try
        lock(trace.lock)
        # Performance Optimization: De-duplicating Derived Function Traces
        if depkey ∉ trace.seen_deps
            push!(trace.ordered_deps, depkey)
            push!(trace.seen_deps, depkey)
        end
    finally
        unlock(trace.lock)
    end
    return nothing
end
function collect_trace(trace::TraceOfDependencyKeys)
    # NOTE: We do not need to lock around this access, because the user's function is
    # guaranteed to have finished by this point, and it is _not supported_ to register
    # more derived functions on the current trace after that. We should have exclusive
    # access to the trace at this point, and it's about to be handed back to the trace pool!

    # Derived functions that spawn tasks currently *must* wait until all tasks have
    # completed before returning. Calling new derived functions from spawned tasks after
    # returning is an error! (But unfortunately we have no way to detect/prevent it.)

    return copy(trace.ordered_deps)
end

# Branching constructor for _TracingRuntime, which also adds the key to the parent
# runtime. Defined in the _TracingRuntime struct, below.
function new_trace_runtime! end

# This is sort of terrible, but since a _TracingRuntime can be _referenced_ from a
# different thread than the one that created it, we need both a thread id and a trace id
# to retrieve its trace from the new thread. Note that this is safe because the parent
# thread will never release its trace until all children have returned, so there can be no
# dangling references to the trace.
struct TraceId
    thread_idx::Int
    trace_idx::Int
end

# We create another version of the Runtime when are evaluating a derived/input function,
# which is able to keep track of the dependencies invoked from that derived function, so
# they can be recorded together with the function's results.
# _TracingRuntime is an immutable struct, because it is branched for each derived
# function call with a fresh dependency trace, so that derived functions can be safely
# called in parallel on multiple threads. This makes the Runtime thread-safe, as long as
# it's used correctly.
struct _TracingRuntime{CT,ST<:AbstractSalsaStorage} <: Runtime{CT,ST}
    # NOTE: We store a Ptr{} to the parent Runtime rather than the Storage instance itself,
    # in order to keep this struct as `isbits`. This is super critical because we create a
    # new instance of this struct on _every function call_ in Salsa!
    # We also use this pointer to allow derived functions to access their Runtime's context.
    #
    # This is guaranteed to be safe, since the parent Runtime will always exist until the
    # function call that created this tracing runtime has completed.
    tl_runtime::Ptr{_TopLevelRuntime{CT,ST}}

    # immediate_dependencies is used to determine the dependencies of derived functions
    # This field is mutable, but we create a new, empty trace every time we branch the
    # Runtime. It's locked internally to allow spawned derived functions on separate threads
    # to record dependencies to the parent thread.
    # NOTE: We store an index into `tls_trace_pool()`, rather than a TraceOfDependencyKeys
    # object itself here, in order to keep this struct as `isbits`. This is super critical
    # because we create a new instance of this struct on _every function call_ in Salsa!
    immediate_dependencies_id::TraceId

    function @__MODULE__().new_trace_runtime!(
        old_rt::_TopLevelRuntime{CT,ST},
        key::DependencyKey,
    )::_TracingRuntime{CT,ST} where {CT,ST<:AbstractSalsaStorage}
        new{CT,ST}(
            reinterpret(Ptr{ST}, pointer_from_objref(old_rt)),
            # Start a new, empty trace (with the provided call stack if in debug mode)
            if Salsa.Debug.debug_enabled()
                get_trace_with_call_stack(SalsaStackFrame(key, nothing))
            else
                get_trace_with_call_stack(nothing)
            end,
        )
    end

    function @__MODULE__().new_trace_runtime!(
        old_rt::_TracingRuntime{CT,ST},
        key::DependencyKey,
    )::_TracingRuntime{CT,ST} where {CT,ST<:AbstractSalsaStorage}
        # Push the new computation onto the current Runtime (if it's not there already)
        push_key!(old_rt, key)
        # Create a new linked list node (pointing to the old stack trace if debug mode).
        new_trace = if Salsa.Debug.debug_enabled()
            old_trace = get_trace(old_rt.immediate_dependencies_id)
            get_trace_with_call_stack(SalsaStackFrame(key, old_trace.call_stack))
        else
            get_trace_with_call_stack(nothing)
        end
        new{CT,ST}(old_rt.tl_runtime, new_trace)
    end
end

# We store the call_stack in the Trace instead of in the runtime, because the trace is a
# linked-list (meaning not isbits), and we want to keep the Runtime isbits to avoid
# allocations. Since the Traces are pre-allocated it's okay for them to contain heap ptrs.
@inline function get_trace_with_call_stack(
    call_stack::Union{SalsaStackFrame,Nothing}
    )
    trace_id = get_free_trace_id()
    # Set the stack frame if running in debug mode.
    if call_stack isa SalsaStackFrame
        get_trace(trace_id).call_stack = call_stack
    end
    return trace_id
end

# --- Salsa Trace Free List -----------------------------
# This section defines a thread-local storage free-list for salsa derived function traces.
# This is needed because each derived function needs its own vector to track dependencies,
# and vectors have to be heap allocated, but allocations are expensive. We can avoid those
# allocations by pre-allocating a pool of trace objects, and reusing them with a freelist.
# We have one instance of pool+freelist per OS Thread, so that no locking is required when
# taking/putting trace objects into/out of the freelist.
#
# The implementation works as follows:
# On each thread we keep a pool and a freelist:
#   - The pool stores all the TraceOfDependencyKeys objects that are allocated, and
#   - The freelist stores _indexes_ into that pool vector, representing traces that are
#     free to use.
#
# When a derived function starts, it takes an index out of the freelist, which is stored in
# the instance of the _TracingRuntime. Then, while executing user code, when any subsequent
# derived functions are called (bringing us back into the Salsa codebase), we access the
# trace via the provided index, and register the newly called derived function dependency.
#
# We use an Int index rather than simply a reference to the trace itself in order to ensure
# that `_TracingRuntime` remains isbits, since we create one instance per derived function
# call, and if it is not isbits, it will incur allocations.
#
# When the derived function call finishes, we copy the dependencies out of the trace, clear
# it, and put its index back into the freelist. If we ever attempt to take out a new index,
# but the pool is empty, the pool will be doubled with new traces. This should be fairly
# rare, since it will only happen the first time we exceed some number of derived functions
# in flight.
#
# NOTE: In the future, we intend to rewrite Salsa to use a Stack, as depth-first-search,
# rather than using function calls as we do now. Based on our early sketches, we think such
# a design will allow us to remove all of this silliness since each Salsa Task will have its
# own local variables that can be freely reused, much more simply. For now, this suffices.

const g_threadlocal_trace_pools = Vector{TraceOfDependencyKeys}[]
const g_threadlocal_trace_freelists = Vector{Int}[]

# Start off with some large number of traces in the pool. This is arbitrary, but we want it
# to be big enough where it doesn't have to get doubled very often. The traces will double
# whenever we have more active derived functions than available traces, which should only
# happen for very-long chains of derived functions or very-wide task parallelism.
const N_INIT_TRACES = 1024

# This function is called in Salsa.__init__() because we don't know the
# number of threads until runtime. (__init__() is defined at the end of this file.)
function _init_thread_local_pools_and_freelists()
    append!(
        g_threadlocal_trace_pools,
        TraceOfDependencyKeys[TraceOfDependencyKeys() for _ = 1:N_INIT_TRACES] for _ = 1:Threads.nthreads()
    )
    append!(
        g_threadlocal_trace_freelists,
        Int[i for i = 1:N_INIT_TRACES] for _ = 1:Threads.nthreads()
    )
    return nothing
end

# Thread-safe accessors: provides one pool+freelist per OS thread.
tls_trace_pool() = g_threadlocal_trace_pools[Threads.threadid()]
tls_trace_freelist() = g_threadlocal_trace_freelists[Threads.threadid()]

# TraceId is defined above
function get_trace(id::TraceId)
    return g_threadlocal_trace_pools[id.thread_idx][id.trace_idx]
end
function get_free_trace_id()::TraceId
    trace_freelist = tls_trace_freelist()
    trace_pool = tls_trace_pool()
    if isempty(trace_freelist)
        @debug_mode @info "DOUBLING SALSA TRACE POOL ON THREAD $(Threads.threadid())"
        old_size = length(tls_trace_pool())
        num_new_traces = old_size # Double the pool
        @debug_mode @assert num_new_traces > 0
        # Grow the pool:
        append!(
            trace_pool,
            TraceOfDependencyKeys[TraceOfDependencyKeys() for _ = 1:num_new_traces],
        )
        for i = (old_size + 1) : (old_size + num_new_traces)
            push!(trace_freelist, i)
        end
    end
    return TraceId(Threads.threadid(), pop!(trace_freelist))
end
function release_trace_id(id::TraceId)
    @assert id.thread_idx === Threads.threadid()
    # NOTE: no locking is needed here because this is all happening per-thread.
    empty_trace!(tls_trace_pool()[id.trace_idx])
    push!(tls_trace_freelist(), id.trace_idx)
    return nothing
end
function empty_trace!(trace::TraceOfDependencyKeys)
    empty!(trace.ordered_deps)
    empty!(trace.seen_deps)
end
# --- End Salsa Trace Free List -------------------------

function push_key!(rt::Runtime, depkey)
    # Test for cycles if in debug mode
    @debug_mode if stack_has_key(get_trace(rt.immediate_dependencies_id).call_stack, depkey)
        error("Cycle in derived function invoking $depkey.")
    end

    trace = get_trace(rt.immediate_dependencies_id)
    push_key!(trace, depkey)
    return nothing
end

function collect_trace(rt::Runtime)
    trace = get_trace(rt.immediate_dependencies_id)
    return collect_trace(trace)
end

_storage(rt::_TopLevelRuntime) = rt.storage
_storage(rt::_TracingRuntime) = unsafe_load(rt.tl_runtime).storage

"""
    context(rt::Runtime{CT}) :: CT

Return the context object that this runtime was constructed with.
"""
context(rt::_TopLevelRuntime) = rt.context
context(rt::_TracingRuntime) = unsafe_load(rt.tl_runtime).context


struct SalsaWrappedException{T} <: Base.Exception
    captured_exception::T
    salsa_trace::Vector{DependencyKey}
end
function Base.showerror(io::IO, exc::SalsaWrappedException)
    print(io, nameof(typeof(exc)))
    println(io, ": Error encountered while executing Salsa derived function:")
    Base.showerror(io, exc.captured_exception)
    println(io, "\n\n------ Salsa Trace -----------------")
    for (idx, dependency_key) in enumerate(reverse(exc.salsa_trace))
        println(io, "[$idx] ", dependency_key)  # Uses pretty-printing for Traces defined below
    end
    println(io, "------------------------------------")
end

# --- Macro utils -----
function _argnames(args)
    [
        name === nothing || name === :_ ? gensym("_$i") : name
        for (i, name) in enumerate(first.(map(MacroTools.splitarg, args)))
    ]
end
function _argtypes(args)
    getindex.(map(MacroTools.splitarg, args), Ref(2))
end
# ---------------

"""
    @derived function foofunc(rt::Salsa.Runtime, x::Int, y::Vector{Int})::Int ... end

This macro is used to mark a julia function as a Salsa Derived Function, which means the
Salsa framework will cache its return value, keyed on its inputs, and correctly re-evaluate
it in future invocations if any of the Salsa computations it invokes have changed.

This function must be a mathematically _pure_ function, meaning it should not depend on any
outside state, and may only access state through other Salsa computations, called via the
`Runtime`, which must be the first argument to any derived function).

The return value of this function is cached, so that (if no dependencies have been
invalidated) the next time this function is called with the same input arguments, the
cached value will be returned instead, and this function will not execute.

During execution of this function, Salsa will automatically track all calls made to other
Salsa computations (calling other `@derived` functions or input functions) on the
provided `Runtime`. This set of runtime _dependencies_ is used to track
_cache-invalidation_. That is, if any of the inputs reachable along a dependency path from
this function are changed, then any subsequent call to this function will trigger a full
computation, and the function will be rerun. (NOTE, though, that the Early-Exit Optimziation
means that if the same value is returned, we can avoid recomputing any values further down
the dependency chain.)

# Example
```julia-repl
julia> @declare_input student_grade(name::String)::Float64
(student_grade, set_student_grade!, delete_student_grade!)

julia> @derived function letter_grade(rt, name)
           println("computing grade for ", name)
           ["D","C","B","A"][Int(round(student_grade(rt, name)))]
       end
letter_grade (generic function with 1 method)

julia> rt = Runtime();

julia> set_student_grade!(rt, "John", 3.25)  # Set initial grade

julia> letter_grade(rt, "John")
computing grade for John
"B"

julia> letter_grade(rt, "John")  # Uses cached value for letter_grade("John") (no output)
"B"

julia> set_student_grade!(rt, "John", 3.8)  # Change input; invalidates cache for letter_grade().

julia> letter_grade(rt, "John")  # Re-runs the computation sinc its input has changed.
computing grade for John
"A"
```
"""
macro derived(f)
    dict = MacroTools.splitdef(f)

    fname = dict[:name]
    args = dict[:args]

    # _argnames and _argtypes fill in anonymous names for unnamed args (`::Int`) and `Any`
    # for untyped args. `fullargs` will have all args w/ names and types.
    argnames = _argnames(args)
    argtypes = _argtypes(args)
    fullargs = [Expr(:(::), argnames[i], argtypes[i]) for i = 1:length(args)]

    # Get the argument types and return types for building the dictionary types.
    # NOTE: I am PRETTY SURE it's okay to eval here. Function definitions already require
    # argument *types* to be defined already, so evaling the types should be A OKAY!
    args_typetuple = Tuple(Core.eval(__module__, t) for t in argtypes)
    # TODO: Use the returntype to strongly type the DefaultStorage dictionaries!
    returntype_assertion = Core.eval(__module__, get(dict, :rtype, Any))
    TT = Tuple{args_typetuple[2:end]...}

    # Assert that the @derived function starts with a runtime arg. The arg can be untyped,
    # or unnamed, it just has to be at least able to hold a Runtime.
    if !(
        length(args_typetuple) >= 1 &&
        # This allows any input like these: `_`, `rt`, `::Runtime`, `::MyRuntime`, `r::Any`
        # And disallows inputs like these: `::Int`, `x::String`.
        (args_typetuple[1] <: Runtime || args_typetuple[1] >: Runtime)
    )
        err_str = "@derived functions must take a `Runtime` as the first argument."
        if length(args_typetuple) >= 1
            err_str *= " Got unexpected $(args_typetuple[1]) instead."
        end
        throw(ArgumentError(err_str))
    end

    # Rename user function.
    userfname = Symbol("%%__user_$fname")
    dict[:name] = userfname
    userfunc = MacroTools.combinedef(dict)

    derived_key_t = :($DerivedKey{typeof($fname),$TT})  # Use type of function, not obj, because closures are not isbits
    derived_key = :($derived_key_t())

    full_TT = Tuple{Runtime, args_typetuple[2:end]...}

    # Construct the originally named, visible function
    dict[:name] = fname
    dict[:args] = fullargs
    dict[:body] = quote
        key = $DependencyKey(key = $derived_key, args = ($(argnames[2:end]...),))
        RT = $(Core.Compiler.return_type)($userfname, $full_TT)
        $memoized_lookup_unwrapped($(argnames[1]), key)::RT
    end
    visible_func = MacroTools.combinedef(dict)

    esc(
        quote
            $userfunc

            # Attach any docstring before this macrocall to the "visible" function.
            Core.@__doc__ $visible_func

            function $Salsa.invoke_user_function(
                $(fullargs[1]),
                ::$derived_key_t,
                $(fullargs[2:end]...),
            )
                return $userfname($(argnames...))
            end

            $fname
        end,
    )
end
# Methods added by the @derived macro, above.
function invoke_user_function end


# We @nospecialize the dependency key argument here for compiler performance.
function memoized_lookup(rt::Runtime, dependency_key::DependencyKey)
    # NOTE: It is important that the tracing happens around all internal computations for
    # derived functions and input functions, as we want to be sure we record _all_
    # dependencies, even those where the result is already cached.
    # You may at first worry that internal checks such as the "Early Exit Optimization"
    # could cause dependencies on _old_ dependency_keys, which may change once our function
    # is invalidated, but in fact it will not. This is because as soon as an old
    # dependency_key returns a new value, we will stop checking old dependencies, and will
    # switch to a full evaluation, which will necessarily include the dependency_key that
    # returned a new value (it will be the first dependency to change its return value).
    rt = new_trace_runtime!(rt, dependency_key)
    # From now on, we're using the newly branched runtime, unique to that function.
    # We also take advantage of this higher-level stack trace to print nicer stack traces
    # whenever a user function throws an exception within Salsa.
    try
        return _memoized_lookup_internal(rt, dependency_key)
        # catch e
        # TODO: Re-enable this once we are comfortable with handling SalsaDerivedExceptions
        #       throughout our codebase!
        # # Wrap the exception in a Salsa exception (at the lowest layer only).
        # if !(e isa SalsaWrappedException)
        #     rethrow(SalsaWrappedException{typeof(e)}(e, collect(rt.call_stack)))
        # else
        #     rethrow()
        # end
    finally
        release_trace_id(rt.immediate_dependencies_id)
    end
end


# To be overridden by concrete Salsa Runtime implementations.
function _memoized_lookup_internal end

function memoized_lookup_unwrapped(rt::Runtime, dependency_key::DependencyKey)
    return _unwrap_salsa_value(rt, memoized_lookup(rt, dependency_key))
end

# To be overridden by concrete Salsa Runtime implementations.
function _previous_output_internal end

"""
    previous_output(rt::Runtime)

Must be called from within a `@derived` function. Returns the value that is currently cached
for that function and its arguments (if any). This can be used to implement incremental
logic, that reuses the previously computed value to compute the next value more efficiently.
"""
function previous_output(rt::Salsa._TracingRuntime)
    dependency_key = get_trace(rt.immediate_dependencies_id).call_stack.dp
    return _previous_output_internal(rt, dependency_key)
    # if maybe_previous_value === nothing
    #     return nothing
    # else
    #     return _unwrap_salsa_value(rt, maybe_previous_value)
    # end
end

function previous_output(::Salsa._TopLevelRuntime)
    error("`previous_output(rt)` may only be called from inside a derived function.")
end

# To be overridden by concrete Salsa Runtime implementations.
#     _unwrap_salsa_value(runtime, v::Any) -> v'::Any
# Optionally, a Salsa Runtime might need to store its values wrapped in some kind of type
# to track, e.g., whether it's been invalidated. This function is the last thing called
# before returning the value to the user.
function _unwrap_salsa_value end

"""
    @declare_input mymap(rt, x::String, y::Int) :: String

Macro to create Salsa Input functions, which are the interface to store and retrieve
"input values" on a `Salsa.Runtime`.

This macro creates a function with the same name to retrieve input values from a Runtime,
as well as a setter function and deleter function to set values of the specified return
type on the Runtime, stored for a given key.

Salsa input functions can be called from derived functions, and will return the currently
stored value. Whenever the user sets/deletes a new value for the given input, it will
invalidate all dependent derived functions, so that they will be rerun and can access the
new value.

# Example
```julia-repl
julia> @declare_input student_grade(rt, name::String)::Float64
(student_grade, set_student_grade!, delete_student_grade!)

julia> rt = Runtime();

julia> set_student_grade!(rt, "Nathan", 3.5)

julia> student_grade(rt, "Nathan")
3.5

julia> delete_student_grade!(rt, "Nathan")

julia> student_grade(rt, "Nathan")
ERROR: KeyError: key DependencyKey(key=InputKey{typeof(student_grade),Tuple{String}}(), args=("Nathan",)) not found
```
"""
macro declare_input(e::Expr)
    # We require the inputs to have a type specified so that Salsa can store them in a
    # typed dictionary per-input.
    @assert e.head === :(::) "Missing type annotation. Expected: @declare_input i(...)::Type"
    @assert length(e.args) >= 1 && e.args[1] isa Expr && e.args[1].head === :call
    value_t = e.args[end]

    callexpr = e.args[1]
    inputname = callexpr.args[1]

    args = callexpr.args[2:end]

    # _argnames and _argtypes fill in anonymous names for unnamed args (`::Int`) and `Any`
    # for untyped args. `fullargs` will have all args w/ names and types.
    argnames = _argnames(args)
    argtypes = _argtypes(args)
    fullargs = [Expr(:(::), argnames[i], argtypes[i]) for i = 1:length(args)]
    # Put the filled-out args back into the original expression.
    args = callexpr.args[2:end] = fullargs

    # TODO: Use the returntype to strongly type the DefaultStorage dictionaries!
    # TODO: Use base's deduced return type (can be more specific) for DefaultStorage.

    runtime_arg = args[1]
    implicit_value_arg = gensym("value")

    # Eval all the argument types, to make sure they're valid.
    args_typetuple = Tuple(Core.eval(__module__, t) for t in argtypes)
    TT = Tuple{args_typetuple[2:end]...}  # Will be Tuple{} if not 2+ args.

    # Assert that the @declare_input starts with a runtime arg. The arg can be untyped, or
    # unnamed, it just has to be at least able to hold a Runtime.
    if !(
        length(args_typetuple) >= 1 &&
        # This allows any input like these: `_`, `rt`, `::Runtime`, `::MyRuntime`, `r::Any`
        # And disallows inputs like these: `::Int`, `x::String`.
        (args_typetuple[1] <: Runtime || args_typetuple[1] >: Runtime)
    )
        err_str = "@declare_input functions must accept a `Runtime` as the first argument."
        if length(args_typetuple) >= 1
            err_str *= " Got unexpected $(args_typetuple[1]) instead."
        end
        throw(ArgumentError(err_str))
    end

    # If we've made it here, there shouldn't be any reason the definitions below will fail,
    # so it's okay to pre-declare the getter function type, which we use in the InputKey,
    # below.
    getter_f = @eval __module__ function $inputname end

    # Build the Key type here, at macro parse time, since it's expensive to construct at runtime.
    # (Use type of function, not obj, because closures are not isbits)
    input_key_t = InputKey{typeof(getter_f),TT}
    input_key = input_key_t()
    #dependency_key_t = DependencyKey{input_key_t}
    dependency_key_expr =
        :($DependencyKey(key = $input_key, args = ($(argnames[2:end]...),)))
    getter_body = quote
        $memoized_lookup_unwrapped($runtime_arg, $dependency_key_expr)::$value_t
    end

    setter_body = quote
        $Salsa._safe_setter_body($runtime_arg, $dependency_key_expr, $implicit_value_arg)
    end

    deleter_body = quote
        $Salsa._safe_deleter_body($runtime_arg, $dependency_key_expr)
    end

    # Construct the Getter function from the provided call expr.
    # (deepcopy the callexpr so that the subsequent calls don't modify it)
    getter = Expr(:function, deepcopy(callexpr), getter_body)

    # Construct the Deleter next, since it has the same interface as the Getter.
    deleter_name = callexpr.args[1] = Symbol("delete_$(inputname)!")
    # (deepcopy the callexpr so that the setter doesn't modify it)
    deleter = Expr(:function, deepcopy(callexpr), deleter_body)

    # Construct the Setter
    # Inject the value to be set & rename the function.
    setter_name = callexpr.args[1] = Symbol("set_$(inputname)!")
    push!(callexpr.args, :($implicit_value_arg::$value_t))
    setter = Expr(:function, callexpr, setter_body)

    esc(quote
        Core.@__doc__ $getter
        $setter
        $deleter
        # Return all the generated functions as a hint to REPL users what we're generating.
        ($inputname, $setter_name, $deleter_name)
    end)
end

# To be overridden by specific Salsa Storage backend implementations.
function set_input! end
function delete_input! end


function _safe_setter_body(runtime::Salsa._TopLevelRuntime, key, value)
    return set_input!(runtime, key, value)
end
function _safe_deleter_body(runtime::Salsa._TopLevelRuntime, key)
    return delete_input!(runtime, key)
end

function _safe_setter_body(::Salsa._TracingRuntime, key, value)
    error("Attempted impure operation in a derived function!: set_input!(rt, $key, $value)")
end
function _safe_deleter_body(::Salsa._TracingRuntime, key)
    error("Attempted impure operation in a derived function!: delete_input!(rt, $key)")
end

# This function is for use by versioned storage that supports epochs. Not needed for DefaultStorage.
function new_epoch! end

# Provide Storage-major view on Runtime types for ease of implementing Storage backends.
const RuntimeWithStorage{ST,CT} = Runtime{CT,ST}
const _TopLevelRuntimeWithStorage{ST,CT} = _TopLevelRuntime{CT,ST}
const _TracingRuntimeWithStorage{ST,CT} = _TracingRuntime{CT,ST}

# Default context manual storage
RuntimeWithStorage{ST}(st = ST()) where {ST} = Runtime{EmptyContext,ST}(EmptyContext(), st)

include("default_storage.jl")
using ._DefaultSalsaStorage: DefaultStorage, DefaultRuntime
include("inspect.jl")

# Simpler print function for default Runtime implementation.
function Base.show(io::IO, rt::_TopLevelRuntime{EmptyContext,DefaultStorage})
    print(io, "Salsa.Runtime($(rt.storage))")
end

function __init__()
    # Init the freelists at runtime based on the number of threads julia is configured with.
    _init_thread_local_pools_and_freelists()
end

end  # module
