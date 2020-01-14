"""
    module Salsa

Implementation of a framework for incremental metadata computations via
memoization, inspired by Rust's Salsa.

* `@component`
* `@derived`
* `@input`
"""
module Salsa

export @component, @input, @derived, @connect, AbstractComponent, InputScalar, InputMap

# TODO (TJG):
# - Mutable-until-shared discipline
#   - branch() / ismutable() methods
# - Add a derived_call_active flag
#   - Wrappers for derived methods set/clear this
#   - Input read methods check flag and skip dependency tracking if not set
#   - Input write methods panic if flag is set

import MacroTools


const Revision = Int

struct InputValue{T}
    value :: T
    changed_at :: Revision
    # Allow converting from abitrary values (e.g. creating a Vector{String} from []).
    InputValue{T}(v, changed_at) where T = new{T}(v, changed_at)
end
InputValue(v::T, changed_at) where T = InputValue{T}(v,changed_at)

# We use Tuples to store the arguments to a Salsa call (both for input maps and derived
# function calls). These Tuples are used as the keys for the maps that implement the caches
# for those calls.
# e.g. foo(component, 2,3,5) -> (2,3,5)
const CallArgs = Tuple  # (Unused, but defined here for clarity. Could be used in future.)
# For each Salsa call that is defined, there will be a unique instance of `AbstractKey` that
# identifies requests for the cached value (ie accessing inputs or calling derived
# functions). Instances of these key types are used to identify _which maps_ the remaining
# arguments (as a CallArgs tuple) are the key to.
# e.g. foo(component, 2,3,5) -> DerivedKey{Symbol("Foo(Int,Int,Int)")}()
abstract type AbstractKey end
# To specify dependencies between Salsa computations, we use a tuple whose first element is a
# key instance that specifies a call (above) and remaining elements are the arguments to
# that call.
# e.g. foo(component, 2,3,5) -> (DerivedKey{Symbol("Foo(Int,Int,Int)")}(), 2,3,5)
const DependencyKey = Tuple{AbstractKey, Vararg{Any}}

mutable struct DerivedValue{T}
    value :: T
    # A list of all the computations that were accessed when computing this value
    dependencies :: Vector{DependencyKey}
    # These Revisions are used to determine whether we need to re-compute this value or not.
    # We need both of them in order to correctly implement the Early-Exit Optimization.
    changed_at :: Revision
    verified_at :: Revision
end

"""
    abstract type AbstractComponent
The abstract supertype of all structs defined via `@component` (Components). A Salsa
Component acts as a wrapper around Salsa Inputs, defined via `@input`, and the Salsa
Runtime, which holds the state of all the derived maps.
"""
abstract type AbstractComponent end

# Instances of these Key types are used to represent calls to Salsa computations, either
# derived function calls or accesses to Salsa inputs.
# We use these keys to connect a DependencyKey to the correct Map in the Runtime that stores
# the cached valuses for that computation.
# Note that for Input maps, there may be more than one map with the same type, so we use
# the pointer to the map instance itself as our key. For derived functions, we use a
# key whose type points back to the map via dynamic dispatch.
# TODO: Reconsider using types (previous approach for Inputs, and current approach for
#       DerivedKeys) vs storing the map as we've done here for InputKeys. We can use
#       multiple dispatch to find the maps, as was done in the previous approach, by
#       registering something like `get_map(db, ::InputKey{T}) = db.__map`
#       and evaling a new type per @input.
#       I'm just not sure which approach is more performant, since runtime dispatch is
#       pretty expensive, so it's a tradeoff between storing a pointer + cheap dispatch vs
#       storing nothing but doing a (potentially expensive) runtime lookup via dispatch.
# NOTE: Though note that one reason the maps are stored inside the Key is to allow us to
#       try implementing `length(::InputMap)` as a derived function. See the TODO note on
#       that method, below.
# (Parameterized on the type of map for type stability)
struct InputKey{T} <: AbstractKey
    map::T  # Index input keys by the _instance_ of the map itself.
end
struct DerivedKey{Func} <: AbstractKey end

function Base.show(io::IO, key::InputKey{T}) where T
    # e.g. InputKey{InputMap{Int,Int}}(@0x00000001117b5780)
    print(io, "InputKey{$T}(@$(repr(UInt(pointer_from_objref(key.map)))))")
end


# --- Component Runtime --------------------------

const DerivedFunctionMapType = IdDict{DerivedKey, Dict}
mutable struct Runtime
    current_revision::Int64
    active_query::Vector{DependencyKey}
    active_traces::Vector{Vector{DependencyKey}}

    derived_function_maps::DerivedFunctionMapType

    Runtime() = new(0, [], [], DerivedFunctionMapType())
end


function push_key(db::Runtime, dbkey)
    # Handle special case of first `push_key`
    if isempty(db.active_query)
        push!(db.active_traces, Vector{DependencyKey}())
    elseif in(dbkey, db.active_query)

        error("Cycle in active query invoking $dbkey: $(db.active_query)")
    end

    push!(db.active_query, dbkey)
    push!(db.active_traces[end], dbkey)
    push!(db.active_traces, Vector{DependencyKey}())
end

function pop_key(db::Runtime)
    pop!(db.active_query)
    deps = pop!(db.active_traces)

    # Handle special case of last `pop_key`
    if isempty(db.active_query)
        pop!(db.active_traces)
        @assert isempty(db.active_traces)
    end

    deps
end

function key_changed_at(component, map::Dict{<:Any, <:DerivedValue}, key::DependencyKey)
    memoized_lookup_derived(component, key).changed_at
end

function invoke_user_function end


function memoized_lookup_derived(component, key::DependencyKey)
    existing_value = nothing
    value = nothing

    runtime = get_runtime(component)

    derived_key, args = key[1], key[2:end]
    map = get_map_for_key(runtime, derived_key)

    push_key(runtime, key)

    if haskey(map, args)
        existing_value = getindex(map, args)
        if existing_value.verified_at == runtime.current_revision
            value = existing_value
        else
            outdated = false
            for depkey in existing_value.dependencies
                dep_changed_at = key_changed_at(component, get_map_for_key(runtime, depkey[1]), depkey)
                if dep_changed_at > existing_value.verified_at
                    outdated = true
                    break
                end
            end
            if !outdated
                existing_value.verified_at = runtime.current_revision
                value = existing_value
            end
        end
    end


    if value === nothing    # N.B., do not use `isnothing`
        v = try
            invoke_user_function(component, key...)
        catch
            pop_key(runtime)
            rethrow()
        end
        # NOTE: We use `isequal` for the Early Exit Optimization, since values are required
        # to be purely immutable (but not necessarily julia `immutable structs`).
        if existing_value !== nothing && isequal(existing_value.value, v)
            # Early Exit Optimization Part 2: If a derived function computes the exact same
            # value, we can terminate early and "backdate" the changed_at field to say this
            # value has _not_ changed.
            existing_value.verified_at = runtime.current_revision
            value = existing_value
            pop_key(runtime)
        else
            # The user function computed a new value, which we must now store.
            deps = pop_key(runtime)
            value = DerivedValue(v, deps, runtime.current_revision, runtime.current_revision)
            setindex!(map, value, args)
        end
    else
        pop_key(runtime)
    end

    return value
end


# =============


# --- Macro utils -----
function _argnames(args)
    [name === nothing ? gensym("_$i") : name
     for (i,name) in enumerate(first.(map(MacroTools.splitarg, args)))]
end
function _argtypes(args)
    getindex.(map(MacroTools.splitarg, args), Ref(2))
end
# ---------------

"""
    @derived function foofunc(component, x::Int, y::Vector{Int}) :: Int ... end

This macro is used to mark a julia function as a Salsa Derived Function, which means the
Salsa framework will cache its return value, keyed on its inputs.

This function must be a mathematically _pure_ function, meaning it cannot depend on any
outside state, and may only access state through a Salsa component (which must be the first
argument to any derived function).

The return value of this function is cached, so that (if no inputs are modified) the next
time this function is called with the same input arguments, the cached value will be
returned instead, and this function will not execute.

During execution of this function, Salsa will automatically track all calls made to other
Salsa computations (calling other `@derived` functions and accessing Inputs from the
provided Component). This set of runtime _dependencies_ is used to track
_cache-invalidation_. That is, if any of the inputs reachable along a dependency path from
this function are changed, then all subsequent calls to this function will trigger a full
computation, and the function will be rerun. (NOTE, though, that the Early-Exit Optimziation
means that if the same value is returned, we can avoid recomputing any values further down
the dependency chain.)

# Example
```julia-repl
julia> @component Classroom begin
           @input student_grades :: InputMap{String, Float64}
       end

julia> @derived function letter_grade(c, name)
           println("computing grade for ", name)
           ["D","C","B","A"][Int(round(c.student_grades[name]))]
       end
letter_grade (generic function with 1 method)

julia> c = Classroom();

julia> c.student_grades["John"] = 3.25  # Set initial grade
3.25

julia> letter_grade(c, "John")
computing grade for John
"B"

julia> letter_grade(c, "John")  # Uses cached value for letter_grade("John") (no output)
"B"

julia> c.student_grades["John"] = 3.8  # Change input; invalidates cache for letter_grade().
3.8

julia> letter_grade(c, "John")  # Re-runs the computation sinc its input has changed.
computing grade for John
"A"
```
"""
macro derived(f)
    dict = MacroTools.splitdef(f)

    fname = dict[:name]
    args = dict[:args]

    if length(args) < 1
        throw(ArgumentError("@derived functions must take a Component as the first argument."))
    end

    # _argnames and _argtypes fill in anonymous names for unnamed args (`::Int`) and `Any`
    # for untyped args. `fullargs` will have all args w/ names and types.
    argnames = _argnames(args)
    argtypes = _argtypes(args)
    dbname = argnames[1]
    fullargs = [Expr(:(::), argnames[i], argtypes[i]) for i in 1:length(args)]

    # Get the argument types and return types for building the dictionary types.
    # TODO: IS IT okay to eval here? Will function defs always be top-level exprs?
    # I _think_ it probably will, because function defs require arg *types* to be defined already.
    args_typetuple = Tuple(Core.eval(__module__, t) for t in argtypes)
    returntype = Core.eval(__module__, get(dict, :rtype, Any))
    if !isconcretetype(returntype)
        returntype = :(<:$returntype)
    end
    call_args_tuple = args_typetuple[2:end]  # Separate out just the CallArgs from args types
    dicttype = :(Dict{Tuple{$(call_args_tuple...)}, $DerivedValue{$returntype}})

    # Rename user function.
    userfname = Symbol("%%__user_$fname")
    dict[:name] = userfname
    userfunc = MacroTools.combinedef(dict)

    DerivedKeyParam = _derived_func_map_name(fname, call_args_tuple)

    derived_key_t = DerivedKey{DerivedKeyParam}  # Use function object, not type, since obj isbits.
    derived_key = derived_key_t()

    # Construct the originally named, visible function
    dict[:name] = fname
    dict[:args] = fullargs
    dict[:body] = quote
        key = ($derived_key, $(argnames[2:end]...),)
        $memoized_lookup_derived($(argnames[1]), key).value
    end
    visible_func = MacroTools.combinedef(dict)

    esc(quote
        $userfunc

        # Attach any docstring before this macrocall to the "visible" function.
        Core.@__doc__ $visible_func

        function $Salsa.get_map_for_key(runtime :: $Runtime, derived_key :: $derived_key_t)
            # NOTE: This implements the dynamic behavior for Salsa Components, allowing
            # users to define derived function methods after the Component, by attaching
            # them to the struct at runtime.
            cache = get!(runtime.derived_function_maps, derived_key) do
                        # PERFORMANCE NOTE: Only construct key inside this do-block to
                        # ensure expensive constructor only called once, the first time.
                        $dicttype()
                    end
            cache
        end

        function $(@__MODULE__()).invoke_user_function($(args[1]), ::$derived_key_t, $(args[2:end]...))
            $userfname($(argnames[1]), $(argnames[2:end]...))
        end

        $fname
    end)
end

# Methods are added to this function for DerivedKeys in the macro, above.
function get_map_for_key end
get_map_for_key(runtime::Runtime, input_key::InputKey) = input_key.map

_derived_func_map_name(@nospecialize(f), @nospecialize(tt::NTuple{N, Type} where N)) =
    Symbol("%%$f%$tt")

get_runtime(db) = db.runtime

# ----------- Component

"""
    @component ComponentName begin ... end

Macro to declare a new Salsa Component type (which is subtyped from AbstractComponent).
An Component is a collection of Inputs, and stores the values for those inputs. The
@component macro also adds an implicit field `runtime` (a [`Salsa.Runtime`](@ref)), which
stores the entire Salsa state (the values for all the derived function caches).

The macro also generates a constructor which simply initializes the Runtime and passes it
through to all the Inputs and any embeded Components.

# Example
```julia
Salsa.@component MyComponent begin
    Salsa.@input input_field :: Salsa.InputMap{Int, Int}

    Salsa.@connect another_component :: AnotherComponent
end
```

The above call will expand to a struct definition like this:
```julia
mutable struct MyComponent <: Salsa.AbstractComponent
    runtime           :: Salsa.Runtime

    input_field       :: Salsa.InputMap{Int, Int}
    another_component :: AnotherComponent

    function MyComponent(runtime = Salsa.Runtime())
        new(runtime, Salsa.InputMap(runtime), AnotherComponent(runtime))
    end
end
```
You can pass an instance of a Component struct as the first argument to  "[`@derived`](@ref)
functions," which access input fields from this struct and calculate derived values, and
store their state in the runtime of the component. For example:
```julia
@derived function foo(my_component, arg1::Int) :: Int
    my_component.input_field[arg1]
end
```
"""
macro component(name, def)
    @assert def isa Expr && def.head == :block  "Expected Usage: @component MyComponent begin ... end"

    user_exprs = []
    provide_decls = ProvideDecl[]
    for expr in def.args
        if expr isa Expr && expr.head == :macrocall
            expanded = macroexpand(__module__, expr)
            if expanded isa ProvideDecl
                push!(provide_decls, expanded)
            else
                push!(user_exprs, expanded)
            end
        else
            # TODO: Consider erroring instead of allowing user defined expressions?
            push!(user_exprs, expr)
        end
    end

    has_user_defined_constructor = false
    for expr in user_exprs
        if expr isa Expr && MacroTools.isdef(expr) && expr.args[1].args[1] == name
            has_user_defined_constructor = true
        end
    end

    esc(quote
        mutable struct $name <: $AbstractComponent
            # All Salsa Components contain a Runtime
            runtime::$Runtime

            $((input.decl for input in provide_decls)...)


            # Function to initialize the Salsa constructs, which is called by default as the
            # main constructor.
            function $Salsa.create(::Type{$name}, runtime::$Runtime = $Runtime())
                # Construct Runtime metadata and Derived Functions
                new(runtime,
                    # Construct input maps (which need the runtime reference)
                    $((:($(i.input_type)(runtime)) for i in provide_decls)...)
                    )
            end

            $(if !has_user_defined_constructor
            :(
            # Define the default constructor only if the user hasn't provided a constructor
            # of their own. (The user's constructor must provide `runtime` to all args.)
            function $name(runtime :: $Runtime = $Runtime())
                $Salsa.create($name, runtime)
            end
            )
            end)

            # Put user's values last so they can be left uninitialized if the user doesn't
            # provide values for them.
            # TODO: decide whether we want to support values in these structs _besides_ the
            # Salsa stuff.  It feels like maybe _no_, but leaving it open for now. Could
            # maybe be useful for debugging or something?
            $(user_exprs...)

        end
    end)
end
function create end

# --- Inputs --------------------------

struct InputScalar{V}
    # TODO: Consider @tjgreen's original `getproperty` optimization to not need to store runtime
    # in every field. We would have to construct a "WrappedInputScalar" in getproperty.
    runtime::Runtime
    v::Base.RefValue{InputValue{V}}
    # Must provide a runtime. Providing a default value for v is optional
    InputScalar{V}(runtime::Runtime) where V = new{V}(runtime, Ref{InputValue{V}}())
    InputScalar{V}(runtime::Runtime, v) where V = new{V}(runtime, Ref{InputValue{V}}(InputValue{V}(v, 0)))
    # No type constructs InputScalar{Any}, which can be converted to the correct type.
    InputScalar(r::Runtime, v) = InputScalar{Any}(r, v)
end
struct InputMap{K,V} <: AbstractDict{K,V}
    runtime::Runtime
    v::Dict{K,InputValue{V}}
    # Must provide a runtime. Providing a default Dict or pairs is optional
    InputMap{K,V}(r::Runtime, pairs::Pair...) where {K,V} = InputMap{K,V}(r, Dict(pairs...))
    InputMap{K,V}(r::Runtime, d::Dict) where {K,V} = new{K,V}(r, Dict(k=>InputValue{V}(v,0) for (k,v) in d))
    # No type constructs InputMap{Any}, which can be converted to the correct type.
    InputMap(r::Runtime, pairs::Pair...) = InputMap{Any,Any}(r, Dict(pairs...))
    InputMap(r::Runtime, d::Dict)        = InputMap{Any,Any}(r, d)
end

const input_types = (InputScalar, InputMap)
const InputTypes = Union{input_types...}

# Support Constructing an Input from an untyped constructor `InputScalar(db)`.
InputScalar(runtime) = InputScalar{Any}(runtime)
InputMap(runtime) = InputMap{Any,Any}(runtime)

Base.convert(::Type{T}, i::InputScalar) where T<:InputScalar = i isa T ? i : T(i)
function InputScalar{T}(i::InputScalar{S}) where {S,T}
    @assert !isassigned(i) || i.v[].changed_at == 0  "Cannot copy an InputScalar{$T} " *
        "from another, active InputScalar{$S}. Conversion only supported for construction."
    InputScalar{T}(i.runtime, i[])
end
Base.convert(::Type{T}, i::InputMap) where T<:InputMap = i isa T ? i : T(i)
function InputMap{K1,V1}(i::InputMap{K2,V2}) where {K1,V1,K2,V2}
    @assert isempty(i) || all(v.changed_at == 0 for v in values(i.v)) "Cannot copy an InputMap{$K1,$V1} " *
        "from another, active InputMap{$K2,$V2}. Conversion only supported for construction."
    InputMap{K1,V1}(i.runtime, Dict(pairs(i)...))
end

# TODO: Either fix, or disallow, calling these "reflection functions" from within a derived
# function (e.g. length, iterate, keys, values, etc). Currently, this kind of reflection
# over the inputs maps is bad, because it doesn't register a dependency on the values stored
# in the map, so it is effectively depending on outside global state.
# Consider whether these could be fixed by making them derived functions themselves, and
# registering their values in some sort of "reflection" map storing "meta" keys. This is
# possible now because there is a pointer to the DB in the inputs themselves.
# For examples of this failure currently, please see these broken tests:
# <broken-url>
Base.length(input  :: InputTypes) = Base.length(input.v)
Base.iterate(input :: InputTypes) = unpack_next(Base.iterate(input.v))
Base.iterate(input :: InputTypes, state) = unpack_next(Base.iterate(input.v, state))

function unpack_next(next)
    if next === nothing
        return nothing
    else
        ((in_key, in_value), state) = next
        return (in_key => in_value.value, state)
    end
end

# -- Helper Utilities --

Base.eltype(input::T) where T<:InputTypes = Base.eltype(T)
Base.eltype(::Type{InputScalar{V}}) where V = V
Base.eltype(::Type{InputMap{K,V}}) where {K,V} = Base.Pair{K,V}

# Copied from Base, i[k1,k2,ks...] is syntactic sugar for i[(k1,k2,ks...)]
# Note that this overload means _at least two_ keys.
Base.getindex(i::InputTypes, k1, k2, ks...)    = Base.getindex(i, tuple(k1,k2,ks...))
Base.setindex!(i::InputTypes, v, k1, k2, ks...) = Base.setindex!(i, v, tuple(k1,k2,ks...))

# Access scalar inputs like a Reference: `input[]`
function Base.getindex(input :: InputScalar)
    memoized_lookup_input(input.runtime, input, (InputKey(input),)).value
end
# Access map inputs like a Dict: `input[k1, k2]`
function Base.getindex(input :: InputMap, call_args...)
    memoized_lookup_input(input.runtime, input, (InputKey(input), call_args...)).value
end

function Base.setindex!(input :: InputScalar{T}, value) where {T}
    # NOTE: We use `isequal` for the Early Exit Optimization, since values are required
    # to be purely immutable (but not necessarily julia `immutable structs`).
    if isassigned(input.v) && isequal(input.v[].value, value)
        # Early Exit Optimization Part 1: Don't dirty anything if setting exactly the same
        # value for an input.
        return
    end
    # TODO (TJG) assert no query active
    input.runtime.current_revision += 1
    input.v[] = InputValue{T}(value, input.runtime.current_revision)
    input
end
function Base.setindex!(input :: InputMap{K,V}, value, key) where {K,V}
    # NOTE: We use `isequal` for the Early Exit Optimization, since values are required
    # to be purely immutable (but not necessarily julia `immutable structs`).
    if haskey(input.v, key) && isequal(input.v[key].value, value)
        # Early Exit Optimization Part 1: Don't dirty anything if setting exactly the same
        # value for an input.
        return
    end
    # TODO (TJG) assert no query active
    input.runtime.current_revision += 1
    input.v[key] = InputValue{V}(value, input.runtime.current_revision)
    input
end

function Base.delete!(input :: InputMap, key)
    # TODO (TJG) assert no query active
    input.runtime.current_revision += 1
    delete!(input.v, key)
    input
end

# TODO: As with the reflection functions above (length, iterate, etc), these accessor
# functions need to become either disallowed from within derived functions, or become
# implemented as memoized_lookups as well, since they are stateful.
# This should be relatively easy!
function Base.isassigned(input :: InputScalar)
    isassigned(input.v)
end
function Base.haskey(input :: InputMap, key)
    # TODO (TJG) assert no query active
    return haskey(input.v, key)
end

function key_changed_at(component, map::InputTypes, key::DependencyKey)
    memoized_lookup_input(component.runtime, map, key).changed_at
end
function memoized_lookup_input(runtime::Runtime, input::InputTypes, key::DependencyKey)
    typedkey, call_args = key[1], key[2:end]
    push_key(runtime, key)
    value = getindex(input.v, call_args...)
    pop_key(runtime)
    return value
end


# These macros are needed to craft a @component constructor that constructs all its elements.
"""
    @input fieldname :: InputType{KeyType, ValueType}

Used within a Salsa Component definition to declare a value as an Input to Salsa.
This macro should be used inside of a [`@component`](@ref), since it is simply used as part
of the macro processing. The macro is used to auto-generate a constructor that correctly
initializes the Input struct. (The generated constructor simply passes the provided Runtime
to each of the inputs.)

# Example
```julia
    Salsa.@component MyComponent begin
        Salsa.@input debug :: Salsa.InputScalar{Bool}
        Salsa.@input source_files :: Salsa.InputMap{String, String}
    end
```
"""
macro input(decl)
    inputname = decl.args[1]
    inputtype = Core.eval(__module__, decl.args[2])
    @assert inputtype <: InputTypes "@input must be called on a field of type ∈ $input_types. Found unexpected type `$inputtype`"
    ProvideDecl(
        inputname,
        inputtype,
        decl
    )
end
"""
    @connect fieldname :: AnotherComponentType

Used within a Salsa Component definition to declare a value to be an embedded Salsa
Component. This macro should be used inside of a [`@component`](@ref), since it is simply
used as part of the macro processing. The macro is used to auto-generate a constructor that
correctly initializes the embedded Component. (The generated constructor simply passes the
provided Runtime through to the Component's constructor.)

# Example
```julia
    Salsa.@component MyComponent begin
        Salsa.@connect compiler :: Compiler
    end
```
"""
macro connect(decl)
    componentname = decl.args[1]
    componenttype = Core.eval(__module__, decl.args[2])
    @assert componenttype <: AbstractComponent "Expected usage: `@connect compiler :: CompilerComponent`, where CompilerComponent was created via `@component`"
    ProvideDecl(
        componentname,
        componenttype,
        decl
    )
end
# This struct is returned from the above macros and accessed from within `@component` to
# access information about the field being declared.
struct ProvideDecl
    input_name::Symbol
    input_type::Type{<:Union{InputTypes, AbstractComponent}}
    decl::Expr
end

end  # module Salsa
