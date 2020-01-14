"""
Implementation of a framework for incremental metadata computations via
memoization, inspired by Rust's Salsa.

* `@querygroup`
* `@query`
* `@input`
"""
module Salsa

export @querygroup, @query, @input

# TODO (TJG):
# - Mutable-until-shared discipline
#   - branch() / ismutable() methods
# - Add a derived_call_active flag
#   - Wrappers for derived methods set/clear this
#   - Input read methods check flag and skip dependency tracking if not set
#   - Input write methods panic if flag is set
# - [NHD] Allow comments / docstrings within the QueryGroup macro?
# - [NHD] Namespace the Key structs so different DBs can have inputs with the same name?

import MacroTools

### Generic helper routines
"""
    camel_to_snake(str::Symbol)

Internal helper to convert e.g. ThisName to this_name
"""
function camel_to_snake(str::Symbol)
    words = split(string(str), r"(?=\p{Lu})")
    words = map(lowercasefirst, words)
    Symbol(join(words, "_"))
end

"""
    snake_to_camel(str::Symbol)

Internal helper to convert e.g. this_name to ThisName
"""
function snake_to_camel(str::Symbol)
    words = split(string(str), "_")
    words = map(uppercasefirst, words)
    Symbol(join(words))
end
###

"""
    QueryGroup

Abstract base type for Salsa query groups.  See `@querygroup`.
"""
abstract type QueryGroup end

const Revision = Int

struct InputValue{T}
    value :: T
    changed_at :: Revision
end

"""
    DatabaseKey

The parameters to a lookup into an input table or for a derived query, with
the concrete type of the `DatabaseKey` carrying the identity of the table or
query.
"""
abstract type DatabaseKey end
abstract type InputKey <: DatabaseKey end
abstract type DerivedKey <: DatabaseKey end

const DatabaseKeys = Vector{DatabaseKey}

mutable struct DerivedValue{T}
    value :: T
    dependencies :: DatabaseKeys
    changed_at :: Revision
    verified_at :: Revision
end

const InputMap{K,V} = Dict{K, InputValue{V}}
const DerivedMap{K,V} = Dict{K, DerivedValue{V}}

"""
    WrappedInput

Wrapper for an input field of a query group intended to hide internal details
of the Salsa implementation (key encoding, dependencies, and revision
information), exposing it just as a key-value map of the types the user
actually wrote.  This is accomplished via property overloading and dispatch:
an input field access returns a wrapper around the actual field, for which
specializations of iterator and dictionary methods are provided.
"""
struct WrappedInput{D<:QueryGroup,K,V} <: Base.AbstractDict{K,V}
    db :: D
    map :: InputMap{K,V}
end

Base.length(input :: WrappedInput) = length(input.map)

function unpack_next(next)
    if next === nothing
        return nothing
    else
        ((in_key, in_value), state) = next
        return (key_to_tuple(in_key) => in_value.value, state)
    end
end

Base.iterate(input :: WrappedInput) =
    unpack_next(Base.iterate(input.map))

Base.iterate(input :: WrappedInput, state) =
    unpack_next(Base.iterate(input.map, state))

function Base.getindex(input :: WrappedInput{D,K,V}, args...) where {D,K,V}
    key = K(args...)
    memoized_lookup(input.db, key).value
end

function Base.setindex!(input :: WrappedInput{D,K,V}, value, args...) where {D,K,V}
    # TODO (TJG) assert no query active
    key = K(args...)
    # NOTE: We use `isequal` for the Early Exit Optimization, since values are required
    # to be purely immutable (but not necessarily julia `immutable structs`).
    if haskey(input.map, key) && isequal(input.map[key].value, value)
        # Early Exit Optimization Part 1: Don't dirty anything if setting exactly the same
        # value for an input.
        return
    end
    input.db.last_revision += 1
    input.map[key] = InputValue(value, input.db.last_revision)
end

function Base.haskey(input :: WrappedInput{D,K,V}, args...) where {D,K,V}
    # TODO (TJG) assert no query active
    key = K(args...)
    return haskey(input.map, key)
end

function Base.delete!(input ::  WrappedInput{D,K,V}, args...) where {D,K,V}
    # TODO (TJG) assert no query active
    input.db.last_revision += 1
    key = K(args...)
    delete!(input.map, key)
end

function push_key(db::QueryGroup, dbkey::DatabaseKey)
    # Handle special case of first `push_key`
    if isempty(db.active_query)
        push!(db.active_traces, DatabaseKeys())
    elseif in(dbkey, db.active_query)
        error("Cycle in active query invoking $dbkey: $(db.active_query)")
    end

    push!(db.active_query, dbkey)
    push!(db.active_traces[end], dbkey)
    push!(db.active_traces, DatabaseKeys())
end

function pop_key(db::QueryGroup)
    pop!(db.active_query)
    deps = pop!(db.active_traces)

    # Handle special case of last `pop_key`
    if isempty(db.active_query)
        pop!(db.active_traces)
        @assert isempty(db.active_traces)
    end

    deps
end

"""
    QueryInfo

Internal Salsa data structure representing a member of a query group (either
an input or a derived query).  `QueryInfo` is constructed from the parse of
a function prototype.
"""
struct QueryInfo
    name :: Symbol
    args :: Vector{Any}
    value :: Any
    isinput :: Bool
end

function map_name(q :: QueryInfo)
    Symbol("__", camel_to_snake(q.name), "_map")
end

function map_type(q :: QueryInfo)
    if q.isinput
        return :( $(@__MODULE__()).InputMap{$(key_name(q)), $(q.value)} )
    else
        return :( $(@__MODULE__()).DerivedMap{$(key_name(q)), $(q.value)} )
    end
end

function key_name(q :: QueryInfo)
    Symbol(snake_to_camel(q.name), "Key")
end

function key_to_tuple end

"""
    key_def(q::QueryInfo)

For a query or input `foo(db, x::Int, y::String) :: Float64`, we generate a
key type
```
struct FooKey
    x::Int
    y::String
end
```
along with specializations of `==` and `hash`, which operate fieldwise, and
a function `key_to_tuple` that converts e.g. `FooKey(1, "hello")` to a tuple
`(1, "Hello")`.

This type is used as a key into the memoization table (for the query) or
storage table (for inputs).  It is also used in the dependency-tracking
scheme, where dispatch on the key type is used to replay queries and input
lookups.
"""
function key_def(q :: QueryInfo)
    name = key_name(q)
    function key_eq(arg)
        argname = arg.args[1]
        return :(a.$argname == b.$argname)
    end
    argnames = Any[arg.args[1] for arg in q.args]

    # Build expression for `key_to_tuple`
    if length(argnames) == 1
        tupling = :( return a.$(argnames[1]) )
        # Elide tupling for simple keys
    else
        # Concatenate the fields of `a` into a tuple for compound keys
        tupling = :( return ($(map(arg -> :(a.$arg), argnames)...),) )
    end

    :(
        struct $name <: $(q.isinput ? InputKey : DerivedKey)
            $(q.args...)
        end
        ;
        function Base.:(==)(a :: $name, b :: $name)
            # Compare all fields of `a` and `b`
            $(reduce((x,y) -> :($x && $y), map(key_eq, q.args); init=:(true)))
        end
        ;
        function Base.hash(a :: $name, h :: UInt)
            # Hash all fields of `a` together
            $(foldr((x,y) -> :(hash($x,$y)), map(arg -> :(a.$(arg.args[1])), q.args); init=:(h)))
        end
        ;
        function $(@__MODULE__()).key_to_tuple(a :: $name)
            $tupling
        end
    )
end

function field_def(q :: QueryInfo)
    :( $(map_name(q)) :: $(map_type(q)) )
end

# A structure only used during macro parsing to represent a user's `@input` declaration
struct InputDeclaration
    def
end

# Implements the `@input` macro; invoked via `@macroexpand` during parsing to transform the
# user's Expr into an InputDeclaration.
function parse_input_def(def)
    InputDeclaration(def)
end

"""
    gather_queries(m::Module, defs)

An internal parsing routine that iterates through the list of queries and
inputs in query group specification and constructs a `QueryInfo` struct for
each.
"""
function gather_queries(m::Module, defs)
    queries = Vector{QueryInfo}()

    for def in defs
        isinput = false

        # If we encounter a macro call within the QueryGroup, it might be an `@input`.
        # We expand the macrocall to allow the `@input` macro to run, and if it wasn't our
        # macro, we leave the expanded code alone (since it's always fine to expand early).
        if def isa Expr && def.head == :macrocall
            expanded = macroexpand(m, def)
            if expanded isa InputDeclaration
                isinput = true
                def = expanded.def
            else
                error("Unsupported macro in block argument to @querygroup: $def")
            end
        end

        if def isa Expr && def.head == :function
            dict = MacroTools.splitdef(def)
            if !haskey(dict, :rtype)
                error("Within @querygroup, function argument and return types must be specified")
            end
            name = dict[:name]
            args = dict[:args][2:end]
            value = dict[:rtype]

            query = QueryInfo(name, args, value, isinput)
            push!(queries, query)
        else
            error("Unexpected expression in block argument to @querygroup: $def")
        end
    end

    return queries
end

user_name(name::Symbol) = Symbol("__user_", name)
user_name(q::QueryInfo) = user_name(q.name)

function memoized_lookup(db::QueryGroup, key::InputKey)
    push_key(db, key)
    value = getindex(db, key)
    pop_key(db)
    return value
end

function key_changed_at(db::QueryGroup, key::DatabaseKey)
    memoized_lookup(db, key).changed_at
end

function invoke_user_function end

function memoized_lookup(db::QueryGroup, key::DerivedKey)
    existing_value = nothing
    value = nothing

    push_key(db, key)

    if haskey(db, key)
        existing_value = getindex(db, key)
        if existing_value.verified_at == db.last_revision
            value = existing_value
        else
            outdated = false
            for dep in existing_value.dependencies
                dep_changed_at = key_changed_at(db, dep)
                if dep_changed_at > existing_value.verified_at
                    outdated = true
                    break
                end
            end
            if !outdated
                existing_value.verified_at = db.last_revision
                value = existing_value
            end
        end
    end

    if value === nothing    # N.B., do not use `isnothing`
        v = invoke_user_function(db, key)
        # NOTE: We use `isequal` for the Early Exit Optimization, since values are required
        # to be purely immutable (but not necessarily julia `immutable structs`).
        if existing_value !== nothing && isequal(existing_value.value, v)
            # Early Exit Optimization Part 2: If a derived function computes the exact same
            # value, we can terminate early and "backdate" the changed_at field to say this
            # value has _not_ changed.
            existing_value.verified_at = db.last_revision
            value = existing_value
            pop_key(db)
        else
            # The user function computed a new value, which we must now store.
            deps = pop_key(db)
            value = DerivedValue(v, deps, db.last_revision, db.last_revision)
            setindex!(db, value, key)
        end
    else
        pop_key(db)
    end

    return value
end

"""
    property_overloads(name, queries::Vector{QueryInfo})

This function implements property overloading for a query group.  See
comments around `WrappedInput`.
"""
function property_overloads(name, queries::Vector{QueryInfo})
    cases = map(filter(q -> q.isinput, queries)) do q
        # wrapped = :( WrappedInput{$(key_name(q)), InputValue{$(q.value)}} )
        wrapped = :( $WrappedInput{$name, $(key_name(q)), $(q.value)} )
        quoted = QuoteNode(q.name)
        :(
            if sym == $quoted
                return $wrapped(db, db.$(map_name(q)))
            end
        )
    end
    quote
        function Base.getproperty(db :: $name, sym :: Symbol)
            $(cases...)
            return getfield(db, sym)
        end
    end
end


function getters_setters(name, queries::Vector{QueryInfo})
    map(filter(q -> !q.isinput, queries)) do q
        argnames = map(arg -> arg.args[1], q.args)
        # Getters for derived values
        return quote
            function $(q.name)(db :: $name, $(q.args...))
                $memoized_lookup(db, $(key_name(q))($(argnames...))).value
            end

            function $(@__MODULE__()).invoke_user_function(db :: $name, key :: $(key_name(q)))
                # __user_bar() will be defined via @query
                $(user_name(q))(db, $(map(name -> :(key.$name), argnames)...))
            end
        end
    end
end

function indexdef(name, query::QueryInfo)
    keytype = key_name(query)
    mapfield = map_name(query)

    ex = :(
        # N.B., these are meant just to be called internally
        function Base.haskey(db :: $name, key :: $keytype)
            Base.haskey(db.$mapfield, key)
        end
        ;
        function Base.getindex(db :: $name, key :: $keytype)
            Base.getindex(db.$mapfield, key)
        end
        ;
        function Base.setindex!(db :: $name, value, key :: $keytype)
            Base.setindex!(db.$mapfield, value, key)
        end
    )
    return ex
end

function construct_defs(name, queries::Vector{QueryInfo})
    # Structs for key definitions
    defs = map(key_def, queries)

    # Fields and constructors for query group struct
    fields = map(field_def, queries)
    fieldcons = map(q -> :($(map_type(q))()), queries)

    querygroup = quote
        mutable struct $name <: $QueryGroup
            $(fields...)
            last_revision :: $Revision
            active_query :: $DatabaseKeys
            active_traces :: Vector{$DatabaseKeys}

            function $name()
                return new($(fieldcons...), 0, $DatabaseKeys(),
                    Vector{$DatabaseKeys}())
            end
        end
    end

    push!(defs, querygroup)

    # haskey / getindex / setindex!
    indexdefs = map(q -> indexdef(name, q), queries)
    append!(defs, indexdefs)

    inputfuns = getters_setters(name, queries)
    append!(defs, inputfuns)

    # property overloading
    push!(defs, property_overloads(name, queries))

    result = quote
        $(defs...)
    end

    Base.remove_linenums!(result)
    return result
end

function querygroup(m::Module, name, ex::Expr)
    ex = MacroTools.rmlines(ex)

    MacroTools.@capture(ex, begin defs__ end) ||
        error("Second argument to @querygroup is expected to be a block expression")

    queries = gather_queries(m, defs)
    global __debug_queries = queries
    defs = construct_defs(name, queries)
    return defs
end

"""
    @querygroup(name, ex)

Macro to define a Salsa query group.  A struct `name` will be generated by
the macro, along with maps for input fields and derived queries as specified
by the function prototypes in the passed `ex` block.
"""
macro querygroup(name, ex)
    return esc(querygroup(__module__, name, ex))
end

"""
    @input(ex)

Annotation macro to mark input fields of query groups.
"""
macro input(ex)
    parse_input_def(ex)
    #esc(isa(ex, Expr) ? pushmeta!(ex, :salsa_input) : ex)
end

function query(ex::Expr)
    # foo() becomes __user_foo()
    dict = MacroTools.splitdef(ex)
    dict[:name] = user_name(dict[:name])
    return MacroTools.combinedef(dict)
end

"""
    @query(ex)

Implementations for queries in a query group are annotated with
this macro, which simply renames e.g. `function foo(db, x)` to
`function __user_foo(db, x)`.
"""
macro query(ex::Expr)
    esc(query(ex))
end

end
