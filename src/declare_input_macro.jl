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
    # NOTE: We don't support default values in inputs!
    # TODO: Do we want to support this? We'd probably have to / want to change the order of
    # the fields in `set_*!()`, so that the value comes first? Or maybe we could fake this
    # by generating multiple methods, but frankly that seems a bit weird.
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
    dependency_key_expr = :($input_key_t(($(argnames[2:end]...),)))
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

        # For type stability
        @inline function $Salsa.get_user_function(
            $(fullargs[1]),
            ::$input_key_t,
        )
            return $getter
        end

        # Return all the generated functions as a hint to REPL users what we're generating.
        ($inputname, $setter_name, $deleter_name)
    end)
end

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
