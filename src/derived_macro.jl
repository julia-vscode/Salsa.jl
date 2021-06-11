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
    # Ensure that every argument is named, even unnamed parameters, so that they can be
    # forwarded from the outer function into the inner one.
    fullargs = deepcopy(args)
    for (i,a) in enumerate(fullargs)
        if a isa Expr && a.head == :(::) && length(a.args) == 1
            pushfirst!(a.args, argnames[i])
        end
    end

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

    full_TT = Tuple{Runtime, args_typetuple[2:end]...}

    # Construct the originally named, visible function
    dict[:name] = fname
    dict[:args] = fullargs
    dict[:body] = quote
        args = ($(argnames[2:end]...),)
        key = $DerivedKey{typeof($fname)}(args)
        # TODO: Without this, derived functions are all type unstable, unless the user puts
        # a return type annotation on the function.. :( But we have to turn this off
        # because the compiler is hanging, taking >1 hour in Delve.
        # TODO: File an issue about this. This shouldn't be happening!
        #RT = $(Core.Compiler.return_type)($userfname, typeof(($(argnames[1]), args...)))
        $memoized_lookup_unwrapped($(argnames[1]), key) #::RT
    end
    visible_func = MacroTools.combinedef(dict)

    esc(
        quote
            $userfunc

            # Attach any docstring before this macrocall to the "visible" function.
            Core.@__doc__ $visible_func

            function $Salsa.get_user_function(
                $(fullargs[1]),
                ::$DerivedKey{typeof($fname), <:Tuple{$(argtypes[2:end]...)}},
            )
                return $userfname
            end

            $fname
        end,
    )
end

########## Methods added by the @derived macro.

# This function needs to be forward declared so that the `@derived` macro can add a method
# to it from the user's module where it was called.
function get_user_function end
