# For each Salsa call that is defined, there will be a unique instance of `AbstractKey` that
# identifies requests for the cached value (ie accessing inputs or calling derived
# functions). Instances of these key types are used to identify both _which method_ was
# called, and what the arguments to that call were.
#
# Dependencies between Salsa computations are represented via instances of these keys,
# which specify everything needed to rerun the computation: (key, function-args)
# The stored arguments do not include the Salsa Runtime object itself, since this is always
# present, and changes as computations are performed.
#
# Given:
#   @declare_input input_str(::Int, ::Int) :: String
#   @derived function foo(rt, x::Int, y::Any, z::Number) ... end
#   @derived function foo(rt, x,y,z) ... end
#
# Examples:
#  derived_foo(rt, 2,3) -> DerivedKey{typeof(derived_foo)}((2,3))
#  input_bar(rt, 2,3) -> InputKey{typeof(input_bar)}((2,3))
#  foo(rt,1,2,3)  -> DerivedKey{typeof(foo), Tuple{Int,Any,Number}}((1, 2, 3))
#  input_str(1,2) -> InputKey{typeof(input_str), Tuple{Int,Int}}((1, 2))
abstract type AbstractKey end

struct DerivedKey{F<:Function,TT<:Tuple} <: AbstractKey
    args::TT
end
DerivedKey{F}(args::TT) where {F<:Function,TT<:Tuple} = DerivedKey{F,TT}(args)

# NOTE: After several iterations, the InputKeys are now essentially identical to the
# DerivedKeys. They only differ to allow distinguishing them for dispatch. We might want to
# do some refactoring to share more code below.
struct InputKey{F<:Function,TT<:Tuple} <: AbstractKey
    args::TT
end
InputKey{F}(args::TT) where {F<:Function,TT<:Tuple} = InputKey{F,TT}(args)

# TODO: Probably don't need both this union and the abstract type. Just pick one.
# Convenience Union for sharing code.
const DependencyKey{F,TT} = Union{DerivedKey{F,TT}, InputKey{F,TT}}

# TODO(NHD): Use AutoHashEquals.jl here instead to autogenerate these.
# Note that floats should be compared for equality, not NaN-ness
function Base.:(==)(x1::DK, x2::DK) where DK <: DependencyKey
    return isequal(x1.args, x2.args)
end
function Base.isless(x1::DK, x2::DK) where DK <: DependencyKey
    return isless(x1.args, x2.args)
end

const INPUT_KEY_SEED = hash(codeunits(string(InputKey)))
const DERIVED_KEY_SEED = hash(codeunits(string(DerivedKey)))

function Base.hash(x::InputKey{F}, h::UInt) where {F}
    return hash(x.args, hash(codeunits(string(F)), h + INPUT_KEY_SEED))
end
function Base.hash(x::DerivedKey{F}, h::UInt) where {F}
    return hash(x.args, hash(codeunits(string(F)), h + DERIVED_KEY_SEED))
end

# Override `Base.show` to minimize redundant printing (skip module name).
# Don't print the TT type on the DependencyKey, since it's recovered by the fields.
function Base.show(io::IO, key::InputKey{F,TT}) where {F,TT}
    print(io, "InputKey{$F}($(key.args))")
end
function Base.show(io::IO, key::DerivedKey{F,TT}) where {F,TT}
    print(io, "DerivedKey{$F}($(key.args))")
end

# Pretty-print a DependencyKey for tracing and printing in DerivedFunctionExceptions:
# @input foo(::Runtime, 1::Int, 2::Int)
# @derived foo(::Runtime, 1::Int, 2::Any, 3::Number)
function _print_dep_key_as_call(
    io::IO,
    dependency::DependencyKey{F,TT}
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
function Base.print(io::IO, dependency::InputKey)
    print(io, "@input ")
    _print_dep_key_as_call(io, dependency)
end
function Base.print(io::IO, dependency::DerivedKey)
    print(io, "@derived ")
    _print_dep_key_as_call(io, dependency)
end
