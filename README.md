# Salsa.jl

[![Build Status](https://travis-ci.com/RelationalAI-oss/Salsa.jl.svg?branch=master)](https://travis-ci.com/RelationalAI-oss/Salsa.jl)

A framework for on-demand, incremental computation via memoization, inspired by Rust lang's
[salsa-rs/salsa](https://github.com/salsa-rs/salsa).

[⏯ Youtube | JuliaCon 2020 | Salsa.jl](https://youtu.be/0uzrH2Ee494)

## Description

Salsa is:
- a memoization framework, with
- runtime dependency tracking, so that
- you can update some inputs and (performantly) automatically invalidate the affected caches.

It provides a framework for automating away the potential pitfalls of cache invalidation, by automatically detecting dependencies between parts of your code (`@derived` functions), and using the detected dependency graph to propagate invalidations when facts about the world have changed.

## Usage

- `@derived`
- `@declare_input`
- `Runtime()`

```julia
julia> using Salsa

julia> @declare_input x(rt)::Int
(x, set_x!, delete_x!)

julia> @derived function x_plus_one(rt)
           println("Running x_plus_one.")
           return x(rt) + 1
       end
x_plus_one (generic function with 1 method)
```
```julia
julia> rt = Salsa.Runtime();

julia> set_x!(rt, 1)

julia> x_plus_one(rt)
Running x_plus_one.
2

julia> x_plus_one(rt)
2

julia> set_x!(rt, 10)

julia> x_plus_one(rt)
Running x_plus_one.
11
```

### Lazy Inputs

By default, accessing an input that hasn't been set throws an error. With **lazy inputs**, you can provide a callback function that computes the value on-demand the first time it is accessed. This is useful for inputs backed by external data sources (files, databases, etc.) where you want values to be loaded only when needed.

```julia
julia> function load_student_grade(ctx, name::String)
           println("Loading grade for $name...")
           return name == "Alice" ? 3.5 : 2.0
       end
load_student_grade (generic function with 1 method)

julia> @declare_input student_grade(rt, name::String)::Float64 load_student_grade
(student_grade, set_student_grade!, delete_student_grade!)

julia> @derived function pass_fail(rt, name::String)
           student_grade(rt, name) >= 3.0 ? "Pass" : "Fail"
       end
pass_fail (generic function with 1 method)
```
```julia
julia> rt = Salsa.Runtime();

julia> pass_fail(rt, "Alice")
Loading grade for Alice...
"Pass"

julia> pass_fail(rt, "Alice")
"Pass"
```

The callback signature is `callback(context, args...)` where `context` is the Runtime's context object and `args...` match the input's key arguments. The callback is guaranteed to be called **at most once** per key, even under concurrent access from multiple threads — other threads requesting the same key will wait for the first computation to complete.

You can override a lazy-computed value with `set_input!`, and you can delete it with the generated `delete_*!` function to force recomputation on next access.

### Cancellation

Salsa has first-class support for cancelling in-flight computations, built on
[CancellationTokens.jl](https://github.com/davidanthoff/CancellationTokens.jl). Attach a
token to a runtime with `Salsa.with_cancellation`, and pass the resulting runtime to your
derived-function calls:

```julia
using CancellationTokens

src = CancellationTokenSource()          # or CancellationTokenSource(30) for a timeout
rt2 = Salsa.with_cancellation(rt, get_token(src))

my_derived_function(rt2, args...)        # cancellable call
# ... from another task:
cancel(src)
```

Once cancellation is requested, the call throws `CancellationTokens.OperationCanceledException`.
The exception arrives **unwrapped** (never inside a `DerivedFunctionException`) — cancellation
is not an error condition. A cancelled call caches nothing, and the runtime/storage remains
fully usable afterwards; simply attach a fresh token for the next call.

The Salsa machinery polls the token automatically between every derived-function/input
lookup and during cache validation, so cancellation is responsive without any user code
changes. For long-running loops *inside* a single derived function, poll explicitly:

```julia
@derived function my_expensive_function(rt, x)
    for item in huge_collection
        Salsa.throw_if_cancellation_requested(rt)
        # ... expensive work ...
    end
end
```

Convenience forms for annotating an existing call site:

```julia
Salsa.with_cancellation(rt, token) do rt      # do-block form
    my_derived_function(rt, x)
end

Salsa.@cancellable token my_derived_function(rt, x)   # macro form
```

Notes:

- `with_cancellation` returns a new runtime sharing the same storage and context; memoized
  values are shared with the original runtime, and concurrent top-level calls with
  different tokens are safe.
- `Salsa.cancellation_token(rt)` returns the attached token (or `nothing`), both at the
  top level and inside derived functions.
- A cancellation that crosses a task boundary inside a derived function (e.g. via
  `@spawn`) arrives as a `TaskFailedException` and is then wrapped in
  `DerivedFunctionException` like any other error; use
  `ExceptionUnwrapping.has_wrapped_exception(e, OperationCanceledException)` to detect it.

### Flags

For maximum performance in deployed software, you can disable all runtime assertions and debug code by setting this environment variable before building Salsa: `SALSA_STATIC_DEBUG=false`.

Or, for a slightly smaller performance gain, you can toggle it at runtime via `Salsa.Debug.disable_debug()`.


## Credits

This package was closely modeled on the Rust
[`salsa`](https://github.com/salsa-rs/salsa) framework, and takes heavy inspiration from
that framework and [adapton](http://adapton.org/).

We highly recommend this talk which motivates the need for incremental, demand-driven
computation, and for packages like Salsa:
- [YouTube: Responsive compilers - Nicholas Matsakis - PLISS 2019](https://www.youtube.com/watch?v=N6b44kMS6OM&t=984s)

### Comparison with the Rust Salsa-rs framework
The underlying principles are very similar to, and inspired from that package:
It can be hard to write correct incremental programs by hand, so we provide macros
that make it easy by automatically tracking dependencies between computations.

If you are familiar with Salsa-rs, you'll see many things that are similar, with
slightly more generic names that are moved away from database-oriented naming:
- **derived queries** => **`@derived` functions**
- **query group** => **`Runtime`**
