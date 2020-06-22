# Salsa.jl

[![Build Status](https://travis-ci.com/RelationalAI-oss/Salsa.jl.svg?branch=master)](https://travis-ci.com/RelationalAI-oss/Salsa.jl)

A framework for on-demand, incremental computation via memoization, inspired by Rust lang's
[salsa-rs/salsa](https://github.com/salsa-rs/salsa).

- `@derived`
- `@declare_input`


## Usage

```julia
julia> @declare_input x(rt)::Int
(x, set_x!, delete_x!)

julia> @derived function x_plus_one(rt)
           println("x_plus_one")
           return x(rt) + 1
       end
x_plus_one (generic function with 1 method)
```
```julia
julia> rt = Runtime();

julia> set_x!(rt, 1)

julia> x_plus_one(rt)
x_plus_one
2

julia> x_plus_one(rt)
2

julia> set_x!(rt, 10)

julia> x_plus_one(rt)
x_plus_one
11
```




## Credits
This package was closely modelled off of the Rust
[`salsa`](https://github.com/salsa-rs/salsa) framework, and takes heavy inspiration from
that framework and [adapton](http://adapton.org/).

### Comparison with the Rust Salsa-rs framework
The underlying principles are very similar to, and inspired from that package:
It can be hard to write correct incremental programs by hand, so we provide macros
that make it easy by automatically tracking dependencies between computations.

If you are familiar with Salsa-rs, you'll see many things that are familiar and a few
slightly more generic, and moved away from the database-oriented naming:
- **derived queries** => **`@derived` functions**
- **query group** => **`Runtime`**
