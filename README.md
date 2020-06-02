# Salsa.jl

[![Build Status](https://travis-ci.com/RelationalAI-oss/Salsa.jl.svg?branch=master)](https://travis-ci.com/RelationalAI-oss/Salsa.jl)

A framework for on-demand, incremental computation via memoization, inspired by Rust lang's
[salsa-rs/salsa](https://github.com/salsa-rs/salsa).

- `@derived`
- `@declare_input`

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
