# Salsa

[![Build Status](https://travis-ci.com/RelationalAI-oss/Salsa.jl.svg?branch=master)](https://travis-ci.com/RelationalAI-oss/Salsa.jl)

A framework for on-demand, incremental computation via memoization, inspired by Rust lang's
[salsa-rs/salsa](https://github.com/salsa-rs/salsa).

- `@component`
- `@derived`
- `@input`

## Warning
This package is in early stages of work-in-progress, and changing frequently.

## Credits
This package was closely modelled off of the Rust
[`salsa`](https://github.com/salsa-rs/salsa) framework, and takes heavy inspiration from
that framework and [adapton](http://adapton.org/).

### Comparison with the Rust Salsa-rs framework
The underlying principles are very similar to, and inspired from that package:
It can be hard to write correct incremental programs by hand, so we provide macros
that make it easy by automatically tracking dependencies between computations.

If you are familiar with Salsa-rs, you'll notice that we've adjusted the naming to be
slightly more generic, and moved away from the database-oriented naming:
- **derived queries** => **`@derived` functions**
- **query groups** => **`@component`s**

We made this change because we are actually using Salsa.jl _to build a Database system_
(http://relational.ai), so it was a bit confusing internally to have overlapping names for
things. :)  However, as we're using Salsa more, and becoming more familiar, we can
definitely see the connection with database concepts, so we're happy to revisit this
decision in the future!
