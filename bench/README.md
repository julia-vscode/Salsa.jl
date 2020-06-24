# Salsa Benchmarks

This directory contains some simple benchmarks for the Salsa package.

## Generic Benchmarks

All benchmarks in this directory should be kept generic so that users can run them with
whatever kind of custom Runtime (with Storage and Config) that they might use in their
system. It's okay to default to the default `Runtime()`.

You can achieve this either by having the user pass in a Runtime object, or by having
them pass in a function to construct a new Runtime instance if you want the construction
to be part of what's measured, etc.

## run()

As a convention it's also nice to provide a single `run()` function for any benchmarks here,
so that if you just want to use all the standard configurations, you can run all the
benchmarks with a single function call.