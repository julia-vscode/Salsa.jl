# Salsa.jl Example: SpreadsheetApp

A very simple incremental spreadsheet application which is correct by construction, and
using incremental computation provided by Salsa to only recompute the minimal number of
cells as needed when an input changes.

The App and the UI were built over a few hours, with the UI using
[TerminalMenus.jl](https://github.com/nick-paul/TerminalMenus.jl). The UI is simple, but
sufficient to show the behavior.

The Spreadsheet App is written in less than two hundred lines and uses Salsa's
macros to automatically detect dependencies between julia functions at runtime. This
creates the necessary dependencies between cells to correctly incrementally recompute
results.

Here is a demo of the simple Spreadsheet App, showcasing formulas containing arbitrary julia
code, resolving references to other cells, and interactive editing:

<a href="https://asciinema.org/a/DmXipKlztALrzSF0vnrK86Lle"><img alt="ascii screen cast" src="https://asciinema.org/a/DmXipKlztALrzSF0vnrK86Lle.svg" align="center" width="640" ></a>
