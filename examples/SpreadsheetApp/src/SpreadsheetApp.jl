module SpreadsheetApp

export main

include("Spreadsheet.jl")
include("UI.jl")

"""
    ss = SpreadsheetApp.main([ss,]; rows=5, cols=5)

Launch an interactive spreadsheeting user interface to edit the Spreadsheet. The Spreadsheet
is returned from this function (so it can be reused) in future calls.

The Spreadsheet is built via Salsa.jl to incrementally update new values whenever their
dependencies change.
"""
function main(ss = Spreadsheet(); rows=5, cols=5)
    ui = UI.SpreadsheetDisplay(ss, maxrows=rows, maxcols=cols)
    UI.run_ui(ui)
    ss
end

end  # module Spreadsheet
