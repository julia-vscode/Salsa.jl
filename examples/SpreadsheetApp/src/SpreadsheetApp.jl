module SpreadsheetApp

export main

include("Spreadsheet.jl")
include("UI.jl")

"""
    SpreadsheetApp.main([ss,]; rows=5, cols=5)

Launch an interactive spreadsheeting user interface to edit the Spreadsheet. The Spreadsheet
is returned from this function (so it can be reused) in future calls.
"""
function main(ss = Spreadsheet(); rows=5, cols=5)
    #set_cell_text!(ss, (1,1), "hi")
    ui = UI.SpreadsheetDisplay(ss, maxrows=rows, maxcols=cols)
    UI.run_ui(ui)
    ss
end

end  # module Spreadsheet
