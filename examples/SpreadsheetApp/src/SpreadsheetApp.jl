module SpreadsheetApp

export main

# --- Build Spreadsheet App -----------------------------------------------------------

using Salsa

const CellId = Tuple{Int,Int}

@component Spreadsheet begin
    @input valid_cells :: InputScalar{Tuple{Vararg{CellId}}}
    @input cell_text :: InputMap{CellId, String}
    function Spreadsheet()
        ss = Salsa.create(Spreadsheet)
        ss.valid_cells[] = ()
        ss
    end
end

function set_cell_text!(ss::Spreadsheet, id::CellId, text::String)
    ss.valid_cells[] = (ss.valid_cells[]..., id)
    ss.cell_text[id] = text
end

@derived function cell_text(ss::Spreadsheet, id::CellId)
    if id in ss.valid_cells[]
        ss.cell_text[id]
    else
        ""
    end
end
@derived function cell_is_formula(ss::Spreadsheet, id::CellId)
    text = cell_text(ss, id)
    !isempty(text) && first(text) === '='
end
@derived function cell_type(ss::Spreadsheet, id::CellId)
    text = cell_text(ss, id)
    !isempty(text) && first(text) === '='
end

@derived function cell_value(ss::Spreadsheet, id::CellId)
    if cell_is_formula(ss, id)
        # TODO
    else
        Meta.parse(cell_text(ss, id))
    end
end

# --- UI ------------------------------------------------------------
include("UI.jl")

function main(; rows=3, cols=3)
    ss = Spreadsheet()
    #set_cell_text!(ss, (1,1), "hi")
    ui = UI.SpreadsheetDisplay(ss, maxrows=rows, maxcols=cols)
    UI.run_ui(ui)
end


end  # module Spreadsheet
