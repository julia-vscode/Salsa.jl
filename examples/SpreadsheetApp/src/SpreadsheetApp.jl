module SpreadsheetApp

export main

# --- UI ------------------------------------------------------------
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
@derived text_is_empty(ss::Spreadsheet, text::String) = isempty(text)
cell_is_empty(ss::Spreadsheet, id::CellId) = text_is_empty(ss, cell_text(ss, id))

@derived function cell_is_formula(ss::Spreadsheet, id::CellId)
    text = cell_text(ss, id)
    !isempty(text) && first(text) === '='
end
@derived function cell_type(ss::Spreadsheet, id::CellId)
    text = cell_text(ss, id)
    !isempty(text) && first(text) === '='
end

abstract type UserFacingException <: Base.Exception end

struct NodeError <: UserFacingException
    err
end
# Define pretty-print method to print in cells
Base.print(io::IO, err::NodeError) = print(io, err.err)

function column_idx_to_name(i)
    out = ""
    while i > 0
        l = (i-1)%26+1
        out = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[l] * out
        i ÷= 26
    end
    out
end
function column_name_to_idx(name)
    name = uppercase(name)
    i = 0
    l = 0
    while l < length(name)
        i += 26^l*findfirst(name[end-l], "ABCDEFGHIJKLMNOPQRSTUVWXYZ")[1]
        l += 1
    end
    i
end
column_idx_to_name(703)
column_name_to_idx("aaa")


function cell_id_from_name(var::Symbol)
    s = String(var)
    s = uppercase(s)
    if (m = match(r"([A-Z]+)([0-9]+)", s); m !== nothing)
        col,row = m.captures
        row,col = parse(Int,row),column_name_to_idx(col)
        return (row,col)
    else
        nothing
    end
end


function replace_varnames(ss::Spreadsheet, expr)
    walk(node) = node
    # var matches :A1
    function walk(var::Symbol)
        id = cell_id_from_name(var)
        if id === nothing
            #throw(NodeError("BAD VAR: `$var`"))
            # Might be builtin symbol, like :sum or :+
            var
        else
            return QuoteNode(cell_value(ss, id))
        end
    end
    function walk(e::Expr)
        # range expression
        if e.head === :call && e.args[1] === Symbol(":")
            length(e.args) === 3 || throw(NodeError("BAD RANGE: `$e`"))
            r_start = cell_id_from_name(e.args[2])
            r_end = cell_id_from_name(e.args[3])
            # Return a 2D Matrix of the values
            [cell_value(ss, (r,c))
             for r in r_start[1]:r_end[1], c in r_start[2]:r_end[2]
            ]
        else
            # Recursive walk
            Expr(e.head, walk.(e.args)...)
        end
    end
    walk(expr)
end
@derived function cell_value(ss::Spreadsheet, id::CellId)
    if cell_is_formula(ss, id)
        try
            expr = Meta.parse(cell_text(ss, id)[2:end])
            expr = replace_varnames(ss, expr)
            Meta.eval(expr)
        catch e
            if e isa UserFacingException
                e  # Return the exception
            elseif !(e isa InterruptException)
                NodeError(e)
            else
                rethrow()
            end
        end
    elseif cell_is_empty(ss, id)
        ""
    else
        try
            Meta.parse(cell_text(ss, id))
        catch
            # If it doesn't parse, treat it as a string! :)
            cell_text(ss, id)
        end
    end
end
function cell_display_str(ss::Spreadsheet, id::CellId)
    try
        v = cell_value(ss, id)
        if v isa Exception
            "#ERR#"
        else
            string(v)
        end
    catch e
        "#CRSH"
    end
end


end  # module Spreadsheet
