module UI

using ..SpreadsheetApp: SpreadsheetApp, Spreadsheet, CellId, cell_text, set_cell_text!

import TerminalMenus
import TerminalMenus: request

Base.@kwdef mutable struct SpreadsheetDisplay <: TerminalMenus.AbstractMenu
    # Spreadsheet fields
    ss::Spreadsheet
    maxrows::Int = 0
    maxcols::Int = 0
    # Track the current position (TerminalMenus only provides vertical scrolling)
    row_cursor::Int = 1
    column_cursor::Int = 1

    # Required TerminalMenus fields
    pagesize::Int = maxrows + 3
    pageoffset::Int = 0
    # Output when user hits enter
    selected::CellId = (-1,-1)
end
SpreadsheetDisplay(ss::Spreadsheet; kwargs...) = SpreadsheetDisplay(ss=ss; kwargs...)


function TerminalMenus.options(ui::SpreadsheetDisplay)
    [
        #"---",  # Fake row for the column cursor
        #([cell_text(ui.ss, (r,i)) for i in 1:ui.maxcols] for r in 1:ui.maxrows)...,
        ""
        for _ in 1:ui.maxrows
    ]
end
TerminalMenus.cancel(ui::SpreadsheetDisplay) = ui.selected = (-1,-1)

function TerminalMenus.header(ui::SpreadsheetDisplay)
    ""
#    "Cell Text:\n" *
#    cell_text(ui.ss, (ui.row_cursor, ui.column_cursor)) *"\n" *
#    repeat("-", 10)
#
#    cursor_len = length(TerminalMenus.CONFIG[:cursor])
#    # print a ">" on the selected entry
#    selected_row ? print(buf, TerminalMenus.CONFIG[:cursor]) : print(buf, repeat(" ", cursor_len))
#    print(buf, " ") # Space between cursor and text
#
#    line = replace(ui.options[idx], "\n" => "\\n")
#    line,ui.scroll_horizontal = _custom_trimWidth(line, term_width, selected_row, cursor_len, ui.scroll_horizontal)
#
#    print(buf, line)
#    #"""
#    #Select a field to recurse into or ↩ to ascend. [q]uit.
#    #"""
#    #=
#    Toggles: [o]ptimize, [w]arn, [d]ebuginfo, [s]yntax highlight for Source/LLVM/Native.
#    Show: [S]ource code, [A]ST, [L]LVM IR, [N]ative code
#    Advanced: dump [P]arams cache.
#    =#
end

function TerminalMenus.keypress(ui::SpreadsheetDisplay, key::UInt32)
    #if key == UInt32('w')
    #    ui.toggle = :warn
    #    return true
    if key == Int(TerminalMenus.ARROW_RIGHT)
        ui.column_cursor = min(ui.maxcols, ui.column_cursor + 1)
    elseif key == Int(TerminalMenus.ARROW_LEFT)
        ui.column_cursor = max(1, ui.column_cursor - 1)
    end
    return false
end

function TerminalMenus.pick(ui::SpreadsheetDisplay, row_cursor::Int)
    ui.selected = (row_cursor, ui.column_cursor)
    return true # break out of the ui
end
function TerminalMenus.writeLine(buf::IOBuffer, ui::SpreadsheetDisplay, idx::Int, selected_row::Bool, term_width::Int)
    row = idx - 3   # To account for the fake rows

    # Store the current cursor in the ui:
    if selected_row
        ui.row_cursor = idx
    end

    # Handle fake row for column cursor
    if idx === 1
        print(buf, "Cell Value:")
    elseif idx === 2
        print(buf, cell_text(ui.ss, (ui.row_cursor, ui.column_cursor)))
    elseif idx === 3
        print(buf, repeat("-", 5))
    else
        cell_row = [cell_text(ui.ss, (row,i)) for i in 1:ui.maxcols]

        # Display the line
        line = if ui.row_cursor == row
            join(["", (lpad(c,3) for c in cell_row[1:ui.column_cursor-1])...], "|") *
            "[" * lpad(cell_row[ui.column_cursor],3) * "]" *
            join([(lpad(c,3) for c in cell_row[ui.column_cursor+1:end])..., ""], "|")
        else
            join(["", (lpad(c,3) for c in cell_row)..., ""], "|")
        end

        print(buf, line)
    end
end


function run_ui(ui::SpreadsheetDisplay)
    while true
        choice = request("", ui)
        print(stdout, "Enter new contents: ")
        new_text = readline(stdin)
        set_cell_text!(ui.ss, choice, new_text)
    end
end

end  # module