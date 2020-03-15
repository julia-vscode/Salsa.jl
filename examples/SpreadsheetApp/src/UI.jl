module UI

import REPL

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
    pagesize::Int = maxrows + 4
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
#    #"""
#    #Select a field to recurse into or ↩ to ascend. [q]uit.
#    #"""
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

    col_width = 5

    # Handle fake row for column cursor
    if idx === 1
        print(buf, "Cell Text:")
    elseif idx === 2
        print(buf, cell_text(ui.ss, (ui.row_cursor, ui.column_cursor)))
    elseif idx === 3
        print(buf,
            join(["  ",
                  (lpad(rpad(SpreadsheetApp.column_idx_to_name(i),2), col_width)
                   for i in 1:ui.maxcols)..., ""], " "))
    # Handle fake final row for error display
    elseif idx === ui.pagesize
        try
            selected_value = SpreadsheetApp.cell_value(ui.ss, (ui.row_cursor, ui.column_cursor))

            if selected_value isa SpreadsheetApp.UserFacingException
                print(buf, selected_value.err)
            elseif selected_value isa Exception
                print(buf, selected_value)
            end
        catch e
            print(buf, "Error while rendering: $e")
        end
    else
        cell_row = [lpad(SpreadsheetApp.cell_display_str(ui.ss, (row,i)), col_width)  for i in 1:ui.maxcols]

        # Display the line
        line = if ui.row_cursor == row
            join(["$row ", cell_row[1:ui.column_cursor-1]...], "|") *
            "[$(cell_row[ui.column_cursor])]" *
            join([cell_row[ui.column_cursor+1:end]..., ""], "|")
        else
            join(["$row ", cell_row..., ""], "|")
        end

        print(buf, line)
    end
end

# This is sad, but we need to override this whole function to correctly handle printing the
# header:
# The generic printMenu function is used for displaying the state of a
#   menu to the screen. Menus must implement `writeLine` and `options`
#   and have fields `pagesize::Int` and `pageoffset::Int` as part of
#   their type definition
function TerminalMenus.printMenu(out, m::SpreadsheetDisplay, cursor::Int; init::Bool=false)
    # Store the current cursor in the ui:
    m.row_cursor = cursor

    # -----------------------------------------------------------------------
    # --- The rest of this function is copied verbatim from TerminalMenus ---


    TerminalMenus.CONFIG[:supress_output] && return

    buf = IOBuffer()

    # Move the cursor to the beginning of where it should print
    # Don't do this on the initial print
    lines = m.pagesize-1
    if init
        m.pageoffset = 0
    else
        print(buf, "\x1b[999D\x1b[$(lines)A")
    end

    for i in (m.pageoffset+1):(m.pageoffset + m.pagesize)
        print(buf, "\x1b[2K")

        if i == m.pageoffset+1 && m.pageoffset > 0
            # first line && scrolled past first entry
            print(buf, TerminalMenus.CONFIG[:up_arrow])
        elseif i == m.pagesize+m.pageoffset && i != length(TerminalMenus.options(m))
            # last line && not last option
            print(buf, TerminalMenus.CONFIG[:down_arrow])
        else
            # non special line
            print(buf, " ")
        end

        term_width = REPL.Terminals.width(TerminalMenus.terminal)

        TerminalMenus.writeLine(buf, m, i, i == cursor, term_width)

        # don't print an \r\n on the last line unless there is only one line
        if m.pagesize == 1 || i != (m.pagesize+m.pageoffset)
            print(buf, "\r\n")
        end
    end

    print(out, String(take!(buf)))
end


function run_ui(ui::SpreadsheetDisplay)
    TerminalMenus.config(down_arrow = ' ')
    while true
        choice = request("", ui)
        if choice === (-1,-1)
            return
        end
        print(stdout, "Enter new contents: ")
        new_text = readline(stdin)
        set_cell_text!(ui.ss, choice, new_text)
    end
end

end  # module
