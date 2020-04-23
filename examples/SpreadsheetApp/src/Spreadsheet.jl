# This Example app uses Salsa to build an incremental Spreadsheet, which is correct by
# construction.
#
# Incremental means that whenever you change the contents of any one cell, the system
# performs _only the minimum necessary computations_ to update the displayed output. That
# is, if you update cell A1, and it is used in the formula for B1, only those two cells'
# values will be recomputed and re-displayed, but any other computations needed to compute
# the display values for other cells will be remembered from when they were last updated,
# and will not need to be recomputed. This is achieved via Salsa's memoization of the
# results of `@derived` functions, and its automatic dependency detection to determine
# when to invalidate those cached values.
#
# We say it's correct by construction because we did not have to manually write _any code_
# to achieve this incrementality. Instead, below we write simple, straight-line code that
# computes the values to display for the _entire spreadsheet_, relying on Salsa to
# automatically determine which intermediate values to keep and which to recompute whenever
# a cell is modified.
#
# The code below stores the text values _entered by users_ as Salsa Inputs, and provides
# functions to _derive_ the output display strings for the entire spreadsheet, given only
# those inputs.
# In order to guarantee correctness, we only have to follow these simple rules:
#   - Do not mutate values stored in Salsa. To make this easier, this example _only uses
#     immutable types_.
#   - Derived functions must be stateless, pure functions (depend on no outside state, and
#     have no side-effects), which the below functions achieve by only depending on the
#     `@input`s on the Spreadsheet.
# In order to achieve good incremental performance, we simply have to consider which
# functions we mark as `@derived` functions, and how we split up our computations to provide
# good caching and invaldiation points. You'll see some comments on this inline, below.

using Salsa

# TODO: Change this to just use a Tuple{Symbol,Int}, since we're not using a sparse
# representation, not a matrix, anyway.
# A Cell is identified by its position. We use this identifier as the key in all the
# derived functions, below, so it's good to use the simplest possible identifier.
const CellId = Tuple{Int,Int}

# --- Salsa Inputs ------------------------------------------------------------------------
# Below we declare the Salsa inputs that we will use for our spreadsheet app. These inputs
# store values that come from users, and are the only part of the Salsa state that we can
# manipulate directly. When we make changes to the cells' source text, those changes will
# ripple through the Salsa derived functions that ultimately compute the displayed values.

# NOTE: The "Manifest Pattern"
# Notice that we have a single input that stores all the cells that the user has
# entered, and a _separate_ input that maps from those cell IDs to their text values.
# This is called the "manifest pattern," because you have one input that acts as the
# manifest (valid_cells), containing all the keys to the other map input(s).
# This is important for two reasons:
#   1. This is a syntactic requirement, because Salsa does not track changes to the
#      keys in a map, it only tracks changes to _values_.
#   2. There might be many separate things that you want to record for a given Cell:
#      Currently, we store only its text, but in the future we may also want to store
#      its color, it's formatting (bold, italics, ...), etc. For the best incremental
#      performance, you will want to keep these inputs _separate_ ("normalized"), so you
#      keep one manifest that contains the keys to all the other maps.
# NOTE: Storing Immutable Values
# For correctness, you should only use _immutable values_ in Salsa. However, for now
# for performance here we use a mutable Set. It would be better to switch to an immutable
# Set once there is better support for immutable collections. For example, we could use a
# proper immutable collection datastructure, such as those provided by
# https://github.com/JuliaCollections/FunctionalCollections.jl.
@declare_input valid_cells(rt) :: Set{CellId}
@declare_input stored_cell_text(rt, cid::CellId) :: String


# Create a struct to represent our Salsa state, which encapsulates the Salsa state,
# and initializes it with valid initial values. A new instance of a Spreadsheet
# will have a clean salsa state, with nothing in the inputs and no derived values cached.
struct Spreadsheet
    rt::Runtime{Salsa.DefaultStorage}
    # The default Constructor initializes the manifest to be empty (a common part of the
    # "Manifest Pattern").
    function Spreadsheet()
        rt = Runtime()
        set_valid_cells!(rt, Set{CellId}())
        new(rt)
    end
end

function set_cell_text!(ss::Spreadsheet, args...)
    set_cell_text!(ss.rt, args...)
end

# The manifest and the attributes (valid_cells and cell_text) must be maintained together,
# so we should always use this Setter function to update the cell text.
function set_cell_text!(salsa::Runtime, id::CellId, text::String)
    if isempty(text)
        current_valid_cells = valid_cells(salsa)
        if id in current_valid_cells
            # TODO: once new Salsa supports delete! this should delete the old value.
            # For now, just overwrite it with the empty text.
            set_stored_cell_text!(salsa, id, "")
            # Invalidate the cell
            set_valid_cells!(salsa, filter(x->x!=id, current_valid_cells))
        end
    else
        new_valid_cells = copy(valid_cells(salsa))
        push!(new_valid_cells, id)
        set_valid_cells!(salsa, new_valid_cells)
        set_stored_cell_text!(salsa, id, text)
    end
end

# This is the first derived function we've seen. It acts like a filter, which generates
# empty text ("") for strings that the user hasn't set directly, which allows us to use a
# sparse representation to store the user-entered cell text values.
# Marking this function as `@derived` causes Salsa to memoize its return values based on
# the input arguments, and also to track any _other_ derived functions or Salsa inputs that
# this function accesses, so that if the values from those functions _change_, this
# function's cached value is invalidated and it will be recomputed the next time it's
# called.
# NOTE: On Tuning the `@derived` annotation
# From a performance perspective, it might not actually make sense to make this a derived
# function, since it's body is very cheap, but we did so for explanation purposes. You will
# need to make trade-off decisions between cache size and cpu performance, motivated by
# running performance measurements. This tuning is common to working with Salsa.
@derived function cell_text(rt::Runtime, id::CellId)::String
    if cell_is_set(rt, id)
        stored_cell_text(rt,id)
    else
        ""
    end
end
# NOTE: "Firewall Pattern"
# Notice that we have a derived function here for what seems like a very cheap operation.
# This is called the "Firewall Pattern" (or the "Selector Pattern"). This derived function
# acts as a recompuation barrier by limiting the exposure to a potentially quite commonly
# modified input: the `valid_cells` manifest.
# Consider: _every time_ a new cell is added or removed, this variable will change. Without
# this barrier, that means that any function that checks if its `id` is in the manifest
# would have to rerun. In this case, if this function weren't marked `@derived`, it would
# mean that whenever you add or remove a CellId, _all the values stored in the `cell_text()`
# function would be invalidated, and would have to rerun. This barrier function prevents
# that.
@derived cell_is_set(rt::Runtime, id::CellId)::Bool = id in valid_cells(rt)

# --- Cell Type Computations -------------------------------------------------------------
# Here we have some derived functions which determine what type of cell we have, based
# on the cell's text. They look up the cell's contents based on its ID, then do some
# computation to determine the cell's type.
#
# Notice that the functions all take the *CellId* as arguments, rather than the cell's text.
# This is important for cache performance: when the cell's contents change, the _key_ for
# the Salsa cache (which method was called, and what were its arguments) remains the same,
# so that the new returned value _overwrites_ the previous one, rather than accumulating
# beside it in the cache.
#
# Notice also that we include return-type annotations on these functions, because Salsa
# uses them to construct strongly-typed Dictionaries for storing the cached values.

@derived cell_is_empty(rt::Runtime, id::CellId)::Bool = isempty(cell_text(rt, id))

@derived function cell_is_formula(rt::Runtime, id::CellId)::Bool
    text = cell_text(rt, id)
    !isempty(text) && first(text) === '='
end

# --- Cell Value Computations -------------------------------------------------------------
# Finally, we have derived functions to determine the value and display text for a given
# cell.

# AbstractUserFacingException are caught and displayed, rather than allowed to propogate and
# terminate execution. These are supposed to be in response to an error in the
# _user-supplied_ logic, rather than an error in our Julia program.
abstract type AbstractUserFacingException <: Base.Exception end

# An Exception that we throw when we encounter a bug in the user's program.
struct UserError <: AbstractUserFacingException
    err
end
# Define pretty-print method to print the UserError in the UI.
Base.print(io::IO, err::UserError) = print(io, err.err)


# This is the main derived function that runs the whole Spreadsheet App. It parses the
# contents of the user's cell into a valid Julia type, and if the cell contains a Formula,
# it evaluates the formula to compute a value.
# Notice that this function can be _recursive_ thanks to evaluating formulas:
#  - The call to `replace_varnames` will find valid `CellId`s in the user's expression,
#    and recursively call `call_value(rt, var_id)` to compute _those_ values. The cached
#    values from any intermediate computations we've written above will be automatically
#    reused.
#  - Salsa will dynamically track the dependencies between the calls to `cell_value()`,
#    which automatically provides the dependencies between cells, based on the user-provided
#    values. Whenever formulas change, they'll be recomputed and the dependencies will be
#    automatically updated.
@derived function cell_value(rt::Runtime, id::CellId)::Any
    if cell_is_empty(rt, id)
        ""
    elseif cell_is_formula(rt, id)
        try
            expr = Meta.parse(cell_text(rt, id)[2:end])
            expr = replace_varnames(rt, expr)
            Meta.eval(expr)
        catch e
            if e isa AbstractUserFacingException
                e  # Return the exception
            elseif !(e isa InterruptException)
                UserError(e)
            else
                rethrow()
            end
        end
    else
        try
            Meta.parse(cell_text(rt, id))
        catch
            # If it doesn't parse, treat it as a string! :)
            cell_text(rt, id)
        end
    end
end

function replace_varnames(rt::Runtime, expr)
    # Recursively walk through the expression replacing references to cell ids and ranges
    # with their values. The recursive walk!() function is defined below and called on
    # `expr` at the very end.

    walk!(node) = node
    # Convert variables that look like :A1 to the value stored at cell-id "A1"
    function walk!(var::Symbol)
        var_id = cell_id_from_name(var)
        if var_id === nothing
            # If it's not a cell id, we assume it's a builtin symbol, like :sum or :+
            var
        else
            # NOTE: Here is the recursive call to cell_value(rt, var_id). Without any
            # explicit work on our part, this creates a dependency from the current CellId
            # being computed to the
            return QuoteNode(cell_value(rt, var_id))
        end
    end
    # Convert Range expressions containing variable names :(A1:B1) to a 2D matrix of the
    # values in the cells stored in that range.
    function walk!(e::Expr)
        if e.head === :call && e.args[1] === Symbol(":")
            length(e.args) === 3 || throw(UserError("BAD RANGE: `$e`"))
            r_start = cell_id_from_name(e.args[2])
            r_end = cell_id_from_name(e.args[3])
            # Return a 2D Matrix of the values
            tasks = Task[Threads.@spawn cell_value(rt, (r,c))
             for r in r_start[1]:r_end[1], c in r_start[2]:r_end[2]
            ]
            map(fetch, tasks)
        else
            # Recursive walk on all elements of the Expr
            Expr(e.head, walk!.(e.args)...)
        end
    end

    # Walk the Expr and do the replacements.
    walk!(expr)
end

# Finally, we have the function that converts a cell's _actual value_ into a String.
# This is also a `@derived` function because printing the value to a string might be
# expensive, so it might be worth caching. Again, this tradeoff should be made after
# performance measurements.
#
# The final missing piece of our spreadsheet app is in the UI.jl file, where the display
# functions loop over the entire range of the spreadsheet currently in-view from the UI, and
# call this function on each cell.
@derived function cell_display_str(rt::Runtime, id::CellId)::String
    try
        v = cell_value(rt, id)
        if v isa Exception
            "#ERR#"
        else
            string(v)
        end
    catch e
        "#CRSH"
    end
end

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
