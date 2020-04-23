module DebugMode

export @debug_mode, DBG

# This file was modeled on the debug mode found in src/QueryEvaluator/trie_interface.jl

DBG = true

"""
`debug_mode` is a flag that enables/disables the debug mode for the query evaluator
"""
const debug_mode = true

if debug_mode
    """
    Execute only in debug mode
    """
    macro debug_mode(instr)
        esc(:(
            if DBG != Nothing
                $instr
            end
        ))
    end

else
    macro debug_mode(instr)
        :()
    end
end

end # module DebugMode
