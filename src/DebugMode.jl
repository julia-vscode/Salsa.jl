module Debug

export @debug_mode, enable_debug, disable_debug, enable_trace_logging, disable_trace_logging


# `static_debug_mode` is a flag that enables/disables all debug mode checks
const static_debug_mode = true


"""
@debug_mode expr...

Execute `expr` only when static and runtime debug modes are enabled.
"""
macro debug_mode(instr)
    if static_debug_mode
        quote
            if debug_enabled()
                $(esc(instr))
            end
        end
    else
        :()
    end
end

if static_debug_mode
    # Runtime debug mode controls
    function enable_debug()
        global _DBG = true
    end
    function disable_debug()
        global _DBG = false
    end
    debug_enabled() = _DBG
    _DBG = true


    # Runtime trace logging controls
    function enable_trace_logging()
        global _tracing = true
    end
    function disable_trace_logging()
        global _tracing = false
    end
    trace_logging_enabled() = _tracing
    _tracing = false
else
    # Runtime Debugging is disabled.
    _emit_debug_warning() = 
        @warn """
        Cannot enable runtime debug statements because debug is disabled statically.
        To enable, reload Salsa after setting `static_debug_mode = true` in:
        $(@__FILE__)
        """

    enable_debug() = _emit_debug_warning()
    disable_debug() = _emit_debug_warning()
    debug_enabled() = false

    enable_trace_logging() = _emit_debug_warning()
    disable_trace_logging() = _emit_debug_warning()
    trace_logging_enabled() = false
end

macro dbg_log_trace(expr)
    if static_debug_mode
        quote
            if trace_logging_enabled()
                $(esc(expr))
            end
        end
    else
        :()
    end
end

end # module Debug
