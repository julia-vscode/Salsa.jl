module Debug

export @debug_mode, enable_debug, disable_debug, enable_trace_logging, disable_trace_logging


# `static_debug_mode` is a flag that enables/disables all debug mode checks
# This defaults to true, but you can disable it either by editing this file, or by
# setting this environment variable when compiling this package. This is useful
# for performance benchmarking and in well-tested production environments.
# e.g. SALSA_STATIC_DEBUG=false
const static_debug_mode = parse(Bool, get(ENV, "SALSA_STATIC_DEBUG", "true"))


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
        _DBG[] = true
    end
    function disable_debug()
        _DBG[] = false
    end
    debug_enabled() = _DBG[]
    const _DBG = Ref(true)


    # Runtime trace logging controls
    function enable_trace_logging()
        _tracing[] = true
    end
    function disable_trace_logging()
        _tracing[] = false
    end
    trace_logging_enabled() = _tracing[]
    const _tracing = Ref(false)
else
    # Runtime Debugging is disabled.
    function _emit_debug_warning()
        @warn """
            Cannot enable runtime debug statements because debug is disabled statically.
            To enable, reload Salsa after setting `static_debug_mode = true` in:
            $(@__FILE__)
            """
    end

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
