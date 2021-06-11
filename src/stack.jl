# A linked list node used to implement a stack trace within Salsa. This trace is used for
# debug tracing (to understand incrementality) and is included in Salsa wrapped Exceptions
# for improved error information.
#
# We use a linked list for the stack since it is immutable and so will be thread-safe, and
# because the linked list can be cheaply branched (becoming a "tree") for derived functions
# called concurrently with the same parent.
struct SalsaStackFrame
    dp::DependencyKey
    next::Union{Nothing,SalsaStackFrame}
end

function Base.collect(stack::SalsaStackFrame)
    out = DependencyKey[stack.dp]
    while stack.next !== nothing
        stack = stack.next
        push!(out, stack.dp)
    end
    return out
end

Base.length(stack::SalsaStackFrame) = stack.next === nothing ? 1 : length(stack.next) + 1

# For cycle detection in Debug-mode:
# NOTE: The quadratic performance hazard here; this is to be disabled in production.
function stack_has_key(stack::Union{SalsaStackFrame,Nothing}, key)
    # NOTE: Important that this is implemented as a while-loop, not a recursive function
    # because it may be called already deep in the stack, and this could effectively double
    # the current stack height. (It seems that the tail-recursion optimization is missed
    # with juila -O0, so it's better to do this manually.)
    while stack !== nothing
        if stack.dp == key
            return true
        end
        stack = stack.next
    end
    return false
end
