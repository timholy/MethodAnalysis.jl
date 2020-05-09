module MethodAnalysis

using Core: MethodInstance, SimpleVector

export visit, visit_backedges, all_backedges, terminal_backedges, call_type

include("visit.jl")
include("backedges.jl")

"""
    call_type(tt)

Split a signature type like `Tuple{typeof(f),ArgTypes...}` back out to `(f, Tuple{ArgTypes...})`
"""
function call_type(tt)
    ft = tt.parameters[1]
    argt = Tuple{tt.parameters[2:end]...}
    name = ft.name
    return (getfield(name.module, Symbol(String(name.name)[2:end])), argt)
end

end # module
