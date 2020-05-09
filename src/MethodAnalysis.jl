module MethodAnalysis

using Core: MethodInstance, SimpleVector

export visit, visit_backedges, all_backedges, terminal_backedges

include("visit.jl")
include("backedges.jl")

end # module
