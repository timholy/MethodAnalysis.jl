using MethodAnalysis
using OrderedCollections
using CodeTracking

const fully_specialized = OrderedSet{Core.MethodInstance}()
const nonspecialized = OrderedSet{Core.MethodInstance}()

function categorizemi(mi::Core.MethodInstance)
    bin = all(isconcretetype, Base.unwrap_unionall(mi.specTypes).parameters) ?
        fully_specialized : nonspecialized
    push!(bin, mi)
    return nothing
end

categorizemi(x) = nothing

visit(categorizemi)
# We're especially interested in the nonspecialized ones.
# Start with the ones in Base
ubase = collect(filter(x->x.def.module === Base, nonspecialized))
# Find specializations that have a TypeVar parameter
function hastv(typ)
    isa(typ, UnionAll) && return true
    if isa(typ, DataType)
        for p in typ.parameters
            hastv(p) && return true
        end
    end
    return false
end
ubasetv = filter(mi->hastv(mi.specTypes), ubase)
println("There are ", length(ubasetv), " MethodInstances that have a TypeVar in specTypes")

# Let's analyze a specific case
m = which(similar, (Vector,))
mitv = Ref{Any}(nothing)
visit(m.specializations) do mi
    mitv[] isa Core.MethodInstance && return nothing
    hastv(mi.specTypes) && (mitv[] = mi)
    return nothing
end
mi = mitv[]
@assert(mi isa Core.MethodInstance)
println("\n## Performing an analysis of ", mi, '\n')
# Find the last callers with TypeVar specializations
callers = Core.MethodInstance[]
visit_backedges(mi) do caller
    hastv(caller.specTypes) || return false
    if !isdefined(caller, :backedges)
        push!(callers, caller)
        return false
    end
    foundone = false
    for edge in caller.backedges
        if !hastv(edge.specTypes)
            push!(callers, caller)
            foundone = true
            break
        end
    end
    return !foundone
end
# Let's look at the code of these callers
for caller in callers
    for be in caller.backedges
        f, t = call_type(be.specTypes)
        println("caller: ", be.def, " with argtypes ", t)
        code_warntype(f, t)
        println()
    end
end
