"""
    all_backedges(mi::MethodInstance)

Return a list of all backedges (direct and indirect) of `mi`.
"""
function all_backedges(mi::MethodInstance)
    backedges = Set{MethodInstance}()
    visit_backedges(x->(push!(backedges, x); true), mi)
    delete!(backedges, mi)
    return collect(backedges)
end

"""
    with_all_backedges(itr)

Return all MethodInstances detected when iterating through items in `itr` and any
their backedges. The result includes both MethodTable and MethodInstance backedges.
"""
function with_all_backedges(iter)
    backedges = Set{MethodInstance}()
    visited = Set{Union{MethodInstance,MethodTable}}()
    for item in iter
        visit_backedges(item, visited) do edge
            if isa(edge, MethodInstance)
                push!(backedges, edge)
            else
                sig, mi = edge
                push!(backedges, mi)
            end
            true
        end
    end
    return collect(backedges)
end

"""
    terminal_backedges(mi::MethodInstance)

Obtain the "ultimate callers" of `mi`, i.e., the reason(s) `mi` was compiled.
"""
function terminal_backedges(mi::MethodInstance)
    backedges = Set{MethodInstance}()
    visit_backedges(mi) do x
        if !isdefined(x, :backedges) || isempty(x.backedges)
            push!(backedges, x)
        end
        true
    end
    delete!(backedges, mi)
    return collect(backedges)
end

"""
    direct_backedges(f::Function; skip=true)

Collect all backedges for a function `f` as pairs `instance=>caller` or `sig=>caller` pairs.
The latter occur for MethodTable backedges.
If `skip` is `true`, any `caller` listed in a MethodTable backedge is omitted from the instance backedges. 
"""
function direct_backedges(f::Function; skip::Bool=true)
    bes = []
    _skip = Set{MethodInstance}()
    mths = methods(f).ms
    callee = nothing
    visit_backedges(f) do item
        if isa(item, Pair)
            push!(bes, item)
            push!(_skip, item.second)
            return false
        else
            mi = item::MethodInstance
            if mi.def ∈ mths
                callee = mi
                return true
            else
                (!skip || mi ∉ _skip) && push!(bes, callee=>mi)
                return false
            end
        end
    end
    return bes
end
