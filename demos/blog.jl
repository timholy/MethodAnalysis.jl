## Counting invalidations

using SnoopCompile
umis(list) = unique(filter(x->isa(x, Core.MethodInstance), list))
# length(umis(@snoopr using <SomePkg>))

## Counting MethodInstances

using MethodAnalysis
using MethodAnalysis: equal
function getmis(parent...)  # use either 0 or 1 argument
    mis = Set{Core.MethodInstance}()
    visit(parent...) do mi
        isa(mi, Core.MethodInstance) && push!(mis, mi)
    end
    return collect(mis)
end
mis = getmis()

# counting by module

const modcounts = Dict{Module,Int}()
for mi in mis
    modcounts[mi.def.module] = get(modcounts, mi.def.module, 0) + 1
end

## When invalidations do and do not happen: the applyf example

f(::Int) = 1
f(::Bool) = 2
function applyf(container)
    x1 = f(container[1])
    x2 = f(container[2])
    return x1 + x2
end
c = Any[1, true]
applyf(c)
w2 = worlds(only(getmis(applyf)))
code2 = only(code_typed(applyf, (Vector{Any},)))
f(::String) = 3
applyf(c)
w3 = worlds(only(getmis(applyf)))
code3 = only(code_typed(applyf, (Vector{Any},)))
f(::AbstractVector) = 4   # if we replaced this with Vector{Int} we'd not have the unnecessary(?) invalidation
applyf(c)
w4 = worlds(only(getmis(applyf)))
code4 = only(code_typed(applyf, (Vector{Any},)))
f(::Missing) = 5
applyf(c)
w5 = worlds(only(getmis(applyf)))
code5 = only(code_typed(applyf, (Vector{Any},)))
f(::Nothing) = 6
applyf(c)
w6 = worlds(only(getmis(applyf)))
code6 = only(code_typed(applyf, (Vector{Any},)))

# One apparently-unnecessary invalidation?
@show equal(code3, code4)
@show w3 == w4

## Determining the source of invalidations

# Mechanism 1: MethodInstances with TypeVar parameters
function hastv(typ)
    isa(typ, UnionAll) && return true
    if isa(typ, DataType)
        for p in typ.parameters
            hastv(p) && return true
        end
    end
    return false
end
mitv = filter(mi->hastv(mi.specTypes), mis)
mitvi = Set(with_all_backedges(mitv))
