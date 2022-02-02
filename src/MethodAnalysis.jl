module MethodAnalysis

using AbstractTrees

using Base: Callable, IdSet
using Core: MethodInstance, CodeInfo, SimpleVector, MethodTable
using Base.Meta: isexpr

export visit, call_type, methodinstance, methodinstances, worlds  # findcallers is exported from its own file
export visit_backedges, all_backedges, with_all_backedges, terminal_backedges, direct_backedges

include("visit.jl")
include("backedges.jl")

if !hasmethod(==, Tuple{Core.PhiNode,Core.PhiNode})
    Base.:(==)(stmt1::Core.PhiNode, stmt2::Core.PhiNode) = stmt1.edges == stmt2.edges && stmt1.values == stmt2.values
end

"""
    call_type(tt)

Split a signature type like `Tuple{typeof(f),ArgTypes...}` back out to `(f, Tuple{ArgTypes...})`
"""
function call_type(tt)
    ft = tt.parameters[1]
    argt = Tuple{tt.parameters[2:end]...}
    name = Symbol(String(ft.name.name)[2:end])  # strip off leading '#'
    return (getfield(ft.name.module, name), argt)
end

"""
    minmaxs = worlds(mi::MethodInstance)

Collect the (min,max) world-age pairs for all CodeInstances associated with `mi`.
"""
function worlds(mi::Core.MethodInstance)
    w = Tuple{UInt,UInt}[]
    if isdefined(mi, :cache)
        ci = mi.cache
        push!(w, (ci.min_world % UInt, ci.max_world % UInt))
        while isdefined(ci, :next)
            ci = ci.next
            push!(w, (ci.min_world % UInt, ci.max_world % UInt))
        end
    end
    return w
end

# Not sure we want to change the meaning of == here, so let's define our own name
# A few fields are deliberately unchecked
function equal(ci1::Core.CodeInfo, ci2::Core.CodeInfo)
    ret = ci1.code == ci2.code &&
          ci1.codelocs == ci2.codelocs &&
          ci1.ssavaluetypes == ci2.ssavaluetypes &&
          ci1.ssaflags == ci2.ssaflags &&
          ci1.method_for_inference_limit_heuristics == ci2.method_for_inference_limit_heuristics &&
          ci1.linetable == ci2.linetable &&
          ci1.slotnames == ci2.slotnames &&
          ci1.slotflags == ci2.slotflags
    if VERSION >= v"1.2"
        ret &= ci1.slottypes == ci2.slottypes &&
               ci1.rettype == ci2.rettype
    end
    return ret
end
equal(p1::Pair, p2::Pair) = p1.second == p2.second && equal(p1.first, p2.first)

"""
    mi = methodinstance(f, types)
    mi = methodinstance(tt::Type{<:Tuple})

Return the `MethodInstance` `mi` for function `f` and the given `types`,
or for the complete signature `tt`.
If no version compiled for these types exists, returns `nothing`.

# Examples

```jldoctest; setup=:(using MethodAnalysis)
julia> f(x, y::String) = 2x; f(x, y::Number) = x + y;

julia> f(1, "hi"); f(1, 1.0);

julia> methodinstance(f, (Int, String))
MethodInstance for f(::Int64, ::String)

julia> methodinstance(Tuple{typeof(f), Int, String})
MethodInstance for f(::Int64, ::String)
```
"""
methodinstance(@nospecialize(f), @nospecialize(types)) =
    _methodinstance(f, types, false)

function methodinstance(@nospecialize(types))
    f, argt = call_type(types)
    return methodinstance(f, types)
end

function _methodinstance(@nospecialize(f), @nospecialize(types), multi::Bool)
    if types isa Tuple
        tt = Tuple{typeof(f), types...}
        return _methodinstance(f, tt, multi)
    end
    kept = MethodInstance[]
    visit(f) do mi
        if isa(mi, MethodInstance)
            if multi ? (mi.specTypes <: types) : (mi.specTypes === types)
                push!(kept, mi)
            end
            return false
        end
        true
    end
    multi && return kept
    length(kept) == 1 && return kept[1]
    length(kept) == 0 && return nothing
    error(length(kept), " MethodInstances matched the specified types")
end

"""
    methodinstances()
    methodinstances(top)

Collect all `MethodInstance`s, optionally restricting them to a particular module, function, method, or methodlist.

# Examples

```
julia> sin(π/2)
1.0

julia> sin(0.8f0)
0.7173561f0

julia> methodinstances(sin)
2-element Vector{Core.MethodInstance}:
 MethodInstance for sin(::Float64)
 MethodInstance for sin(::Float32)

julia> m = which(convert, (Type{Bool}, Real))
convert(::Type{T}, x::Number) where T<:Number in Base at number.jl:7

julia> methodinstances(m)
68-element Vector{Core.MethodInstance}:
 MethodInstance for convert(::Type{UInt128}, ::Int64)
 MethodInstance for convert(::Type{Int128}, ::Int64)
 MethodInstance for convert(::Type{Int64}, ::Int32)
 MethodInstance for convert(::Type{UInt64}, ::Int64)
 ⋮
```

Note the method `m` was broader than the signature we queried with, and the returned `MethodInstance`s reflect that breadth.
See [`methodinstances`](@ref) for a more restrictive subset.
"""
function methodinstances(top=())
    if isa(top, Module) || isa(top, Function) || isa(top, Type) || isa(top, Method) || isa(top, Base.MethodList)
        top = (top,)
    end
    mis = Core.MethodInstance[]
    visit(top...) do item
        isa(item, Core.MethodInstance) || return true
        push!(mis, item)
        false
    end
    return mis
end

"""
    methodinstances(f, types)
    methodinstances(tt::Type{<:Tuple})

Return all MethodInstances whose signature is a subtype of `types`.

# Example

```
julia> methodinstances(convert, (Type{Bool}, Real))
2-element Vector{Core.MethodInstance}:
 MethodInstance for convert(::Type{Bool}, ::Bool)
 MethodInstance for convert(::Type{Bool}, ::Int64)
```

Compare this to the result from [`methodinstance`](@ref).
"""
methodinstances(@nospecialize(f), @nospecialize(types)) =
    _methodinstance(f, types, true)

function methodinstances(@nospecialize(types::Type))
    f, argt = call_type(types)
    return methodinstances(f, types)
end


if isdefined(Core, :MethodMatch)
    include("findcallers.jl")
end

# AbstractTrees interface
AbstractTrees.children(mi::MethodInstance) = isdefined(mi, :backedges) ? mi.backedges : []

# deprecations
@deprecate instance methodinstance

end # module
