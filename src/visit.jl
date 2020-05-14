"""
    visit(operation, obj; print=false)

Scan `obj` and all of its "sub-objects" (e.g., functions if `obj::Module`,
methods if `obj::Function`, etc.) recursively.
`operation(x)` should return `true` if `visit` should descend
into "sub-objects" of `x`. 

# Example

To collect all MethodInstances of a function,

```jldoctest; setup=:(using MethodAnalysis), filter=r"[dd]-element"
julia> mis = Core.MethodInstance[];

julia> visit(findfirst) do x
           if isa(x, Core.MethodInstance)
               push!(mis, x)
               return false
           end
           true
       end

julia> mis
31-element Array{Core.MethodInstance,1}:
 MethodInstance for findfirst(::BitArray{1})
[...]
```    
"""
visit(@nospecialize(operation), @nospecialize(obj); print::Bool=false) =
    _visit(operation, obj, IdSet{Any}(), print)

"""
    visit(operation)

Scan all loaded modules with `operation`.

# Example

Collect all loaded modules, even if they are internal.

```jldoctest; setup=:(using MethodAnalysis), filter=r"[ddd]-element"
julia> mods = Module[];

julia> visit() do x
           if isa(x, Module)
               push!(mods, x)
               return true
           end
           false
       end
```

julia> mods
113-element Array{Module,1}:
 Random
 Random.DSFMT
[...]
"""
function visit(@nospecialize(operation); print::Bool=false)
    visited = IdSet{Any}()
    for mod in Base.loaded_modules_array()
        _visit(operation, mod, visited, print)
    end
    return nothing
end

# These are non-keyword functions due to https://github.com/JuliaLang/julia/issues/34516

function _visit(@nospecialize(operation), mod::Module, visited::IdSet{Any}, print::Bool)
    mod ∈ visited && return nothing
    push!(visited, mod)
    print && println("Module ", mod)
    if operation(mod)
        for nm in names(mod; all=true)
            if isdefined(mod, nm)
                obj = getfield(mod, nm)
                _visit(operation, obj, visited, print)
            end
        end
    end
    return nothing
end

function _visit(@nospecialize(operation), @nospecialize(f::Callable), visited::IdSet{Any}, print::Bool)
    f ∈ visited && return nothing
    push!(visited, f)
    print && println("Callable ", f)
    if operation(f)
        ml = methods(f)
        _visit(operation, ml.mt, visited, print)
        Base.visit(ml.mt) do m
            _visit(operation, m, visited, print)
        end
    end
    return nothing
end

function _visit(@nospecialize(operation), mt::MethodTable, visited::IdSet{Any}, print::Bool)
    mt ∈ visited && return nothing
    push!(visited, mt)
    print && println("MethodTable ", mt)
    operation(mt)
    return nothing
end

function _visit(@nospecialize(operation), m::Method, visited::IdSet{Any}, print::Bool)
    m ∈ visited && return nothing
    push!(visited, m)
    print && println("Method ", m)
    if operation(m)
        for fn in (:specializations, :invokes)
            if isdefined(m, fn)
                spec = getfield(m, fn)
                if spec === nothing
                elseif isa(spec, Core.TypeMapEntry) || isa(spec, Core.TypeMapLevel)
                    Base.visit(spec) do mi
                        _visit(operation, mi, visited, print)
                    end
                elseif isa(spec, Core.SimpleVector)
                    _visit(operation, spec, visited, print)
                else
                    error("unhandled type ", typeof(spec), ": ", spec)
                end
            end
        end
    end
    return nothing
end

function _visit(@nospecialize(operation), sv::SimpleVector, visited::IdSet{Any}, print::Bool)
    for i = 1:length(sv)
        if isassigned(sv, i)
            _visit(operation, sv[i], visited, print)
        end
    end
    return nothing
end

function _visit(@nospecialize(operation), mi::MethodInstance, visited::IdSet{Any}, print::Bool)
    mi ∈ visited && return nothing
    push!(visited, mi)
    print && println(mi)
    if operation(mi)
        if isdefined(mi, :cache)
            _visit(operation, mi.cache, visited, print)
        end
    end
    return nothing
end

if isdefined(Core, :CodeInstance)
    function _visit(@nospecialize(operation), ci::Core.CodeInstance, visited::IdSet{Any}, print::Bool)
        ci ∈ visited && return nothing
        push!(visited, ci)
        print && println(ci)
        if operation(ci)
            if isdefined(ci, :next)
                _visit(operation, ci.next, visited, print)
            end
        end
        return nothing
    end
end

_visit(@nospecialize(operation), @nospecialize(x), visited::IdSet{Any}, print::Bool) = nothing

"""
    visit_backedges(operation, obj)

Visit the backedges of `obj` and apply `operation` to each.
`operation` may need to be able to handle two call forms, `operation(mi)` and
`operation(sig=>mi)`, where `mi` is a `MethodInstance` and `sig` is a `Tuple`-type.
The latter arises from `MethodTable` backedges and can be ignored if `obj` does not
contain `MethodTable`s.

`operation(edge)` should return `true` if the backedges of `edge` should in turn be visited,
`false` otherwise.
However, no `MethodInstance` will be visited more than once.

The set of visited objects includes `obj` itself.
For example, `visit_backedges(operation, f::Function)` will visit all methods of `f`,
and this in turn will visit all MethodInstances of these methods.
"""
function visit_backedges(@nospecialize(operation), obj)
    visited = IdSet{MethodInstance}()
    visit_backedges(operation, obj, visited)
end

function visit_backedges(@nospecialize(operation), obj, visited::IdSet{MethodInstance})
    function opwrapper(@nospecialize(x))
        if isa(x, MethodInstance)
            _visit_backedges(operation, x, visited)
            return false
        elseif isa(x, MethodTable)
            mt = x::MethodTable
            if isdefined(mt, :backedges)
                sigmis = mt.backedges::Vector{Any}
                for i = 1:2:length(sigmis)
                    sig, mi = sigmis[i], sigmis[i+1]
                    _visit_backedges(operation, Pair{Any,MethodInstance}(sig, mi), visited)
                end
            end
            return false
        end
        return true
    end

    visit(opwrapper, obj)
end

function _visit_backedges(@nospecialize(operation), mi::MethodInstance, visited)
    mi ∈ visited && return nothing
    push!(visited, mi)
    if operation(mi) && isdefined(mi, :backedges)
        for edge in mi.backedges
            _visit_backedges(operation, edge, visited)
        end
    end
    return nothing
end

function _visit_backedges(@nospecialize(operation), misig::Pair{Any,MethodInstance}, visited)
    if operation(misig)
        _visit_backedges(operation, misig.second, visited)
    end
    return nothing
end
