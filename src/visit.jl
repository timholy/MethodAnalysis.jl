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
    visiting = IdSet{Any}()
    for mod in Base.loaded_modules_array()
        _visit(operation, mod, visiting, print)
    end
    return nothing
end

# These are non-keyword functions due to https://github.com/JuliaLang/julia/issues/34516

function _visit(@nospecialize(operation), mod::Module, visiting::IdSet{Any}, print::Bool)
    mod ∈ visiting && return nothing
    push!(visiting, mod)
    print && println("Module ", mod)
    if operation(mod)
        for nm in names(mod; all=true)
            if isdefined(mod, nm)
                obj = getfield(mod, nm)
                _visit(operation, obj, visiting, print)
            end
        end
    end
    return nothing
end

function _visit(@nospecialize(operation), @nospecialize(f::Callable), visiting::IdSet{Any}, print::Bool)
    f ∈ visiting && return nothing
    push!(visiting, f)
    print && println("Callable ", f)
    if operation(f)
        ml = methods(f)
        _visit(operation, ml.mt, visiting, print)
        Base.visit(ml.mt) do m
            _visit(operation, m, visiting, print)
        end
    end
    return nothing
end

function _visit(@nospecialize(operation), mt::MethodTable, visiting::IdSet{Any}, print::Bool)
    mt ∈ visiting && return nothing
    push!(visiting, mt)
    print && println("MethodTable ", mt)
    operation(mt)
    return nothing
end

function _visit(@nospecialize(operation), m::Method, visiting::IdSet{Any}, print::Bool)
    m ∈ visiting && return nothing
    push!(visiting, m)
    print && println("Method ", m)
    if operation(m)
        for fn in (:specializations, :invokes)
            if isdefined(m, fn)
                spec = getfield(m, fn)
                if spec === nothing
                elseif isa(spec, Core.TypeMapEntry) || isa(spec, Core.TypeMapLevel)
                    Base.visit(spec) do mi
                        _visit(operation, mi, visiting, print)
                    end
                elseif isa(spec, Core.SimpleVector)
                    _visit(operation, spec, visiting, print)
                else
                    error("unhandled type ", typeof(spec), ": ", spec)
                end
            end
        end
    end
    return nothing
end

function _visit(@nospecialize(operation), sv::SimpleVector, visiting::IdSet{Any}, print::Bool)
    for i = 1:length(sv)
        if isassigned(sv, i)
            _visit(operation, sv[i], visiting, print)
        end
    end
    return nothing
end

function _visit(@nospecialize(operation), mi::MethodInstance, visiting::IdSet{Any}, print::Bool)
    mi ∈ visiting && return nothing
    push!(visiting, mi)
    print && println(mi)
    if operation(mi)
        if isdefined(mi, :cache)
            _visit(operation, mi.cache, visiting, print)
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

_visit(@nospecialize(operation), @nospecialize(x), visiting::IdSet{Any}, print::Bool) = nothing

"""
    visit_backedges(operation, obj)

Visit the backedges of `obj` and apply `operation` to each.
`operation` may need to be able to handle two call forms, `operation(mi)` and
`operation(sig=>mi)`, where `mi` is a `MethodInstance` and `sig` is a `Tuple`-type.
The latter arises from `MethodTable` backedges and can be ignored if `obj` does not
contain `MethodTable`s.

`operation(edge)` should return `true` if the backedges of `edge` should in turn be visited,
`false` otherwise.

The set of visited objects includes `obj` itself.
For example, `visit_backedges(operation, f::Function)` will visit all methods of `f`,
and this in turn will visit all MethodInstances of these methods.
"""
visit_backedges(operation, obj) =
    visit_backedges(operation, obj, Set{Union{MethodInstance,MethodTable}}())

function _visit_backedges(operation, mi::MethodInstance, visited)
    if isdefined(mi, :backedges)
        for edge in mi.backedges
            visit_backedges(operation, edge, visited)
        end
    end
    return nothing
end

function visit_backedges(operation, mi::MethodInstance, visited)
    mi ∈ visited && return nothing
    push!(visited, mi)
    operation(mi) && _visit_backedges(operation, mi, visited)
    return nothing
end

function visit_backedges(operation, mt::MethodTable, visited)
    mt ∈ visited && return nothing
    push!(visited, mt)
    if isdefined(mt, :backedges)
        for i = 1:2:length(mt.backedges)
            sig, mi = mt.backedges[i], mt.backedges[i+1]
            if operation(sig=>mi) && mi ∉ visited
                push!(visited, mi)
                _visit_backedges(operation, mi, visited)
            end
        end
    end
    return nothing
end

function visit_backedges(operation, m::Method, visited)
    visit(m) do mi
        if isa(mi, MethodInstance)
            visit_backedges(operation, mi, visited)
            return false
        end
        true
    end
    return nothing
end

function visit_backedges(operation, f::Callable, visited)
    ml = methods(f)
    visit_backedges(operation, ml.mt, visited)
    for m in ml
        visit_backedges(operation, m, visited)
    end
    return nothing
end
