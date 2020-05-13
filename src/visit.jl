"""
    visit(operation)

Scan all loaded modules with `operation`. `operation(x)` should handle `x::Module`, `x::Function`,
`x::Method`, `x::MethodInstance`. Any return value from `operation` will be discarded.
"""
function visit(operation; print=true)
    visiting=Set{Module}()
    for mod in Base.loaded_modules_array()
        visit(operation, mod, visiting; print=print)
    end
    return nothing
end

function visit(operation, mod::Module, visiting=Set{Module}(); print=true)
    mod ∈ visiting && return nothing
    push!(visiting, mod)
    operation(mod)
    print && println("Module ", mod)
    for nm in names(mod; all=true)
        if isdefined(mod, nm)
            obj = getfield(mod, nm)
            if isa(obj, Module)
                visit(operation, obj, visiting; print=print)
            else
                visit(operation, obj)
            end
        end
    end
    return nothing
end

function visit(operation, f::Function)
    operation(f)
    Base.visit(methods(f).mt) do m
        visit(operation, m)
    end
    return nothing
end

function visit(operation, m::Method)
    operation(m)
    for fn in (:specializations,) # :invokes)   not sure if invokes contains additional methods
        if isdefined(m, fn)
            spec = getfield(m, fn)
            if spec === nothing
            elseif isa(spec, Core.TypeMapEntry) || isa(spec, Core.TypeMapLevel)
                Base.visit(spec) do mi
                    visit(operation, mi)
                end
            elseif isa(spec, Core.SimpleVector)
                visit(operation, spec)
            else
                error("unhandled type ", typeof(spec), ": ", spec)
            end
        end
    end
    return nothing
end

function visit(operation, sv::SimpleVector)
    for i = 1:length(sv)
        if isassigned(sv, i)
            visit(operation, sv[i])
        end
    end
    return nothing
end

function visit(operation, mi::MethodInstance)
    operation(mi)
    if isdefined(mi, :cache)
        visit(operation, mi.cache)
    end
    return nothing
end

# TODO: CodeInstance

visit(operation, x) = nothing

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
        end
    end
    return nothing
end

function visit_backedges(operation, f::Function, visited)
    ml = methods(f)
    visit_backedges(operation, ml.mt, visited)
    for m in ml
        visit_backedges(operation, m, visited)
    end
    return nothing
end
