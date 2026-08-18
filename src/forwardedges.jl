export CallEdge, codeinstances, direct_calls, visit_calls, all_calls, callgraph

"""
A callee recorded on a forward edge: a `MethodInstance` when the callee has been
specialized, or a `Method` when it has not.
"""
const CallTarget = Union{MethodInstance,Method}

"""
    CallEdge

One call from a caller to a callee, as recorded by type inference.

# Extended help

Fields:

- `callee`: the [`CallTarget`](@ref MethodAnalysis.CallTarget), or `nothing` for `:nomatch` edges
- `code`: the callee's `CodeInstance` if one was recorded, otherwise `nothing`
- `kind`: `:direct`, `:invoke`, `:dispatch`, `:nomatch`, or `:indirect` (see below)
- `sig`: the call signature, a `Tuple` type; `nothing` for `:direct` edges
- `covered`: whether `callee` is the only possible target for `sig`

`kind` may be:

- `:direct`: inference resolved the call to a single callee.
- `:invoke`: an `invoke` call, where `sig` names the method explicitly.
- `:dispatch`: dispatch on `sig` may reach `callee`. There is one edge per
  applicable method. If `covered` is `false`, other methods may also be reached.
- `:nomatch`: no method applies to `sig`, so reaching this call site raises a
  `MethodError`. `callee` and `code` are `nothing`.
- `:indirect`: [`callgraph`](@ref) omitted the intermediate callees between the
  caller and `callee`. `sig` and `covered` describe the original call site.

See [`direct_calls`](@ref) for call sites that do not produce an edge.
"""
struct CallEdge
    callee::Union{CallTarget,Nothing}
    code::Union{CodeInstance,Nothing}
    kind::Symbol
    sig::Any
    covered::Bool
end

function Base.show(io::IO, edge::CallEdge)
    print(io, "CallEdge(", edge.kind)
    edge.callee === nothing || print(io, ", ", edge.callee)
    edge.sig === nothing || print(io, ", sig=", edge.sig)
    edge.covered || print(io, ", incomplete")
    print(io, ')')
end

"""
    cis = codeinstances(mi::MethodInstance; owner=nothing)

Collect the `CodeInstance`s cached for `mi`. By default only results from Julia's own
compiler are returned; pass `owner` to select those produced by an external
`AbstractInterpreter` instead.
"""
function codeinstances(mi::MethodInstance; owner=nothing)
    cis = CodeInstance[]
    isdefined(mi, :cache) || return cis
    ci = mi.cache
    while true
        hasowner(ci, owner) && push!(cis, ci)
        isdefined(ci, :next) || break
        ci = ci.next
    end
    return cis
end

@static if hasfield(CodeInstance, :owner)
    hasowner(ci::CodeInstance, @nospecialize(owner)) = ci.owner === owner
else
    hasowner(::CodeInstance, @nospecialize(owner)) = owner === nothing
end

target(ci::CodeInstance) = Core.Compiler.get_ci_mi(ci)
target(x::CallTarget) = x

"""
    edges = direct_calls(ci::CodeInstance)
    edges = direct_calls(mi::MethodInstance)

Return the [`CallEdge`](@ref)s recorded while compiling `ci`, or their union over
all of `mi`'s [`codeinstances`](@ref).

Calls through a value typed only as `Function` are not recorded.
`InteractiveUtils.@trace_dispatch` can identify their runtime targets.

!!! compat
    Requires at least Julia 1.12.

# Example

```julia
julia> f(x) = sum(abs2, x);

julia> f([1.0]);

julia> direct_calls(methodinstance(f, (Vector{Float64},)))
1-element Vector{CallEdge}:
 CallEdge(direct, MethodInstance for sum(::typeof(abs2), ::Vector{Float64}))
```
"""
@static if !hasfield(CodeInstance, :edges)

direct_calls(::CodeInstance) =
    error("forward call edges require at least Julia 1.12")

else

function direct_calls(ci::CodeInstance)
    edges = CallEdge[]
    items = ci.edges
    i = 1
    while i <= length(items)
        item = items[i]
        if isa(item, CodeInstance)
            push!(edges, CallEdge(target(item), item, :direct, nothing, true))
            i += 1
        elseif isa(item, MethodInstance)
            push!(edges, CallEdge(item, nothing, :direct, nothing, true))
            i += 1
        elseif isa(item, Int)
            # A negative count means the matches do not exhaust `sig`.
            nmatches, covered = abs(item), item > 0
            sig = items[i+1]
            if nmatches == 0
                push!(edges, CallEdge(nothing, nothing, :nomatch, sig, false))
            end
            for j = 1:nmatches
                match = items[i+1+j]
                code = isa(match, CodeInstance) ? match : nothing
                push!(edges, CallEdge(target(match), code, :dispatch, sig, covered))
            end
            i += 2 + nmatches
        elseif isa(item, Core.Binding)
            i += 1   # a dependency on a global, not a call
        else
            sig, callee = item::Type, items[i+1]
            # MethodTable entries are dependencies, not calls.
            if !isa(callee, MethodTable)
                code = isa(callee, CodeInstance) ? callee : nothing
                push!(edges, CallEdge(target(callee), code, :invoke, sig, true))
            end
            i += 2
        end
    end
    return edges
end

end # @static

function direct_calls(mi::MethodInstance)
    edges = CallEdge[]
    for ci in codeinstances(mi)
        append!(edges, direct_calls(ci))
    end
    return unique(edges)
end

direct_calls(::Method) = CallEdge[]   # an unspecialized callee has no recorded calls

"""
    visit_calls(operation, obj)

Walk forward call edges, starting from `obj` and calling `operation(caller, edge)` for
each [`CallEdge`](@ref) reached. `caller` is the [`CallTarget`](@ref
MethodAnalysis.CallTarget) the edge was recorded on.

`operation(caller, edge)` should return `true` to visit the callee's edges and `false`
to skip them. No callee is visited more than once.

`obj` may be a `MethodInstance`, a `CodeInstance`, a collection of either, or anything
[`methodinstances`](@ref) accepts, such as a function or a module.
"""
function visit_calls(@nospecialize(operation), obj)
    visited = IdSet{CallTarget}()
    queue = Pair{CallTarget,Vector{CallEdge}}[]
    for root in callroots(obj)
        mi = target(root)
        mi ∈ visited && continue
        push!(visited, mi)
        push!(queue, mi => direct_calls(root))
    end
    while !isempty(queue)
        caller, edges = popfirst!(queue)
        for edge in edges
            descend = operation(caller, edge)
            callee = edge.callee
            (descend && callee !== nothing) || continue
            callee ∈ visited && continue
            push!(visited, callee)
            push!(queue, callee => direct_calls(something(edge.code, callee)))
        end
    end
    return nothing
end

callroots(mi::MethodInstance) = [mi]
callroots(ci::CodeInstance) = [ci]
callroots(objs::AbstractVector) = objs
callroots(obj) = methodinstances(obj)

"""
    mis = all_calls(obj)

Return every [`CallTarget`](@ref MethodAnalysis.CallTarget) reachable from `obj` by
forward call edges, excluding the starting points themselves. See [`visit_calls`](@ref)
for the accepted forms of `obj`.
"""
function all_calls(obj)
    callees = IdSet{CallTarget}()
    visit_calls((caller, edge) -> (edge.callee === nothing || push!(callees, edge.callee); true), obj)
    for root in callroots(obj)
        delete!(callees, target(root))
    end
    return collect(callees)
end

"""
    graph = callgraph(obj; follow=Returns(true), keep=Returns(true))

Build the forward call graph reachable from `obj`, as a dictionary mapping each
[`CallTarget`](@ref MethodAnalysis.CallTarget) to its [`CallEdge`](@ref)s. See
[`visit_calls`](@ref) for the accepted forms of `obj`.

`follow(callee)` controls traversal. Rejected callees remain in the graph but have
no outgoing edges.

`keep(callee)` decides which callees appear in `graph`; the starting points always do.
A rejected callee is still traversed. Kept targets reached through rejected callees
are connected to the nearest kept caller by `:indirect` edges.

# Examples

To keep all calls among methods of `MyPkg`, use

```julia
inmodule(t) = (m = t isa Method ? t : t.def; m isa Method && m.module === MyPkg)
callgraph(MyPkg; keep=inmodule)
```

# Extended help

Each caller has at most one `:indirect` edge per callee. A direct edge takes
precedence over an indirect edge for the same pair.

`follow` controls traversal, whereas `keep` filters the resulting graph. Because
`keep` cannot include targets beyond a callee rejected by `follow`, use `keep` alone
to restrict a graph to one module:
"""
function callgraph(obj; follow=Returns(true), keep=Returns(true))
    roots = callroots(obj)
    graph = IdDict{CallTarget,Vector{CallEdge}}()
    for root in roots
        get!(valtype(graph), graph, target(root))
    end
    visit_calls(roots) do caller, edge
        edges = get!(valtype(graph), graph, caller)
        push!(edges, edge)
        callee = edge.callee
        callee === nothing && return false
        get!(valtype(graph), graph, callee)
        return follow(callee)
    end
    kept = IdSet{CallTarget}(target(root) for root in roots)
    for node in keys(graph)
        keep(node) && push!(kept, node)
    end
    length(kept) == length(graph) && return graph
    return collapse(graph, kept)
end

# Connect each kept node to kept targets reachable through omitted nodes
function collapse(graph::IdDict{CallTarget,Vector{CallEdge}}, kept::IdSet{CallTarget})
    collapsed = IdDict{CallTarget,Vector{CallEdge}}()
    for caller in kept
        edges = CallEdge[]
        recorded = IdSet{CallTarget}()
        skipped = IdSet{CallTarget}()
        queue = CallTarget[]
        for edge in graph[caller]
            callee = edge.callee
            if callee === nothing || callee ∈ kept
                callee === nothing || push!(recorded, callee)
                push!(edges, edge)
            elseif callee ∉ skipped
                push!(skipped, callee)
                push!(queue, callee)
            end
        end
        while !isempty(queue)
            for edge in graph[popfirst!(queue)]
                callee = edge.callee
                callee === nothing && continue
                if callee ∈ kept
                    callee ∈ recorded && continue
                    push!(recorded, callee)
                    push!(edges, CallEdge(callee, edge.code, :indirect, edge.sig, edge.covered))
                elseif callee ∉ skipped
                    push!(skipped, callee)
                    push!(queue, callee)
                end
            end
        end
        collapsed[caller] = edges
    end
    return collapsed
end
