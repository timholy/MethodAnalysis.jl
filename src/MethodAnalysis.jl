module MethodAnalysis

using AbstractTrees

using Base: Callable, IdSet
using Core: MethodInstance, CodeInfo, SimpleVector, MethodTable

export visit, call_type, methodinstance, methodinstances, worlds, findcallers
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
function methodinstance(@nospecialize(f), @nospecialize(types))
    if types isa Tuple
        m = which(f, types)
        tt = Tuple{typeof(f), types...}
        return methodinstance(m, tt)
    end
    inst = nothing
    visit(f) do mi
        if isa(mi, MethodInstance)
            if mi.specTypes === types
                inst = mi
            end
            return false
        end
        true
    end
    return inst
end
function methodinstance(@nospecialize(types))
    f, argt = call_type(types)
    m = which(f, argt)
    return methodinstance(m, types)
end

"""
    methodinstances()
    methodinstances(mod::Module)
    methodinstances(f)

Collect all `MethodInstance`s, optionally restricting them to a particular module or function.
"""
function methodinstances(top=())
    if isa(top, Module) || isa(top, Function) || isa(top, Type)
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

function get_typed_instances!(srcs, mi::MethodInstance, world, interp)
    # This is essentially take from code_typed_by_type
    tt = mi.specTypes
    matches = Base._methods_by_ftype(tt, -1, world)
    if matches === false
        error("signature $(item.specTypes) does not correspond to a generic function")
    end
    for match in matches
        match.method == mi.def || continue
        meth = Base.func_for_method_checked(match.method, tt, match.sparams)
        (src, ty) = Core.Compiler.typeinf_code(interp, meth, match.spec_types, match.sparams, false)
        push!(srcs, src)
    end
    return srcs
end

function get_typed_instances(mi::MethodInstance; world=typemax(UInt), interp=Core.Compiler.NativeInterpreter(world))
    return get_typed_instances!(CodeInfo[], mi, world, interp)
end

"""
    callers = findcallers(f, argmatch::Union{Nothing,Function}, mis; callhead=:call | :iterate)

Find "static" callers of a function `f` with *inferred* argument types for which `argmatch(types)` returns true.
Optionally pass `nothing` for `argmatch` to allow any calls to `f`.
`mis` is the list of `MethodInstance`s you want to check, for example obtained from [`instances`](@ref).

`callhead` controls whether you're looking for "static" dispatch (`:call`) or a splatted call (`:iterate`).

`callers` is a vector of tuples `t`, where `t[1]` is the `MethodInstance`, `t[2]` is the corresponding `CodeInfo`,
`t[3]` is the statement number on which the call occurs, and `t[4]` holds the inferred argument types.

# Examples

```julia
f(x) = rand()
function g()
    a = f(1.0)
    z = Any[7]
    b = f(z[1]::Integer)
    c = f(z...)
    return nothing
end

g()
mis = union(methodinstances(f), methodinstances(g))

# callhead = :call is the default
callers1 = findcallers(f, argtyps->length(argtyps) == 1 && argtyps[1] === Float64, mis)

# Get the partial-inference case with known number of arguments (this is definitely :call)
callers2 = findcallers(f, argtyps->length(argtyps) == 1 && argtyps[1] === Integer, mis; callhead=:call)

# Get the splat call
callers3 = findcallers(f, argtyps->length(argtyps) == 1 && argtyps[1] === Vector{Any}, mis; callhead=:iterate)
"""
function findcallers(f, argmatch::Union{Function,Nothing}, mis::AbstractVector{Core.MethodInstance};
                     callhead::Symbol=:call, world=typemax(UInt), interp=Core.Compiler.NativeInterpreter(world))
    callhead === :call || callhead === :invoke || callhead === :iterate || error(":call and :invoke are supported, got ", callhead)
    # if callhead === :iterate
    #     return findcallers(mis, GlobalRef(Core, :_apply_iterate), argt->(argtargmatch(argt[4:end]); callhead=:call)
    # end
    extract(a) = isa(a, Core.Const) ? typeof(a.val) :
                 isa(a, Core.PartialStruct) ? (a.typ <: Tuple{Any} ? a.typ.parameters[1] : a.typ) :
                 isa(a,Type) ? a : typeof(a)
    # Construct a GlobalRef version of `f`
    ref = GlobalRef(parentmodule(f), nameof(f))
    callers = Tuple{MethodInstance,CodeInfo,Int,Vector{Any}}[]
    srcs = CodeInfo[]
    for item in mis
        empty!(srcs)
        try
            get_typed_instances!(srcs, item, world, interp)
        catch err
            # println("skipping ", item, ": ", err)
            continue
        end
        for src in srcs
            for (i, stmt) in enumerate(src.code)
                if isa(stmt, Expr)
                    g = nothing
                    if stmt.head === :(=)
                        stmt = stmt.args[2]   # call must come from right hand side of assignment
                        isa(stmt, Expr) || continue
                    end
                    stmt = stmt::Expr
                    if callhead === :iterate && stmt.head === :call && isglobalref(stmt.args[1], Core, :_apply_iterate)
                        if isa(stmt.args[3], GlobalRef)  # TODO: handle SSAValues?
                            g = stmt.args[3]::GlobalRef
                        end
                    elseif stmt.head === callhead && isa(stmt.args[1], GlobalRef)  # TODO: handle SSAValue?
                        g = stmt.args[1]::GlobalRef
                    end
                    if isa(g, GlobalRef)
                        if isglobalref(g, ref.mod, ref.name) ||
                           (isdefined(g.mod, g.name) && getfield(g.mod, g.name) === f)
                            # Collect the arg types
                            argtypes = []
                            for i = (callhead === :iterate ? 4 : 2):length(stmt.args)
                                a = stmt.args[i]
                                if isa(a, Core.SSAValue)
                                    push!(argtypes, extract(src.ssavaluetypes[a.id]))
                                elseif isa(a, Core.SlotNumber)
                                    push!(argtypes, extract(src.slottypes[a.id]))
                                else
                                    push!(argtypes, extract(a))
                                end
                            end
                            # Check whether the types match
                            if argmatch === nothing
                                push!(callers, (item, src, i, argtypes))
                                break
                            else
                                if argmatch(argtypes)
                                    push!(callers, (item, src, i, argtypes))
                                    break
                                end
                            end
                        end
                    end
                end
            end
        end
    end
    return callers
end

isglobalref(@nospecialize(g), mod::Module, name::Symbol) = isa(g, GlobalRef) && g.mod === mod && g.name === name


# AbstractTrees interface
AbstractTrees.children(mi::MethodInstance) = isdefined(mi, :backedges) ? mi.backedges : []

# deprecations
@deprecate instance methodinstance

end # module
