export findcallers

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
        (src, ty) = isdefined(Core.Compiler, :NativeInterpreter) ?
            Core.Compiler.typeinf_code(interp, meth, match.spec_types, match.sparams, false) :
            Core.Compiler.typeinf_code(meth, match.spec_types, match.sparams, false, interp)
        push!(srcs, src)
    end
    return srcs
end

defaultinterp(world) = isdefined(Core.Compiler, :NativeInterpreter) ?
                       Core.Compiler.NativeInterpreter(world) :
                       Core.Compiler.Params(world)

function get_typed_instances(mi::MethodInstance; world=typemax(UInt), interp=defaultinterp(world))
    return get_typed_instances!(CodeInfo[], mi, world, interp)
end

"""
    callers = findcallers(f, argmatch::Union{Nothing,Function}, mis; callhead=:call | :iterate)

Find "static" callers of a function `f` with *inferred* argument types for which `argmatch(types)` returns true.
Optionally pass `nothing` for `argmatch` to allow any calls to `f`.
`mis` is the list of `MethodInstance`s you want to check, for example obtained from [`methodinstances`](@ref).

`callhead` controls whether you're looking for an ordinary (`:call`) or a splatted (varargs) call (`:iterate`).

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

!!! compat
    `findcallers` is available on Julia 1.6 and higher

!!! warn
    `findcallers` is not guaranteed to find all calls. Calls can be "obfuscated" by many mechanisms,
    including calls from top level, calls where the function is a runtime variable, etc.
"""
function findcallers(f, argmatch::Union{Function,Nothing}, mis::AbstractVector{Core.MethodInstance};
                     callhead::Symbol=:call, world=typemax(UInt), interp=defaultinterp(world))
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
