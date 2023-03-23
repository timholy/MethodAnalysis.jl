export findcallers

function get_typed_instances!(srcs, @nospecialize(tt), method::Method, world, interp)
    # This is essentially taken from code_typed_by_type
    matches = Base._methods_by_ftype(tt, -1, world)
    if matches === false
        error("signature $(item.specTypes) does not correspond to a generic function")
    end
    for match in matches
        match.method == method || continue
        meth = Base.func_for_method_checked(match.method, tt, match.sparams)
        (src, ty) = isdefined(Core.Compiler, :NativeInterpreter) ?
            Core.Compiler.typeinf_code(interp, meth, match.spec_types, match.sparams, false) :
            Core.Compiler.typeinf_code(meth, match.spec_types, match.sparams, false, interp)
        push!(srcs, (src, match.sparams))
    end
    return srcs
end
get_typed_instances!(srcs, mi::MethodInstance, world, interp) = get_typed_instances!(srcs, mi.specTypes, mi.def, world, interp)

defaultinterp(world) = isdefined(Core.Compiler, :NativeInterpreter) ?
                       Core.Compiler.NativeInterpreter(world) :
                       Core.Compiler.Params(world)

function get_typed_instances(mi::MethodInstance; world=typemax(UInt), interp=defaultinterp(world))
    return get_typed_instances!(Tuple{CodeInfo,Core.SimpleVector}[], mi, world, interp)
end
function get_typed_instances(@nospecialize(tt), method::Method; world=typemax(UInt), interp=defaultinterp(world))
    return get_typed_instances!(Tuple{CodeInfo,Core.SimpleVector}[], tt, method, world, interp)
end

"""
    CallMatch

A structure to summarize a "matching" caller/callee pair. The fields are:

- `mi`: the `MethodInstance` for the caller
- `src`: its corresponding `CodeInfo`
- `sparams`: the type parameters for the caller, given `mi`'s signature (alternatively use `mi.sparam_vals`)
- `line`: the statement number (in SSAValue sense) on which the call occurs
- `argtypes`: the caller's inferred types passed as arguments to the callee
"""
struct CallMatch
    mi::MethodInstance
    src::CodeInfo
    sparams::SimpleVector
    line::Int
    argtypes::Vector{Any}
end

function Base.show(io::IO, cm::CallMatch)
    print(io, "CallMatch from ", cm.mi, " on statement ", cm.line, "/", length(cm.src.code), " with inferred argument types ", cm.argtypes)
end

"""
    callers = findcallers(f, argmatch::Union{Nothing,Function}, mis; callhead=:call | :iterate)

Find "static" callers of a function `f` with *inferred* argument types for which `argmatch(types)` returns true.
Optionally pass `nothing` for `argmatch` to allow any calls to `f`.
`mis` is the list of `MethodInstance`s you want to check, for example obtained from [`methodinstances`](@ref).

`callhead` controls whether you're looking for an ordinary (`:call`) or a splatted (varargs) call (`:iterate`).

`callers` is a vector of [`CallMatch`](@ref) objects.

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
```

!!! compat
    `findcallers` is available on Julia 1.6 and higher

!!! warn
    `findcallers` is not guaranteed to find all calls. Calls can be "obfuscated" by many mechanisms,
    including calls from top level, calls where the function is a runtime variable, etc.
"""
function findcallers(f, argmatch::Union{Function,Nothing}, mis::AbstractVector{Core.MethodInstance};
                     callhead::Symbol=:call, world=typemax(UInt), interp=defaultinterp(world))
    callhead === :call || callhead === :invoke || callhead === :iterate || error(":call and :invoke are supported, got ", callhead)
    # Check that f is not a type with specified parameters
    if f isa DataType && !isempty(f.parameters)
        throw(ArgumentError("findcallers does not support constructor calls with type parameters, got $f. Pass in the type without parameters."))
    end
    # Construct a GlobalRef version of `f`
    ref = GlobalRef(parentmodule(f), nameof(f))
    callers = CallMatch[]
    srcs = Tuple{CodeInfo,Core.SimpleVector}[]
    for item in mis
        empty!(srcs)
        try
            get_typed_instances!(srcs, item, world, interp)
        catch err
            # println("skipping ", item, ": ", err)
            continue
        end
        for (src, sparams) in srcs
            for (i, stmt) in enumerate(src.code)
                if isa(stmt, Expr)
                    if stmt.head === :(=)
                        stmt = stmt.args[2]   # call must come from right hand side of assignment
                        isa(stmt, Expr) || continue
                    end
                    stmt = stmt::Expr
                    callee = nothing
                    if stmt.head === callhead
                        callee = stmt.args[1]
                        if isglobalref(callee, Core, :kwcall) && length(stmt.args) >= 3
                            callee = stmt.args[3]
                        end
                    elseif callhead === :iterate && stmt.head === :call && isglobalref(stmt.args[1], Core, :_apply_iterate)
                        callee = stmt.args[3]
                    end
                    callee === nothing && continue
                    if isa(callee, Core.SSAValue)
                        callee = try
                            eval_ssa(src, sparams, callee.id)
                        catch err
                            @show src i
                            throw(err)
                        end
                    end
                    matches = false
                    if callee === f
                        matches = true
                    elseif isa(callee, GlobalRef)
                        if isglobalref(callee, ref.mod, ref.name) ||
                           (isdefined(callee.mod, callee.name) && getfield(callee.mod, callee.name) === f)
                            matches = true
                        end
                    elseif isa(f, Type) && isa(callee, DataType) && !isempty(callee.parameters)
                        # This came from a `typeof(a)(b)` call
                        matches = callee <: f
                    end
                    matches || continue
                    # Collect the arg types
                    argtypes = []
                    for i = (callhead === :iterate ? 4 : 2):length(stmt.args)
                        a = stmt.args[i]
                        if isa(a, Core.SSAValue)
                            push!(argtypes, extract(src.ssavaluetypes[a.id], sparams))
                        elseif isa(a, Core.SlotNumber)
                            push!(argtypes, extract(src.slottypes[a.id], sparams))
                        elseif isa(a, GlobalRef)
                            push!(argtypes, Core.Typeof(getfield(a.mod, a.name)))
                        elseif isexpr(a, :static_parameter)
                            a = a::Expr
                            push!(argtypes, Type{sparams[a.args[1]::Int]})
                        else
                            push!(argtypes, extract(a, sparams))
                        end
                    end
                    # Check whether the types match
                    if argmatch === nothing
                        push!(callers, CallMatch(item, src, sparams, i, argtypes))
                        break
                    else
                        if argmatch(argtypes)
                            push!(callers, CallMatch(item, src, sparams, i, argtypes))
                            break
                        end
                    end
                end
            end
        end
    end
    return callers
end

isglobalref(@nospecialize(g), mod::Module, name::Symbol) = isa(g, GlobalRef) && g.mod === mod && g.name === name

extract(a, sparams) = isa(a, Core.Const) ? Core.Typeof(a.val) :
                      isa(a, Core.PartialStruct) ? (a.typ <: Tuple{Any} ? a.typ.parameters[1] : a.typ) :
                      isa(a,Type) ? a :
                      isexpr(a, :static_parameter) ? sparams[(a::Expr).args[1]] :
                      Core.Typeof(a)

# This is deliberately simple to prevent performance from tanking
function eval_ssa(src, sparams, id)
    stmt = src.code[id]
    if isa(stmt, Expr)
        if stmt.head === :call
            callee = stmt.args[1]
            if isglobalref(callee, Core, :apply_type)
                return stmt.args[2]
            elseif isglobalref(callee, Base, :getproperty)
                modg, objq = stmt.args[2], stmt.args[3]
                if isa(modg, Core.SSAValue)
                    modg = eval_ssa(src, sparams, modg.id)
                end
                if isa(modg, GlobalRef)
                    isdefined(modg.mod, modg.name) || return nothing
                    mod = getfield(modg.mod, modg.name)
                    objname = objq.value::Symbol
                    return GlobalRef(mod, objname)
                end
            elseif isglobalref(callee, Core, :kwfunc)
                return stmt.args[2]
            elseif isa(callee, GlobalRef) && callee.name === :typeof && length(stmt.args) == 2
                isa(stmt.args[2], Core.SlotNumber) && return src.slottypes[(stmt.args[2]::Core.SlotNumber).id]
                isa(stmt.args[2], Core.SSAValue) && return eval_ssa(src, sparams, (stmt.args[2]::Core.SSAValue).id)
            end
        end
    end
    isa(stmt, GlobalRef) && return stmt
    isa(stmt, QuoteNode) && return stmt.value
    return src.ssavaluetypes[id]
end
