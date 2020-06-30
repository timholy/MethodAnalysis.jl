using MethodAnalysis
using Base: _methods_by_ftype, get_world_counter, to_tuple_type, func_for_method_checked, remove_linenums!
using Core: CodeInfo, SSAValue, SlotNumber, SimpleVector

if isdefined(Core.Compiler, :NativeInterpreter)
    getcode(meth, x, world, optimize; interp=Core.Compiler.NativeInterpreter(world)) =
        Core.Compiler.typeinf_code(interp, meth, x[1], x[2], optimize)
else
    getcode(meth, x, world, optimize; params=Core.Compiler.Params(world)) =
        Core.Compiler.typeinf_code(meth, x[1], x[2], optimize, params)
end

function infer_with_sig(m::Method; optimize=true, debuginfo=:none, world=get_world_counter(), kwargs...)
    tt = to_tuple_type(m.sig)
    meths = _methods_by_ftype(tt, -1, world)
    for x in meths
        x[3] == m || continue
        meth = func_for_method_checked(x[3], tt, x[2])
        (code, ty) = getcode(meth, x, world, optimize; kwargs...)
        debuginfo === :none && code !== nothing && remove_linenums!(code)
        return (code, x[2])=>ty
    end
    error("no match for ", m)
end

struct BadCall
    callee::GlobalRef
    argtyps
    rettype
end

function peeltype(@nospecialize(T))
    isa(T, Core.Compiler.Const) && return Core.Typeof(T.val)
    isa(T, Core.Compiler.PartialStruct) && return T.typ
    isa(T, Core.Compiler.MaybeUndef) && return T.typ
    return T
end

resolve(g::GlobalRef) = isdefined(g.mod, g.name) ? getfield(g.mod, g.name) : nothing
resolve(T::Type) = T

"""
    `tfunc(argtyps, rettype)` returns `true` if `rettype` is the expected type
"""
function bad_calls(src::CodeInfo, sparams::SimpleVector, @nospecialize(ty), tfuncs::AbstractDict)
    function lookup(a; typof::Bool=true)
        if isa(a, SSAValue)
            return peeltype(src.ssavaluetypes[a.id])
        elseif isa(a, SlotNumber)
            return peeltype(src.slottypes[a.id])
        elseif isdefined(Core.Compiler, :Argument) && isa(a, Core.Compiler.Argument)
            return peeltype(src.slottypes[a.n])
        elseif isa(a, GlobalRef) && isdefined(a.mod, a.name)
            return Core.Typeof(getfield(a.mod, a.name))
        elseif isa(a, Expr)
            if a.head === :static_parameter
                n = a.args[1]
                t = Any
                if 1 <= n <= length(sparams)
                    t = sparams[n]
                end
                return t
            else
                error("unrecognized Expr head ", a.head)
            end
        end
        return typof ? Core.Typeof(peeltype(a)) : peeltype(a)
    end

    badstmts = Pair{Int,BadCall}[]
    for (i, stmt) in enumerate(src.code)
        if isa(stmt, Expr)
            stmt.head === :call || continue
            g = stmt.args[1]
            isa(g, GlobalRef) || isa(g, Type) || continue
            tfunc = get(tfuncs, resolve(g), nothing)
            if tfunc !== nothing
                atyps = []
                for j = 2:length(stmt.args)
                    a = stmt.args[j]
                    push!(atyps, lookup(a))
                end
                sttyp = peeltype(src.ssavaluetypes[i])
                # Check to see if the next line has a typeassert
                if i < length(src.code)
                    nextstmt = src.code[i+1]
                    if isa(nextstmt, Expr) && nextstmt.head === :call
                        c = nextstmt.args[1]
                        if isa(c, GlobalRef) && c.mod === Core && c.name === :typeassert && nextstmt.args[2] == SSAValue(i)
                            tatyp = lookup(nextstmt.args[3]; typof=false)
                            sttyp = typeintersect(sttyp, tatyp)
                        end
                    end
                end
                if !tfunc(atyps, sttyp)
                    push!(badstmts, i => BadCall(g, atyps, sttyp))
                end
            end
        end
    end
    return badstmts
end

function tfunc_promote(atyps, @nospecialize(rettyp))
    # peeltyp(T) = T<:Type ? T.parameters[1] : T
    T = atyps[1]
    isa(T, TypeVar) && return true
    for i = 2:length(atyps)
        T = promote_type(T, atyps[i])
    end
    return rettyp === T
end

function tfunc_promote_or_subtype(atyps, @nospecialize(rettyp))
    tfunc_promote(atyps, rettyp) && return true
    for a in atyps
        rettyp <: a && return true
    end
    return false
end

function tfunc_sub1(atyps, @nospecialize(rettyp), @nospecialize(U))
    T = only(atyps)
    return T<:U && rettyp<:U
end

tfunc_returns(atyps, @nospecialize(rettyp), @nospecialize(U)) = rettyp <: U

function gettyp(T)
    if isa(T, TypeVar)
        return gettyp(T.ub)
    elseif isa(T, UnionAll)
        return gettyp(Base.unwrap_unionall(T))
    elseif isa(T, DataType) && T<:Type
        return length(T.parameters) == 1 ? gettyp(T.parameters[1]) : Any
    else
        return T
    end
end

function tfunc_convert(atyps, @nospecialize(rettyp))
    T = gettyp(atyps[1])
    return gettyp(rettyp) <: T
end

function tfunc_iterate(atyps, @nospecialize(rettyp))
    atyps[1] <: AbstractString && return rettyp <: Union{Nothing,Tuple{AbstractChar,Int}}
    if atyps[1] <: AbstractArray
        T = eltype(atyps[1])
        return rettyp <: Union{Nothing,Tuple{T,Union{Int,CartesianIndex}}}
    end
    return rettyp <: Union{Nothing,Tuple{Any,Any}}
end

function tfunc_getindex(atyps, @nospecialize(rettyp))
    Tel = gettyp(eltype(atyps[1]))
    if all(T->(Tt = gettyp(T); isa(Tt,Type) ? Tt<:Integer : false), atyps[2:end])
        return gettyp(rettyp) <: Tel
    end
    return true  # don't try to infer non-scalar indexing
end

tfuncs = IdDict{Any,Function}(
    Base.:(&) => tfunc_promote,
    Base.:(|) => tfunc_promote,
    Base.:(!) => (a,t)->tfunc_returns(a,t,Union{Bool,Missing}),
    Base.:(+) => tfunc_promote_or_subtype,
    Base.:(-) => tfunc_promote_or_subtype,
    Base.:((==))    => (a,t)->tfunc_returns(a,t,Union{Bool,Missing}),
    Base.:((<))     => (a,t)->tfunc_returns(a,t,Union{Bool,Missing}),
    Base.:((<=))    => (a,t)->tfunc_returns(a,t,Union{Bool,Missing}),
    Base.:((>))     => (a,t)->tfunc_returns(a,t,Union{Bool,Missing}),
    Base.:((>=))    => (a,t)->tfunc_returns(a,t,Union{Bool,Missing}),
    Base.:(cmp)     => (a,t)->tfunc_returns(a,t,Int),
    Base.:(convert) => tfunc_convert,
    Base.:(cconvert) => tfunc_convert,
    Base.:(unsafe_convert) => tfunc_convert,
    Base.:(iterate) => tfunc_iterate,
    Base.:(getindex) => tfunc_getindex,
    Base.:(leading_zeros) => (a,t)->tfunc_sub1(a, t, Integer),
    Base.:(thisind) => (a,t)->tfunc_returns(a,t,Int),
    Base.:(prevind) => (a,t)->tfunc_returns(a,t,Int),
    Base.:(nextind) => (a,t)->tfunc_returns(a,t,Int),
    Base.:(ncodeunits) => (a,t)->tfunc_returns(a,t,Int),
    Base.:(codeunit) => (a,t)->tfunc_returns(a,t,Type{Union{UInt8,UInt16,UInt32}}),
    Base.:(eof) => (a,t)->tfunc_returns(a,t,Bool),
    Base.:(readline) => (a,t)->tfunc_returns(a,t,AbstractString),
    Base.:(displaysize) => (a,t)->tfunc_returns(a,t,Tuple{Int,Int}),
    Base.:(sizeof) => (a,t)->tfunc_returns(a,t,Int),
    Base.:(length) => (a,t)->tfunc_returns(a,t,Union{Int,UInt}),
    Base.:(size) => (a,t)->tfunc_returns(a,t,length(a) == 1 ? Tuple{Vararg{Int}} : Int),
    Base.:(axes) => (a,t)->tfunc_returns(a,t,length(a) == 1 ? Tuple{Vararg{<:AbstractUnitRange}} : AbstractUnitRange),
    Base.:(resize!) => (a,t)->tfunc_returns(a,t,a[1]),
    Base.:(copyto!) => (a,t)->tfunc_returns(a,t,a[1]),
)
for sym in (
        :isabspath,
        :isapprox,
        :isascii,
        :isblockdev,
        :ischardev,
        :iscntrl,
        :isdigit,
        :isdir,
        :isdirpath,
        :isdisjoint,
        :isempty,
        :isequal,
        :iseven,
        :isfifo,
        :isfile,
        :isfinite,
        :isinf,
        :isinteger,
        :isinteractive,
        :isless,
        :isletter,
        :islink,
        :islocked,
        :islowercase,
        :ismarked,
        :ismissing,
        :ismount,
        :isnan,
        :isnothing,
        :isnumeric,
        :isodd,
        :isone,
        :isopen,
        :ispath,
        :isperm,
        :ispow2,
        :isprint,
        :ispunct,
        :isreadable,
        :isreadonly,
        :isready,
        :isreal,
        :issetequal,
        :issetgid,
        :issetuid,
        :issocket,
        :issorted,
        :isspace,
        :issticky,
        :issubnormal,
        :issubset,
        :istaskdone,
        :istaskfailed,
        :istaskstarted,
        :istextmime,
        :isuppercase,
        :isvalid,
        :iswritable,
        :isxdigit,
        :iszero,
    )
        f = resolve(GlobalRef(Base, sym))
        f === nothing && continue
        tfuncs[f] = (a,t)->tfunc_returns(a,t,Union{Bool,Missing})
end

function parcel_by_callee(badcalls::Dict{Method,Any})
    callers = IdDict{Any,Set{Method}}()
    for (m, prs) in badcalls
        for (idx, bc) in prs
            g = resolve(bc.callee)
            list = get!(callers, g, Set{Method}())
            push!(list, m)
        end
    end
    return callers
end
function print_sorted(callees)
    strs = String[]
    for (callee, list) in callees
        push!(strs, string(callee, ": ", length(list)))
    end
    sort!(strs)
    for str in strs
        println(str)
    end
    nothing
end

bfs = Dict{Method,Any}()
visit(Base) do item
    if isa(item, Method)
        isdefined(item, :generator) && return false
        try
            (src, sparams), ty = infer_with_sig(item)
            bs = bad_calls(src, sparams, ty, tfuncs)
            isempty(bs) || (bfs[item] = bs)
        catch err
            @show item
            throw(err)
        end
        return false
    end
    return true
end

callees = parcel_by_callee(bfs)
print_sorted(callees)
