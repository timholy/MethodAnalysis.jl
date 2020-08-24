using MethodAnalysis


"""
    atrisktyp(tt)

Given a Tuple-type signature (e.g., `Tuple{typeof(sum),Vector{Int}}`), determine whether this signature
is "at risk" for invalidation. Essentially it returns `true` if one or more arguments are of abstract type,
although there are prominent exceptions:

- `Function` is allowed
- any constructor call is allowed
- `convert(X, x)` where `isa(x, X)` is true
- `setindex!` and `push!` methods where the valtype is a subtype of the eltype (likewise keytype for AbstractDicts)
- `getindex`, `length`, `isempty`, and `iterate` on any tuple
"""
function atrisktype(@nospecialize(typ))
    # signatures like `convert(Vector, a)`, `foo(::Vararg{Synbol,N}) where N` do not seem to pose a problem
    isa(typ, TypeVar) && return false
    # isbits parameters are not a problem
    isa(typ, Type) || return false
    if isa(typ, UnionAll)
        typ = Base.unwrap_unionall(typ)
    end
    # Exclude signatures with Union{}
    typ === Union{} && return false
    isa(typ, Union) && return atrisktype(typ.a) | atrisktype(typ.b)
    # Type{T}: signatures like `convert(::Type{AbstractString}, ::String)` are not problematic
    typ <: Type && return false
    if typ <: Tuple && length(typ.parameters) >= 1
        p1 = typ.parameters[1]
        # Constructor calls are not themselves a problem (any `convert`s they trigger might be, but those are covered)
        isa(p1, Type) && p1 <: Type && return false
        # convert(::Type{T}, ::S) where S<:T is not problematic
        if p1 === typeof(Base.convert) || p1 === typeof(Core.convert)
            p2, p3 = typ.parameters[2], typ.parameters[3]
            if isa(p2, Type)
                p2 = Base.unwrap_unionall(p2)
                if isa(p2, DataType) && length(p2.parameters) === 1
                    T = p2.parameters[1]
                    isa(p3, Type) && isa(T, Type) && p3 <: T && return false
                end
            end
        # `getindex`, `length`, etc are OK for various Tuple{T1,T2,...}
        elseif p1 === typeof(Base.getindex) ||
               p1 === typeof(Base.length)  ||
               p1 === typeof(Base.isempty) ||
               p1 === typeof(Base.iterate) || p1 === typeof(Core.iterate)
            p2 = typ.parameters[2]
            if isa(p2, Type)
                p2 = Base.unwrap_unionall(p2)
                p2 <: Tuple && return false
            end
        # show(io::IO, x) is OK as long as typeof(x) is safe
        elseif p1 === typeof(Base.show) || p1 === typeof(Base.print) || p1 === typeof(Base.println)
            # atrisktype(typ.parameters[2]) && return true
            for i = 3:length(typ.parameters)
                atrisktype(typ.parameters[i]) && return true
            end
            return false
        # setindex!(a, x, idx) and push!(a, x) are safe if typeof(x) <: eltype(a)
        elseif (p1 === typeof(Base.setindex!) || p1 === typeof(Base.push!)) && length(typ.parameters) >= 3
            p2, p3 = typ.parameters[2], typ.parameters[3]
            if isconcretetype(p2)
                if p2 <: AbstractDict && length(typ.parameters) >= 4
                    p4 = typ.parameters[4]
                    p3 <: valtype(p2) && p4 <: keytype(p2) && return false
                else
                    p3 <: eltype(p2) && return false
                end
            end
        end
    end
    # Standard DataTypes
    isconcretetype(typ) && return false
    # ::Function args are excluded
    typ === Function && return false
    !isempty(typ.parameters) && (any(atrisktype, typ.parameters) || return false)
    return true
end

@assert  atrisktype(Tuple{typeof(==),Any,Any})
@assert  atrisktype(Tuple{typeof(==),Symbol,Any})
@assert  atrisktype(Tuple{typeof(==),Any,Symbol})
@assert !atrisktype(Tuple{typeof(==),Symbol,Symbol})
@assert !atrisktype(Tuple{typeof(convert),Type{Any},Any})
@assert !atrisktype(Tuple{typeof(convert),Type{AbstractString},AbstractString})
@assert !atrisktype(Tuple{typeof(convert),Type{AbstractString},String})
@assert  atrisktype(Tuple{typeof(convert),Type{String},AbstractString})
@assert !atrisktype(Tuple{typeof(map),Function,Vector{Any}})
@assert !atrisktype(Tuple{typeof(getindex),Dict{Union{String,Int},Any},Union{String,Int}})
@assert  atrisktype(Tuple{typeof(getindex),Dict{Union{String,Int},Any},Any})
@assert !atrisktype(Tuple{Type{BoundsError},Any,Any})
@assert  atrisktype(Tuple{typeof(sin),Any})
@assert !atrisktype(Tuple{typeof(length),Tuple{Any,Any}})
@assert  atrisktype(Tuple{typeof(setindex!),Vector{Int},Any,Int})
@assert !atrisktype(Tuple{typeof(setindex!),Vector{Any},Any,Int})
@assert  atrisktype(Tuple{typeof(push!),Vector{Int},Any})
@assert !atrisktype(Tuple{typeof(push!),Vector{Any},Any})

isexported(mi::Core.MethodInstance) = isdefined(Main, mi.def.name)
getfunc(mi::Core.MethodInstance) = getfunc(mi.def)
getfunc(m::Method) = getfield(m.module, m.name)
nmethods(mi::Core.MethodInstance) = length(methods(getfunc(mi)))

# Test whether a module is Core.Compiler or inside it
# Methods there are protected from invalidation by other means
function fromcc(mod::Module)
    fn = fullname(mod)
    return length(fn) >= 2 && fn[1] === :Core && fn[2] === :Compiler
end

const mis = Dict{Method,Vector{Core.MethodInstance}}()
visit() do item
    if item isa Method && !fromcc(item.module)
        m = item
        mis[m] = methodinstances(m)
        return false
    end
    return true
end

# Count # of backedges for MethodInstances with abstract types
const becounter = Dict{Core.MethodInstance,Int}()
visit() do item
    if item isa Core.MethodInstance && !fromcc(item.def.module)
        if atrisktype(item.specTypes)
            becounter[item] = length(all_backedges(item))
        end
        return false
    end
    return true
end

prs = sort!(collect(becounter); by=last)
open("/tmp/methdata_$VERSION.log", "w") do io
    for (mi, c) in prs
        c == 0 && continue
        println(io, mi.specTypes=>c)
    end
end

# Split into exported & private functions
mtup = (nmethods = 0, nbackedges = 0)
miexp = Pair{Core.MethodInstance,typeof(mtup)}[]
mipriv = similar(miexp)
for (mi, c) in prs
    n = nmethods(mi)
    pr = mi=>(nmethods=n, nbackedges=c)
    if isexported(mi)
        push!(miexp, pr)
    else
        push!(mipriv, pr)
    end
end
