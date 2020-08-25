using MethodAnalysis

"""
    is_atrisk_type(tt)

Given a Tuple-type signature (e.g., `Tuple{typeof(sum),Vector{Int}}`), determine whether this signature
is "at risk" for invalidation. Essentially it returns `true` if one or more arguments are of abstract type,
although there are prominent exceptions:

- Constructor calls with arbitrary argument types
- `convert(X, x)` where `isa(x, X)`
- `setindex!` and `push!` methods where the valtype is a subtype of the eltype (for AbstractDicts, likewise for the keytype)
- `getindex`, `length`, `isempty`, and `iterate` on any tuple

All of these are "allowed," meaning that they return `false`.
Moreover, some specific non-concrete argument types---like `Union`s of concrete types and `Function`---
do not trigger a return of `true`, although other at-risk argument types can lead to an overall `true` return
for the signature.
"""
function is_atrisk_type(@nospecialize(typ))
    # signatures like `convert(Vector, a)`, `foo(::Vararg{Synbol,N}) where N` do not seem to pose a problem
    isa(typ, TypeVar) && return false
    # isbits parameters are not a problem
    isa(typ, Type) || return false
    if isa(typ, UnionAll)
        typ = Base.unwrap_unionall(typ)
    end
    # Exclude signatures with Union{}
    typ === Union{} && return false
    isa(typ, Union) && return is_atrisk_type(typ.a) | is_atrisk_type(typ.b)
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
                    if isa(T, TypeVar)
                        T = T.ub
                    end
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
            # is_atrisk_type(typ.parameters[2]) && return true
            for i = 3:length(typ.parameters)
                is_atrisk_type(typ.parameters[i]) && return true
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
    !isempty(typ.parameters) && (any(is_atrisk_type, typ.parameters) || return false)
    return true
end

@assert  is_atrisk_type(Tuple{typeof(==),Any,Any})
@assert  is_atrisk_type(Tuple{typeof(==),Symbol,Any})
@assert  is_atrisk_type(Tuple{typeof(==),Any,Symbol})
@assert !is_atrisk_type(Tuple{typeof(==),Symbol,Symbol})
@assert !is_atrisk_type(Tuple{typeof(convert),Type{Any},Any})
@assert !is_atrisk_type(Tuple{typeof(convert),Type{AbstractString},AbstractString})
@assert !is_atrisk_type(Tuple{typeof(convert),Type{AbstractString},String})
@assert  is_atrisk_type(Tuple{typeof(convert),Type{String},AbstractString})
@assert !is_atrisk_type(Tuple{typeof(convert),Type{Union{Int,Float32}},Int})
@assert !is_atrisk_type(Tuple{typeof(convert),Type{Union{Int,Float32}},Int32})
@assert  is_atrisk_type(Tuple{typeof(convert),Type{Union{Int,Float32}},Integer})
@assert !is_atrisk_type(Tuple{typeof(convert),Type{T} where T<:Union{Int,Float32},Int})
@assert !is_atrisk_type(Tuple{typeof(map),Function,Vector{Any}})
@assert !is_atrisk_type(Tuple{typeof(getindex),Dict{Union{String,Int},Any},Union{String,Int}})
@assert  is_atrisk_type(Tuple{typeof(getindex),Dict{Union{String,Int},Any},Any})
@assert !is_atrisk_type(Tuple{Type{BoundsError},Any,Any})
@assert  is_atrisk_type(Tuple{typeof(sin),Any})
@assert !is_atrisk_type(Tuple{typeof(length),Tuple{Any,Any}})
@assert  is_atrisk_type(Tuple{typeof(setindex!),Vector{Int},Any,Int})
@assert !is_atrisk_type(Tuple{typeof(setindex!),Vector{Any},Any,Int})
@assert  is_atrisk_type(Tuple{typeof(push!),Vector{Int},Any})
@assert !is_atrisk_type(Tuple{typeof(push!),Vector{Any},Any})

# Get the name of a method as written in the code. This strips keyword-method mangling.
function codename(sym::Symbol)
    symstr = String(sym)
    # Body methods
    m = match(r"^#(.*?)#\d+$", symstr)
    m !== nothing && return Symbol(only(m.captures))
    # kw methods
    m = match(r"^(.*?)##kw$", symstr)
    m !== nothing && return Symbol(only(m.captures))
    return sym
end

isexported(mi::Core.MethodInstance) = isdefined(Main, codename(mi.def.name))
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
        if is_atrisk_type(item.specTypes)
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
miexp = Pair{Core.MethodInstance,Int}[]
mipriv = similar(miexp)
for (mi, c) in prs
    if isexported(mi)
        push!(miexp, mi=>c)
    else
        push!(mipriv, mi=>c)
    end
end
