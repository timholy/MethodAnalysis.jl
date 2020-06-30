using MethodAnalysis

# Analyze MethodInstance signatures and select those that seem at risk for being invalidated.
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
    # Type{T}: signatures like `convert(::Type{AbstractString}, ::String)` are not problematic, so mark Type as OK
    typ <: Type && return false
    if typ <: Tuple && length(typ.parameters) >= 1
        p1 = typ.parameters[1]
        # Constructor calls are not themselves a problem (any `convert`s they trigger might be, but those are covered)
        isa(p1, Type) && p1 <: Type && return false
        # convert(::Type{T}, ::S) where S<:T is not problematic
        if p1 === typeof(Base.convert) || p1 === typeof(Core.convert) || p1 === typeof(Core.Compiler.convert)
            p2, p3 = typ.parameters[2], typ.parameters[3]
            if isa(p2, Type)
                p2 = Base.unwrap_unionall(p2)
                if isa(p2, DataType) && length(p2.parameters) === 1
                    T = p2.parameters[1]
                    isa(p3, Type) && isa(T, Type) && p3 <: T && return false
                end
            end
        # `getindex`, `length`, etc are OK for various Tuple{T1,T2,...}
        elseif (p1 === typeof(Base.getindex) || p1 === typeof(Core.Compiler.getindex)) ||
               (p1 === typeof(Base.length)  || p1 === typeof(Core.Compiler.length)) ||
               (p1 === typeof(Base.isempty) || p1 === typeof(Core.Compiler.isempty)) ||
               (p1 === typeof(Base.iterate) || p1 === typeof(Core.iterate) || p1 === typeof(Core.Compiler.iterate))
            p2 = typ.parameters[2]
            if isa(p2, Type)
                p2 = Base.unwrap_unionall(p2)
                p2 <: Tuple && return false
            end
        # show(io::IO, x) is OK as long as typeof(x) is safe
        elseif p1 === typeof(Base.show)
            atrisktype(typ.parameters[2]) && return true
            length(typ.parameters) == 3 && atrisktype(typ.parameters[3]) && return true
            return false
        end
    end
    # Standard DataTypes
    isconcretetype(typ) && return false
    # ::Function args are excluded
    typ === Function && return false
    !isempty(typ.parameters) && (any(atrisktype, typ.parameters) || return false)
    return true
end

# A few tests
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

function collect_mis(m::Method)
    list = Core.MethodInstance[]
    visit(m) do item
        if isa(item, Core.MethodInstance)
            push!(list, item)
            return false
        end
        return true
    end
    return list
end

const mis = Dict{Method,Vector{Core.MethodInstance}}()
visit() do item
    if item isa Method
        m = item
        mis[m] = collect_mis(m)
        return false
    end
    return true
end

# Count # of backedges for MethodInstances with abstract types
const becounter = Dict{Core.MethodInstance,Int}()
visit() do item
    if item isa Core.MethodInstance
        if atrisktype(item.specTypes)
            becounter[item] = length(all_backedges(item))
        end
        return false
    end
    return true
end

prs = sort!(collect(becounter); by=last)

# Organize them by method instead

const mcounter = Dict{Method,Int}()
for (mi, c) in becounter
    oc = get(mcounter, mi.def, 0)
    mcounter[mi.def] = oc + c
end

mprs = sort!(collect(mcounter); by=last)

open("/tmp/methinstdata_$VERSION.log", "w") do io
    for (mi, c) in prs
        c == 0 && continue
        println(io, mi.specTypes=>c)
    end
end
open("/tmp/methdata_$VERSION.log", "w") do io
    for (m, c) in mprs
        c == 0 && continue
        println(io, m.sig=>c)
    end
end
