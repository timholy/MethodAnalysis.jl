using MethodAnalysis
using Test
using Logging
using ImageCore
using Pkg

module Outer
    module Inner
        g(::AbstractString) = 0
        h(::Integer) = true
        h(::AbstractFloat) = false
    end
    module OtherInner
        using ..Inner   # catch duplicates
    end
    f(x) = 1
    callh(x) = Inner.h(x)
    callcallh(x) = callh(x)

    f2(x, y::String) = 2x
    f2(x, y::Number) = x + y
end


@testset "visit" begin
    @test Outer.Inner.g("hi") == 0
    @test Outer.f(nothing) == 1
    @test Outer.callh(1)
    @test !Outer.callh(1.0)
    @test Outer.callcallh(1)

    mis = Core.MethodInstance[]
    visit(Outer) do x
        isa(x, Core.MethodInstance) && push!(mis, x)
        true
    end
    @test any(mi->mi.specTypes === Tuple{typeof(Outer.f), Nothing}, mis)
    @test count(mi->mi.specTypes === Tuple{typeof(Outer.Inner.g), String}, mis) == 1
    @test !any(mi->mi.specTypes=== Tuple{typeof(Outer.Inner.g), SubString{String}}, mis)
    @test any(mi->mi.specTypes === Tuple{typeof(Outer.Inner.h), Int}, mis)
    @test any(mi->mi.specTypes === Tuple{typeof(Outer.Inner.h), Float64}, mis)
    @test any(mi->mi.specTypes === Tuple{typeof(Outer.callh), Int}, mis)
    @test any(mi->mi.specTypes === Tuple{typeof(Outer.callh), Float64}, mis)
    @test any(mi->mi.specTypes === Tuple{typeof(Outer.callcallh), Int}, mis)
    @test !any(mi->mi.specTypes === Tuple{typeof(Outer.callcallh), Float64}, mis)

    mods = Module[]
    visit() do x
        if isa(x, Module)
            push!(mods, x)
            return true
        end
        false
    end
    @test all(in(mods), Base.loaded_modules_array())
    @test Base ∈ mods
    @test Base.Checked ∈ mods
    @test length(mods) == length(unique(mods))

    # methods(Vararg) throws an error
    nitems = Ref(0)
    visit(Vararg) do item
        nitems[] += 1
        true
    end
    @test nitems[] == 0

    # Handle constructors properly
    visit(IndexStyle) do m
        isa(m, Method) && @test Base.unwrap_unionall(m.sig).parameters[1].parameters[1] === IndexStyle
        return m === IndexStyle
    end
end

@testset "child_modules" begin
    m = Module()
    Base.eval(m, :(
        module Inner
        export Base
        end))
    mmods = child_modules(m)
    @test m ∈ mmods
    @test m.Inner ∈ mmods
    @test length(mmods) == 2
    imods = child_modules(m.Inner)
    @test m.Inner ∈ mmods
    @test length(imods) == 1

    # Base is interesting because it's not its own parent
    bmods = child_modules(Base)
    @test Base ∈ bmods
    @test Base.Sort ∈ bmods
    @test Main ∉ bmods
    smods = child_modules(Base.Sort)
    @test Base ∉ smods
    @test Base.Sort ∈ smods
end

@testset "methodinstance(s)" begin
    sum(1:3)
    mi = methodinstance(sum, (UnitRange{Int},))
    @test mi isa Core.MethodInstance
    mis = methodinstances(sum)
    @test mis isa Vector{Core.MethodInstance}
    @test mi ∈ mis
    @test length(mis) > 1

    mi = methodinstance(convert, (Type{String}, String))
    mis = methodinstances(methods(convert, (Type{String}, Any)))
    @test length(mis) > 10  # in fact, there are many more
    @test mi ∈ mis
    mis = methodinstances(which(convert, (Type{String}, AbstractString)))
    @test length(mis) > 2
    @test mi ∉ mis   # that's covered by a different Method

    f(x) = 1
    f(1)
    f(1.0)
    @test methodinstance(f, (Real,)) === nothing
    mi = methodinstance(f, (Float64,))
    @test mi.specTypes == Tuple{typeof(f),Float64}
    for mis in (methodinstances(f, (Real,)), methodinstances(Tuple{typeof(f),Real}))
        @test length(mis) == 2
        @test all(mis) do mi
            mi.specTypes ∈ (Tuple{typeof(f),Float64}, Tuple{typeof(f),Int})
        end
    end
end

@testset "AbstractTrees integration" begin
    mi = methodinstance(findfirst, (BitVector,))
    io = IOBuffer()
    MethodAnalysis.AbstractTrees.print_tree(io, mi)
    str = String(take!(io))
    @test occursin("├─", str)
end

@testset "methodinstances_owned_by" begin
    function owned_by(mi::Core.MethodInstance, mod::Module)
        m = mi.def
        m isa Method && return m.module == mod
        return m == mod
    end
    # ImageCore is a package that does a lot of `@reexport`ing
    mis = methodinstances(ImageCore)
    @test  any(mi -> owned_by(mi, ImageCore), mis)
    @test  any(mi -> owned_by(mi, ImageCore.ColorTypes), mis)  # ColorTypes is a dependency of ImageCore
    mis = methodinstances_owned_by(ImageCore)
    @test  any(mi -> owned_by(mi, ImageCore), mis)
    @test !any(mi -> owned_by(mi, ImageCore.ColorTypes), mis)
    mis = methodinstances_owned_by(ImageCore; external=true)
    @test  any(mi -> owned_by(mi, ImageCore), mis)
    @test  any(mi -> owned_by(mi, ImageCore.ColorTypes), mis)
end

@testset "Backedges" begin
    mi = methodinstance(Outer.Inner.h, (Int,))
    @test length(all_backedges(mi)) == 2
    @test terminal_backedges(mi) == [methodinstance(Outer.callcallh, (Int,))]
    mi = methodinstance(Outer.Inner.h, (Float64,))
    @test length(all_backedges(mi)) == 1
    @test terminal_backedges(mi) == [methodinstance(Outer.callh, (Float64,))]
    @test all_backedges(mi) == terminal_backedges(mi)

    bes = []
    visit_backedges(x->(push!(bes, x); true), Outer.Inner.h)
    @test methodinstance(Outer.Inner.h, (Int,)) ∈ bes
    @test methodinstance(Outer.Inner.h, (Float64,)) ∈ bes
    @test methodinstance(Outer.callh, (Int,)) ∈ bes
    @test methodinstance(Outer.callh, (Float64,)) ∈ bes
    @test methodinstance(Outer.callcallh, (Int,)) ∈ bes
    @test length(bes) == 5

    tt = Tuple{typeof(Outer.f2),Int,String}
    @test methodinstance(tt) === methodinstance(Outer.f2, (Int, String)) === nothing
    Outer.f2(1, "hello")
    m = which(Outer.f2, (Int, String))
    @test methodinstance(Outer.f2, (Int, String)) === methodinstance(m, tt) === methodinstance(tt)
    @test methodinstance(tt) !== nothing

    hbes = filter(mi->mi.def ∈ methods(Outer.Inner.h), bes)
    @test length(hbes) == 2
    allhbes = with_all_backedges(hbes)
    @test length(allhbes) == length(bes) && all(mi->mi∈bes, allhbes)

    f(::Integer) = 1
    function applyf(container)
        x1 = f(container[1])
        x2 = f(container[2])
        return x1 + x2
    end
    applyf(Any[1, true])

    bes = direct_backedges(f)
    @test length(bes) == 1
    pr = bes[1]
    @test pr.first == Tuple{typeof(f),Any}
    @test pr.second == methodinstance(applyf, (Vector{Any},))

    bes = direct_backedges(f; skip=false)
    @test length(bes) == 2
    pr = bes[1]
    @test pr.first == Tuple{typeof(f),Any}
    @test pr.second == methodinstance(applyf, (Vector{Any},))
    pr = bes[2]
    @test pr.first == methodinstance(f, (Integer,))
    @test pr.second == methodinstance(applyf, (Vector{Any},))

    nocallers(x) = x
    nocallers(3)
    mi = methodinstance(nocallers, (Int,))
    @test isempty(direct_backedges(mi))
    callnocallers(x) = nocallers(x)
    callnocallers(3)
    @test !isempty(direct_backedges(mi))
end

@testset "call_type" begin
    m = which(Outer.Inner.g, (String,))
    _g, argtypes = call_type(m.sig)
    @test _g === Outer.Inner.g
    @test argtypes === Tuple{AbstractString}
end

# These must be done at toplevel because @testset pulls method definitions out to toplevel, violating order
module Invalidation
using MethodAnalysis
f(::Integer) = 1
function applyf(container)
    x1 = f(container[1])
    x2 = f(container[2])
    return x1 + x2
end
applyf(Any[1, true])

const w = worlds(methodinstance(applyf, (Vector{Any},)))
const src = code_typed(applyf, (Vector{Any},))[1]
f(::Bool) = 2
applyf(Any[1, true])
end

@testset "Invalidation" begin
    if VERSION >= v"1.2"
        src, w = Invalidation.src, Invalidation.w
        mi = methodinstance(Invalidation.applyf, (Vector{Any},))
        @test MethodAnalysis.equal(src, src)
        @test worlds(mi) != w
        @test !MethodAnalysis.equal(src, code_typed(Invalidation.applyf, (Vector{Any},))[1])
    end
end

@testset "hasbox" begin
    function abmult(r::Int)
        if r < 0
            r = -r
        end
        f = x -> x * r
        return f
    end
    function abmult2(r::Int)
        f = x -> x * abs(r)
        return f
    end
    abmult(1)
    mi = methodinstance(abmult, (Int,))
    abmult2(1)
    mi2 = methodinstance(abmult2, (Int,))
    if isdefined(Base, :code_typed_by_type)
        @test hasbox(mi)
        @test !hasbox(mi2)
    else
        @test_throws ErrorException("hasbox requires at least Julia 1.6") hasbox(mi)
    end
end

module Callers

f(x) = rand()
function g()
    x = sin(rand())
    y = f(x)
    z = round(Int, x)
    y = f(z)
    n = Any[7]
    a = f(n[1])
    b = f(n[1]::Integer)
    c = f(n...)
    return nothing
end

# Constructors
myzeros(::Type{T}, shp) where T = zeros(T, shp)
h(c) = myzeros(Float32, c[1])
k1(c) = Vector(c[1])
k2(c) = Vector{Int}(c[1])
# typeof constructor calls
constrtypeof1(arg1, arg2) = typeof(arg2)(arg1)
constrtypeof2(arg1, arg2) = typeof(sum(arg2))(arg1)

# Keyword functions
kw1(x; a=1, b=false) = x+a+b
kw1c(x) = kw1(x; a=3)

# Scoped calls
callh() = Main.OtherModule.h(5)

end

module OtherModule
    h(x) = 22
end

if isdefined(MethodAnalysis, :findcallers)
    @testset "findcallers" begin
        Callers.g()
        mis = methodinstances(Callers)
        mi = methodinstance(Callers.g, ())
        # Why are there no `:invoke`s, only `:call`s?
        callers1 = findcallers(Callers.f, argtyps->length(argtyps) == 1 && argtyps[1] === Float64, mis; callhead=:invoke)
        callers2 = findcallers(Callers.f, argtyps->length(argtyps) == 1 && argtyps[1] === Int, mis; callhead=:invoke)
        callers3 = findcallers(Callers.f, argtyps->length(argtyps) == 1 && argtyps[1] === Any, mis; callhead=:call)
        callers4 = findcallers(Callers.f, argtyps->length(argtyps) == 1 && argtyps[1] === Integer, mis; callhead=:call)
        callers5 = findcallers(Callers.f, argtyps->length(argtyps) == 1 && argtyps[1] === Vector{Any}, mis; callhead=:iterate)
        @test_broken callers1[1] == mi && callers1[3] < callers3[3]
        @test_broken callers2[1] == mi && callers2[3] < callers4[3]
        # Given that :invoke isn't showing up, grab callers1 & callers2 again
        callers1 = findcallers(Callers.f, argtyps->length(argtyps) == 1 && argtyps[1] === Float64, mis; callhead=:call)
        callers2 = findcallers(Callers.f, argtyps->length(argtyps) == 1 && argtyps[1] === Int, mis; callhead=:call)
        allcallers = [callers1, callers2, callers3, callers4, callers5]
        @test all(x->length(x) == 1, allcallers)
        @test all(x->only(x).mi == mi, allcallers)
        stmtid = map(x->only(x).line, allcallers)
        @test issorted(stmtid) && length(unique(stmtid)) == 5

        c = AbstractUnitRange[Base.OneTo(3)]
        Callers.h(c)
        mis = methodinstances(Callers)
        callers = findcallers(Callers.myzeros, argtyps->length(argtyps) == 2 && argtyps[1] === Type{Float32} && AbstractUnitRange<:argtyps[2], mis)
        cm = only(callers)
        @test cm.mi.def === which(Callers.h, (Any,))
        @test cm.argtypes == Any[Type{Float32}, AbstractUnitRange]
        callers = findcallers(zeros, argtyps->length(argtyps) == 2 && argtyps[1] === Type{Float32} && AbstractUnitRange<:argtyps[2], mis)
        cm = only(callers)
        @test cm.mi.def === only(methods(Callers.myzeros))
        @test cm.argtypes == Any[Type{Float32}, AbstractUnitRange]
        misz = methodinstances(zeros)
        callers = findcallers(zeros, argtyps->length(argtyps) == 2 && argtyps[1] === Type{Float32} && Tuple{AbstractUnitRange}<:argtyps[2], misz)
        allargtypes = map(cm->cm.argtypes, callers)
        @test Any[Type{Float32}, Tuple{AbstractUnitRange}] ∈ allargtypes ||
              Any[Type{Float32}, AbstractUnitRange] ∈ allargtypes

        Callers.k1(c)
        Callers.k2(c)
        mis = methodinstances(Callers)
        callers = findcallers(Vector, argtyps->length(argtyps) == 1 && AbstractUnitRange<:argtyps[1], mis)
        allmeths = map(cm->cm.mi.def, callers)
        @test length(allmeths) == 2 && only(methods(Callers.k1)) ∈ allmeths && only(methods(Callers.k2)) ∈ allmeths

        @test Callers.constrtypeof1([0 1; 1 0], [true true; true false]) isa Matrix{Bool}
        mis = methodinstances(Callers)
        callers = findcallers(Matrix, nothing, mis)
        cm = only(callers)
        @test cm.mi.def === which(Callers.constrtypeof1, (Any,Any))
        @test Callers.constrtypeof2(3.0, Float16[true true; true false]) isa Float16
        mis = methodinstances(Callers)
        callers = findcallers(Float16, nothing, mis)
        cm = only(callers)
        @test cm.mi.def === which(Callers.constrtypeof2, (Any,Any))

        Callers.kw1c(2)
        mis = methodinstances(Callers)
        callers = findcallers(Callers.kw1, nothing, mis)
        cm = only(callers)
        @test cm.mi.def === which(Callers.kw1c, (Any,))

        @test Callers.callh() == 22
        # Callers.callcolors(Base.CoreLogging.Debug)
        Logging.default_logcolor(Base.CoreLogging.Debug)
        mis = append!(methodinstances(Callers), methodinstances(Logging.default_logcolor))
        callers = findcallers(OtherModule.h, nothing, mis)
        cm = only(callers)
        @test cm.mi.def === which(Callers.callh, ())
        append!(mis, methodinstances(Pkg.BinaryPlatforms))
        callers2 = findcallers(OtherModule.h, nothing, mis)
        @test length(callers2) == 1

        # show
        io = IOBuffer()
        print(io, cm)
        str = String(take!(io))
        @test startswith(str, "CallMatch")
        @test occursin("on statement", str)

        # internals
        @test !isempty(MethodAnalysis.get_typed_instances(cm.mi))
    end
end
