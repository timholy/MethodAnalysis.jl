using MethodAnalysis
using Test

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
end


@testset "visit" begin
    @test Outer.Inner.g("hi") == 0
    @test Outer.f(nothing) == 1
    @test Outer.callh(1)
    @test !Outer.callh(1.0)
    @test Outer.callcallh(1)

    mis = Core.MethodInstance[]
    visit(Outer; print=false) do x
        isa(x, Core.MethodInstance) && push!(mis, x)
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
    visit(;print=false) do x
        if isa(x, Module)
            push!(mods, x)
        end
    end
    @test all(in(mods), Base.loaded_modules_array())
    @test Base ∈ mods
    @test Base.Checked ∈ mods
    @test length(mods) == length(unique(mods))
end

@testset "Backedges" begin
    mi = instance(Outer.Inner.h, (Int,))
    @test length(all_backedges(mi)) == 2
    @test terminal_backedges(mi) == [instance(Outer.callcallh, (Int,))]
    mi = instance(Outer.Inner.h, (Float64,))
    @test length(all_backedges(mi)) == 1
    @test terminal_backedges(mi) == [instance(Outer.callh, (Float64,))]
    @test all_backedges(mi) == terminal_backedges(mi)

    bes = []
    visit_backedges(x->(push!(bes, x); true), Outer.Inner.h)
    @test instance(Outer.Inner.h, (Int,)) ∈ bes
    @test instance(Outer.Inner.h, (Float64,)) ∈ bes
    @test instance(Outer.callh, (Int,)) ∈ bes
    @test instance(Outer.callh, (Float64,)) ∈ bes
    @test instance(Outer.callcallh, (Int,)) ∈ bes
    @test length(bes) == 5

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
    @test pr.second == instance(applyf, (Vector{Any},))

    bes = direct_backedges(f; skip=false)
    @test length(bes) == 2
    pr = bes[1]
    @test pr.first == Tuple{typeof(f),Any}
    @test pr.second == instance(applyf, (Vector{Any},))
    pr = bes[2]
    @test pr.first == instance(f, (Integer,))
    @test pr.second == instance(applyf, (Vector{Any},))
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

const w = worlds(instance(applyf, (Vector{Any},)))
const src = code_typed(applyf, (Vector{Any},))[1]
f(::Bool) = 2
applyf(Any[1, true])
end

@testset "Invalidation" begin
    if VERSION >= v"1.2"
        src, w = Invalidation.src, Invalidation.w
        mi = instance(Invalidation.applyf, (Vector{Any},))
        @test MethodAnalysis.equal(src, src)
        @test worlds(mi) != w
        @test !MethodAnalysis.equal(src, code_typed(Invalidation.applyf, (Vector{Any},))[1])
    end
end
