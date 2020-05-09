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


@testset "MethodAnalysis.jl" begin
    @test Outer.Inner.g("hi") == 0
    @test Outer.f(nothing) == 1
    @test Outer.callh(1)
    @test !Outer.callh(1.0)
    @test Outer.callcallh(1)

    function mmatches(mi, mod, name, spectypes)
        mi.def.module === mod || return false
        mi.def.name === name || return false
        for (t1, t2) in zip(Iterators.drop(mi.specTypes.parameters, 1), spectypes)
            t1 === t2 || return false
        end
        return true
    end

    mis = Core.MethodInstance[]
    visit(Outer) do x
        isa(x, Core.MethodInstance) && push!(mis, x)
    end
    @test any(mi->mmatches(mi, Outer, :f, (Nothing,)), mis)
    @test count(mi->mmatches(mi, Outer.Inner, :g, (String,)), mis) == 1
    @test !any(mi->mmatches(mi, Outer.Inner, :g, (SubString,)), mis)
    @test any(mi->mmatches(mi, Outer.Inner, :h, (Int,)), mis)
    @test any(mi->mmatches(mi, Outer.Inner, :h, (Float64,)), mis)
    @test any(mi->mmatches(mi, Outer, :callh, (Int,)), mis)
    @test any(mi->mmatches(mi, Outer, :callh, (Float64,)), mis)
    @test any(mi->mmatches(mi, Outer, :callcallh, (Int,)), mis)
    @test !any(mi->mmatches(mi, Outer, :callcallh, (Float64,)), mis)

    mi = mis[findfirst(mi->mmatches(mi, Outer.Inner, :h, (Int,)), mis)]
    @test length(all_backedges(mi)) == 2
    @test terminal_backedges(mi) == [mis[findfirst(mi->mmatches(mi, Outer, :callcallh, (Int,)), mis)]]
    mi = mis[findfirst(mi->mmatches(mi, Outer.Inner, :h, (Float64,)), mis)]
    @test length(all_backedges(mi)) == 1
    @test terminal_backedges(mi) == [mis[findfirst(mi->mmatches(mi, Outer, :callh, (Float64,)), mis)]]
    @test all_backedges(mi) == terminal_backedges(mi)
end
