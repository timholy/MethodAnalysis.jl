atrisk_backedges = with_all_backedges(keys(becounter))

function atrisk_method(m::Method, atrisk_backedges)
    for mi in methodinstances(m)
        mi ∈ atrisk_backedges && return true
    end
    return false
end

function atrisk_triggers(m::Method, atrisk_instances)
    triggers = Set{Core.MethodInstance}()
    for mi in atrisk_instances
        if atrisk_method(m, all_backedges(mi))
            push!(triggers, mi)
        end
    end
    return triggers
end

# This removes MethodInstances that no one in their right mind should ever invalidate by specialization.
function remove_unlikely_methodinstances(list)
    out = Core.MethodInstance[]
    for mi in list
        mi = mi::Core.MethodInstance   # must have MethodInstance elements
        # All `continue` statements below omit the MethodInstance
        name = codename(mi.def.name)
        name ∈ (:invokelatest, :unwrap_unionall, :rewrap_unionall) && continue
        name ∈ (:print, :sprint) && length(mi.specTypes.parameters) - mi.def.nkw > 3 && continue
        # No one should ever specialize on notify or schedule's `val` argument
        name === :notify && !is_atrisk_type(mi.specTypes.parameters[2]) &&
            !any(is_atrisk_type, mi.specTypes.parameters[4:end]) && continue
        name === :schedule && !any(is_atrisk_type, mi.specTypes.parameters[2:end-1]) && continue
        # Add more removal-filters here

        # We've decided to keep it
        push!(out, mi)
    end
    return out
end

using Test

# Invalidating the code that loads packages leads to major slowdowns, especially if it happens repeatedly
# in a dependent chain of package loads. Ideally, we'd make this code-path "bulletproof".
for m in methods(Base.require)
    @test_broken isempty(remove_unlikely_methodinstances(atrisk_triggers(m, first.(miexp))))
    @test_broken isempty(remove_unlikely_methodinstances(atrisk_triggers(m, first.(mipriv))))
end

# Test overall number of atrisk MethodInstances and their average number of backedges
badexp = Set(remove_unlikely_methodinstances(first.(miexp)))
badcounts = filter(pr->pr.first ∈ badexp, miexp)
@test length(badcounts) < 1250  # original target 1000
if length(badcounts) < 1000 # 800
    @info "There are now only $(length(badcounts)) at-risk specializations of exported methods, consider dropping the threshold"
end
meancounts = sum(last.(badcounts))/length(badcounts)
@test meancounts < 32
if meancounts < 24
    @info "The mean number of at-risk backedges is now only $meancounts, consider dropping the threshold"
end

# Check for inference quality in specific functions.
# This is valid only for functions that should always return a particular type for any valid call of their methods.
function function_returns(@nospecialize(f), @nospecialize(typ); allow_missing_for_missing=true, minargs=0)
    for m in methods(f)
        sig = Base.unwrap_unionall(m.sig)
        for rt in Base.return_types(call_type(Base.unwrap_unionall(m.sig))...)
            rt <: typ && continue
            if allow_missing_for_missing && any(T->T===Missing, sig.parameters[2:end]) && rt === Missing
                continue
            end
            length(sig.parameters) - 1 < minargs && continue
            return false
        end
    end
    return true
end

# All the is* functions
# Not all of the broken cases have been checked carefully; it's possible some of these should return `Union{Bool,Missing}`
# or something.
@test_broken function_returns(isabspath, Bool)
@test function_returns(isabstracttype, Bool)
@test_broken function_returns(isapprox, Bool)
@test_broken function_returns(isascii, Bool)
# @test function_returns(isassigned, Bool)
@test function_returns(isbits, Bool)
@test function_returns(isbitstype, Bool)
@test function_returns(isblockdev, Bool)
@test function_returns(ischardev, Bool)
@test function_returns(iscntrl, Bool)
@test function_returns(isconcretetype, Bool)
@test function_returns(isconst, Bool)
@test function_returns(isdefined, Bool)
@test function_returns(isdigit, Bool)
@test function_returns(isdir, Bool)
@test function_returns(isdirpath, Bool)
@test_broken function_returns(isdisjoint, Bool)
@test function_returns(isdispatchtuple, Bool)
@test_broken function_returns(isempty, Bool)
@test_broken function_returns(isequal, Bool; minargs=2)
@test_broken function_returns(iseven, Bool)
@test function_returns(isexported, Bool)
@test function_returns(isfifo, Bool)
@test function_returns(isfile, Bool)
@test_broken function_returns(isfinite, Bool)
@test_broken function_returns(isinf, Bool)
@test_broken function_returns(isinteger, Bool)
@test function_returns(isinteractive, Bool)
@test_broken function_returns(isless, Bool)
@test function_returns(isletter, Bool)
@test function_returns(islink, Bool)
@test function_returns(islocked, Bool)
@test function_returns(islowercase, Bool)
@test_broken function_returns(ismarked, Bool)
@test function_returns(ismissing, Bool)
@test function_returns(ismount, Bool)
@test function_returns(ismutable, Bool)
@test_broken function_returns(isnan, Bool)
@test function_returns(isnothing, Bool)
@test function_returns(isnumeric, Bool)
@test_broken function_returns(isodd, Bool)
@test_broken function_returns(isone, Bool)
@test_broken function_returns(isopen, Bool)
@test function_returns(ispath, Bool)
@test_broken function_returns(isperm, Bool)
@test function_returns(ispow2, Bool)
@test function_returns(isprimitivetype, Bool)
@test function_returns(isprint, Bool)
@test function_returns(ispunct, Bool)
@test_broken function_returns(isreadable, Bool)
@test_broken function_returns(isreadonly, Bool)
@test_broken function_returns(isready, Bool)
@test_broken function_returns(isreal, Bool)
@test function_returns(issetequal, Bool)
@test function_returns(issetgid, Bool)
@test function_returns(issetuid, Bool)
@test function_returns(issocket, Bool)
@test_broken function_returns(issorted, Bool)
@test function_returns(isspace, Bool)
@test function_returns(issticky, Bool)
@test function_returns(isstructtype, Bool)
@test_broken function_returns(issubnormal, Bool)
@test_broken function_returns(issubset, Bool)
@test function_returns(istaskdone, Bool)
@test function_returns(istaskfailed, Bool)
@test function_returns(istaskstarted, Bool)
@test_broken function_returns(istextmime, Bool)
@test function_returns(isuppercase, Bool)
@test_broken function_returns(isvalid, Bool)
@test_broken function_returns(iswritable, Bool)
@test function_returns(isxdigit, Bool)
@test_broken function_returns(iszero, Bool)

@test function_returns(eof, Bool)

# Check that we never infer certain methodinstances
for f in (==, isequal, <, <=)
    for mi in methodinstances(==)
        if any(T->T<:Real, mi.specTypes.parameters)
            @test !is_atrisk_type(mi.specTypes)
        end
    end
end
