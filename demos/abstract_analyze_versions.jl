# Do `include("abstract.jl")` on two Julia versions and then run this script.
# You will likely have to change the path for `sigstable` below.
# Typically this should be run using the in-development version of Julia (or whatever "latest" is in your comparison)

using PyPlot

function parseline(line)
    m = match(r"(.*) => (.*)", line)
    sigstr, count = m.captures[1], parse(Int, m.captures[2])
    sig = try
        ex = Meta.parse(sigstr)
        eval(ex)
    catch
        return nothing
    end
    return sig, count
end

function parsedata(filename)
    lines = readlines(filename)
    sigcount = IdDict{Any,Int}()
    for line in lines
        ret = parseline(line)
        ret === nothing && continue
        sig, count = ret
        sigcount[sig] = count
    end
    return sigcount
end

function split_comparable(sigc1, sigc2)
    c1, c2, sigs = Int[], Int[], Any[]
    for (sig, c) in sigc1
        push!(sigs, sig)
        push!(c1, sigc1[sig])
        push!(c2, get(sigc2, sig, 0))
    end
    for (sig, c) in sigc2
        if !haskey(sigc1, sig)
            push!(sigs, sig)
            push!(c1, 0)
            push!(c2, c)
        end
    end
    return sigs, c1, c2
end

function tally0(c1, c2)
    nz1, nz2 = 0, 0
    for (a1, a2) in zip(c1, c2)
        a1 == a2 == 0 && continue
        a1 == 0 && (nz1 += 1)
        a2 == 0 && (nz2 += 1)
    end
    return nz1, nz2
end

sigmaster = parsedata("/tmp/methdata_$VERSION.log")
sigstable, stablever = parsedata("/tmp/methdata_1.5.1-pre.29.log"), "1.5"
# sigstable, stablever = parsedata("/tmp/methdata_1.4.2.log"), "1.4"

sigs, c1, c2 = split_comparable(sigstable, sigmaster)
nz1, nz2 = tally0(c1, c2)
println("$stablever has $nz1 with no backedges, master has $nz2")
mx1, mx2 = maximum(c1), maximum(c2)
get_fname(@nospecialize(fT::DataType)) = @static VERSION ≥ v"1.13.0-DEV.647" ? fT.name.singletonname : fT.name.mt.name
isexported(sig) = (ft = Base.unwrap_unionall(sig).parameters[1]; isdefined(Main, get_fname(ft)))
colors = [isexported(sig) ? "magenta" : "green" for sig in sigs]

function on_click(event)
    x, y = event.xdata, event.ydata
    normsqrdist(pr) = ((pr[1]-x)/mx1)^2 + ((pr[2]-y)/mx2)^2
    idx = argmin(normsqrdist.(zip(c1, c2)))
    println(sigs[idx])
end
begin
    hfig, axs = plt.subplots(2, 1)
    plt.subplots_adjust(hspace=0.3)
    logedges = LinRange(0, log10(max(mx1, mx2)+2), 30)
    ax = axs[1]
    ax.hist(log10.(c1 .+ 1), bins=logedges)
    ax.set_xlabel("log₁₀(# backedges + 1), $stablever")
    ax.set_ylabel("# 'at risk' signatures")
    ax = axs[2]
    ax.hist(log10.(c2 .+ 1), bins=logedges)
    ax.set_xlabel("log₁₀(# backedges + 1), 1.6")
    ax.set_ylabel("# 'at risk' signatures")

    display(hfig)
    fig, ax = plt.subplots()
    ax.scatter(c1 .+ 1, c2 .+ 1, c=colors)  # + 1 for the log-scaling
    ax.set_xlabel("# backedges + 1, $stablever")
    ax.set_ylabel("# backedges + 1, 1.6")
    ax.set_xscale("log")
    ax.set_yscale("log")
    ax.set_aspect("equal")
    fig.canvas.callbacks.connect("button_press_event", on_click)
    fig
end
