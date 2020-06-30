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

sigc16 = parsedata("/tmp/methdata_$VERSION.log")
sigc14 = parsedata("/tmp/methdata_1.4.3-pre.0.log")

sigs, c1, c2 = split_comparable(sigc14, sigc16)
mx1, mx2 = maximum(c1), maximum(c2)
isexported(sig) = (ft = Base.unwrap_unionall(sig).parameters[1]; isdefined(Main, ft.name.mt.name))
colors = [isexported(sig) ? "magenta" : "green" for sig in sigs]

function on_click(event)
    x, y = event.xdata, event.ydata
    normsqrdist(pr) = ((pr[1]-x)/mx1)^2 + ((pr[2]-y)/mx2)^2
    idx = argmin(normsqrdist.(zip(c1, c2)))
    println(sigs[idx])
end
begin
    fig, ax = plt.subplots()
    ax.scatter(c1 .+ 1, c2 .+ 1, c=colors)  # + 1 for the log-scaling
    ax.set_xlabel("# backedges + 1, 1.4")
    ax.set_ylabel("# backedges + 1, 1.6")
    ax.set_xscale("log")
    ax.set_yscale("log")
    fig.canvas.callbacks.connect("button_press_event", on_click)
    fig
end

# Ones we've made progress on:
# ==(::Any, Symbol)
# ==(::Symbol, ::Any)
# ==(::Any, ::Nothing)
# ==(::UUID, ::Any)
# ==(::AbstractString, ::String)
# isequal(::Symbol, ::Any)
# isequal(::Any, ::Symbol)
# isequal(::Any, ::Nothing)
# isequal(::UUID, ::Any)
# cmp(::AbstractString, ::String)
# convert(::Type{Int}, ::Integer)
# convert(::Type{UInt}, ::Integer)
# convert(::Type{Union{Nothing,Module}}, ::Any)
# Base.to_index(::Integer)
# iterate(::Base.OneTo, ::Any)
# repr(::Any)
# thisind(::AbstractString, ::Int)
# getindex(::String, ::Any)
# string(::String, ::Integer, ::String)
# ^(::String, ::Integer)
# repeat(::String, ::Integer)
# Base.isidentifier(::AbstractString)
# +(::Ptr{UInt8}, ::Integer)
# Base._show_default(::Base.GenericIOBuffer{Array{UInt8,1}}, ::Any)

# Ones that are better but I don't remember helping with
# isconcretetype(::Any)
# pointer(::String, ::Integer)

# Regressions:
# basename(::AbstractString)
# splitdir(::AbstractString)
# isfile(::Any)
# joinpath(::AbstractString, ::String)
# sizeof(::Unsigned)
# +(::Int, ::Any, ::Any)
# Base.split_sign(::Integer)
# in(::Any, ::Tuple{Symbol})
