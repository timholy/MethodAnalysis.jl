using Documenter, MethodAnalysis

makedocs(;
    modules=[MethodAnalysis],
    format=Documenter.HTML(),
    pages=[
        "Home" => "index.md",
    ],
    repo="https://github.com/timholy/MethodAnalysis.jl/blob/{commit}{path}#L{line}",
    sitename="MethodAnalysis.jl",
    authors="Tim Holy <tim.holy@gmail.com>",
)

deploydocs(;
    repo="github.com/timholy/MethodAnalysis.jl",
)
