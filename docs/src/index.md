# MethodAnalysis.jl

This package facilitates introspection of Julia's internals, with a particular focus on its MethodInstances and their backedges.

!!! warning
    Julia's internals are not subject to the same interface compatibility guarantee that the rest of the language enjoys.

## Demonstrations

A few demonstrations will give you a taste of what can be done with this package.

### Collecting all submodules of Base

```jldoctest
julia> using MethodAnalysis

julia> mods = Module[];

julia> visit(Base) do obj
           if isa(obj, Module)
               push!(mods, obj)
               return true     # descend into submodules
           end
           false   # but don't descend into anything else (MethodTables, etc.)
       end

julia> Base.FastMath ∈ mods
true
```

### Collecting all Methods in Core.Compiler

`visit` also descends into functions, methods, and MethodInstances:

```jldoctest; setup=:(using MethodAnalysis)
julia> meths = []
Any[]

julia> visit(Core.Compiler) do item
           isa(item, Method) && push!(meths, item)
           true   # walk through everything
       end

julia> first(methods(Core.Compiler.typeinf_ext)) ∈ meths
true
```

### Getting a MethodInstance for a particular set of types

```jldoctest; setup=:(using MethodAnalysis)
julia> foo(::AbstractVector) = 1
foo (generic function with 1 method)

julia> methodinstance(foo, (Vector{Int},))   # we haven't called it yet, so it's not compiled


julia> foo([1,2])
1

julia> methodinstance(foo, (Vector{Int},))
MethodInstance for foo(::Vector{Int64})
```

### Collecting a subset of MethodInstances for a particular function

Let's collect all single-argument compiled instances of `findfirst`:

```jldoctest findfirst; setup=:(using MethodAnalysis)
julia> mis = Core.MethodInstance[];

julia> visit(findfirst) do item
           isa(item, Core.MethodInstance) && length(Base.unwrap_unionall(item.specTypes).parameters) == 2 && push!(mis, item)
           true
       end

julia> mis
1-element Vector{Core.MethodInstance}:
 MethodInstance for findfirst(::BitVector)
```

We checked that the length was 2, rather than 1, because the first parameter is the function type itself:

```jldoctest findfirst
julia> mis[1].specTypes
Tuple{typeof(findfirst), BitVector}
```

There's also a convenience shortcut:

```julia
julia> mis = methodinstances(findfirst)
```

### Getting the backedges for a function

Let's see all the compiled instances of `Base.setdiff` and their immediate callers:

```jldoctest; setup=(using MethodAnalysis)
julia> direct_backedges(setdiff)
6-element Vector{Any}:
     MethodInstance for setdiff(::Base.KeySet{Any, Dict{Any, Any}}, ::Base.KeySet{Any, Dict{Any, Any}}) => MethodInstance for REPL.LineEdit.keymap_merge(::Dict{Char, Any}, ::Dict{Any, Any})
     MethodInstance for setdiff(::Base.KeySet{Any, Dict{Any, Any}}, ::Base.KeySet{Any, Dict{Any, Any}}) => MethodInstance for REPL.LineEdit.keymap_merge(::Dict{Char, Any}, ::Union{Dict{Any, Any}, Dict{Char, Any}})
   MethodInstance for setdiff(::Base.KeySet{Char, Dict{Char, Any}}, ::Base.KeySet{Any, Dict{Any, Any}}) => MethodInstance for REPL.LineEdit.keymap_merge(::Dict{Char, Any}, ::Union{Dict{Any, Any}, Dict{Char, Any}})
   MethodInstance for setdiff(::Base.KeySet{Any, Dict{Any, Any}}, ::Base.KeySet{Char, Dict{Char, Any}}) => MethodInstance for REPL.LineEdit.keymap_merge(::Dict{Char, Any}, ::Union{Dict{Any, Any}, Dict{Char, Any}})
 MethodInstance for setdiff(::Base.KeySet{Char, Dict{Char, Any}}, ::Base.KeySet{Char, Dict{Char, Any}}) => MethodInstance for REPL.LineEdit.keymap_merge(::Dict{Char, Any}, ::Union{Dict{Any, Any}, Dict{Char, Any}})
                                   MethodInstance for setdiff(::Vector{Base.UUID}, ::Vector{Base.UUID}) => MethodInstance for Pkg.Operations.deps_graph(::Pkg.Types.Context, ::Dict{Base.UUID, String}, ::Dict{Base.UUID, Pkg.Types.VersionSpec}, ::Dict{Base.UUID, Pkg.Resolve.Fixed})
```

### Printing backedges as a tree

MethodAnalysis uses [AbstractTrees](https://github.com/JuliaCollections/AbstractTrees.jl) to display the complete set of backedges:

```jldoctest; setup=:(using MethodAnalysis)
julia> mi = methodinstance(findfirst, (BitVector,))
MethodInstance for findfirst(::BitVector)

julia> MethodAnalysis.print_tree(mi)
MethodInstance for findfirst(::BitVector)
├─ MethodInstance for Pkg.Resolve.prune_graph!(::Graph)
│  └─ MethodInstance for Pkg.Resolve.var"#simplify_graph!#111"(::Bool, ::typeof(simplify_graph!), ::Graph, ::Set{Int64})
│     └─ MethodInstance for Pkg.Resolve.simplify_graph!(::Graph, ::Set{Int64})
│        └─ MethodInstance for Pkg.Resolve.simplify_graph!(::Graph)
│           ├─ MethodInstance for Pkg.Resolve.trigger_failure!(::Graph, ::Vector{Int64}, ::Tuple{Int64, Int64})
│           │  ⋮
│           │
│           └─ MethodInstance for Pkg.Operations.resolve_versions!(::Context, ::Vector{PackageSpec})
│              ⋮
│
└─ MethodInstance for Pkg.Resolve.update_solution!(::SolutionTrace, ::Graph)
   └─ MethodInstance for Pkg.Resolve.converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)
      ├─ MethodInstance for Pkg.Resolve.converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)
      │  ├─ MethodInstance for Pkg.Resolve.converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)
      │  │  ├─ MethodInstance for Pkg.Resolve.converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)
      │  │  │  ⋮
      │  │  │
      │  │  └─ MethodInstance for Pkg.Resolve.maxsum(::Graph)
      │  │     ⋮
      │  │
      │  └─ MethodInstance for Pkg.Resolve.maxsum(::Graph)
      │     └─ MethodInstance for Pkg.Resolve.resolve(::Graph)
      │        ⋮
      │
      └─ MethodInstance for Pkg.Resolve.maxsum(::Graph)
         └─ MethodInstance for Pkg.Resolve.resolve(::Graph)
            ├─ MethodInstance for Pkg.Resolve.trigger_failure!(::Graph, ::Vector{Int64}, ::Tuple{Int64, Int64})
            │  ⋮
            │
            └─ MethodInstance for Pkg.Operations.resolve_versions!(::Context, ::Vector{PackageSpec})
               ⋮
```

### Finding the callers of a method

To find already-compiled callers of `sum(::Vector{Int})`

```julia
# Collect all MethodInstances
mis = methodinstances();
# Create a function that returns `true` for the correct set of argument types
argmatch(argtyps) = length(argtyps) == 1 && argtyps[1] === Vector{Int}
# Find the calls that match
findcallers(sum, argmatch, mis)
```

There are more options, see the help for `findcallers`.

## API reference

### visit

```@docs
visit
visit_backedges
```

### backedges

```@docs
all_backedges
direct_backedges
terminal_backedges
with_all_backedges
```

### utilities

```@docs
methodinstance
methodinstances
call_type
findcallers
worlds
```

### types

```@docs
MethodAnalysis.CallMatch
```
