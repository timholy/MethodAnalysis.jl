# MethodAnalysis.jl

This package facilitates introspection of Julia's internals, with a particular focus on its MethodInstances and their backedges.

!!! warning
    Julia's internals are not subject to the same stability guarantee that the rest of the language enjoys.

## Demonstrations

A few demonstrations will give you a taste of what can be done with this package.

### Collecting all submodules of Base

```jldoctest
julia> using MethodAnalysis

julia> mods = Module[];

julia> visit(Base; print=false) do obj
           if isa(obj, Module)
               push!(mods, obj)
           end
       end

julia> Base.FastMath ∈ mods
true
```

### Collecting all Methods in Core.Compiler

`visit` also descends into functions, methods, and MethodInstances:

```jldoctest; setup=:(using MethodAnalysis)
julia> meths = []
Any[]

julia> visit(Core.Compiler) do item    # without print=false it will display the modules it visits
           isa(item, Method) && push!(meths, item)
       end
Module Core.Compiler
Module Core.Compiler.CoreDocs
Module Core.Compiler.Iterators
Module Core.Compiler.Order
Module Core.Compiler.Sort
Module Core.Compiler.Sort.Float

julia> first(methods(Core.Compiler.typeinf_ext)) ∈ meths
true
```

### Getting a MethodInstance for a particular set of types

```jldoctest; setup=:(using MethodAnalysis)
julia> foo(::AbstractVector) = 1
foo (generic function with 1 method)

julia> instance(foo, (Vector{Int},))   # we haven't called it yet, so it's not compiled

julia> foo([1,2])
1

julia> instance(foo, (Vector{Int},))
MethodInstance for foo(::Array{Int64,1})
```

### Collecting a subset of MethodInstances for a particular function

Let's collect all single-argument compiled instances of `findfirst`:

```jldoctest findfirst; setup=:(using MethodAnalysis)
julia> mis = Core.MethodInstance[];

julia> visit(findfirst) do item
           isa(item, Core.MethodInstance) && length(Base.unwrap_unionall(item.specTypes).parameters) == 2 && push!(mis, item)
       end

julia> mis
1-element Array{Core.MethodInstance,1}:
 MethodInstance for findfirst(::BitArray{1})
```

We checked that the length was 2, rather than 1, because the first parameter is the function type itself:

```jldoctest findfirst
julia> mis[1].specTypes
Tuple{typeof(findfirst),BitArray{1}}
```

### Getting the backedges for a function

Let's see all the compiled instances of `Base.setdiff` and their immediate callers:

```jldoctest; setup=(using MethodAnalysis)
julia> direct_backedges(setdiff)
3-element Array{Any,1}:
 MethodInstance for setdiff(::Base.KeySet{Any,Dict{Any,Any}}, ::Base.KeySet{Any,Dict{Any,Any}}) => MethodInstance for keymap_merge(::Dict{Char,Any}, ::Dict{Any,Any})
 MethodInstance for setdiff(::Base.KeySet{Any,Dict{Any,Any}}, ::Base.KeySet{Any,Dict{Any,Any}}) => MethodInstance for keymap_merge(::Any, ::Dict{Any,Any})
                         MethodInstance for setdiff(::Array{Base.UUID,1}, ::Array{Base.UUID,1}) => MethodInstance for deps_graph(::Pkg.Types.Context, ::Dict{Base.UUID,String}, ::Dict{Base.UUID,Pkg.Types.VersionSpec}, ::Dict{Base.UUID,Pkg.Resolve.Fixed})
```

### Printing backedges as a tree

MethodAnalysis uses [AbstractTrees](https://github.com/JuliaCollections/AbstractTrees.jl) to display the complete set of backedges:

```jldoctest; setup=:(using MethodAnalysis)
julia> mi = instance(findfirst, (BitVector,))
MethodInstance for findfirst(::BitArray{1})

julia> MethodAnalysis.print_tree(mi)
MethodInstance for findfirst(::BitArray{1})
├─ MethodInstance for prune_graph!(::Graph)
│  └─ MethodInstance for #simplify_graph!#111(::Bool, ::typeof(simplify_graph!), ::Graph, ::Set{Int64})
│     └─ MethodInstance for simplify_graph!(::Graph, ::Set{Int64})
│        └─ MethodInstance for simplify_graph!(::Graph)
│           ├─ MethodInstance for trigger_failure!(::Graph, ::Array{Int64,1}, ::Tuple{Int64,Int64})
│           │  ⋮
│           │  
│           └─ MethodInstance for resolve_versions!(::Context, ::Array{PackageSpec,1})
│              ⋮
│              
└─ MethodInstance for update_solution!(::SolutionTrace, ::Graph)
   └─ MethodInstance for converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)
      ├─ MethodInstance for converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)
      │  ├─ MethodInstance for converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)
      │  │  ├─ MethodInstance for converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)
      │  │  │  ⋮
      │  │  │  
      │  │  └─ MethodInstance for maxsum(::Graph)
      │  │     ⋮
      │  │     
      │  └─ MethodInstance for maxsum(::Graph)
      │     └─ MethodInstance for resolve(::Graph)
      │        ⋮
      │        
      └─ MethodInstance for maxsum(::Graph)
         └─ MethodInstance for resolve(::Graph)
            ├─ MethodInstance for trigger_failure!(::Graph, ::Array{Int64,1}, ::Tuple{Int64,Int64})
            │  ⋮
            │  
            └─ MethodInstance for resolve_versions!(::Context, ::Array{PackageSpec,1})
               ⋮
```

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
instance
call_type
worlds
```
