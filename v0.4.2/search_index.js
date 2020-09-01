var documenterSearchIndex = {"docs":
[{"location":"#MethodAnalysis.jl","page":"Home","title":"MethodAnalysis.jl","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"This package facilitates introspection of Julia's internals, with a particular focus on its MethodInstances and their backedges.","category":"page"},{"location":"","page":"Home","title":"Home","text":"warning: Warning\nJulia's internals are not subject to the same interface compatibility guarantee that the rest of the language enjoys.","category":"page"},{"location":"#Demonstrations","page":"Home","title":"Demonstrations","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"A few demonstrations will give you a taste of what can be done with this package.","category":"page"},{"location":"#Collecting-all-submodules-of-Base","page":"Home","title":"Collecting all submodules of Base","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"julia> using MethodAnalysis\n\njulia> mods = Module[];\n\njulia> visit(Base) do obj\n           if isa(obj, Module)\n               push!(mods, obj)\n               return true     # descend into submodules\n           end\n           false   # but don't descend into anything else (MethodTables, etc.)\n       end\n\njulia> Base.FastMath ∈ mods\ntrue","category":"page"},{"location":"#Collecting-all-Methods-in-Core.Compiler","page":"Home","title":"Collecting all Methods in Core.Compiler","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"visit also descends into functions, methods, and MethodInstances:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> meths = []\nAny[]\n\njulia> visit(Core.Compiler) do item\n           isa(item, Method) && push!(meths, item)\n           true   # walk through everything\n       end\n\njulia> first(methods(Core.Compiler.typeinf_ext)) ∈ meths\ntrue","category":"page"},{"location":"#Getting-a-MethodInstance-for-a-particular-set-of-types","page":"Home","title":"Getting a MethodInstance for a particular set of types","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"julia> foo(::AbstractVector) = 1\nfoo (generic function with 1 method)\n\njulia> methodinstance(foo, (Vector{Int},))   # we haven't called it yet, so it's not compiled\nMethodInstance for foo(::Vector{Int64})\n\njulia> foo([1,2])\n1\n\njulia> methodinstance(foo, (Vector{Int},))\nMethodInstance for foo(::Vector{Int64})","category":"page"},{"location":"#Collecting-a-subset-of-MethodInstances-for-a-particular-function","page":"Home","title":"Collecting a subset of MethodInstances for a particular function","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Let's collect all single-argument compiled instances of findfirst:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> mis = Core.MethodInstance[];\n\njulia> visit(findfirst) do item\n           isa(item, Core.MethodInstance) && length(Base.unwrap_unionall(item.specTypes).parameters) == 2 && push!(mis, item)\n           true\n       end\n\njulia> mis\n1-element Vector{Core.MethodInstance}:\n MethodInstance for findfirst(::BitVector)","category":"page"},{"location":"","page":"Home","title":"Home","text":"We checked that the length was 2, rather than 1, because the first parameter is the function type itself:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> mis[1].specTypes\nTuple{typeof(findfirst),BitVector}","category":"page"},{"location":"","page":"Home","title":"Home","text":"There's also a convenience shortcut:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> mis = methodinstances(findfirst)","category":"page"},{"location":"#Getting-the-backedges-for-a-function","page":"Home","title":"Getting the backedges for a function","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Let's see all the compiled instances of Base.setdiff and their immediate callers:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> direct_backedges(setdiff)\n3-element Vector{Any}:\n MethodInstance for setdiff(::Base.KeySet{Any,Dict{Any,Any}}, ::Base.KeySet{Any,Dict{Any,Any}}) => MethodInstance for keymap_merge(::Dict{Char,Any}, ::Dict{Any,Any})\n MethodInstance for setdiff(::Base.KeySet{Any,Dict{Any,Any}}, ::Base.KeySet{Any,Dict{Any,Any}}) => MethodInstance for keymap_merge(::Any, ::Dict{Any,Any})\n                           MethodInstance for setdiff(::Vector{Base.UUID}, ::Vector{Base.UUID}) => MethodInstance for deps_graph(::Pkg.Types.Context, ::Dict{Base.UUID,String}, ::Dict{Base.UUID,Pkg.Types.VersionSpec}, ::Dict{Base.UUID,Pkg.Resolve.Fixed})","category":"page"},{"location":"#Printing-backedges-as-a-tree","page":"Home","title":"Printing backedges as a tree","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"MethodAnalysis uses AbstractTrees to display the complete set of backedges:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> mi = methodinstance(findfirst, (BitVector,))\nMethodInstance for findfirst(::BitVector)\n\njulia> MethodAnalysis.print_tree(mi)\nMethodInstance for findfirst(::BitVector)\n├─ MethodInstance for prune_graph!(::Graph)\n│  └─ MethodInstance for #simplify_graph!#111(::Bool, ::typeof(simplify_graph!), ::Graph, ::Set{Int64})\n│     └─ MethodInstance for simplify_graph!(::Graph, ::Set{Int64})\n│        └─ MethodInstance for simplify_graph!(::Graph)\n│           └─ MethodInstance for resolve_versions!(::Context, ::Vector{PackageSpec})\n│              ⋮\n│\n└─ MethodInstance for update_solution!(::SolutionTrace, ::Graph)\n   └─ MethodInstance for converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)\n      ├─ MethodInstance for converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)\n      │  ├─ MethodInstance for converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)\n      │  │  ├─ MethodInstance for converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)\n      │  │  │  ⋮\n      │  │  │\n      │  │  └─ MethodInstance for maxsum(::Graph)\n      │  │     ⋮\n      │  │\n      │  └─ MethodInstance for maxsum(::Graph)\n      │     └─ MethodInstance for resolve(::Graph)\n      │        ⋮\n      │\n      └─ MethodInstance for maxsum(::Graph)\n         └─ MethodInstance for resolve(::Graph)\n            ├─ MethodInstance for trigger_failure!(::Graph, ::Vector{Int64}, ::Tuple{Int64,Int64})\n            │  ⋮\n            │\n            └─ MethodInstance for resolve_versions!(::Context, ::Vector{PackageSpec})\n               ⋮","category":"page"},{"location":"#Finding-the-callers-of-a-method","page":"Home","title":"Finding the callers of a method","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"To find already-compiled callers of sum(::Vector{Int})","category":"page"},{"location":"","page":"Home","title":"Home","text":"# Collect all MethodInstances\nmis = methodinstances();\n# Create a function that returns `true` for the correct set of argument types\nargmatch(argtyps) = length(argtyps) == 1 && argtyps[1] === Vector{Int}\n# Find the calls that match\nfindcallers(sum, argmatch, mis)","category":"page"},{"location":"","page":"Home","title":"Home","text":"There are more options, see the help for findcallers.","category":"page"},{"location":"#API-reference","page":"Home","title":"API reference","text":"","category":"section"},{"location":"#visit","page":"Home","title":"visit","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"visit\nvisit_backedges","category":"page"},{"location":"#MethodAnalysis.visit","page":"Home","title":"MethodAnalysis.visit","text":"visit(operation, obj; print::Bool=false)\n\nScan obj and all of its \"sub-objects\" (e.g., functions if obj::Module, methods if obj::Function, etc.) recursively. operation(x) should return true if visit should descend into \"sub-objects\" of x.\n\nIf print is true, each visited object is printed to standard output.\n\nExample\n\nTo collect all MethodInstances of a function,\n\njulia> mis = Core.MethodInstance[];\n\njulia> visit(findfirst) do x\n           if isa(x, Core.MethodInstance)\n               push!(mis, x)\n               return false\n           end\n           true\n       end\n\njulia> length(mis)\n34\n\nThe exact number of MethodInstances will depend on what code you've run in your Julia session.\n\n\n\n\n\nvisit(operation; print::Bool=false)\n\nScan all loaded modules with operation. See visit(operation, obj) for further detail.\n\nExample\n\nCollect all loaded modules, even if they are internal.\n\njulia> mods = Module[];\n\njulia> visit() do x\n           if isa(x, Module)\n               push!(mods, x)\n               return true\n           end\n           false\n       end\n\njulia> mods 113-element Array{Module,1}:  Random  Random.DSFMT [...]\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.visit_backedges","page":"Home","title":"MethodAnalysis.visit_backedges","text":"visit_backedges(operation, obj)\n\nVisit the backedges of obj and apply operation to each. operation may need to be able to handle two call forms, operation(mi) and operation(sig=>mi), where mi is a MethodInstance and sig is a Tuple-type. The latter arises from MethodTable backedges and can be ignored if obj does not contain MethodTables.\n\noperation(edge) should return true if the backedges of edge should in turn be visited, false otherwise. However, no MethodInstance will be visited more than once.\n\nThe set of visited objects includes obj itself. For example, visit_backedges(operation, f::Function) will visit all methods of f, and this in turn will visit all MethodInstances of these methods.\n\n\n\n\n\n","category":"function"},{"location":"#backedges","page":"Home","title":"backedges","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"all_backedges\ndirect_backedges\nterminal_backedges\nwith_all_backedges","category":"page"},{"location":"#MethodAnalysis.all_backedges","page":"Home","title":"MethodAnalysis.all_backedges","text":"all_backedges(mi::MethodInstance)\n\nReturn a list of all backedges (direct and indirect) of mi.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.direct_backedges","page":"Home","title":"MethodAnalysis.direct_backedges","text":"direct_backedges(f::Function; skip=true)\n\nCollect all backedges for a function f as pairs instance=>caller or sig=>caller pairs. The latter occur for MethodTable backedges. If skip is true, any caller listed in a MethodTable backedge is omitted from the instance backedges. \n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.terminal_backedges","page":"Home","title":"MethodAnalysis.terminal_backedges","text":"terminal_backedges(mi::MethodInstance)\n\nObtain the \"ultimate callers\" of mi, i.e., the reason(s) mi was compiled.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.with_all_backedges","page":"Home","title":"MethodAnalysis.with_all_backedges","text":"with_all_backedges(itr)\n\nReturn all MethodInstances detected when iterating through items in itr and any their backedges. The result includes both MethodTable and MethodInstance backedges.\n\n\n\n\n\n","category":"function"},{"location":"#utilities","page":"Home","title":"utilities","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"methodinstance\nmethodinstances\ncall_type\nfindcallers\nworlds","category":"page"},{"location":"#MethodAnalysis.methodinstance","page":"Home","title":"MethodAnalysis.methodinstance","text":"mi = methodinstance(f, types)\nmi = methodinstance(tt::Type{<:Tuple})\n\nReturn the MethodInstance mi for function f and the given types, or for the complete signature tt. If no version compiled for these types exists, returns nothing.\n\nExamples\n\njulia> f(x, y::String) = 2x; f(x, y::Number) = x + y;\n\njulia> f(1, \"hi\"); f(1, 1.0);\n\njulia> methodinstance(f, (Int, String))\nMethodInstance for f(::Int64, ::String)\n\njulia> methodinstance(Tuple{typeof(f), Int, String})\nMethodInstance for f(::Int64, ::String)\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.methodinstances","page":"Home","title":"MethodAnalysis.methodinstances","text":"methodinstances()\nmethodinstances(mod::Module)\nmethodinstances(f)\n\nCollect all MethodInstances, optionally restricting them to a particular module, function, method, or methodlist.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.call_type","page":"Home","title":"MethodAnalysis.call_type","text":"call_type(tt)\n\nSplit a signature type like Tuple{typeof(f),ArgTypes...} back out to (f, Tuple{ArgTypes...})\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.findcallers","page":"Home","title":"MethodAnalysis.findcallers","text":"callers = findcallers(f, argmatch::Union{Nothing,Function}, mis; callhead=:call | :iterate)\n\nFind \"static\" callers of a function f with inferred argument types for which argmatch(types) returns true. Optionally pass nothing for argmatch to allow any calls to f. mis is the list of MethodInstances you want to check, for example obtained from methodinstances.\n\ncallhead controls whether you're looking for an ordinary (:call) or a splatted (varargs) call (:iterate).\n\ncallers is a vector of tuples t, where t[1] is the MethodInstance, t[2] is the corresponding CodeInfo, t[3] is the statement number on which the call occurs, and t[4] holds the inferred argument types.\n\nExamples\n\n```julia f(x) = rand() function g()     a = f(1.0)     z = Any[7]     b = f(z[1]::Integer)     c = f(z...)     return nothing end\n\ng() mis = union(methodinstances(f), methodinstances(g))\n\ncallhead = :call is the default\n\ncallers1 = findcallers(f, argtyps->length(argtyps) == 1 && argtyps[1] === Float64, mis)\n\nGet the partial-inference case with known number of arguments (this is definitely :call)\n\ncallers2 = findcallers(f, argtyps->length(argtyps) == 1 && argtyps[1] === Integer, mis; callhead=:call)\n\nGet the splat call\n\ncallers3 = findcallers(f, argtyps->length(argtyps) == 1 && argtyps[1] === Vector{Any}, mis; callhead=:iterate)\n\ncompat: Compat\nfindcallers is available on Julia 1.6 and higher\n\nwarn: Warn\nfindcallers is not guaranteed to find all calls. Calls can be \"obfuscated\" by many mechanisms, including calls from top level, calls where the function is a runtime variable, etc.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.worlds","page":"Home","title":"MethodAnalysis.worlds","text":"minmaxs = worlds(mi::MethodInstance)\n\nCollect the (min,max) world-age pairs for all CodeInstances associated with mi.\n\n\n\n\n\n","category":"function"}]
}
