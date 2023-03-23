var documenterSearchIndex = {"docs":
[{"location":"#MethodAnalysis.jl","page":"Home","title":"MethodAnalysis.jl","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"This package facilitates introspection of Julia's internals, with a particular focus on its MethodInstances and their backedges.","category":"page"},{"location":"","page":"Home","title":"Home","text":"warning: Warning\nJulia's internals are not subject to the same interface compatibility guarantee that the rest of the language enjoys.","category":"page"},{"location":"#Demonstrations","page":"Home","title":"Demonstrations","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"A few demonstrations will give you a taste of what can be done with this package.","category":"page"},{"location":"#Collecting-all-submodules-of-Base","page":"Home","title":"Collecting all submodules of Base","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"julia> using MethodAnalysis\n\njulia> mods = Module[];\n\njulia> visit(Base) do obj\n           if isa(obj, Module)\n               push!(mods, obj)\n               return true     # descend into submodules\n           end\n           false   # but don't descend into anything else (MethodTables, etc.)\n       end\n\njulia> Base.FastMath ∈ mods\ntrue","category":"page"},{"location":"","page":"Home","title":"Home","text":"You can do this more easily with the convenience utility child_modules.","category":"page"},{"location":"#Collecting-all-Methods-of-functions-defined-in-Core.Compiler","page":"Home","title":"Collecting all Methods of functions defined in Core.Compiler","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"visit also descends into functions, methods, and MethodInstances:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> meths = []\nAny[]\n\njulia> visit(Core.Compiler) do item\n           isa(item, Method) && push!(meths, item)\n           true   # walk through everything\n       end\n\njulia> first(methods(Core.Compiler.typeinf_ext)) ∈ meths\ntrue","category":"page"},{"location":"","page":"Home","title":"Home","text":"note: Note\nMethods are found by visiting the function. This has an important consequence: if PkgB defines a new method for PkgA.f, you won't find that method by visiting PkgB: you have to visit PkgA.f (which you can find by visiting PkgA). This is a consequence of how Julia stores Methods, not a limitation of MethodAnalysis.Thus, to find all methods defined in PkgB, you have to traverse the entire system (visit() do ... end), and check the meth.module field of every Method to determine which module created it.For methods that accept keyword arguments, Julia creates \"hidden\" methods for filling in the default values. Prior to Julia 1.9, you could find these by visiting the module that owns the \"parent\" function. On Julia 1.9 and above, these instead get added as methods of Core.kwcall. Consequently, these methods cannot be found by visiting the module that owns the parent function.","category":"page"},{"location":"#Getting-a-MethodInstance-for-a-particular-set-of-types","page":"Home","title":"Getting a MethodInstance for a particular set of types","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"julia> foo(::AbstractVector) = 1\nfoo (generic function with 1 method)\n\njulia> methodinstance(foo, (Vector{Int},))   # we haven't called it yet, so it's not compiled\n\n\njulia> foo([1,2])\n1\n\njulia> methodinstance(foo, (Vector{Int},))\nMethodInstance for foo(::Vector{Int64})","category":"page"},{"location":"#Collecting-a-subset-of-MethodInstances-for-a-particular-function","page":"Home","title":"Collecting a subset of MethodInstances for a particular function","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Let's collect all single-argument compiled instances of findfirst:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> mis = Core.MethodInstance[];\n\njulia> visit(findfirst) do item\n           isa(item, Core.MethodInstance) && length(Base.unwrap_unionall(item.specTypes).parameters) == 2 && push!(mis, item)\n           true\n       end\n\njulia> mis\n2-element Vector{Core.MethodInstance}:\n MethodInstance for findfirst(::BitVector)\n MethodInstance for findfirst(::Vector{Bool})","category":"page"},{"location":"","page":"Home","title":"Home","text":"We checked that the length was 2, rather than 1, because the first parameter is the function type itself:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> mis[1].specTypes\nTuple{typeof(findfirst), BitVector}","category":"page"},{"location":"","page":"Home","title":"Home","text":"There's also a convenience shortcut:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> mis = methodinstances(findfirst)","category":"page"},{"location":"#Getting-the-backedges-for-a-function","page":"Home","title":"Getting the backedges for a function","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Let's see all the compiled instances of Base.setdiff and their immediate callers:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> direct_backedges(setdiff)\n7-element Vector{Any}:\n     MethodInstance for setdiff(::Base.KeySet{Any, Dict{Any, Any}}, ::Base.KeySet{Any, Dict{Any, Any}}) => MethodInstance for REPL.LineEdit.keymap_merge(::Dict{Char, Any}, ::Dict{Any, Any})\n     MethodInstance for setdiff(::Base.KeySet{Any, Dict{Any, Any}}, ::Base.KeySet{Any, Dict{Any, Any}}) => MethodInstance for REPL.LineEdit.keymap_merge(::Dict{Char, Any}, ::Union{Dict{Any, Any}, Dict{Char, Any}})\n   MethodInstance for setdiff(::Base.KeySet{Char, Dict{Char, Any}}, ::Base.KeySet{Any, Dict{Any, Any}}) => MethodInstance for REPL.LineEdit.keymap_merge(::Dict{Char, Any}, ::Union{Dict{Any, Any}, Dict{Char, Any}})\n   MethodInstance for setdiff(::Base.KeySet{Any, Dict{Any, Any}}, ::Base.KeySet{Char, Dict{Char, Any}}) => MethodInstance for REPL.LineEdit.keymap_merge(::Dict{Char, Any}, ::Union{Dict{Any, Any}, Dict{Char, Any}})\n MethodInstance for setdiff(::Base.KeySet{Char, Dict{Char, Any}}, ::Base.KeySet{Char, Dict{Char, Any}}) => MethodInstance for REPL.LineEdit.keymap_merge(::Dict{Char, Any}, ::Union{Dict{Any, Any}, Dict{Char, Any}})\n                                         MethodInstance for setdiff(::Set{Base.UUID}, ::Set{Base.UUID}) => MethodInstance for Pkg.Operations.deps_graph(::Pkg.Types.EnvCache, ::Vector{Pkg.Registry.RegistryInstance}, ::Dict{Base.UUID, String}, ::Dict{Base.UUID, Pkg.Versions.VersionSpec}, ::Dict{Base.UUID, Pkg.Resolve.Fixed}, ::Nothing)\n                                         MethodInstance for setdiff(::Set{Base.UUID}, ::Set{Base.UUID}) => MethodInstance for Pkg.Operations.deps_graph(::Pkg.Types.EnvCache, ::Vector{Pkg.Registry.RegistryInstance}, ::Dict{Base.UUID, String}, ::Dict{Base.UUID, Pkg.Versions.VersionSpec}, ::Dict{Base.UUID, Pkg.Resolve.Fixed}, ::VersionNumber)","category":"page"},{"location":"#Printing-backedges-as-a-tree","page":"Home","title":"Printing backedges as a tree","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"MethodAnalysis uses AbstractTrees to display the complete set of backedges:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> mi = methodinstance(findfirst, (BitVector,))\nMethodInstance for findfirst(::BitVector)\n\njulia> MethodAnalysis.print_tree(mi)\nMethodInstance for findfirst(::BitVector)\n├─ MethodInstance for Pkg.Resolve.prune_graph!(::Graph)\n│  └─ MethodInstance for Pkg.Resolve.var\"#simplify_graph!#117\"(::Bool, ::typeof(simplify_graph!), ::Graph, ::Set{Int64})\n│     └─ MethodInstance for Pkg.Resolve.simplify_graph!(::Graph, ::Set{Int64})\n│        └─ MethodInstance for Pkg.Resolve.simplify_graph!(::Graph)\n│           ├─ MethodInstance for Pkg.Resolve.trigger_failure!(::Graph, ::Vector{Int64}, ::Tuple{Int64, Int64})\n│           │  ⋮\n│           │\n│           ├─ MethodInstance for Pkg.Operations.resolve_versions!(::EnvCache, ::Vector{RegistryInstance}, ::Vector{PackageSpec}, ::Nothing)\n│           │  ⋮\n│           │\n│           └─ MethodInstance for Pkg.Operations.resolve_versions!(::EnvCache, ::Vector{RegistryInstance}, ::Vector{PackageSpec}, ::VersionNumber)\n│              ⋮\n│\n├─ MethodInstance for Pkg.Resolve.update_solution!(::SolutionTrace, ::Graph)\n│  └─ MethodInstance for Pkg.Resolve.converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)\n│     ├─ MethodInstance for Pkg.Resolve.converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)\n│     │  ├─ MethodInstance for Pkg.Resolve.converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)\n│     │  │  ├─ MethodInstance for Pkg.Resolve.converge!(::Graph, ::Messages, ::SolutionTrace, ::NodePerm, ::MaxSumParams)\n│     │  │  │  ⋮\n│     │  │  │\n│     │  │  └─ MethodInstance for Pkg.Resolve.maxsum(::Graph)\n│     │  │     ⋮\n│     │  │\n│     │  └─ MethodInstance for Pkg.Resolve.maxsum(::Graph)\n│     │     ├─ MethodInstance for Pkg.Resolve._resolve(::Graph, ::Array, ::Array)\n│     │     │  ⋮\n│     │     │\n│     │     ├─ MethodInstance for Pkg.Resolve._resolve(::Graph, ::Nothing, ::Nothing)\n│     │     │  ⋮\n│     │     │\n│     │     └─ MethodInstance for Pkg.Resolve._resolve(::Graph, ::Vector{Int64}, ::Vector{Int64})\n│     │        ⋮\n│     │\n│     └─ MethodInstance for Pkg.Resolve.maxsum(::Graph)\n│        ├─ MethodInstance for Pkg.Resolve._resolve(::Graph, ::Array, ::Array)\n│        │  ├─ MethodInstance for Pkg.Resolve._resolve(::Graph, ::Array, ::Array)\n│        │  │  ⋮\n│        │  │\n│        │  └─ MethodInstance for Pkg.Resolve._resolve(::Graph, ::Nothing, ::Nothing)\n│        │     ⋮\n│        │\n│        ├─ MethodInstance for Pkg.Resolve._resolve(::Graph, ::Nothing, ::Nothing)\n│        │  └─ MethodInstance for Pkg.Resolve.resolve(::Graph)\n│        │     ⋮\n│        │\n│        └─ MethodInstance for Pkg.Resolve._resolve(::Graph, ::Vector{Int64}, ::Vector{Int64})\n│           ├─ MethodInstance for Pkg.Resolve._resolve(::Graph, ::Vector{Int64}, ::Vector{Int64})\n│           │  ⋮\n│           │\n│           └─ MethodInstance for Pkg.Resolve._resolve(::Graph, ::Nothing, ::Nothing)\n│              ⋮\n│\n└─ MethodInstance for LoweredCodeUtils.selective_eval_fromstart!(::typeof(finish_and_return!), ::Frame, ::BitVector, ::Bool)\n   └─ MethodInstance for LoweredCodeUtils.selective_eval_fromstart!(::Frame, ::BitVector, ::Bool)","category":"page"},{"location":"#Finding-the-callers-of-a-method","page":"Home","title":"Finding the callers of a method","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"To find already-compiled callers of sum(::Vector{Int})","category":"page"},{"location":"","page":"Home","title":"Home","text":"# Collect all MethodInstances\nmis = methodinstances();\n# Create a function that returns `true` for the correct set of argument types\nargmatch(argtyps) = length(argtyps) == 1 && argtyps[1] === Vector{Int}\n# Find the calls that match\nfindcallers(sum, argmatch, mis)","category":"page"},{"location":"","page":"Home","title":"Home","text":"There are more options, see the help for findcallers.","category":"page"},{"location":"#API-reference","page":"Home","title":"API reference","text":"","category":"section"},{"location":"#visit","page":"Home","title":"visit","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"visit\nvisit_backedges","category":"page"},{"location":"#MethodAnalysis.visit","page":"Home","title":"MethodAnalysis.visit","text":"visit(operation, obj; print::Bool=false)\n\nScan obj and all of its \"sub-objects\" (e.g., functions if obj::Module, methods if obj::Function, etc.) recursively. operation(x) should return true if visit should descend into \"sub-objects\" of x.\n\nIf print is true, each visited object is printed to standard output.\n\nExample\n\nTo collect all MethodInstances of a function,\n\njulia> mis = Core.MethodInstance[];\n\njulia> visit(findfirst) do x\n           if isa(x, Core.MethodInstance)\n               push!(mis, x)\n               return false\n           end\n           true\n       end\n\njulia> length(mis)\n34\n\nThe exact number of MethodInstances will depend on what code you've run in your Julia session.\n\n\n\n\n\nvisit(operation; print::Bool=false)\n\nScan all loaded modules with operation. See visit(operation, obj) for further detail.\n\nExample\n\nCollect all loaded modules, even if they are internal.\n\njulia> mods = Module[];\n\njulia> visit() do x\n           if isa(x, Module)\n               push!(mods, x)\n               return true\n           end\n           false\n       end\n\njulia> mods 113-element Array{Module,1}:  Random  Random.DSFMT [...]\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.visit_backedges","page":"Home","title":"MethodAnalysis.visit_backedges","text":"visit_backedges(operation, obj)\n\nVisit the backedges of obj and apply operation to each. operation may need to be able to handle two call forms, operation(mi) and operation(sig=>mi), where mi is a MethodInstance and sig is a Tuple-type. The latter arises from either invoke calls or MethodTable backedges.\n\noperation(edge) should return true if the backedges of edge should in turn be visited, false otherwise. However, no MethodInstance will be visited more than once.\n\nThe set of visited objects includes obj itself. For example, visit_backedges(operation, f::Function) will visit all methods of f, and this in turn will visit all MethodInstances of these methods.\n\n\n\n\n\n","category":"function"},{"location":"#backedges","page":"Home","title":"backedges","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"all_backedges\ndirect_backedges\nterminal_backedges\nwith_all_backedges","category":"page"},{"location":"#MethodAnalysis.all_backedges","page":"Home","title":"MethodAnalysis.all_backedges","text":"all_backedges(mi::MethodInstance)\n\nReturn a list of all backedges (direct and indirect) of mi.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.direct_backedges","page":"Home","title":"MethodAnalysis.direct_backedges","text":"direct_backedges(f::Function; skip=true)\n\nCollect all backedges for a function f as pairs instance=>caller or sig=>caller pairs. The latter occur for MethodTable backedges. If skip is true, any caller listed in a MethodTable backedge is omitted from the instance backedges.\n\n\n\n\n\ndirect_backedges(mi::MethodInstance)\n\nA vector of all direct backedges of mi. This is equivalent to mi.backedges except that it's \"safe,\" meaning it returns an empty list even when mi.backedges is not defined.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.terminal_backedges","page":"Home","title":"MethodAnalysis.terminal_backedges","text":"terminal_backedges(mi::MethodInstance)\n\nObtain the \"ultimate callers\" of mi, i.e., the reason(s) mi was compiled.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.with_all_backedges","page":"Home","title":"MethodAnalysis.with_all_backedges","text":"with_all_backedges(itr)\n\nReturn all MethodInstances detected when iterating through items in itr and any their backedges. The result includes both MethodTable and MethodInstance backedges.\n\n\n\n\n\n","category":"function"},{"location":"#utilities","page":"Home","title":"utilities","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"methodinstance\nmethodinstances\nmethodinstances_owned_by\nchild_modules\ncall_type\nfindcallers\nhasbox\nworlds","category":"page"},{"location":"#MethodAnalysis.methodinstance","page":"Home","title":"MethodAnalysis.methodinstance","text":"mi = methodinstance(f, types)\nmi = methodinstance(tt::Type{<:Tuple})\n\nReturn the MethodInstance mi for function f and the given types, or for the complete signature tt. If no version compiled for these types exists, returns nothing.\n\nExamples\n\njulia> f(x, y::String) = 2x; f(x, y::Number) = x + y;\n\njulia> f(1, \"hi\"); f(1, 1.0);\n\njulia> methodinstance(f, (Int, String))\nMethodInstance for f(::Int64, ::String)\n\njulia> methodinstance(Tuple{typeof(f), Int, String})\nMethodInstance for f(::Int64, ::String)\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.methodinstances","page":"Home","title":"MethodAnalysis.methodinstances","text":"methodinstances()\nmethodinstances(top)\n\nCollect all MethodInstances, optionally restricting them to a particular module, function, method, or methodlist.\n\nExamples\n\njulia> sin(π/2)\n1.0\n\njulia> sin(0.8f0)\n0.7173561f0\n\njulia> methodinstances(sin)\n2-element Vector{Core.MethodInstance}:\n MethodInstance for sin(::Float64)\n MethodInstance for sin(::Float32)\n\njulia> m = which(convert, (Type{Bool}, Real))\nconvert(::Type{T}, x::Number) where T<:Number in Base at number.jl:7\n\njulia> methodinstances(m)\n68-element Vector{Core.MethodInstance}:\n MethodInstance for convert(::Type{UInt128}, ::Int64)\n MethodInstance for convert(::Type{Int128}, ::Int64)\n MethodInstance for convert(::Type{Int64}, ::Int32)\n MethodInstance for convert(::Type{UInt64}, ::Int64)\n ⋮\n\nNote the method m was broader than the signature we queried with, and the returned MethodInstances reflect that breadth. See methodinstances for a more restrictive subset, and methodinstances_owned_by for collecting MethodInstances owned by specific modules.\n\n\n\n\n\nmethodinstances(f, types)\nmethodinstances(tt::Type{<:Tuple})\n\nReturn all MethodInstances whose signature is a subtype of types.\n\nExample\n\njulia> methodinstances(convert, (Type{Bool}, Real))\n2-element Vector{Core.MethodInstance}:\n MethodInstance for convert(::Type{Bool}, ::Bool)\n MethodInstance for convert(::Type{Bool}, ::Int64)\n\nCompare this to the result from methodinstance.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.methodinstances_owned_by","page":"Home","title":"MethodAnalysis.methodinstances_owned_by","text":"mis = methodinstances_owned_by(mod::Module; include_child_modules::Bool=true, kwargs...)\n\nReturn a list of MethodInstances that are owned by mod. If include_child_modules is true, this includes sub-modules of mod, in which case kwargs are passed to child_modules.\n\nThe primary difference between methodinstances(mod) and methodinstances_owned_by(mod) is that the latter excludes MethodInstances that belong to re-exported dependent packages.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.child_modules","page":"Home","title":"MethodAnalysis.child_modules","text":"mods = child_modules(mod::Module; external::Bool=false)\n\nReturn a list that includes mod and all sub-modules of mod. By default, modules loaded from other sources (e.g., packages or those defined by Julia itself) are excluded, even if exported (or @reexported, see https://github.com/simonster/Reexport.jl), unless you set external=true.\n\nExamples\n\njulia> module Outer\n       module Inner\n       export Base\n       end\n       end\nMain.Outer\n\njulia> child_modules(Outer)\n2-element Vector{Module}:\n Main.Outer\n Main.Outer.Inner\n\njulia> child_modules(Outer.Inner)\n1-element Vector{Module}:\n Main.Outer.Inner\n\nExtended help\n\nIn the example above, because of the export Base, the following visit-based implementation would also collect Base and all of its sub-modules:\n\njulia> mods = Module[]\nModule[]\n\njulia> visit(Outer) do item\n           if item isa Module\n               push!(mods, item)\n               return true\n           end\n           return false\n       end\n\njulia> Base ∈ mods\ntrue\n\njulia> length(mods) > 20\ntrue\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.call_type","page":"Home","title":"MethodAnalysis.call_type","text":"call_type(tt)\n\nSplit a signature type like Tuple{typeof(f),ArgTypes...} back out to (f, Tuple{ArgTypes...})\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.findcallers","page":"Home","title":"MethodAnalysis.findcallers","text":"callers = findcallers(f, argmatch::Union{Nothing,Function}, mis; callhead=:call | :iterate)\n\nFind \"static\" callers of a function f with inferred argument types for which argmatch(types) returns true. Optionally pass nothing for argmatch to allow any calls to f. mis is the list of MethodInstances you want to check, for example obtained from methodinstances.\n\ncallhead controls whether you're looking for an ordinary (:call) or a splatted (varargs) call (:iterate).\n\ncallers is a vector of CallMatch objects.\n\nExamples\n\nf(x) = rand()\nfunction g()\n    a = f(1.0)\n    z = Any[7]\n    b = f(z[1]::Integer)\n    c = f(z...)\n    return nothing\nend\n\ng()\nmis = union(methodinstances(f), methodinstances(g))\n\n# callhead = :call is the default\ncallers1 = findcallers(f, argtyps->length(argtyps) == 1 && argtyps[1] === Float64, mis)\n\n# Get the partial-inference case with known number of arguments (this is definitely :call)\ncallers2 = findcallers(f, argtyps->length(argtyps) == 1 && argtyps[1] === Integer, mis; callhead=:call)\n\n# Get the splat call\ncallers3 = findcallers(f, argtyps->length(argtyps) == 1 && argtyps[1] === Vector{Any}, mis; callhead=:iterate)\n\ncompat: Compat\nfindcallers is available on Julia 1.6 and higher\n\nwarn: Warn\nfindcallers is not guaranteed to find all calls. Calls can be \"obfuscated\" by many mechanisms, including calls from top level, calls where the function is a runtime variable, etc.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.hasbox","page":"Home","title":"MethodAnalysis.hasbox","text":"hasbox(mi::MethodInstance)\n\nReturn true if the code for mi has a Core.Box. This often arises from a limitation in Julia's type-inference, see https://docs.julialang.org/en/v1/manual/performance-tips/#man-performance-captured.\n\n\n\n\n\n","category":"function"},{"location":"#MethodAnalysis.worlds","page":"Home","title":"MethodAnalysis.worlds","text":"minmaxs = worlds(mi::MethodInstance)\n\nCollect the (min,max) world-age pairs for all CodeInstances associated with mi.\n\n\n\n\n\n","category":"function"},{"location":"#types","page":"Home","title":"types","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"MethodAnalysis.CallMatch","category":"page"},{"location":"#MethodAnalysis.CallMatch","page":"Home","title":"MethodAnalysis.CallMatch","text":"CallMatch\n\nA structure to summarize a \"matching\" caller/callee pair. The fields are:\n\nmi: the MethodInstance for the caller\nsrc: its corresponding CodeInfo\nsparams: the type parameters for the caller, given mi's signature (alternatively use mi.sparam_vals)\nline: the statement number (in SSAValue sense) on which the call occurs\nargtypes: the caller's inferred types passed as arguments to the callee\n\n\n\n\n\n","category":"type"}]
}
