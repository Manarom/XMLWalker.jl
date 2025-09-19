push!(LOAD_PATH,"../src/")
using Documenter, XMLWalker
mathengine = Documenter.MathJax3()
makedocs(
        sitename = "XMLWalker.jl",
        repo="https://github.com/Manarom/XMLWalker.jl/blob/{commit}{path}#{line}",
        highlightsig = false,
        checkdocs = :none,
        format=Documenter.HTML(size_threshold = 2000 * 2^10),
        pages=[
                "Home" => "index.md",
                "API" =>"API.md"
                ]
)
deploydocs(;
                repo="https://github.com/Manarom/XMLWalker.jl/blob/{commit}{path}#{line}", 
                devbranch = "main",
                devurl="dev",
                target = "build",
                branch = "gh-pages",
                versions = ["stable" => "v^", "v#.#" ]
        )