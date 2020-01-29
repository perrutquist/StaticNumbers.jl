using Documenter
using StaticNumbers

makedocs(
    sitename = "StaticNumbers",
    format = Documenter.HTML(),
    modules = [StaticNumbers],
    pages = [
        "index.md"
        "reference.md"
        "example.md"
    ]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
deploydocs(
    repo = "git@github.com:perrutquist/StaticNumbers.jl.git"
)
