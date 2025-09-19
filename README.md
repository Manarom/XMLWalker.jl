

[![Build Status](https://github.com/your-GitHub-username/XMLWalker.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/Manarom/XMLWalker.jl/actions/workflows/CI.yml?query=branch%3Amain)

[![dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://manarom.github.io/XMLWalker.jl)

# XMLWalker.jl

After loading the XML file using `XML.jl`, all data is stored in an encapsulated, tree-like structure of `Nodes`. Each node element has a children field, which is a vector of child `Nodes`, as well as other fields like `attributes`, `value` etc. `XMLWalker.jl` provides tools for traversing the XML tree using a set of matchers that check the content of nodes. Matchers can be created from strings and combined into chain strings, forming a sequence of matchers to find nodes that meet multiple content restrictions. The function `XMLWalker.find_nodes` converts input string to machers, recursively applies these matchers and returns a vector of nodes that satisfy the specified conditions.

# Quick start
```julia
import Pkg
Pkg.add("XMLWalker.jl")

import XMLWalker
import XML

xml_file = XML.read(xml_file,Node) #  loads data as a structure with multiple embedded nodes
matched_nodes = XMLWalker.find_nodes(starting_node,"A/[B,C]/D") # finds nodes, matching the 
# specified path (chain) or single matching "A"=>"B"=>"D" or "A"=>"C"=>"D" route 
```
Search string can contain various elements like regular expressions, partial matching patterns, patterns unions and intersections, searching nodes by content  etc. 

More complicated search examples:
```julia
    XMLWalker.find_nodes(starting_node,"*/[[\\d]::regex].attributes({id,p1})/*/D") 
    # will search for all subnodes of a starting_node that has:
    # - has field `tag` containing digits, 
    # - field `attributes` with both "id" and "p1" keys
    # - any subnode
    # - sub-subnode with field tag equal  to "D"
    XMLWalker.find_nodes(starting_node,"[ABC,BBB].parameters=[A,B,C]") # searches for the nodes that has "ABC" or "BBB" tag
    # and "A","B" or "C" values of `parameters` field
    XMLWalker.find_nodes(starting_node,"*.attributes({id,*par})",:Name) # searches for nodes with any Name field, but with 
    # attributes field, which must have key "id" and any key which contains "par" as a substring
```
It is also possible to create a matcher objects from string using `@to_matcher_str` macro. 
This macro can be usefull for multiple searches of the same pattern.

```julia
    using XMLWalker
    matcher_object =XMLWalker.to_matcher"[[\\d]::regex].attributes({id,p1})" # returns matchers objects vector
    XMLWalker.find_nodes(starting_node1 , matcher_object[])
    XMLWalker.find_nodes(starting_node2 , matcher_object[])
```
Full search string specification and package API are availabel at   [documentation page](https://manarom.github.io/XMLWalker.jl/).

