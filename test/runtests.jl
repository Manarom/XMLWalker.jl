
using Test
using OrderedCollections
push!(LOAD_PATH,"../src/")
using XMLWalker
pwd()
@testset "matchers_tests" begin
 
    @test !XMLWalker.MatchesPat("abc")("asdfg")
    @test XMLWalker.MatchesPat("abc")("abc")
    @test XMLWalker.MatchesPat(("aaa","bbb"))(("aaa","bbb"))
    @test XMLWalker.MatchesPat([ Regex(raw"(.)\1+"), Regex("b{2,}")])(("aaa","bbb")) # regex are also supported
    @test XMLWalker.MatchesPat(("aaa","bbb"))(("aaa","bbb"))
    @test XMLWalker.MatchesPat(["aaa","bbb"])(("aaa","bbb"))
    @test !XMLWalker.MatchesPat(["aaa","bbb"])(("ccc","bbb"))
    @test XMLWalker.MatchesPat(Dict("aaa"=>1,"bbb"=>2))(("aaa"=>1,"bbb"=>2))
    @test !XMLWalker.MatchesPat(Dict("aaa"=>1,"bbb"=>2))(("aaa"=>1,"bbb"=>3))

    struct A 
        tag
    end
    a1 = A("bbb")
    @test XMLWalker.MatchesPat("bbb",:tag)(a1)
    @test !XMLWalker.MatchesPat("ccc",:tag)(a1)


    a2 = A(Dict("a"=>1,"b"=>2)) # struct with field :tag
    @test (XMLWalker.MatchesPat(("a"=>1,"b"=>2),:tag)(a2))
    
    # ContainsPat  - true if the input contains the pattern
    @test XMLWalker.ContainsPat("ab")("abc")
    @test !XMLWalker.ContainsPat("ab")("acb")
    @test XMLWalker.ContainsPat(("aaa"=>1,"bbb"=>2))(("aaa"=>1,"bbb"=>2, "ccc"=>10))
    @test !XMLWalker.ContainsPat(("aaa"=>1,"bbb"=>999))(("aaa"=>1,"bbb"=>2, "ccc"=>10))
    @test XMLWalker.ContainsPat(Regex("b{2,}"))("bbb")
    @test !XMLWalker.ContainsPat(Regex("c{2,}"))(("aaa","bbb"))

    a2 = A("abba") # struct with field :tag
    @test XMLWalker.ContainsPat("ab",:tag)(a2)
    a2 = A(Dict("a"=>1,"b"=>2)) # struct with field :tag
    
    @test XMLWalker.ContainsPat(("a"=>1,"b"=>2),:tag)(a2)
    @test !XMLWalker.ContainsPat(("a"=>11,"b"=>2),:tag)(a2)
    @test XMLWalker.ContainsPat(Dict("a"=>1),:tag)(a2)
    @test XMLWalker.ContainsPat(r"a",:tag)(a2)

    # PatContains true if the pattern contains the input
    @test XMLWalker.PatContains("abc")("ab")
    @test !XMLWalker.PatContains("ab")("abc")
    @test XMLWalker.PatContains(("aaa"=>1,"bbb"=>999))(("aaa"=>1,))
    a2 = A(Dict("a"=>1,"b"=>2)) # struct with field :tag
    @test XMLWalker.PatContains(("a"=>1,"b"=>2,"c"=>10),:tag)(a2)
    @test XMLWalker.PatContains(("a"=>1,"b"=>2),:tag)(a2)
    @test !XMLWalker.PatContains(Dict("a"=>1),:tag)(a2)

    # ContainsAnyPat - true if tag contains any of the pattern content
    @test XMLWalker.ContainsAnyPat(["a" , "b" ,"c"])(["a"])
    @test XMLWalker.ContainsAnyPat(("a"=>1 , "a"=>2 ,"c"=>2))(("a"=>1,))
    a2 = A(("a"=>3, "a"=>2 ,"a"=> 5)) # struct with field :tag
    @test !XMLWalker.ContainsAnyPat(("a"=>1 ,),:tag)(a2)
    
     # HasAnyKeyPat - true if the input has at least element of pat as a key
    a2 = A(Dict("a"=>3, "b"=>2 ,"c"=> 5)) # struct with field :tag
    @test XMLWalker.HasAnyKeyPat(["a" ],:tag)(a2)
    @test !XMLWalker.HasAnyKeyPat(["f","d" ],:tag)(a2)

    # HasAllKeysPat - true if the inout has all elements of pat as keys
    a2 = A(Dict("a"=>3, "b"=>2 ,"c"=> 5)) # struct with field :tag
    @test XMLWalker.HasAllKeysPat(["a" ],:tag)(a2)
    @test !XMLWalker.HasAllKeysPat(["a","c","f" ],:tag)(a2)

    # AnyPat  - must match all patterns
    @test XMLWalker.AnyPat(["a" , "b" ,"c"])(["a"])
    @test XMLWalker.AnyPat(("a"=>1 , "a"=>2 ,"c"=>2))(("a"=>1,))
    @test XMLWalker.AnyPat(("a" => 1 ,),:tag)(a2)

    # testing node chains 
end

# test data for the next set 
AttrType = Union{OrderedDict,Nothing, String,Float64,Vector{String}}
mutable struct NodeImitation
    tag::String
    children::Union{Vector{NodeImitation},Nothing}
    attributes::AttrType
    NodeImitation(;tag::String,children=nothing,attributes::AttrType=nothing) = new(tag,children,attributes)
end

node11 = NodeImitation(tag="leaf1",attributes = OrderedDict("id"=>"p1","b"=>10, "d"=>10))
node12 = NodeImitation(tag="leaf2",attributes = OrderedDict("id"=>"p2","c"=>10 , "d"=>11))
node21 = NodeImitation(tag="leaf3",attributes = "2.34")
node22 = NodeImitation(tag="leaf4",attributes = 2.34)
node23 = NodeImitation(tag="leaf5",attributes = OrderedDict("id"=>"p2","c"=>10 , "d"=>11))
node1 = NodeImitation(tag="branch1",attributes = 2.0, children = [node11,node12])
node2 = NodeImitation(tag="branch2",attributes = "asff",children = [node21,node22])
root_node = NodeImitation(tag="root",children = [node1,node2])

@testset "single_matchers_from_string" begin

    m1 = XMLWalker.ContainsPat("leaf",:tag)
    m2 = XMLWalker.HasAnyKeyPat(["id"],:attributes)
    mat_set = XMLWalker.MatchersSet((m1,m2),:all)
    @test mat_set(node11)
    # testing find_nodes function using matchers
    m4 = XMLWalker.chain_string_token_to_matcher("branch1")
    @test m4(node1)
    m5 = XMLWalker.chain_string_token_to_matcher("[leaf4,leaf3,leaf5].attributes = 2.34")
    @test m5(node22)
    @test !m5(node21)
    m6 = XMLWalker.chain_string_token_to_matcher("[leaf1,leaf2].attributes({id, [bd]::regex})") 
    @test m6(node11)
    @test m6(node12)
    @test !m6(node21)
    m7 = XMLWalker.chain_string_token_to_matcher("[leaf1,leaf2].attributes([id])") 
    @test m7(node11)

    m7 = XMLWalker.chain_string_token_to_matcher("[leaf1,leaf2].attributes([id = p1])") 
    @test m7(node11)
    @test !m7(node12)

    # testing to matcher macro
    m8 = to_matcher"*leaf.attributes([id])/*/" 
    @test m8[1](node11)
    @test m8[2](root_node)

    m6 = XMLWalker.chain_string_token_to_matcher("*.attributes = 2.34::text") 
    @test m6(node21)
end
@testset "finding nodes by matchers"   begin
    @test node1 == find_nodes(root_node,XMLWalker.ContainsPat("branch",:tag))[1]
    @test node2 == find_nodes(root_node,XMLWalker.ContainsPat("branch",:tag))[2]
    @test node11 == find_nodes(node1,XMLWalker.PatContains("leaf1leaf2",:tag))[1]
    @test node11 == find_nodes(root_node,XMLWalker.ContainsPat("leaf",:tag))[1]
    @test node11 == find_nodes(root_node,XMLWalker.ContainsPat(("id"=>"p1",),:attributes))[]
    leaf_nodes_branches = find_nodes(root_node,XMLWalker.HasAnyKeyPat(("id",),:attributes))
    @test node11 == leaf_nodes_branches[1]
    @test node12 == leaf_nodes_branches[2]
    # creating matchers set
    matchers_set = XMLWalker.MatchersSet(:all) # empty intersection of matchers
    push!(matchers_set,XMLWalker.ContainsPat("leaf",:tag))
    push!(matchers_set,XMLWalker.ContainsPat([Regex("id")],:attributes))

    @test matchers_set(node11)
    @test matchers_set(node12)
    @test !matchers_set(node2)

end

@testset "finding nodes from single string" begin
    @test node1 == find_nodes(root_node,"branch1",:tag)[]
    out = find_nodes(root_node,"[branch1,leaf1]") # [] means that at least one of the patterns should be matched
    @test (node1 ∈ out) && (node11 ∈ out)
    out = find_nodes(root_node,"*branch")
    @test (node1 ∈ out) && (node2 ∈ out)
    out = XMLWalker.find_nodes(root_node,"[ [\\d]::regex ].attributes([id])") # finds all nodes containing digits and "id" key
    @test (node11 ∈ out) && (node12 ∈ out) && !(node1 ∈ out)
end

@testset "finding nodes chain" begin
    nodes_found = XMLWalker.find_nodes(root_node,"root/*/*leaf.attributes([id=p1,id=p2])")
    @test node11 ∈ nodes_found && node12 ∈ nodes_found && !(node1 ∈ nodes_found)
    nodes_found = XMLWalker.find_nodes(root_node,"*/[branch1,branch2]/*leaf.attributes([id=p1,id=p2])")
    @test node11 ∈ nodes_found && node12 ∈ nodes_found && !(node23 ∈ nodes_found)
    nodes_found = XMLWalker.find_nodes(root_node,"*/[branch1,branch2]/*leaf.attributes([*id])")
    @test node11 ∈ nodes_found && node12 ∈ nodes_found && !(node23 ∈ nodes_found)
end

# test_doc = XML.read(joinpath(".","test","test_file.xml"),Node)
# test_doc = XML.read("test_file.xml",Node)
# nodes_data = find_nodes(test_doc,"EngineeringData")
# nodes_materials = find_nodes(test_doc,"Materials")
# bafs_node = find_nodes(nodes_materials[],"*/*/Material/BulkDetails/Name/*.value=BAFS")
 
