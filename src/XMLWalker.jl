module XMLWalker
    using OrderedCollections,InteractiveUtils
    public chain_string_specification

    const SymbolOrNothing = Union{Symbol,Nothing}
    const VectOrSymbolOrNothing = Union{Symbol,Nothing,Vector{SymbolOrNothing}}
    const PatterTypeUnion = Union{String,OrderedDict,Float64}
    const TOKENS_SEPARATOR = "/"
    const ROOT_NODE_SEPARATOR = "//"
    const DEFAULT_ROOT_TAG = :tag

    export find_nodes, @to_matcher_str
    
    abstract type AbstractMatcher{T,P} end

    include("StringUtils.jl")

    is_matched(pat::AbstractPattern,s::AbstractString) = !isnothing(match(pat,s))
    is_matched(pat::AbstractString,s::AbstractPattern) = !isnothing(match(s,pat))
    is_matched(pat,s::AbstractPattern) = false
    is_matched(pat::AbstractPattern,s) = false
    is_matched(pat) = Base.Fix1(is_matched,pat)

    internal_isequal(a,b) = isequal(a,b)
    internal_isequal(a::AbstractPattern,b::AbstractString) = is_matched(a,b)
    internal_isequal(a::AbstractString,b::AbstractPattern) = is_matched(b,a)
    internal_isequal(a::Number,b::Union{AbstractPattern,AbstractString}) = false
    internal_isequal(a::Union{AbstractPattern,AbstractString} ,b::Number) = false
    internal_isequal(a::Pair,b::AbstractPattern) = isa(a[1],AbstractString) && is_matched(b,a[1])
    internal_isequal(b::AbstractPattern,a::Pair) = isa(a[1],AbstractString) && is_matched(b,a[1])
    internal_isequal(pat) = Base.Fix1(internal_isequal,pat)

    internal_contains(collection::AbstractString,pattern::AbstractString) = contains(collection,pattern) 
    internal_contains(collection,pattern) = false
    internal_contains(collection, a::AbstractPattern) = any(bi->internal_isequal(a,bi),collection)
    internal_contains(collection::AbstractString, pattern::AbstractPattern) = is_matched(collection,pattern)
    internal_contains(collection::AbstractPattern , pattern::AbstractString) = is_matched(collection,pattern)

    internal_contains(::Nothing,b) = false
    internal_contains(collection,::Nothing) = false
    internal_contains(pat) = Base.Fix2(internal_contains,pat) # can be applied to collection


    swap_contains(pattern,collection) = internal_contains(collection,pattern)
    swap_contains(pattern) = Base.Fix1(swap_contains,pattern)


    always_true(_,_) = true
    internal_haskey(D,k) = isa(D,AbstractDict) && haskey(D,k)
    haskey_swapped(k,D) =  internal_haskey(D,k)
    internal_in(k,itr) = in(k,itr)
    internal_in(a::AbstractString,b::AbstractString) = internal_isequal(a,b)
    internal_in(pat::AbstractPattern,s::AbstractString) = internal_isequal(pat,s) 
    internal_in(pat::AbstractString ,s::AbstractPattern) = internal_isequal(pat,s) 
    internal_in(pat) = Base.Fix1(internal_in,pat)

    function internal_in(pat::AbstractPattern,itr) 
        for i_i in itr 
            !is_matched(pat,i_i) || return true
        end
        return false
    end
    function internal_in(pat::AbstractPattern,itr::AbstractDict) 
        for i_i in keys(itr) 
            !is_matched(pat,i_i) || return true
        end
        return false
    end
    function internal_in(pat::AbstractString,itr::Vector{Regex}) 
        for p_i in itr 
            !is_matched(p_i,pat) || return true
        end
        return false
    end
    function internal_in(pat::Pair,itr::Vector{Regex}) 
        for p_i in itr[1] 
            !is_matched(p_i,pat) || return true
        end
        return false
    end
    # IN ALL FUNCTIONS THE FIRST ARGUMENT IS PATTERN THE SECOND ARGUMENTS IS THE NODE CONTENT OR INPUT ITSELF
    # contains(a,b) search for 
    # MatchesPat -   true if pattern is totally matched
    # PatContains -  true if the pattern contains the input contains(pat,input) = true
    # ContainsPat  - true if the input contains the pattern contains(inout,pat) = true
    # HasAnyKeyPat - true if the input has at least element of pat as a key
    # HasAllKeysPat - true if the inout has all elements of pat as keys
    # ContainsAnyPat - true if tag contains any of patterns

    _itp = NamedTuple{(:fun,:internal,:consumer)}
    for (matcher_type,d) in (:MatchesPat  => _itp((:matches_pat,:internal_isequal,:all)),
                            :PatContains =>  _itp((:pat_contains,:internal_contains,:all)),
                            :ContainsPat => _itp((:contains_pat,:swap_contains,:all)),
                            :ContainsAnyPat => _itp((:contains_any_pat,:internal_contains,:any)),
                            :ContainsAllPats => _itp((:contains_all_pats,:internal_contains,:all)),
                            :HasAnyKeyPat => _itp((:has_any_key_pat,:haskey_swapped,:any)),
                            :HasAllKeysPat => _itp((:has_all_key_pat,:haskey_swapped,:all)),
                            :AnyPat => _itp((:any_pat, :always_true,:any)))

        @eval struct $matcher_type{T,P} <: AbstractMatcher{T,P}
            pat::P
            $matcher_type(s::P,type::SymbolOrNothing=nothing) where P =  new{type,P}(s)
        end
        @eval $(d.fun)(pat::P,input,::Nothing) where P <: Union{AbstractPattern,AbstractString,Number} =  $(d.consumer)($(d.internal)(pat),input)
        @eval $(d.fun)(pat::P,input::AbstractString,::Nothing) where P <: Union{AbstractPattern,AbstractString,Number} =  $(d.internal)(pat,input)
        @eval $(d.fun)(pat::P,input,T::Symbol) where P <: Union{AbstractPattern, AbstractString,Number} = hasproperty(input,T) && $(d.internal)(pat, getproperty(input,T))
        @eval $(d.fun)(pat,input) = $(d.fun)(pat,input,nothing)


        @eval (tag::$matcher_type{T})(input) where T = $(d.fun)(tag.pat,input,T)
        (iterate_over, look_in, in_checker) = if matcher_type == :PatContains
                                                    (:_input, :pat, :internal_in) 
                                                elseif matcher_type == :AnyPat
                                                    (:pat, :pat, :internal_in)  
                                                elseif (matcher_type == :HasAnyKeyPat) || (matcher_type == :HasAllKeysPat)
                                                    (:pat, :_input, :haskey_swapped)   
                                                else 
                                                    (:pat, :_input, :internal_in)
                                                end
        # when ierating over the collection iterate_over is the collection which members we are looking in look_in    
        @eval function $(d.fun)(pat::P,input,T) where  P <: Union{AbstractVector,AbstractDict,NTuple}
            if !isnothing(T)
                hasproperty(input,T) || return false
                _input = getfield(input,T) 
                !isnothing(_input) || return false
            else
                _input  = input
            end
            if isa($iterate_over,AbstractString) # string iterates over chars 
                return $in_checker($iterate_over,$look_in)
            else
                return $(d.consumer)(si-> $in_checker(si,$look_in) ,$iterate_over)
            end
        end
    end
"""
    matchers_set = MatchersSet(matchers_collection::P,type::Symbol) 
    matchers_set_empty = MatchersSet(type::Symbol)

Combines several matchers from `matchers_collection`,`type` can be `:all` or `:any`
`matchers_set` is a callable object, it applies matchers from the collecion and 
and if `type = :all` or `type = :any`  if all or any of matchers returns `true`.
As far as `MatchersSet` is a subtype of `AbstractMatcher` it can be used as an input
for [`find_nodes(starting_node,::AbstractMatcher)`](@ref) function to find nodes matching 
the matchers set. Calling `MatchersSet` only `type` input creates an empty set, which can
be filled with matchers using Base.push! function.

MatchersSet can be used to construct more complicated matchers than those provided by 
parsing strings, for instance, it can be used to make matchers for multiple properties:

```julia
matchers_intersection = MatchersSet( (MatchesPat("A",:tag), MatchesPat("B",:variable), MatchesPat("C",:attributes)),:all)
find_nodes(starting_node, matchers_intersection) # will search for nodes, that contain "A","B" and "C" in their tag, variable and attributes fields simultaneously
```

"""
    struct MatchersSet{T,P} <: AbstractMatcher{T,P} # 
        matchers::P
        MatchersSet(matchers_collection::P,type::Symbol) where P = begin
            @assert in(type,(:any,:all))
            return new{type,P}(matchers_collection)
        end
        MatchersSet(type::Symbol) = begin
            matchers_collection = Vector{AllTagMatchersUnion}()
            return MatchersSet(matchers_collection,type)
        end
    end
    Base.push!(ms::MatchersSet,v::AbstractMatcher) = Base.push!(ms.matchers,v)

    function any_matchers(matchers_collection,node)
        for m in matchers_collection
            !m(node) || return true
        end
        return false
    end

    function all_matchers(matchers_collection,node)
        for m in matchers_collection
            m(node) || return false
        end
        return true
    end
    (m::MatchersSet{:any})(node) = any_matchers(m.matchers,node)
    (m::MatchersSet{:all})(node) = all_matchers(m.matchers,node)

    const AllTagMatchersUnion = Union{subtypes(AbstractMatcher)...}

    """   
    matcher  = MatchesPat(pat,field_name::Union{Symbol,Nothing}=nothing);

matcher(input) returns true if `pat` is matched to the `input`
object field `field_name` if `field_name` is `nothing` than `input` itself is matched.

Other matchers: $(AllTagMatchersUnion)
"""
    MatchesPat 
    """
        matcher  = PatContains(pat,field_name::Union{Symbol,Nothing}=nothing);

matcher(input) - true if  `pat` contains the `input.field_name` if `field_name` is `nothing` than `input` itself is matched.
Other matchers: $(AllTagMatchersUnion)
"""
    PatContains 
    """
        matcher  = ContainsPat(pat,field_name::Union{Symbol,Nothing}=nothing);

matcher(input) true if the `input.field_name` contains the pattern  `pat`
if `field_name` is `nothing` than `input` itself is matched.

Other matchers: $(AllTagMatchersUnion)
"""
    ContainsPat 
    """
        matcher  = ContainsPat(pat,field_name::Union{Symbol,Nothing}=nothing);

matcher(input) - true if the` input.field_name` has at least one element of `pat` as a key
if `field_name` is `nothing` than `input` itself is matched.

Other matchers: $(AllTagMatchersUnion)
"""
    HasAnyKeyPat 
    """
        matcher  = ContainsPat(pat,field_name::Union{Symbol,Nothing}=nothing);

matcher(input) - true if the `input.field_name` has all elements of `pat` as keys
if `field_name` is `nothing` than `input` itself is matched.

Other matchers: $(AllTagMatchersUnion)
"""
    HasAllKeysPat 
    """
    matcher  = ContainsAnyPat(pat,field_name::Union{Symbol,Nothing}=nothing);

matcher(input) - true if `input.field_name` contains any of pat elements (pat can be an iterable collection)
if `field_name` is `nothing` than `input` itself is matched.

Other matchers: $(AllTagMatchersUnion)
"""
    ContainsAnyPat
    """
    matcher  = ContainsAllPats(pat,field_name::Union{Symbol,Nothing}=nothing);

matcher(input) - true if `input.field_name` contains all of pat elements (pat can be an iterable collection)
if `field_name` is `nothing` than `input` itself is matched.

Other matchers: $(AllTagMatchersUnion)
        """
    ContainsAllPats
    # to utils
"""
        ChainMatcher

Is a wrapper type for a vector of matchers, which is iterable
"""
    struct ChainMatcher
        pat_vect::Vector{AllTagMatchersUnion}
        state::Int
        ChainMatcher(pat_string::String,field_types::VectOrSymbolOrNothing=nothing) = begin
            pat_vect = chain_string_to_matchers(pat_string, field_types)
            new(pat_vect,1)
        end
    end
    Base.iterate(t::ChainMatcher) = iterate(t.pat_vect);
    Base.iterate(t::ChainMatcher,state) = iterate(t.pat_vect,state);
    """
    chain_string_to_matchers(s::AbstractString,root_field_name::SymbolOrNothing=:tag)

Converts chain string with multiple tokens to Matchers vector according to [`chain_string_specification`](@ref)
"""
function chain_string_to_matchers(s::AbstractString,root_field_name::SymbolOrNothing=DEFAULT_ROOT_TAG)
        tag_vect = Vector{AllTagMatchersUnion}()
        counter = 0
        for si in eachsplit(s,TOKENS_SEPARATOR)
            !isempty(si) || continue
            counter +=1
            push!(tag_vect,chain_string_token_to_matcher(strstr(si),root_field_name))
        end
        return tag_vect
    end



    """
    field_string_to_matcher(s::AbstractString)


Converts field string to a single `<:AbstractMatcher` object, field string can be `"field_name = args1"` or `"field_name(args2)"`
In the first case, `args1` can be a single value number or string or regex-style object viz [a,b,c] or {a,b,c}, where `{}` or `[]`   
braces tell that `all` or `any` args values viz "a", "b" and "c" should be in `field_name` field. All `args1`  elements must be separated 
by the comma  `,`.

In the second case `"field_name(args1)"`, the content of `(...)` is interpreted as arguments which are the members of AbstractDict
stored in `field_name`, args2 must be embraced in `{}` or `[]`. E.g. `"field_name([a,b,c] )"` is interpreted as `any of keys "a","b"
 and "c" must be among keys of `field_name` dictionary, when args1 also contains `=`, e.g. `args1 = [a=1,b=2,c=3]` this means
key-value pairs specification, field_name({a=1,b=2,c=3}) means that `field_name` dictionary must contain the following 
key-value pairs: `"a"=>1,"b"=>2,"c"=>3` 

"""
function field_string_to_matcher(s::AbstractString)
        #@show s
        if is_dictionary_field(s) # field_name(....) syntax for checking the arguments
            field_name = Symbol(strstr(extract_before(s,"(")))
            args = extract_embraced_round(strstr(s))
            is_embraced(args) || error("set of keys must be embraced in {...} or [...]")
            return convert_braced_arguments_to_matcher(args,field_name,as_keys=true)
        elseif has_equal(s) #field_name=.... syntax to check the value of field itself
            (field_name_string,value) = split_equality(s) # parses as key-value pair
            length(value) > 0 || error("Incorrect string format $(s)")
            field_name = Symbol(field_name_string)
            try_parse = tryparse(Float64,value)
            isnothing(try_parse) || return single_string_or_number_to_matcher(try_parse,field_name)
            is_embraced_square_or_curl(value) || return single_string_or_number_to_matcher(value,field_name)
            return convert_braced_arguments_to_matcher(value,field_name,as_keys=false)
        else
            error("Incorrect string format $(s)")
        end
    end

    """
    extract_embraced_args_square_or_curl(s;convert_to_regex::Bool=false)

Switches embracer type  (`"{..}"` or `"[...]"`) from the string itself and retrurns the vector of parsed values
"""
function extract_embraced_args_square_or_curl(s;convert_to_regex::Bool=false) 
        is_embraced_square_or_curl(s) || error("Arguments must be embraced in square or curl braces like [a,b,c] or {a,b,c}")
        if !convert_to_regex
            return  is_embraced_square(s) ? (extract_embraced_args_square(s),:any) : (extract_embraced_args_curl(s),:all)
        else
            if  is_embraced_square(s) 
                return ((convert_args_vector_to_regex_vector  ∘ each_embraced_arg_square)(s),:any)
            else 
                return ((convert_args_vector_to_regex_vector ∘  each_embraced_arg_curl)(s),:all) 
            end 
        end
    end

    """
    convert_args_vector_to_regex_vector(input::AbstractVector)

Converts the vector of args to regex, removes `*` and `::regex` if any

"""
function convert_args_vector_to_regex_vector(input)
        return map(regex_converter, input)
    end
    regex_converter(vi::Pair) = regex_converter(vi[1])
    regex_converter(vi::AbstractString) = vi |> strstr |> remove_asterisk |> remove_regex |>  Regex
    regex_converter(vi::Regex) = vi   
    regex_converter(vi) = regex_converter(strstr(vi)) 
    """
    convert_braced_arguments_to_matcher(args::AbstractString,field_name::SymbolOrNothing; as_keys::Bool=false)

Input variables:
args - string to be converted, e.g. "{a,b,c}"
field_name - field name of the matcher 
as_keys - if true all args are interpreted as keys of key-value pairs of `field_name` dictionary

Braced arguments like `{a,b,c}`, `[c,d,f]` are converted to matcher with the field name `field_name`
`as_keys` flag means that arges are interpreted as elements contained in dictionary `field_name`, otherwise
as  - values the field `field_name` should match itself.
```julia
struct A  tag end
matcher  = convert_braced_arguments_to_matcher("[a,b,c]",:tag,as_keys=false)
matcher(A("a")) # true
matcher(A(["a","b"])) # true

matcher  = convert_braced_arguments_to_matcher("[a,b,c]",:tag,as_keys=true)
matcher(A(Dict("a"=>1)))) # true

matcher  = convert_braced_arguments_to_matcher("[a=1,a=3]",:tag,as_keys=true)
matcher(A(Dict("a"=>2)))) # false because key-value doesnt matches  any  of `a=>1.0` or `a=>3`

```
If any of embraced patterns contain `::regex` or `*` symbol all of them are forced to
be converted to regular expressions, if they are key-value pairs values are ignored
```julia
matcher  = convert_braced_arguments_to_matcher("[*a=1]",:tag,as_keys=true)


```
Returned matcher can be used to find nodes see [`find_nodes`](@ref)

"""
function convert_braced_arguments_to_matcher(args::AbstractString,field_name::SymbolOrNothing; as_keys::Bool=false)
        if !has_asterisk(args) # if 
            is_only_keys = as_keys && !has_equal(args) # field has keys but not key-value pairs
            is_contains_regex = has_regex(args) # check if arguments string contains ::regex
            (args_vect, set_type) = extract_embraced_args_square_or_curl(args, convert_to_regex = is_contains_regex)
            if set_type == :all 
                return (is_only_keys && !is_contains_regex) ? HasAllKeysPat(args_vect,field_name) : ContainsAllPats(args_vect,field_name)
            elseif set_type == :any
                return (is_only_keys && !is_contains_regex) ? HasAnyKeyPat(args_vect,field_name) : ContainsAnyPat(args_vect,field_name)
            else
                error("Unsupported field_name = $(field_name) and arguments = $(args)")
            end
        else
            if !is_embraced_curl(args) && !is_embraced_square(args) # simple argument has equalities but no braces
                return single_string_or_number_to_matcher(args,field_name)
            else # embraced!!! 
                (values_vect,set_type) = extract_embraced_args_square_or_curl(args, convert_to_regex = as_keys)
                if as_keys # 
                    if set_type == :all
                       return ContainsAllPats(values_vect,field_name)   
                    elseif set_type == :any
                        return ContainsAnyPat(values_vect,field_name) 
                    else
                        error("Unsupported field_name = $(field_name) and arguments = $(args)")
                    end
                end
                matchers_set = MatchersSet(set_type)
                for vi in values_vect
                    push!(matchers_set, single_string_or_number_to_matcher(vi,field_name))
                end
                return matchers_set
            end
        end
    end    

    """
    single_string_or_number_to_matcher(value::AbstractString,field_name::SymbolOrNothing=:tag)

Converts single string to matcher, this is a root function 
"""
function single_string_or_number_to_matcher(value::AbstractString,field_name::SymbolOrNothing=DEFAULT_ROOT_TAG)
        if has_text(value)
            value = remove_text(value)
        end
        if has_regex(value) 
            value_str = remove_regex(value)
            return  MatchesPat(Regex(value_str),field_name) 
        elseif !has_asterisk(value) 
            return MatchesPat(value,field_name)
        elseif has_asterisk(value)
            value != "*" || return AnyPat(value,field_name)
            value = remove_asterisk(value)
            return ContainsPat(value,field_name) 
        else
            error("Unsupported string $(value)")
        end
    end
    single_string_or_number_to_matcher(value::Regex,field_name::SymbolOrNothing=:tag) = MatchesPat(value,field_name)
    single_string_or_number_to_matcher(value::Number,field_name::Symbol=:tag) = MatchesPat(value,field_name)
    """
    chain_string_token_to_matcher(s::String,field_name::SymbolOrNothing=DEFAULT_ROOT_TAG)

Converts single token to a matcher object, for string specification see [`chain_string_specification`](@ref)
"""
    function chain_string_token_to_matcher(s::String,field_name::SymbolOrNothing=DEFAULT_ROOT_TAG)
        if is_simple_pattern(s) # does not contain any special symbols like braces, 
            # dots and equal signs, but can contain regex ::regex or ::text
            return single_string_or_number_to_matcher(s,field_name)
        elseif !has_nondigit_dot(s) # has no encoded field, but has 
            return convert_braced_arguments_to_matcher(s,field_name; as_keys=false)
        elseif has_nondigit_dot(s) # string like "field_name.attribute_name()
            splitted = split_tag_and_field_name(s)
            tag_matcher = chain_string_token_to_matcher(splitted[1],field_name)
            field_matcher = field_string_to_matcher(splitted[2])
            return MatchersSet((tag_matcher,field_matcher),:all)
        else
            return error("Incorrect syntax $(s)")
        end
    end


    """
    find_nodes!(node_vector::Vector{T},node::T,matcher::AbstractMatcher) where T

Finds matching node and pushs it to the node_vector
"""
function find_nodes!(node_vector::Vector{T},node::T,matcher::AbstractMatcher) where T
        !matcher(node) || push!(node_vector,node)
        !isnothing(getfield(node,:children)) || return node_vector
        for ndi in getfield(node,:children)
            find_nodes!(node_vector,ndi,matcher)
        end
        return node_vector
    end
    """
    find_nodes(starting_node, search_string::AbstractString, field_name::SymbolOrNothing = DEFAULT_ROOT_TAG)

First parses `search_string`, than finds nodes. For string specification see [`chain_string_specification`](@ref).
`field_name` is the root field name, by default it is a `$(DEFAULT_ROOT_TAG)` field. Root field name is the field, 
where the first part of string token is searched, e.g.
```julia
find_nodes(starting_node, "A.values = 5") # "A"  will be recursively searched in `starting_node.tag` and in all children nodes of `starting_node` etc.
find_nodes(starting_node, "A.values = 5", :Name) # "A"  will be recursively searched in `starting_node.Name` and in all children nodes of `starting_node` etc.
``` 
"""
function find_nodes(starting_node, search_string::AbstractString, field_name::SymbolOrNothing = DEFAULT_ROOT_TAG)
        if contains(search_string,TOKENS_SEPARATOR) # string contains several tokens
            return find_nodes_chain(starting_node,search_string, field_name)
        else
             return find_nodes(starting_node,chain_string_token_to_matcher(search_string,field_name))
        end
    end
    """
    find_nodes(starting_node::T, matcher::AbstractMatcher) where T

Finds nodes, that match the matcher object
"""
function find_nodes(starting_node::T, matcher::AbstractMatcher) where T
        node_vector = Vector{T}()
        find_nodes!(node_vector,starting_node,matcher)
        return node_vector
    end
    """
    find_nodes_chain(starting_node::T,xml_chain_string::String, field_name::SymbolOrNothing = DEFAULT_ROOT_TAG) where T

Finds nodes chain, for details of xml_chain_string see [`chain_string_specification`](@ref)
"""
function find_nodes_chain(starting_node::T,xml_chain_string::String, field_name::SymbolOrNothing = DEFAULT_ROOT_TAG) where T
        node_vector = Vector{T}()
        return find_nodes_chain!(node_vector,starting_node,ChainMatcher(xml_chain_string,field_name))
    end
    """
    find_nodes_chain!(node_vector::Vector{T},node::T,tag_chain::ChainMatcher,state::Int=1,first_node::Bool=true) where T

"""
function find_nodes_chain!(node_vector::Vector{T},node::T,tag_chain::ChainMatcher,state::Int=1,first_node::Bool=true) where T
        (matcher,next_state) = iterate(tag_chain,state)
        has_next_state = !isnothing(iterate(tag_chain,next_state))
        if matcher(node) 
            if !has_next_state
                push!(node_vector,node)
                !first_node || return node_vector
            end
        elseif first_node
            return node_vector
        end
        !isnothing(node.children) || return node_vector
        for ndi in node.children
            isa(ndi,T) || continue
            find_nodes_chain!(node_vector,ndi,tag_chain,next_state)
        end
        return node_vector
    end
    macro to_matcher_str(s)
        chain_string_to_matchers(s)
    end

    """
    Chain strings specification

## Simple chain by tags

A single string input (without the token separator symbol `"/"`) to the `XMLWalker.find_nodes` function, such as 
`"node1_tag"`, represents a single token. By default, this token is assumed to be the tag field of the node and  
`XMLWalker.find_nodes("node1_tag")` it finds all nodes (if any) matching the pattern specified by that string. 
Additionally, the field name can be provided as an optional second input argument.
```julia
XMLWalker.find_nodes(starting_node, "ABB",:attributes) # returns nodes, that has "ABB" value of field `attributes`  
```
The symbol `"/"` is used as a token separator, indicating that all substrings separated by this symbol should be 
interpreted as a sequence of matchings. Each token in the string corresponds to a node in the XML tree. The function
 `XMLWalker.find_nodes` uses these chain-strings to create matchers that are checked sequentially, searching for 
 nodes that fit the entire sequence. For the chain-string `"A/B/C"`, `XMLWalker.find_nodes` returns nodes reachable
  by following the path with tags `"A"` → `"B"` → `"C"`. 

```julia
find_nodes(starting_node,"A/B/C") # returns a vector of nodes which can be reached following the "A"->"B"->"C" chain 
# (starting_node must has a field :tag = "A") 
```
## Single token specification

The following sections describe a single token syntax, all of this is also applicable to the string-chain as far as 
string-chain  is a sequance of tokens.

### Additional fields values check

When a tag name is followed by a dot, as in `"tag_name.field_name = field_value"`, the `field_name` is interpreted as the
name of the node's field, and the value after the `=` (i.e., `field_value`) is treated as the value of that field. 
For example, `"A.value = ABC"` represents a node with the `tag` field equal to `"A"` and the `value` field equal to `"ABC"`. 
By default, the value is parsed as a string, but if it can be interpreted as a number, such as in `"tag_name.field_name = 25.4"`,
it means the `field_name` of the node tagged `tag_name` has the value `25.4` (Float64). Additionally, the annotation `::text` 
can be added to force the value to be parsed as a string, so `/tag_name.field_name = 25.4::text/` searches for the `field_name` 
with the string value `"25.4"`(String).

```julia
find_nodes(starting_node,"B.value = 356::text") # searches for nodes with "B" tags and field value ="356"
```

### Nodes with dictionary fields

Node fields can also be of the `AbstractDict` type, allowing for searches of specific keys or key-value pairs. The syntax for this is as follows: `"tag_name.field_name([key1,key2,key3])"` when field name is followed by `"(....)"`. This pattern matches nodes where the `field_name` dictionary of a `tag_name` node contains any of the keys `"key1"`, `"key2"`, or `"key3"`. All keys must be enclosed in either `"[]"` or `"{}"`— where `"[]"` represents any of the keys, and `"{}"` means all keys must be present. For example, `"A.attributes({p1,p2})"` refers to a node with the tag `"A"` having the `attributes` field that contains both `"p1"` and `"p2"` keys.

It is also possible to search for nodes with specific key-value pairs within the field-dictionary. The syntax is similar to the key search pattern, but each key is followed by an equals sign (`=`). For instance, `"tag_name.field_name({key1=value1,key2=value2})"` matches a node with tag `"tag_name"` whose `field_name` dictionary contains both `"key1"=>"value1"` and `"key2"=>"value2"`.

```julia
find_nodes(starting_node,"B.value([p1,p2])") # searches for nodes with "B" tags and field value with any of p1 or p2 keys
find_nodes(starting_node,"B.value([p1=2,p2=20])") # searches for nodes with "B" tags and field value with all of "p1" => 2.0 and "p2" => 20.0 key-value pairs
```

### Special Symbols

All special symbols in this section apply to tags, field values, and field dictionary keys.

#### `"[...]"` (any) and `"{...}"` (all) Patterns

To find nodes within a specific set of tags, for example `"tg1"`, `"tg2"`, or `"tg3"`, these tag names must be enclosed in square brackets `"[]"`. This pattern returns true if **any** of the enclosed tags match. For example, `"[tg1,tg2,tg3]"` searches for nodes with any of the tags `"tg1"`, `"tg2"`, or `"tg3"`. 

Similarly, this pattern can be used to match field values, such as `"A.field_name = [a,b]"`, or to match field dictionary keys like `"A.field_name([k1,k2])"`.


```julia
find_nodes(starting_node,"[A,B].field1 = [ab,bc]") # finds nodes with "A" or "B" tag field and "ab" or "ac" field1 field value 
```

If all patterns need to be matched simultaneously, the `{}` block can be used. This block is especially useful for specifying field values.  For example, `"[A,B].field_name({a = 1, b = 2})"` will match nodes tagged as `"A"` or `"B"` that contain both key-value pairs `"a" => 1.0` and `"b" => 2.0` in their `field_name` field.

#### `*` (always match), `*...` (contains) and `...::regex` (regular expression) patterns

To skip a pattern node in a search tree, the `*` symbol, which represents an always-match wildcard, can be used. For example, `"*.field_name = c"` matches nodes with any tag and a `field_name` value of `"c"`. Hence, the `"*.tag = A"` token is equivalent to
just `"A"`.

When the `*` symbol appears anywhere within a string, it indicates a partial match (i.e., the pattern is contained within the string). For instance, `"*Prop"` matches nodes with tags containing `"Prop"`, such as `"BulkProp"` or `"PropertyOne"`. This rule also applies to field values and dictionary keys. However, for key-value pairs matching, partial match patterns using `*` are not supported. If any key in the key-value pairs contains `*`, all values in that pair are ignored. For example, `"A.field_name([*abb=1,b=2])"` behaves the same as `"A.field_name([*abb,b])"`. Both field names and values in key-value pairs cannot contain the `*` symbol; thus, `"node_tag.*partial_name = c"` or `"A.field_name([a=*b , b=2])"` are not supported, but `"node_tag.field_name = "*ca"` is allowed.

If the field value or key is marked with `"::regex"` it is interpreted as a regular expression, the main rule here is that thus marked pattern should be matched if julia `match(::Regex,str)` returns not `nothing`. To use regular expression for tag search it should be embraced in `{}` or `[]`. For instance, matcher string token for nodes containing digits in their tag field and attributes field dictionary with key `id` will be `"[ [\\d]::regex ].attributes([id])"`
"""
struct chain_string_specification end

end