# XMLWalker.jl

After loading the XML file using `XML.jl`, all data is stored in an encapsulated, tree-like structure of `Nodes`. Each node element has a children field, which is a vector of child `Nodes`, as well as other fields like `attributes`, `value` etc. `XMLWalker.jl` provides tools for traversing the XML tree using a set of matchers that check the content of nodes. Matchers can be created from strings and combined into chain strings, forming a sequence of matchers to find nodes that meet multiple content restrictions. The function `XMLWalker.find_nodes` converts input string to machers, recursively applies these matchers and returns a vector of nodes that satisfy the specified conditions.

# Quick start
```julia
import Pkg
Pkg.add(url=https://github.com/Manarom/XMLWalker.jl.git)
import XMLWalker
import XML

xml_file = XML.read(xml_file,XML.Node) #  loads data as a structure with multiple embedded nodes, root node is the `xml_file` object
matched_nodes = XMLWalker.find_nodes(starting_node,"A/[B,C]/D") # finds nodes, which match the specified path or single matching "A"=>"B"=>"D" or "A"=>"C"=>"D" route , starting_node can be the `xml_file` or any of its children
```
# Chain strings specification

## Simple chain by tags

A single string input (without the token separator symbol `"/"`) to the `XMLWalker.find_nodes` function, such as `"node1_tag"`, represents a single token. By default, this token is assumed to be the tag field of the node and  `XMLWalker.find_nodes(starting_node,"node1_tag")` it finds all nodes (if any) matching the pattern specified by that string. Additionally, the field name can be provided as an optional second input argument.
```julia
XMLWalker.find_nodes(starting_node,"ABB",:attributes) # returns nodes, that has "ABB" value of field `attributes`  
```
The symbol `"/"` is used as a token separator, indicating that all substrings separated by this symbol should be interpreted as a sequence of matchings. Each token in the string corresponds to a node in the XML tree. The function `XMLWalker.find_nodes` uses these chain-strings to create matchers that are checked sequentially, searching for nodes that fit the entire sequence. For the chain-string `"A/B/C"`, `XMLWalker.find_nodes` returns nodes reachable by following the path with tags `"A"` → `"B"` → `"C"`. 

```julia
find_nodes(starting_node,"A/B/C") # returns a vector of nodes which can be reached following the "A"->"B"->"C" chain (starting_node must has a field :tag = "A") 
```
## Single token specification

The following sections describe a single token syntax, all of this is also applicable to the string-chain.

### Additional fields values check

When a tag is followed by a dot, as in `"tag_name.field_name = field_value"`, the `field_name` is interpreted as the name of the node's field, and the value after the `=` (i.e., `field_value`) is treated as the value of that field. For example, `"A.value = ABC"` represents a node with the `tag` field equal to `"A"` and the `value` field equal to `"ABC"`. By default, the value is parsed as a string, but if it can be interpreted as a number, such as in `"tag_name.field_name = 25.4"`, it means the `field_name` of the node tagged `tag_name` has the value `25.4` (Float64). Additionally, the annotation `::text` can be added to force the value to be parsed as a string, so `/tag_name.field_name = 25.4::text/` searches for the `field_name` with the string value `"25.4"`(String).

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
Here is the corrected and paraphrased version of your text:

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