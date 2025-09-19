strstr(s) = string(strip(string(s)))
const has_round = contains("(")
const has_square = contains("[")
const has_curl = contains("{")
const has_asterisk = contains("*")
const has_equal = contains("=")
const has_dot = contains(".")
const has_regex = contains("::regex")
function remove_regex(s::AbstractString) 
    s_rem = strstr(extract_before(s,"::regex"))
    return isempty(s_rem) ? s : s_rem
end
remove_asterisk(s) = replace(s,"*"=>"")
to_regex(s) = s |> remove_regex |> Regex 
const has_text = contains("::text")
remove_text(s) = strstr(extract_before(s,"::text"))
const NON_DIGIT_DOT_REGEX = r"\.\D+" #r"(?<!\d)\.(?!\d)" # regex to check for non-digit dot when parsing string
const DIGIT_DOT_REGEX  = r"\d+\.\d+" #r"(?<=\d)\.(?=\d)"
not_isempty(s) = !isempty(s)
"""
    has_nondigit_dot(s::AbstractString)

Checks for any dot symbol `.`, which is not decimal digits separator
"""
has_nondigit_dot(s::AbstractString) = begin 
    for mi in eachmatch(NON_DIGIT_DOT_REGEX,s)
        isnothing(mi) || return true
    end
    return false
end
"""
    has_digit_dot(s::AbstractString)

Checks for any dot symbol `.`, which is decimal digits separator
"""
has_digit_dot(s::AbstractString) = begin 
    for mi in eachmatch(DIGIT_DOT_REGEX,s)
        isnothing(mi) || return true
    end
    return false
end

const IS_SIMPLE_REGEX = r"(\.\D+)|([\{\]\}\[\)\(\=])"  
"""
Regex to check if string contains braces, non-digital dots and equality symbols
"""
IS_SIMPLE_REGEX
is_simple_pattern(s::AbstractString)  = isnothing(match(IS_SIMPLE_REGEX,s))

"""
    extract_field(s)

Returns right part of string  separated by non-digit dot `.` 
"""
function extract_field(s)
    m = match(NON_DIGIT_DOT_REGEX, s)
    return isnothing(m) ? "" : s[m.offset + 1 : end]
end
"""
    split_tag_and_field_name(s)

Splits  string  into two parts by non-digit dot `.` 
"""
function split_tag_and_field_name(s)
    m = match(NON_DIGIT_DOT_REGEX,s)
    !isnothing(m) || error("Incorrect call of  split_tag_and_field_name on $(s)")
    return (strstr(s[1:m.offset-1]),strstr(s[m.offset + 1:end]))
end
"""
    split_equality(s::AbstractString)

Splits string by first equality symbol `=`
"""
function split_equality(s::AbstractString)
    m = match(Regex("\\Q=\\E"),s)
    return isnothing(m) ? (s,"") : (strstr(s[1:m.offset-1]),strstr(s[m.offset+1 : end]))
end

function extract_between(s::AbstractString,pat_left::AbstractString,pat_right::AbstractString;include_pats::Bool=false)
    reg = Regex("\\Q$(pat_left)\\E(.*?)\\Q$(pat_right)\\E")
    m = match(reg, s)
    include_pats || return isnothing(m) ? "" : string(m.captures[1])
    return isnothing(m) ? "" : pat_left*m.captures[1]*pat_right
end
function extract_after(s::AbstractString,pat::AbstractString;include_pats::Bool=false)
    reg = Regex("\\Q$(pat)\\E(.*)")
    m = match(reg, s)
    include_pats || return isnothing(m) ? "" : string(m.captures[1])
    return isnothing(m) ? "" : pat*string(m.captures[1])
end
function extract_before(s::AbstractString,pat::AbstractString;include_pats::Bool=false)
    regex = Regex("^(.*?)\\Q$(pat)\\E")
    m = match(regex, s)
    include_pats || return isnothing(m) ? "" : m.captures[1]
    return isnothing(m) ? "" : string(m.captures[1])*pat
end
function parse_single_string_key_value(s_str)
        spliterator = eachsplit(s_str,"=")
        (val1,state) = iterate(spliterator)
        key = parse_single_string_or_number(val1)
        (val2,) = iterate(spliterator,state)
        value = parse_single_string_or_number(val2)
    return key => value
end
function parse_single_string_or_number(s_str)
    if has_equal(s_str)
        return parse_single_string_key_value(s_str)
    end
    s_parsed = tryparse(Float64,s_str)
    if isnothing(s_parsed)
        !contains(s_str,"::text") || return strstr(extract_before(s_str,"::text"))
        !has_regex(s_str)  || return to_regex(s_str)
        !has_asterisk(s_str) || return s_str |> remove_asterisk |> to_regex
        return strstr(s_str);
    else
        return s_parsed
    end
end

function split_delimited_args(s::AbstractString)
    contains(s,",") || return [parse_single_string_or_number(s)]
    return map(parse_single_string_or_number,eachsplit(s,","))
end
function each_delimited_arg(s::AbstractString)
    contains(s,",") || return Iterators.map(parse_single_string_or_number,[s])
    return Iterators.map(parse_single_string_or_number,Iterators.filter(not_isempty,eachsplit(s,",")))
end
function first_match(s::AbstractString,pat)
    for (i,c) in enumerate(s)
        !isequal(c,pat) || return i
    end
    return 0
end
function last_match(s::AbstractString,pat)
    N = length(s)
    for (i,c) in enumerate(reverse(s))
        !isequal(c,pat) || return N - i + 1
    end
    return 0
end
for (name_str,left_char,right_char) in zip(("_square","_curl","_round"),('[','{','('),(']','}',')'))
    is_embraced_cur = Symbol("is_embraced"*name_str)
    @eval function $is_embraced_cur(s::AbstractString)
        s = strip(s)
        return length(s) > 0 ? s[1]==$left_char && s[end] == $right_char  : false
    end
    is_ends_with_cur = Symbol("is_ends_with"*name_str)
    @eval function $is_ends_with_cur(s::AbstractString)
        s = strip(s)
        return length(s) > 0 ? s[end] == $right_char  : false
    end

    extract_embraced_cur = Symbol("extract_embraced"*name_str)
    # reg = Regex("\\Q$(left_char)\\E(.*?)\\Q$(right_char)\\E")
    @eval function $extract_embraced_cur(s)
        left_ind = first_match(s,$left_char)
        left_ind != 0 || return "" 
        right_ind = last_match(s,$right_char)
        right_ind > left_ind || return ""
        return s[left_ind + 1 : right_ind - 1]
    end
    extract_embraced_args_cur = Symbol("extract_embraced_args"*name_str)
    @eval function $extract_embraced_args_cur(s)
        return  s |> $extract_embraced_cur |> split_delimited_args 
    end
    embraced_args_iterator = Symbol("each_embraced_arg"*name_str)
    @eval function $embraced_args_iterator(s)
        return  s |> $extract_embraced_cur |> each_delimited_arg 
    end
end
is_embraced(s) = is_embraced_curl(s) || is_embraced_square(s) || is_embraced_round(s)
is_embraced_square_or_curl(s) = is_embraced_curl(s) || is_embraced_square(s)
is_dictionary_field(s) = has_round(s) && is_ends_with_round(s)

