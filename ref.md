### ref card of some sort

```
k has atom, list (2;`c), dict `a`b!(2;`c) and func {[x;y]x+y}.
20 primitives/verbs, 6 operators/adverbs and 3 system functions.

Verb      (unary)   Adverb               Noun      (null)
: gets              '  each              name  `a`b `
+ plus     flip     /  over|join         char  "ab" " "
- minus    negate   \  scan|split        int   2 34 0N(nan)
* times    first    ': eachprior         float 2 .3 0n(nan) 0w(inf)
% divide   sqrt     /: eachright         date  2016.06.28    (.z.d)
! mod|map  enum|key \: eachleft          time  12:34:56.789  (.z.t)
& min|and  where                         
| max|or   reverse  System               list (2;3.4;`ab)       
< less     asc      0: file r/w(line)    dict `a`b!(2;`c)
> more     desc     1: file r/w(byte)    view f::32+1.8*c TODO
= equal    group    2: open/msg/close    func {[c]32+1.8*c}
~ match    not
, concat   enlist                        
^ except   null                          \t:n x  time
# take|rsh count    #[t;c;b[;a]] select  \l a.k  load   
_ drop|cut floor                         \w workspace
$ cast|sum string   $[c;t;f]     COND    \v variables
? find|rnd distinct ?[x;I;[f;]y] insert  \f functions
@ at       type     @[x;i;[f;]y] amend   \a ancestors
. dot      eval|val .[x;i;[f;]y] dmend   \d directory

atoms are ncif(name char int float) and mdhrst(month day hour min sec milli)
vector is unitype list of atoms, e.g. (2;3) is 2 3 and ("a";"b") is "ab"
matrix is uniform list of vectors, e.g. m:(0 1 2;1 2 3)
atomic functions (+-*%!&|<>=$;-%~_^$) penetrate, e.g. 2 3+m is (2 3 4;4 5 6)
default function parameters are x y z, e.g. {z+x*y}[3;2;1] is 7
indexing and function call use []'s, e.g. m[1;2] is {x+y}[1;2] is 3
no precedence among verbs: 2*4-3 is 2* 4- 3 is 2
grammar: E:E;e|e e:nve|te| t:n|v v:tA|V n:t[E]|(E)|{E}|N
unary list needs ",", e.g. 2(atom) ,2(list)
unary verb arg needs ":", e.g. #'(take each) #:'(count each)
unary call doesn't need []'s, e.g. f x is f[x]
datetime: 2016.06.28+3 week:7*-7!d bar5:5*-5!r etc.
generate: !n(enum) !N(odometer) ?n(uniform) ?-n(normal) n?(draw) -n?(deal) =n(identity matrix)

0:""0:"prompt" /readline writeline
/ comment
\ exit

on error(inspect locals etc.)
> ' up
> \ out
2+ \3  trace

while(c){..};if(c){..}
exp log sin cos [x]cmb;in within
count first last sum min max;[x]avg [x]var [x]dev

TODO: parallel/mapreduce
f':jobs         / across machine
handles 2:jobs  / across cluster

LIMITS: names8 params8 locals8 globals16 constants128 jump256 v[i]:m[i;j] {z}'/\
```

### BNF of some sort
```
grammar: E:E;e|e e:nve|te| t:n|v v:tA|V n:t[E]|(E)|{E}|N

<Exprs>  ::=  <Exprs> ";" <expr>  |  <expr>
<expr>   ::=  <noun> <verb> <expr>  |  <term> <expr>  |  empty
<term>   ::=  <noun>  |  <verb>
<verb>   ::=  <term> <Adverb>  |  <Verb>
<noun>   ::=  <term> "[" <Exprs> "]"  |  "(" <Exprs> ")"  |  "{" <Exprs> "}"  |  <Noun>

Further, <Adverb> <Verb> <Noun> can be defined,
with minor variations between dialects, as:

<Adverb> ::=  "'" | "/" | "\" | "':" | "/:" | "\:"
<Verb>   ::=  <Verb1> | <Verb1> ":"
<Verb1>  ::=  ":" | "+" | "-" | "*" | "%" | "!" | "&" | "|" | "<" | ">" | "=" | "~" | "," |
              "^" | "#" | "_" | "$" | "?" | "@" | "." | <Digit> ":"
<Noun>   ::=  <Names>  |  <Ints>  |  <Floats>  |  <String>  |  <Symbols>
<Names>  ::=  <Names> "." <Name>  |  <Name>
<Name>   ::=  <Letter>  |  <Name> <Letter>  |  <Name> <Digit>
<Ints>   ::=  <Int>  |  <Ints> " " <Int>
<Int>    ::=  "-" <Digits>  |  <Digits>
<Floats> ::=  <Float>  |  <Floats> " " <Float>
<Float>  ::=  <Int>  |  <Int> "." <Digits>  |  <Int> "." <Digits> "e" <Int>
<String> ::=  '"' <Chars> '"'  |  "0x" <Bytes>
<Chars>  ::=  <Chars> <Char>  |  empty
<Char>   ::=  "\0" | "\t" | "\n" | "\r" | '\"' | "\\" | any
<Bytes>  ::=  <Bytes> <Hex> <Hex>  |  empty
<Symbols>::=  <Symbols> <Symbol>  |  <Symbol>
<Symbol> ::=  "`"  |  "`" <Name>  |  "`" <String>
<Digits> ::=  <Digit>  |  <Digits> <Digit>
<Digit>  ::=  "0" | .. | "9"
<Hex>    ::=  <Digit> | "a" | .. | "f"
<Letter> ::=  "A" | .. | "Z" | "a" | .. | "z"
```

### And yet another BNF I found somewhere
```
root ::= top* separator_or_eof

top ::= namespace_declaration | command_statement | mode_directive | expression_list

expression_list ::= separator | expression (separator+ expression)*
expression ::= q_sql | noun_or_verb noun_or_verb? adverb? expression?
q_sql ::= q_sql_template [expression] q_sql_from [expression]

noun_or_verb ::= noun | primitive_verb [colon] | system_function
noun ::= control_flow | args | group_or_list | primitive_noun | assignment | user_id

control_flow ::= control_statement | conditional_evaluation
control_statement ::= control args
conditional_evaluation ::= conditional args
args ::= open_bracket expression_list* close_bracket

system_function ::= q_system_function | signal | trace | colon
group_or_list ::= open_paren expression_list* close_paren

lambda ::= open_brace [lambda_params] adverb? expression_list* close_brace
lambda_params ::= open_bracket [user_id (separator user_id)*] close_bracket

separator ::= semicolon | newline
separator_or_eof ::= separator | eof

command_statement ::= command [expression]
namespace_declaration ::= current_namespace user_id
mode_directive ::= mode expression

assignment ::= user_id [args] (colon [colon] | verb colon | colon verb colon)

control="if"|"do"|"while"
conditional=":"|"?"|"$"|"@"|"."

primitive_noun ::= vector | atom | lambda
vector ::= symbol_vector | number_vector | string
atom ::= symbol | number | char
symbol_vector ::= symbol symbol+
symbol ::= symbol_token

user_id ::= user_identifier | q_system_function

symbol_token ::= 'symbol_token'
newline ::= '\n'
semicolon ::= ';'
eof ::= 'eof'
conditional ::= 'conditional'
control ::= 'control'
colon ::= ':'
open_paren ::= '('
close_paren ::= ')'
open_bracket ::= '['
close_bracket ::= ']'
adverb ::= '/'
close_brace ::= '{'
open_brace ::= '}'
mode ::= 'q' | 'k'
current_namespace ::= 'namespace'
command ::= 'command'
char ::= 'a' | 'z'
number ::= '0'|'9'
string ::= [char]
number_vector ::= [number]
```