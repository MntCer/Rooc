# The Rooc Language Reference

## Introduction

Rooc seeks to retain the syntactic style of C/C++ while eliminating pointers. 
It also aims to modernize the type system, eschewing inheritance in favor of traits, akin to Rust, to avoid the problems about subtype.

## Notation

### Syntax notation

The syntax grammar is specified using [Wirth syntax notation](https://en.wikipedia.org/wiki/Wirth_syntax_notation) to express the Extended Backus-Naur Form (EBNF):

```ebnf
Syntax      = { Production } .
Production  = production_name "=" [ Expression ] "." .
Expression  = Term { "|" Term } .
Term        = Factor { Factor } .
Factor      = production_name
            | literal
            | Group 
            | Option 
            | Repetition

Group       = "(" Expression ")" .
Option      = "[" Expression "]" .
Repetition  = "{" Expression "}" .
```

The equals sign indicates a production. 
The element on the left is defined to be the combination of elements on the right. 

A production is terminated by a full stop (period).

Lowercase production names and elements quoted in `""` are used to identify lexical (terminal) tokens. Non-terminals are in CamelCase. 

The three operators below are in increasing precedence.

```ebnf
|   alternation
()  grouping
[]  option (0 or 1 times)
{}  repetition (0 to n times)
```

### Lexical notation

The lexical grammar is primarily described using [POSIX extended regular expressions](https://en.wikibooks.org/wiki/Regular_Expressions/POSIX-Extended_Regular_Expressions).

In our notation, expressions are named to represent specific tokens, and can be referenced in other expressions as patterns by enclosing the name with `<` and `>` brackets. 
A pattern is a segment of the regular expression treated as a single unit.

To match the raw "<" and ">" characters in a string, use `\<` and `\>` respectively.

```
token1 : (a|\<)
token2 : <token1>(b|\>)
```

The `token2` expression expands to `(a|<)(b|>)` in standard ERE.

In ERE, a raw space character is a valid element. 
However, in most cases, Rooc will not allow to include a "space" as part of lexical tokens(not for string literal, comments, etc.) So, the spaces presented in the following regular expressions are only for readability and don't denote actual space characters.
To represent a "space" explicitly in the following regular expressions, we will utilize `\s`.

## Lexical elements

### Alphabet

Rooc uses the ASCII character set as its alphabet. This consists of all characters with values from 0 to 127 in the ASCII table, encompassing:

* Control characters (0-31)
* Standard printable characters including:
    * Uppercase alphabets (A-Z)
    * Lowercase alphabets (a-z)
    * Digits (0-9)
    * Punctuation symbols (e.g., !, @, #, etc.)
    * Special characters (e.g., [, ], {, }, etc.)
    * Space
* The DEL character (127)

### Letters and Digits

```
digit   : [0-9]
letter  : [a-zA-Z]
```

### Comments

Comments will not be parsed by the compiler. Rooc has two comment styles:

1. _Line comments_ start with the character sequence `//` and stop at the end of the line.
2. _General comments_ start with the character sequence `/*` and stop with the first subsequent character sequence `*/`.

```
line_comment    : \/\/.*(\r?\n)
general_comment : \/\*.*?\*\/
comment         : <line_comment>|<general_comment>
```

### Whitespace

<!-- ;TODO -->

### Identifiers

Identifiers name program entities such as variables and functions. An identifier is a sequence of one or more letters, digits and underscores, but the *first* character must be a letter.

```
identifier : <letter>(<letter>|<digit>|_)*
```

The `production_name` referred before also follow this rule.


### Semicolon

To allow complex statements to occupy a single line, Rooc use semicolon as the terminator of a statement. 

```
;
```

### Operators and punctuation

```
=       +       -       *       /       (       )

==      !=      <       <=      >       >=      &&  
||      !       

{       }       ,       :      ->     .
```

<!-- ```
;NOTE: following style is too verbose
assign  : =
plus    : \+
minus   : -
times   : \*
divide  : /
lparen  : \(
rparen  : \)

eq      : ==
neq     : !=
lt      : <
leq     : <=
gt      : \>
geq     : \>=
and     : &&
or      : \|\|
not     : !

lbrace  : {
rbrace  : }
comma   : ,
colon   : :
``` -->

### Keywords

The following keywords are reserved and may not be used as identifiers.

```
//TODO
true        false       var         let         
fun         struct      trait       impl  
int         float       bool        str         
void        list        break       this
```

### Integer literals

An integer literal is a sequence of digits representing an integer constant. Now only support decimal integer.

<!-- ;TODO: non-decimal base integer literal -->

```
decimal_int_lit : <digit>+
int_lit         : <decimal_int_lit>
```


### Floating-point literals

A floating-point literal is a decimal of a floating-point constant.
A decimal floating-point literal consists of an integer part (decimal digits), a decimal point, a fractional part (decimal digits).
The fractional part can be omitted if it is zero.

<!-- ;TODO: exponent part -->
<!-- ;TODO: omit leading zero -->
```
decimal_float_lit : <digit>+\.<digit>*
float_lit         : <decimal_float_lit>
```

### Boolean literals

A boolean literal represents a constant of boolean type and can take one of two values: true or false.

```
boolean_lit    : true | false
```


### String literals

A string constant is a sequence of zero or more letters, digits, and escape sequences enclosed within double quotation marks. Rooc will mark the first `"` as the string's beginning and the second `"` as the string's ending. 

```
string_lit : "(<digit>|<letter>|\s|\\[nrt])*"
```

Now, the supported escape sequences are:

* `\n` : newline
* `\r` : carriage return
* `\t` : horizontal tab


## Types

<!-- ;TODO: should struct be a type? -->


```ebnf
Type = PrimitiveType | GenericType.
```

### Primitive types

<!-- ;TODO: detailed type introduction -->

```ebnf
PrimitiveType   = Int | Float | Bool | String .
Int             = "int".
Float           = "float".
Bool            = "bool".
String          = "str".
Void            = "void".   
```


### Generic type (TODO)

```ebnf
GenericType = List .
List        = "list" "(" Type ")" .
```

## Variables

A variable is a storage location for holding a value. The set of permissible values is determined by the variable's type.

```ebnf
IdtyPair  = identifier ":" Type .
VarDecl   = "var" IdtyPair "=" Expression ";" .
```

The type of expression and variable must be consistent.

Example:

```plaintext
var a:int = 1;          
var b:bool = true;
var c:float = 3.0;
var d:string = "a";
var e:list(int) = [a,2,3];
```

### Constants

Use `let` rather than `var` to declare a constant.

```ebnf
LetDecl = "let" IdtyPair "=" Expression ";" .
```

<!-- ;TODO
Struct types
Function types
trait types
 -->

## Function

```ebnf
FunDecl      = FunSignature "{" Statements "}" .
FunSignature = "fun" identifier "(" ParamList ")" "->" Type .
ParamList    = [IdtyPair] 
             | IdtyPair {"," IdtyPair} .
Statements   = {Statement} .
```

```
fun get_second (x:int,y:float) -> float
{
    return y;
};
```

### Control flow


```ebnf
IfStatement = "if" "(" Expression ")" "{" Statements "}" 
            | "if" "(" Expression ")" "{" Statements "}" "else" "{" Statements "}".
```

```ebnf
ForStatement   = "for" "(" Expression ";" Expression ";" Expression ")" "{" Statements "}" .
WhileStatement = "while" "(" Expression ")" "{" Statements "}" .
```


<!-- ``` ;TODO: need to support the type-inference first;
for(<id> in <list_id>){
    <stmt_list>
}
``` -->


## Expressions

## Struct

```ebnf
StructDecl = "struct" identifier "{" VarDecl "}" .
```

```
struct Point {
    var m:int;
    var n:int;
};
```

### Trait

In Rooc, polymorphism is supported through the use of traits, rather than class inheritance. 
Traits define a set of methods that multiple structs can implement. This allows for type-safe, flexible code without the complications that inheritance can bring.

```ebnf
TraitDecl = "trait" identifier "{" FunSignatures "}" .
FunSignatures = {FunSignature ";"} .
```

```
trait Drawable {
    public void draw();
};
```

### Impl

```ebnf
ImplDecl = "impl" identifier "{" FunDecls "}" 
         | "impl" identifier "for" identifier "{" FunDecls "}" .
FunDecls = {FunDecl} .
```

```
impl Point {
    fun new(x:int, y:int) -> Point { 
        this.x = x;
        this.y = y;
        return this;
    };

    fun getX() -> int {
        return this.x;
    };

    fun getY() -> int {
        return this.y;
    };
};
```

```
// Implement the Drawable trait for the Point struct
impl Drawable for Point {
    fun draw() -> void {
        // Drawing logic for Point
    };
};

// Implement the Drawable trait for the Circle struct
struct Circle {
    var x:int;
    var y:int;
    var radius:int;
};

impl Drawable for Circle {
    fun draw() -> void {
        // Drawing logic for Circle
    };
};
```

With traits, you can write functions that operate on any type that implements a specific trait:


```
fun render(d: Drawable) -> void {
    d.draw();
};

var p:Point = Point.new(1, 2);
var c:Circle = Circle.new(3, 4, 5);

render(p);  // Calls Point's draw method
render(c);  // Calls Circle's draw method
```

## Statements



## Built-in functions
```
//TODO
print()
```