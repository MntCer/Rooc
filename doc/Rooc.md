# The Rooc Language Reference

## Program structure

<!-- %TODO: the cross-compile part -->

## Notation

### Syntax notation

The syntax grammar is specified using [Wirth syntax notation](https://en.wikipedia.org/wiki/Wirth_syntax_notation) to express the Extended Backus-Naur Form (EBNF) in those monospaced font blocks:

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

The equals sign indicates a production. The element on the left is defined to be the combination of elements on the right. 

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

The lexical grammar is primarily described using [POSIX extended regular expressions](https://en.wikibooks.org/wiki/Regular_Expressions/POSIX-Extended_Regular_Expressions)(ERE).

$$
\begin{align*}
\textbf{digit}      & : \texttt{['0'-'9']} \\
\textbf{integer}    & : \textbf{digit}\texttt{+} \\
\end{align*}
$$

<!-- %TODO: polish this paragraph -->
A colon indicates a lexical token's definition. The symbols in textwriter font represent operators in ERE regular expression. Strings in bold font refer to named lexical tokens. Symbol in $\texttt{'}$ denotes a single character in Rooc's alphabet.

Worth noting, standard ERE will match space character in input expression. But in this manual, all space characters are just used for formatting and readability. Rooc will use $\backslash \texttt{s}$ to denote a space character in token's definition.

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
// Note '\' means escape character.
line_comment    : \/\/.*(\r?\n)
general_comment : \/\*.*?\*\/
comment         : <line_comment>|<general_comment>
```

### Whitespace

```
 whitespace : [' ' '\t' '\r' '\n'] 
 separator  : whitespace {whitespace}
 ```

We use whitespace, tab, carriage return, and newline character to separate tokens, otherwise ignored by the compiler. 

### Identifiers

Identifiers name program entities such as variables and functions. An identifier is a sequence of one or more letters, digits and underscores, but the *first* character must be a letter.

```
identifier : <letter>(<letter>|<digit>|_)*
```

The `production_name` referred before also follow this rule.

### Semicolon

To allow complex statements to occupy a single line, Rooc use semicolon as the terminator of a statement or a declaration. 

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



<!-- ``` ;NOTE: following style is too verbose 
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

```
-->


### Keywords

The following keywords are reserved and may not be used as identifiers.

```
/*booleans*/    true        false       
/*define*/      var         let         fun         struct      trait       
                impl  
/*type*/        int         float       bool        str         void        
                list
/*control*/     if          else        for         while       return
/*OOP*/         this
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

## Module

A file will be compiled as a `module`. 


## Type

```ebnf
Type = PrimitiveType | GenericType | identifier.
```

### Primitive types

<!-- ;TODO: detailed type introduction -->
We implement common primitive types.

```ebnf
PrimitiveType   = Int | Float | Bool | String | Void.
Int             = "int".
Float           = "float".
Bool            = "bool".
String          = "str".
Void            = "void".   
```

### Generic type

```ebnf
GenericType = List .
List        = "list" "(" Type ")" .
```

Elements in a list should have the same type.

### Struct type

Declaration of a new struct leads to a new type which appears the same as the name of the struct.

Struct type example:

```
Struct A{
    var num: int;
}

Struct B{
    /* Here `A` is the type of variable `a` */
    var a: A;
}
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

Use `let` rather than `var` to declare a constant. Attempts to modify them constants result in an error.

```ebnf
LetDecl = "let" IdtyPair "=" Expression ";" .
```

Example:
```
let a = 1;
a = 2; // This won't compile, instead it raises error: "`a` is unmutable"
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
Statements   = { Statement } .
```

Functions can be declared without a body within a `trait` where we don't expect an implementation. We call such declarations function signatures (`FunSignature`). The only difference between it and a function declaration is the latter does have a body of statements.

Example:

```plaintext
/* It's a function signature */
fun get_second (x:int,y:float) -> float

/* It's a function declaration */
fun get_second (x:int,y:float) -> float
{
    return y;
};
```


### Control flow

#### If Statement

```ebnf
IfStatement = "if" "(" Expression ")" "{" Statements "}" { "else" "{" Statements "}" }.
```


If `Expression` is evaluated to `true`, the execution flow goes to the first `Statements` and ignores the second `Statements`(if any). Otherwise, only execute the `Statements` after `else`(if any). "`else {Statements}`" is not required.

#### While Statement and For Statement

```ebnf
ForStatement   = "for" "(" Expression ";" Expression ";" Expression ")" "{" Statements "}" .
WhileStatement = "while" "(" Expression ")" "{" Statements "}" .
```

<!-- ;TODO: need to support the type-inference first;--> 

To repeat the statements in the curly bracket, we use `while` loop to execute until the `end_condition` evaluates to `false`.

```
while(end_condition){
    do_something();
};
```

`ForStatement`'s behavior can be explained with `WhileStatement` since they are interchangeable.

The following two blocks are equivalent:

```
start_condition;
while(end_condition){
    do_something();
    step_update;
};
```

```
for(start_condition; end_condition; step_update){
    do_something();
};
```

<!-- ``` 
for(<id> in <list_id>){
    <stmt_list>
}
``` -->

## Expressions

Except for common expressions like in **C**, Rooc has some OO-styled expressions:

```ebnf
Member     = ("this" | identifier) "." identifier .
CallMember = ("this" | identifier) ":" identifier "." identifier "(" ArgList ")"
ArgList    = [Expression {"," Expression}]
```

These two expressions attempt to access members of a struct. `Member` tries to access a field of a struct instance. `MemberCall` tries to call a function of a struct instance implemented by a implmentation.

Example:

```
struct Struct_name{
    var field: int;
}

impl Impl_name for Struct_name{
    fun Member_func() -> int{
        return 1;
    }
}

fun main() -> int{
    var a: Struct_name = ...;
    let _ = 
      a.field                   // This is a `Member`
    let _ = 
      a:Impl_name.Member_func() // This is a `MemberCall`
    return 0;
}

```


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

Example:

```
trait Drawable {
    public draw() -> void;
};
```

### Impl

```ebnf
ImplDecl = "impl" identifier "{" FunDecls "}" 
         | "impl" identifier "for" identifier "{" FunDecls "}" .
FunDecls = {FunDecl} .
```

Example:

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


## Built-in functions

Next step: we want to develop a print function spcifically for string such that it can be used for furthur debugging.

```
print_str(to_print : str) -> void
```

We also want to implement a function for language user to get to know type of a expression.

```
print_typeof(...) -> void
```