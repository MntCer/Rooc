# The Rooc Language Reference

## Program structure

<!-- %TODO: the cross-compile part -->

## Notation

### Syntax notation

The syntax grammar is specified using [Wirth syntax notation](https://en.wikipedia.org/wiki/Wirth_syntax_notation) to express the Extended Backus-Naur Form (EBNF) in those monospaced font blocks:

```ebnf
Syntax      = { Production } .
Production  = PRODUCTION_NAME "=" [ Expression ] "." .
Expression  = Term { "|" Term } .
Term        = Factor { Factor } .
Factor      = PRODUCTION_NAME
            | LITERAL
            | Group 
            | Option 
            | Repetition

Group       = "(" Expression ")" .
Option      = "[" Expression "]" .
Repetition  = "{" Expression "}" .
```

The equals sign indicates a production. The element on the left is defined to be the combination of elements on the right. 

A production is terminated by a full stop (period).

Elements in capitals quoted in `""` are used to identify lexical (terminal) tokens. Non-terminals are in CamelCase. 

The three operators below are in increasing precedence.

```ebnf
|   alternation
()  grouping
[]  option (0 or 1 times)
{}  repetition (0 to n times)
```

### Lexical notation

The lexical grammar is primarily described using [POSIX extended regular expressions](https://en.wikibooks.org/wiki/Regular_Expressions/POSIX-Extended_Regular_Expressions)(ERE) in Math box.

$$
\begin{align*}
\textbf{DIGIT}      & : \texttt{['0'-'9']} \\
\textbf{INTEGER}    & : \textbf{DIGIT}\texttt{+} \\
\end{align*}
$$

<!-- %TODO: polish this paragraph -->
A colon indicates a lexical token's definition. The symbols in textwriter font represent operators in ERE regular expression. Strings in bold font refer to named lexical tokens. Symbol in $\texttt{'}$ denotes a single character in Rooc's alphabet.

Worth noting, standard ERE will match space character in input expression. But in this manual, all space characters are just used for formatting and readability. Rooc will use $\backslash \texttt{s}$ to denote a space character in token's definition.

## Lexical elements

### Alphabet

<!-- %TODO: rethink, there are still unclear parts. For special characters, do some expriements. -->

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

<!-- %TODO: rewrite -->
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

The `PRODUCTION_NAME` referred before also follow this rule.

### Semicolon

To allow complex statements to occupy a single line, Rooc use semicolon as the terminator of a statement or a declaration. 
<!-- #TODO: not semicolon for block -->

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

Note: When assigning objects, passing them to functions, or copying values, our current implementation only supports a shallow copy. This means that the objects or values involved will reference the same memory location rather than creating an entirely independent duplicate.

### Keywords

The following keywords are reserved and may not be used as identifiers.

```      
const       var         let         fun         struct      impl        trait  
self        int         float       bool        str         list
return      if          else        for         while       break       continue
true        false
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

<!-- %TODO: same name? -->
A file will be compiled as a `Module`. A module is a container for zero or more items. Each module has its own separate namespace.

```ebnf
Module = { 
    Item 
} .
```

A module that contains a main function in its root scope can be compiled to an executable.

<!-- %TODO: special limitation on main function. -->
```rust
fn main() -> int {};
```

## Item

<!-- %TODO:
```ebnf
    | UseDeclaration
    | TypeAlias
    | Enumeration
    | Union
    | StaticItem
    | ExternBlock
``` -->

```ebnf
Item =
      Function
    | ConstantValue
    | Struct
    | Trait
    | Implementation .
```

Items are entirely determined at compile-time, generally remain fixed during execution, and may reside in read-only memory.

### Constant item

```ebnf
ConstantValue =
    "const" IDENTIFIER ":" Type "=" Expr ";" .
```

A constant value is not associated with a specific memory location in the program. Constants must be explicitly typed and initialized.
<!-- %TODO: Constants are essentially inlined wherever they are used, meaning that they are copied directly into the relevant context when used. -->

### Function

<!-- %TODO: FunctionQualifier -->
<!-- %TODO: Generic params -->

```ebnf
Function = "fun" identifier "(" [ FunctionParams ] ")" "->" Type Block .

FunctionParams = Param { "," Param } [ "," ] .

Param = identifier ":" Type .

Method = "fun" identifier "(" [ MethodParams] ")" "->" Type Block ";".

MethodSignature = "fun" identifier "(" [ MethodParams] ")" "->" Type .

MethodParams = "self" { "," Param } [ "," ] .
```

A function consists of a block, along with a name, a set of parameters, and an output type. 

Functions may declare a set of input variables as parameters, through which the caller passes arguments into the function, and the output type of the value the function will return to its caller on completion. The parameters are optional.

<!-- %TODO: revise the words -->
Function must have a function body. 

Method works like function, the only difference is the use of `self` as parameter. Method can only be defined in a `impl` block or a `trait` block.


Functions and methods could be forward referenced, that is to say, as long as the called function or method is visiable in the called scope, this call is valid.

<!-- %TODO: yield a first-class function value -->
Example:

```rust
fun get_first (x:int,y:float) -> int {
    return x;
}
```

#### Function parameters

<!-- %TODO: Do after the struct type is cleared -->
If the first parameter is a `self`, this indicates that the function is a method. 
Functions with a `self` parameter may only appear as an associated function in a trait or implementation.

#### Function body

The block of a function is conceptually wrapped in a block that binds the argument and then returns the value of the function's block. 

#TODO: add reference to block in statements



### Struct


```ebnf
Struct =
    "struct" IDENTIFIER "{" [ StructFields ] "}" .
StructFields =
    StructField {"," StructField } [","] .
StructField =
    IDENTIFIER ":" Type .
```

```
struct Point {
    m:int,
    n:int,
}
```
<!-- %TODO: ref -->
A struct is a nominal struct type defined with the keyword `struct`.
<!--%TODO: memory layout-->

### Trait

```ebnf
Trait =
    "trait" IDENTIFIER "{" 
        #TODO: not decided yet.
    "}".

```

A trait describes an abstract interface that types can implement.

<!-- 
#TODO
types
constants -->

<!-- #TODO: ref -->
Traits are implemented for specific types through separate implementations.

<!-- #TODO: ref -->
All functions are public visibility by default.

Trait functions may omit the function body by replacing it with a semicolon. This indicates that the implementation must define the function. If the trait function defines a body, this definition acts as a default for any implementation which does not override it.

Example:

```
trait Drawable {
    fun draw() -> ();
};
```

### Implementation

```ebnf
Implementation =
    InherentImpl 
  | TraitImpl .

InherentImpl =
    "impl" Type "{" { AssociatedItem } "}" .

<!-- %TODO: not identifier, but type -->
TraitImpl =
    "impl" IDENTIFIER "for" IDENTIFIER
        "{" { AssociatedItem } "}"
```
An implementation is an item that associates items with an implementing type. Implementations are defined with the keyword `impl` and contain methods that belong to an instance of the type that is being implemented or to the type statically.


Example:

```rust
impl Point {
    fun new(x:int, y:int) -> Point { 
        self.x = x;
        self.y = y;
        return self;
    }

    fun getX() -> int {
        return self.x;
    }

    fun getY() -> int {
        return self.y;
    }
}
```



## Statements

```ebnf
Statements :
     Statement { Statement } .
```

<!--%TODO: not right now  -->
Statements serve mostly to contain and explicitly sequence expression evaluation. A statement is a component of a block, which is in turn a component of an outer expression or function.


```ebnf
Statement :
      ";"
    | DeclarationStmt
    | ExprStmt 
    | ForStmt
    | WhileStmt
    | ContinueStmt
    | BreakStmt
    | IfStmt 
    | ReturnStmt 
    | Block .
```
<!-- %TODO: now just allow first-level item
   | Item 
-->

### Declaration statement

A declaration statement is one that introduces one or more names into the enclosing statement block. The declared names may denote new variables.

```ebnf
DeclarationStatement = VarDeclaration
                     | LetDeclaration .
VarDeclaration       = "var" identifier ":" Type [ "=" Expr ] ";" .
LetDeclaration       = "let" identifier ":" Type [ "=" Expr ] ";" .
```

A `let` or `var` statement introduce a new set of variables. A variable is a storage location for holding a value. The set of permissible values is determined by the variable's type.

Any variables introduced by a variable declaration are visible from the point of declaration until the end of the enclosing block scope, except when they are shadowed by another variable declaration.

The variable introduced by a `let` statement is immutable, and `var` variable is mutable.

### Expression statement

```ebnf
ExprStmt = Expr ";" .
```

### Block

```ebnf
Block = "{" [ Statements ] "}" .
```

A block is a sequences of statements and anonymous namespace scope for items and variable declarations. As an anonymous namespace scope, item declarations are only in scope inside the block itself and variables declared by let statements are in scope from the next statement until the end of the block.

The syntax for a block starts with a "`{`" along with any number of statements, and finally ends with "`}`".

When evaluating a block each statement is executed sequentially.

### Return Statement

```ebnf
ReturnExpr =
   "return" Expr .
```

Return expressions are denoted with the keyword `return`. Evaluating a return expression moves its argument into the designated output location for the current function call, destroys the current function activation frame, and transfers control to the caller frame.


### For Statement

```ebnf
ForStmt   = 
    "for" "(" Expr ";" Expr ";" Expr ")" Block .
```

### While expression

```ebnf
WhileStmt = 
    "while" "(" Expr ")" Block .
```

<!-- ;TODO: need to support the type-inference first;--> 


`ForStmt`'s behavior can be explained with `WhileStmt` since they are interchangeable.

The following two snippets are equivalent:

```rust
start_condition;
while(end_condition){
    do_something();
    step_update;
};
```

```rust
for(start_condition; end_condition; step_update){
    do_something();
};
```


### Break Statement

```ebnf
BreakStmt =
   "break" ";".
```

When break is encountered, the current loop is immediately terminated, returning control to the next statement after the loop body.

### Continue Statment

```ebnf
ContinueStmt =
   "continue" ";".
```

When continue is encountered, the current iteration of the associated loop body is immediately terminated, returning control to the loop head.

<!-- ``` 
for(<id> in <list_id>){
    <stmt_list>
}
``` -->

### If Statment

%TODO: this one could be changed into more convient form

```ebnf
IfStatement = 
    "if" "(" Expr ")" Block 
        "else"  Block .
```

## Expressions

Most forms of value-producing or effect-causing evaluation are directed by the uniform syntax category of *expression*s in Rooc.

```ebnf
Expr =
    LiteralExpr
  | OperatorExpr
  | GroupedExpr
  | StructExpr        #TODO
  | CallExpr
  | MethodCallExpr    #TODO
  | FieldAccessExpr . 

```

Expressions are divided into two main categories: place expressions and value expressions. Which is, the left value and the right value.
Within each expression, operands may likewise occur in either place context or value context. The evaluation of an expression depends both on its own category and the context it occurs within.

#TODO: add each category's expression.


### Literal expression

```ebnf
LiteralExpr =
     STRING_LITERAL
   | INTEGER_LITERAL
   | FLOAT_LITERAL
   | BOOL_LITERAL .
```

A literal expression is an expression consisting of a single token, rather than a sequence of tokens, that immediately and directly denotes the value it evaluates to, rather than referring to it by name or some other evaluation rule.

A literal is a form of constant expression, so is evaluated (primarily) at compile time.


### Operator expression

```ebnf
OperatorExpr =
    UnaryExpr
  | ArithmeticExpr
  | LogicalExpr
  | ComparisonExpr
  | AssignmentExpr .
```

#### unary expression

```ebnf
UnaryExpr =
    "-" Expr 
  | "!" Expr .
```

#### Arithmetic or logical expression

```ebnf
ArithmeticxExpr =
    Expr "+" Expr
  | Expr "-" Expr
  | Expr "*" Expr
  | Expr "/" Expr .

LogicalExpr =
  | Expr "&&" Expr
  | Expr "||" Expr .
```

#### Comparison expression

```ebnf
ComparisonExpr =
    Expr "==" Expr
  | Expr "!=" Expr
  | Expr ">" Expr
  | Expr "<" Expr
  | Expr ">=" Expr
  | Expr "<=" Expr .
```

#### Assignment expression

<!-- #TODO: Not right now -->

```ebnf
AssignmentExpr =
    Expr "=" Expr .
```

### Grouped expression

```ebnf
GroupedExpr = 
    "(" Expr ")" .
```

### Call expression

```ebnf
CallExpr =
   ID "(" { CallParams } ")" .

CallParams =
   Expr { "," Expr } [ "," ] .
```

A call expression calls a function.

The syntax of a call expression is an expression, called the function operand, followed by a parenthesized comma-separated list of expression, called the argument operands.

### Field access expression

```ebnf
FieldAccessExpr =
    IDENTIFIER "." IDENTIFIER .
```

A field expression is a place expression that evaluates to the location of a field of a struct. When the operand is mutable, the field expression is also mutable.

The syntax for a field expression is an expression, then a `.`, and finally an identifier. 
Field expressions cannot be followed by a parenthetical comma-separated list of expressions, as that is instead parsed as a method call expression. That is, they cannot be the function operand of a call expression.


<!-- 

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

``` -->


## Type

```ebnf
Type = PrimitiveType 
    | GenericType 
    | identifier .
```

### Primitive types

<!-- ;TODO: detailed type introduction -->
We implement common primitive types.

```ebnf
PrimitiveType   = 
    Int 
  | Float 
  | Bool 
  | String 
  | Unit .
Int             = "int" .
Float           = "float" .
Bool            = "bool" .
String          = "str" .
Unit            = "()" .
```

### Generic type

```ebnf
GenericType = List .
List        = "list" "(" Type ")" .
```

Elements in a list should have the same type. The list's implementation is similar to how C++ manages vectors, ensuring that elements are stored contiguously, facilitating easy access and traversal through iterators. When inserting new elements, if the list's size exceeds its capacity, a reallocation process is initiated. This involves allocating a new block of memory, relocating the existing elements to this new space, and subsequently releasing the old memory locations. This dynamic reallocation mechanism ensures the list can dynamically grow in size while maintaining efficient memory usage.

### Struct type

<!-- %TODO: -->

## Name analysis

### Visibility

## Built-in functions

We want to develop a print function spcifically for string such that it can be used for furthur debugging.

```
fun print_str(to_print : str) -> ();
```

<!-- We also want to implement a function for language user to get to know type of a expression.

```
print_typeof(...) -> ()
``` -->