# The Rooc Language Reference

## Introduction

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
token1 = (a|\<)
token2 = <pattern1>(b|\>)
```

The `token2` expression expands to `(a|<)(b|>)` in standard ERE.

In ERE, a raw space character is a valid element. 
However, in most cases, Rooc will not allow to include a "space" as part of lexical tokens(not for string literal, comments, etc.) So, the spaces present in the following regular expressions are for readability and don't denote actual space characters.
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
digit   = [0-9]
digits  = digit+
letter  = [a-zA-Z]
```

### Comments

Comments will not be parsed by the compiler. Rooc has two comment styles:

1. _Line comments_ start with the character sequence `//` and stop at the end of the line.
2. _General comments_ start with the character sequence `/*` and stop with the first subsequent character sequence `*/`.

```
line_comment    = \/\/.*(\r?\n)
general_comment = \/\*.*?\*\/
comment         = <line_comment>|<general_comment>
```

### Semicolon

To allow complex statements to occupy a single line, Rooc use semicolon as the terminator of a statement. 

```
semi =;
```

### Identifiers

Identifiers name program entities such as variables and functions. An identifier is a sequence of one or more letters, digits and underscores, but the *first* character must be a letter.

```
identifier = <letter>(<letter>|<digit>|_)*
```

### Operators and punctuation

```
assign  = =
plus    = \+
minus   = -
times   = \*
divide  = /
lparen  = \(
rparen  = \)

eq      = ==
neq     = !=
lt      = <
leq     = <=
gt      = \>
geq     = \>=
and     = &&
or      = \|\|
not     = !

lbrace  = {
rbrace  = }
comma   = ,
```

### Keywords

The following keywords are reserved and may not be used as identifiers.

```
// ;TODO
var = var 
fun = fun 
let = let 

break
case
```

### Integer literals

An integer literal is a sequence of digits representing an integer constant. Now only support decimal integer.

```
// ;TODO
```


### Floating-point literals

A floating-point literal is a decimal of a floating-point constant.


```
// ;TODO
```


### String literals

A string literal represents a string constant obtained from concatenating a sequence of characters.

```

```

## Constants


## Variables


## Types

### Boolean types

### Numeric types

### String types

### List types


<!-- ;TODO
Struct types
Function types
trait types
 -->

## Expressions

## Statements

## Built-in functions