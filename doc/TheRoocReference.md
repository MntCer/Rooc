# The Rooc Language Reference

## Introduction

## Notation

The syntax is specified using [Wirth syntax notation](https://en.wikipedia.org/wiki/Wirth_syntax_notation) to express the Extended Backus-Naur Form (EBNF):

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

Lowercase production names are used to identify lexical (terminal) tokens. Non-terminals are in CamelCase. 
Tokens which are enclosed in double quotes "" are also lexical tokens.

The three operators below are in increasing precedence.

```ebnf
|   alternation
()  grouping
[]  option (0 or 1 times)
{}  repetition (0 to n times)
```

## Lexical elements

### Characters, Letters and Digits

```ebnf
// lexical rules
digit     = "0" … "9" .
digits    = digit { digit } .
letter    = "a" … "z" | "A" … "Z" .
character = /* All printable ASCII characters from value 0 to 127 */ .
```

The notation `a … b` denotes the set of characters ranging from `a` to `b` inclusively. The horizontal ellipsis `…` is employed elsewhere in this reference as an informal representation of unspecified enumerations or code snippets. 

In Rooc, comments are denoted using `/* … */` and `//`. These comment styles will also be utilized in this reference to provide clearer rule explanations or to simplify lengthy enumerations.


### Comments

Comments will not be parsed by the compiler. There are two forms:

1. _Line comments_ start with the character sequence `//` and stop at the end of the line.
2. _General comments_ start with the character sequence `/*` and stop with the first subsequent character sequence `*/`.

```ebnf
// lexical rules
eol       = "\n" | "\r\n" .
comment = line_comment | general_comment .
line_comment = "//" { character } eol .
general_comment = "/*" { character } "*/" .
```

### Semicolon

To allow complex statements to occupy a single line, Rooc use semicolon as the terminator of a statement. 

```ebnf
// lexical rules
semi = ";" .
```

### Identifiers

Identifiers name program entities such as variables and functions. An identifier is a sequence of one or more letters, digits and underscores, but the first character must be a letter.

```ebnf
// lexical rules
identifier = letter { letter | digit | "_" } .
```

### Operators and punctuation

```
// lexical rules
assign = "=" .
plus = "+" .
minus = "-" .
times = "*" .
divide = "/" .
lparen = "(" .
rparen = ")" .

eq= "==" .
neq = "!=" .
lt = "<" .
leq = "<=" .
gt = ">" .
geq = ">=" .
and = "&&" .
or = "||" .
not = "!" .

lbrace = "{" .
rbrace = "}" .
comma = "," .
```

### Keywords

The following keywords are reserved and may not be used as identifiers.

```ebnf
// lexical rules
// ;TODO
break
case
```

### Integer literals

### Floating-point literals

### String literals

### Constants


### Variables


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