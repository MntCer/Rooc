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
digit     = "0" … "9" .
letter    = "a" … "z" | "A" … "Z" .
character = /* All printable ASCII characters from value 0 to 127 */ .
```

The form `a … b` represents the set of characters from a through b as alternatives. The horizontal ellipsis `…` is also used elsewhere in the reference to informally denote various enumerations or code snippets that are not further specified.  

The `/* … */` is one of Rooc's comment styles, which will also be used in the rules to give explicit descriptions and avoid too many enumerations.

### Comments


### Tokens
### Semicolons
### Identifiers
### Keywords
### Operators and punctuation
### Integer literals
### Floating-point literals
### String literals
### Constants
### Variables

## Types

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