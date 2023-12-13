# Working_note

## Nov 15

### How to analyze a "field"?

Traits and structures and all impl should have its own scope. When try to analyze, can loop up for "struct" name and then look down for "method" name.

### what type should a pathExpr has? 

> //TODO

And also for other "uncommon" expr.

## Dec 7

Today, we aim to successfully run our project end2end.

* [ ] function's code generation part.
* [ ] builtin functions' codegen
* [ ] more about expression codegen.

## Dec 10

* [x] skeleton of function codegen.
* [x] int arith -> expression codegen.
* [x] builtin functions -> codegen and sement.
* [x] call_expr -> codegen.
* [x] call_expr -> sement
* [ ] check the test's standard output.

## Dec 11

* [x] expr -> id
* [ ] check no return if 'void'
* [ ] "return;"

# Dec 12

## For box and reference type

Semantic Analysis:

For Box, you'll manage heap allocation and ownership semantics.

Box cannot take a reference.

Code Generation:

In Rust, Box<T> is a smart pointer that allocates T on the heap and owns it. need to decide how Box manages the lifecycle of the object it owns, especially regarding allocation and deallocation.

## Target List

* [x] need to implement "2 traverse" to support forward reference.
* [x] block_expr's semantic analysis part.
* [x] analyse function's parameters.
* [x] special treatment for main function
* [ ] inefficient code in the two pass analysis for items.