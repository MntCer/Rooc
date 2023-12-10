# Working_note

## Nov 15

### How to analyze a "field"?

Traits and structures and all impl should have its own scope. When try to analyze, can loop up for "struct" name and then look down for "method" name.

### what type should a pathExpr has? 

> //TODO

And also for other "uncommon" expr.

## Dec 7

Today, we aim to successfully run our project end2end.

* [x] function's code generation part.
* [ ] builtin functions' codegen
* [x] more about expression codegen.


## Target List

* [x] need to implement "2 traverse" to support forward reference.
* [x] block_expr's semantic analysis part.
* [x] analyse function's parameters.
* [x] special treatment for main function
* [ ] inefficient code in the two pass analysis for items.