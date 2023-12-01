# Working_note

## Nov 15

### How to analyze a "field"?

Traits and structures and all impl should have its own scope. When try to analyze, can loop up for "struct" name and then look down for "method" name.

### what type should a pathExpr has? 

> //TODO

And also for other "uncommon" expr.

## Target

* [x] need to implement "2 traverse" to support forward reference.
* [x] block_expr's semantic analysis part.
* [ ] init function's argument into the function's scope.
* [ ] special treatment for main function
* [ ] function's code generation part.
* [ ] more features about expressions.
* [ ] inefficient code in the two pass analysis for items.