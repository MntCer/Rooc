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
* [x] builtin functions' codegen
* [x] more about expression codegen.

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

## Dec 12

### For `Box`

Semantic Analysis:

For Box, you'll manage heap allocation and ownership semantics.

Box cannot take a reference.

Code Generation:

In Rust, Box<T> is a smart pointer that allocates T on the heap and owns it. need to decide how Box manages the lifecycle of the object it owns, especially regarding allocation and deallocation.

> Result: Box need complex ownership system, We don't do that, so just unsafe raw pointer.

### For struct

"struct in struct" can only be done by "include raw pointer".

### mut ref and mut ptr

```rust
let mut a = StructA { b: std::ptr::null_mut() };
let mut b = StructB { a: std::ptr::null_mut() };

a.b = &mut b as *mut StructB; // need 'as', not seemed as same type
b.a = &mut a as *mut StructA;
```

> Result: don't do any ref and ptr, it's too many problem in semantic phase.

### For anonymous struct

We requires that all types, including structs, be defined explicitly with a name before they can be instantiated and used.

## Dec 14

### for access field expr

Implementing a flattened path approach for field access can simplify the process. 
When encounter a field access expression like `a.b.c`, flatten it into a single path and resolve it in one go, rather than dealing with each field access individually. 
This requires maintaining context about the base struct and its nested field types.


## Dec 15

* [x] recursively access field
* [x] access and modify field


## Target List

* [x] need to implement "2 traverse" to support forward reference.
* [x] block_expr's semantic analysis part.
* [x] analyse function's parameters.
* [x] special treatment for main function
* [ ] inefficient code in the two pass analysis for items.