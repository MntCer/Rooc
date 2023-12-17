# Rooc


## how to run

The basic dependencies are

* Opam 2.1.5
* dune 3.10.0
* Menheir 2.0
* LLVM 10.0.0
* gcc 9.4.0
* GNU Make 4.2.1

Run `make` in this root directory could build our **Rooc** compiler.

Run `dune build` also works.

### Run all tests

Run `make test` will execute our test script `testall.sh`.
This script can also directly be executed by `./testall.sh`.

### compile on a specific file

Run `make run /path/to/file.rooc` will compile the file and generate a executable file `/file.exe` in `./out/file/` directory.


### Clean generated files

Use `make clean` to delete our **Rooc** compiler.


## Tests illustration.

To find the test cases used as demo, please refer to `./tests_to_pre/` directory.

### negative cases

```
test-fail-arith.rooc       
// type mismatch in arithmatic expression         

test-fail-call1.rooc       
// function params number mismatch

test-fail-call2.rooc       
// type mismatch in call expression

test-fail-comp1.rooc       
// unsupported comparion in expression

test-fail-function1.rooc   
// call undefined function

test-fail-logic.rooc       
// logic op on non-bool type

test-fail-scope1.rooc      
// for variable, use before declaration.

test-fail-struct0.rooc     
// same name field in struct

test-fail-struct1.rooc     
// error in struct expression

test-fail-struct2.rooc     
// declare a variable in type of undefined struct

test-fail-struct3.rooc     
// error field name in struct expression

test-fail-struct4.rooc     
// type mismatch in struct expression

test-fail-struct5.rooc     
// didn't have field name in struct expression

test-fail-var.rooc         
// try to mutate a immutable variable

```

### positive cases

```
test-success-call.rooc
// function call

test-success-compare.rooc
// comparision expression

test-success-concatstr.rooc
// builtin functon

test-success-fib.rooc
// fibonacci

test-success-float-arith.rooc
// basic arithmatic expression

test-success-ftoi.rooc
//builtin function

test-success-function.rooc
// recursive call

test-success-grouped.rooc
// basic parenthesis function

test-success-int-arith.rooc
// basic arithmatic expression

test-success-itof.rooc
// builtin function

test-success-nested-callexpr.rooc
// complex call expression

test-success-plus.rooc
// basic plus expression

test-success-printbool.rooc
// builtin function

test-success-printfloat.rooc
// builtin function

test-success-printint.rooc
// builtin function

test-success-printstr.rooc
// builtin function

test-success-rec-callexpr.rooc
// complex call expression

test-success-returnunit.rooc
// return empty expression

test-success-scope1.rooc
// basic scope mechanism

test-success-scope2.rooc
// forward reference

test-success-struct0.rooc
// struct def

test-success-struct1.rooc
// any order in struct expr

test-success-struct2.rooc
// field access

test-success-struct3.rooc
// recursive field access

test-success-struct4.rooc
// complex function of field access

test-success-struct5.rooc
// mutual reference

test-success-struct6.rooc
// recursive field access

test-success-struct7.rooc
// more complex example of field access and function.

test-success-unary.rooc
// basic unary

test-success-var.rooc
// let variable

test-success-while.rooc
// basic while loop
```