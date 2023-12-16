# Rooc

> Yuanfei Wang yuanfei.wang@tufts.edu  
> Xinyang Zhao xinyang.zhao@tufts.edu  
> Mona Ma zhifei.ma@tufts.edu  

## how to run

The project directory is like

```bash
./
├── Makefile
├── README.md
├── bin
│   ├── ast.ml
│   ├── builtins.ml
│   ├── codegen.ml
│   ├── dune
│   ├── parser.mly
│   ├── rooc.ml
│   ├── sast.ml
│   ├── scanner.mll
│   ├── semantic.ml
│   ├── structualIR.ml
│   └── util.ml
├── doc
│   └── Rooc.md
├── dune-project
├── lib
│   └── dune
├── testall.sh
└── tests
    ├── fail-function1.rooc
    ├── ...
    └── test-success-struct2.rooc
```

### compile

Run `make` in this root directory could build our **Rooc** compiler.

Run `dune build` also works.

### Run all tests

Run `make test` will execute our test script `testall.sh`.
This script can also directly be executed by `./testall.sh`.

### compile on a specific file

Use `./testall.sh -k <filename>` to compile a specific file.

### Clean generated files

Use `make clean` to delete our **Rooc** compiler.

## Tests

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

