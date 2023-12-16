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
//         

test-fail-call1.rooc       
//

test-fail-call2.rooc       
//

test-fail-comp1.rooc       
//

test-fail-function1.rooc   
//

test-fail-logic.rooc       
//

test-fail-scope1.rooc      
//

test-fail-struct0.rooc     
//

test-fail-struct1.rooc     
//

test-fail-struct2.rooc     
//

test-fail-struct3.rooc     
//

test-fail-struct4.rooc     
//

test-fail-struct5.rooc     
//

test-fail-type1.rooc       
//

test-fail-var.rooc         
//

```

### positive cases

