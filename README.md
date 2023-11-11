# Rooc

### How to Compile

The project directory is like

```bash
.
├── Makefile
├── README.md
├── bin
│   ├── ast.ml
│   ├── codegen.ml
│   ├── dune
│   ├── parser.mly
│   ├── rooc.ml
│   ├── sast.ml
│   ├── scanner.mll
│   └── semant.ml
├── dune-project
├── lib
│   └── dune
├── testall.sh
└── tests
    ├── test-success-printhello
    ├── test-success-printhello.rooc
    ├── test-success-printworld
    └── test-success-printworld.rooc
```

run `make` in this root directory could build our **Rooc** compiler.


### How to Run Test Script

run `make test` will execute our test script `testall.sh`.
This script can also directly be executed by `./testall.sh`. Use `./testall.sh -h` for more information.

### Clean generated files

If execute `./testall.sh` without `-k` flag, this script will clean all **Rooc** compiled results. Use `make clean` to delete our **Rooc** compiler.


### Names and Email addresses

Yuanfei Wang yuanfei.wang@tufts.edu  
Xinyang Zhao xinyang.zhao@tufts.edu  
Mona Ma zhifei.ma@tufts.edu  