# Rooc

### How to Compile

The project directory is like
```bash
.
├── README.md
├── bin
│   ├── ast.ml
│   ├── dune
│   ├── parser.mly
│   ├── rooc.ml
│   └── scanner.mll
├── dune-project
├── lib
│   └── dune
├── testall.sh
└── tests
    ├── fail-func
    ├── fail-func.rooc
    ├── fail-if
    ├── fail-if.rooc
    ├── fail-impl
    ├── fail-impl.rooc
    ├── fail-struct
    ├── fail-struct.rooc
    └── ......
```

Run the command `dune build` in this project root directory could build our **Rooc** project.

### How to Execute a Single .rooc File

Run `dune exec Rooc ./tests/test-success-bool.rooc` after `dune build`.

### How to Run Test Script

Assume a file in `./tests/` names `a.rooc`, the standard test result of it is the file named `a`.


Use the `./testall.sh` to run the test.

`./testall.sh [options] [.rooc files]`

options are:  
`-k`: Keep intermediate files(`.out` and `.err`)  
`-h`: Print this help  

If no `.rooc` files are given, all `.rooc` files in the tests directory will be tested.

Test results will be found in `testall.log`  

### Syntax need to be added

- let for defining constants
- Built-in List
- Declaration of "var" at initialization
- In functions, local variables can be declared anywhere instead of only at the top

### Names and Email addresses

Yuanfei Wang yuanfei.wang@tufts.edu  
Xinyang Zhao xinyang.zhao@tufts.edu  
Mona Ma zhifei.ma@tufts.edu  