# Rooc

We sincerely apologize for the delay in completing the assignment due to modifications in the architecture design, which led to it taking longer than expected. 
As a result, the version we are submitting cannot actually generate LLVM IR. Additionally, many language features that should be supported in the syntax analysis phase are not fully implemented. 
However, the submission includes test cases for the language features we intend to implement, along with related testing scripts for your review. But because no LLVM IR will be generated, the script cannot acutally generated a executable binary.
We plan to complete the actual implementation of the functionalities included in these test cases over this weekend, ensuring that the overall progress of the project ultimately meets expectations.

## Tests

### fail-function1

```
fun main() -> int {
    print_int(undefined_function());
};
```

Should report when undefined function is called.

No `return` expr in body but do have a return type is not allowed.

### fail-scope1

```
fun main()->int{
    print_int(x);
    var x=1;

    return 0;
};
```

There is no forward reference in function scope, only sequential order. Referenced variable must be declared before use.

### fail-struct1

```
struct Point {
    x: int;
    y: int;
};

fun main() -> {
    var p = Point {x = 1, y = 2};
    print_int(p::z);  // 'z' is not a field of Point

    return 0;
};
```

should report when undefined field is accessed.

### fail-type1

```
fun main() -> int {
    var a:int = "This is not an integer";
    print_int(a);

    return 0;
};
```

Type mismatch.


### test-success-builtins1

```
fun main() -> int {
    print_int(1);
    return 0;
};
```

Test for the built-in function `print_int`.

### test-success-expr1

```
struct Test {
    field1:int;
    field2:float;
};

fun main() -> int {
    var test = Test {field1 = 1;field2 = 1.0};
    print_int(test::field1);
    return 0;
};
```

Test for the path expression, which will be evaulated to a struct's field.

### test-success-function

```
fun square(x:int) -> int {
    return x * x;
};

fun sum_of_squares(a:int, b:int) -> int {
    return square(a) + square(b);
};

fun main() -> int {
    var result = sum_of_squares(3, 4);
    print_int(result);
    return 0;
};
```

Nested function calls.

### test-success-scope1

```
fun main()->int {
    var x:int = 1;
    x=2;
    {
        var x:int = 1;
        print_int(x);
    }
    return 0;
};
```

A block expression come with a new scope.

### test-success-scope2

fun main() -> int {
    test(); // forward reference
};


fun test() -> () {
    print_int(1);
    return 0;
};

The highest level scope (only has items in it) allowed forward reference, so we can call `test()` in `main` function.

### test-success-struct1

```
struct Test {
    field1:int;
    field2:float;
};

fun main() -> int {
    var test = Test {field1 = 1;field2 = 1.0};
    return 0;
};
```

Instantiate a struct.

### test-success-struct2

```
struct Point {
    x: int;
    y: int;
};

fun create_point(x:int, y:int) -> Point {
    return Point {x = x, y = y};
};

fun main() -> int {
    var p = create_point(3, 4);
    print_int(p::x);
    print_int(p::y);
    return 0;
};

```

Test return a struct.

## Compile and automatic script

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


### Names and Email addresses

Yuanfei Wang yuanfei.wang@tufts.edu  
Xinyang Zhao xinyang.zhao@tufts.edu  
Mona Ma zhifei.ma@tufts.edu  