# "TODO

search for `"TODO` to rename our language.

## Motivation

The C language, distinguished by its use of pointers, has proven both powerful and problematic. 
While we appreciate the elegance of C-style syntax, we aim to develop a more user-friendly, high-level language inspired by it. 
Our proposed language, "TODO, seeks to retain the syntactic style of C/C++ while eliminating pointers. 
It also aims to modernize the type system, eschewing inheritance in favor of traits, akin to Rust, to avoid the problems about subtype.

> Another point for discussion is the inconsistent initialization methods prevalent in C-family languages.

## Features

### Primitive Types

"TODO supports only three primitive types: int, float, and str. 
> not the final version.

Declaration Syntax:
```
int a = 1;
float b = 1.0;
str c = "hello world";
```

### support of built-in list?

TODO

> the two subsections above maybe too detailed to put in proposal.

###  ML-like Type System

"TODO features a strong, static type system optimized for static analysis. 

#### Custom Type

```c
// type alias
type Distance = int;
type Velocity = float;

// composite type
type Point = Point(int, int);
type Shape = Circle(Point, int) 
          or Rectangle(Point, Point);

// parameterized type
type List<T> = EmptyList() or Cons(T, List<T>);
```

#### static analysis

One of the compelling features of "TODO is its robust static analysis capabilities, designed to minimize runtime errors, improve performance, and assist in code maintenance.The static type system serves as the foundation for these static analysis features.

##### Type Safety

"TODO enforces strict type checking at compile-time, mitigating issues such as type coercion. 
Any type violations are flagged as compile-time errors, ensuring that type-related bugs do not propagate into the runtime environment.

##### Dead Code Elimination

The static analysis toolchain can identify and flag or remove unreachable code sections, reducing the size of the executable and improving performance.

##### Constant Propagation

Compile-time evaluation of constant expressions allows for optimizations where values are computed at compile-time instead of runtime, resulting in faster execution.

### Memory Management Without Pointers

One of the key goals of "TODO is to eliminate pointers to simplify the language and enhance safety. 
To compensate for the lack of pointers, "TODO introduces several high-level abstractions and safe alternatives.

#### Safe References

In "TODO, we introduce a Safe Reference type to substitute for pointers. The `Reference` type encapsulates the logic for referencing variables, while also performing safety checks to avoid issues like null pointer exceptions or dangling references.

> Concrete design needs further discussion.

#### Garbage Collection

Eliminating pointers means you can't manually free allocated memory. 
Therefore, "TODO would feature automatic memory management.

### OO feature

The Object-Oriented design of "TODO is influenced by the desire to simplify and modernize C++'s OO mechanisms. 
To provide encapsulation and polymorphism without inheritance and the issues it entails, we introduce Rust-like traits. 

#### struct

Unlike in C++, structs in "TODO do not contain methods within their definitions. 
This separation makes them function as composite types. 
Methods can be added to structs using a separate impl block, offering a clean separation of data and behavior.

```c
struct Point {
    private int x;
    public int y;
}

impl Point as Point_impl{
    public new(int x, int y) { 
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return this.x;
    }

    public int getY() {
        return this.y;
    }
}

// a uniform style to initialize struct with specified implementation
Point p = Point@Point_impl.new(1, 2);

// if no implementation is specified, a default implementation will be used
Point pp = Point.new(1, 2);
```

#### Polymorphism

In "TODO, polymorphism is supported through the use of traits, rather than class inheritance. 
Traits define a set of methods that multiple structs can implement. This allows for type-safe, flexible code without the complications that inheritance can bring.

```c
// Define a trait with methods that return values; no implementation is given
trait Drawable {
    public void draw();
}

// Implement the Drawable trait for the Point struct
impl Drawable for Point as Point_painter{
    public void draw() {
        // Drawing logic for Point
    }
}

// Implement the Drawable trait for a new Circle struct
struct Circle {
    public int radius;
}

impl Drawable for Circle as Circle_painter{
    public void draw() {
        // Drawing logic for Circle
    }
}
```

With traits, you can write functions that operate on any type that implements a specific trait:

```c
public void render(Drawable d) {
    d.draw();
}

Point p = Point.new(1, 2);
Circle c = Circle.new(5);

render(p);  // Calls Point's draw method
render(c);  // Calls Circle's draw method
```

> This form of polymorphism is resolved at compile-time, the runtime version still needs further discussion.

### Discussion of FP feature

The initial motivation for "TODO is to create a language that maintains the syntactical style of C/C++ while introducing modern features and omitting some of the older, more problematic aspects like pointers. 
However, while Functional Programming (FP) paradigms offer many advantages, integrating them into this language poses a few significant challenges:

1. Syntax Inconsistency: 
   C-style syntax, particularly function syntax, is not naturally conducive to supporting first-class functions without substantial alteration, it restrict the function's return type to primitive types.
   If we finally decide to add FP support, we may use another keyword like `func` to declare a function and use the type inference to infer it's type.
2. mutable and immutable variables:
   FP needs immutable variables, I am not sure about how to balance and design the grammar without bring more confusion.
