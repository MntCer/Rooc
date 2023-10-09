# Rooc

## Quick Start

<!-- ;TODO -->

## Syntax

### Variables

```plaintext
var <var_name>:<type>=<value>;

var a:int = 1;
var b:int = 2;
var c:float = 3.0;
var d:string = "a";
```

### Data type

#### primitive types

`int`, `float`, `string`

#### compound types

##### built-in list

Because we didn't include type inference now, we need to explicitly specfy the type of list. All the element in the list should be with same type.

```
var <list_name>:<type> = [<ele_list>];
<ele_list>=<ele>,<ele>,...
<ele>:=
    <var_id>
   |<literal>

var list_a:int = [1,2,3];
var list_b:int = [a,b];
```

### Function

```
fun <fun_name> (<param_list>) -> <return_type> {<function_body>}
<param_list>::=<param>:<type>,....

fun get_second (x:int,y:float) -> float
{
    return y;
}
```

### Control flow

We didn't treat the `if-else` as expression, just like C.
The curly braces are required to avoid potential problem.

```
if(<expr>){
    <stmt_list>
}
<elif(<expr>){
    <stmt_list>
}>
<else{
    <stmt_list>
}>
```

Rooc support 2 kinds of loop design.  

```
for(<expr>;<expr>;<expr>){
    <stmt_list>
}

while (<expr>){
    <stmt_list>
}
```


<!-- ``` ;TODO: need to support the type-inference first;
for(<id> in <list_id>){
    <stmt_list>
}
``` -->

### Struct

<!-- ;TODO: encapsulation -->

```
struct <struct_name>
{
    <var_declaration_stmt_list>
}

struct Point {
    var m:int;
    var n:int;
}
```

```
impl <id> {
    <fun_definition_stmt_list>
}

impl Point {
    fun new(x:int, y:int) -> Point { 
        this.x = x;
        this.y = y;
        return this;
    }

    fun getX() -> int {
        return this.x;
    }

    fun getY() -> int {
        return this.y;
    }
}
```

#### Polymorphism

In Rooc, polymorphism is supported through the use of traits, rather than class inheritance. 
Traits define a set of methods that multiple structs can implement. This allows for type-safe, flexible code without the complications that inheritance can bring.

```

trait <trait_id> {
    <fun_declaration_stmt_list>;
}

trait Drawable {
    public void draw();
}
```

```
impl <trait_id> for <struct_id> {
    <fun_definition_stmt_list>;
} 

// Implement the Drawable trait for the Point struct
impl Drawable for Point {
    fun draw() -> void {
        // Drawing logic for Point
    }
}

// Implement the Drawable trait for the Circle struct
struct Circle {
    var x:int;
    var y:int;
    var radius:int;
}

impl Drawable for Circle {
    fun draw() -> void {
        // Drawing logic for Circle
    }
}
```

With traits, you can write functions that operate on any type that implements a specific trait:

```c
fun render(d: Drawable) -> void {
    d.draw();
}

var p:Point = Point.new(1, 2);
var c:Circle = Circle.new(3, 4, 5);

render(p);  // Calls Point's draw method
render(c);  // Calls Circle's draw method
```

## TODO

- [ ] Choose a LRM presentation style.