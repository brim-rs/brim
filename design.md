# This file specifies the language design of Brim.

## Values

### Primitives

| Type    | Description                    |
|---------|--------------------------------|
| `i8`    | 8-bit signed integer           |
| `i16`   | 16-bit signed integer          |
| `i32`   | 32-bit signed integer          |
| `i64`   | 64-bit signed integer          |
| `i128`  | 128-bit signed integer         |
| `u8`    | 8-bit unsigned integer         |
| `u16`   | 16-bit unsigned integer        |
| `u32`   | 32-bit unsigned integer        |
| `u64`   | 64-bit unsigned integer        |
| `u128`  | 128-bit unsigned integer       |
| `usize` | Pointer-sized unsigned integer |
| `isize` | Pointer-sized signed integer   |
| `f16`   | 16-bit floating-point number   |
| `f32`   | 32-bit floating-point number   |
| `f64`   | 64-bit floating-point number   |
| `f128`  | 128-bit floating-point number  |
| `bool`  | Boolean. `true` or `false`     |
| `void`  | No return value                |
| `char`  | Unicode character              |

### `undefined` value

`undefined` is a special value that represents an uninitialized value.

### Compound types

#### Tuples

Tuples are a collection of values of different types.

```brim
let tuple = (1, "Hello", 3.14)
```

#### Arrays

Arrays are a collection of values of the same type with a fixed size.
`Vec` is a dynamic array that can change its size. It's a part of the standard library.

```brim
let array = [1, 2, 3, 4, 5]
```

```brim
let array: int[5] = [1, 2, 3, 4, 5]
```

> Arrays are useful when you know the size of the collection at compile time and know that it won't change.

- Accessing elements in an array is done using the `[]` operator.

```brim
let array: int[5] = [1, 2, 3, 4, 5]
let first = array[0]
```

⚠️ **Warning**: Accessing an element outside the bounds of the array will result in a panic. This prevents memory
issues.

## Comments

Basic comments are supported using the `//` symbol.

```brim
// This is a comment
pub fn main() {
    println("Hello, World!")
}
```

### Block comments

Block comments are used for documentation and start with `///`.

```brim
/// This function prints "Hello, World!"
pub fn main() {
    println("Hello, World!")
}
```

## Functions

Functions are defined using the `fn`, arguments list, and return type.

> Public functions are defined by prefixing the function with the `pub` keyword.

```brim
fn add(a: int, b: int): int {
    return a + b
}
```

### Return types

Return types are specified after the `->` followed by the type. Similar to many other languages.

```brim
fn add(a: int, b: int) -> int {
    return a + b
}
```

### Anonymous functions

Anonymous functions are defined using the `|` symbol.

```brim
fn add(a: int, b: int) -> int {
    let result = |a, b| a + b

    return result(a, b)
}
```

#### Errors

`!` after the return type denotes that the function can return an error.

```brim
fn parse(input: string) -> string !MyError {
     if input.len() <= 0 {
         return MyError::ParsingError {
             message: "Input is empty",
             span: Span::new(0, 0)
         }
     }
     
     return input
}
```

> You can specify the error type after the `!` symbol.

### Built-in functions

Built-in functions are functions that are provided by the compiler itself. They are prefixed with the `@` symbol.

## Errors

### Errors can be defined using the `error` keyword.

```brim
error MyError {
    ParsingError: {
        message: string,
        span: Span
    },
    UnknownError
}
```

> Error types are similar to struct definitions, but they are used to represent errors that can be returned from
> functions.

> A single error can be a simple enum, or a struct with multiple fields.

### Defining a function that returns an error

Look at the [Functions](#functions) section for more information.

### `try` keyword

Try evaluates the union type. If it's an error, it returns from the function. If it's a value, it unwraps it.

```brim
fn parse(input: string) -> string !MyError {
     let value = try parse(input) // If parse fails, it returns the error and stops execution of the function.
     println(value)
     return value
}
```

### `catch` keyword

Catch is used to handle the returned error if one is returned.

```brim
fn parse(input: string) -> string !MyError {
     let value = parse(input) catch |err| {
        println(err)
     }
     return value
}
```

> The `catch` expression has to return the same type as the `try` expression.

### `@panic` keyword

`@panic` is a built-in function that can be used to panic the program. It immediately stops the execution of the program
and returns valuable information about the error.

```brim
fn parse(input: string) -> string !MyError {
     if input.len() <= 0 {
         @panic("Input is empty")
     }
     
     return input
}
```
