# Brim Language Specification

## Types and Values

### Primitive Types

| Category            | Type    | Description                                        |
|---------------------|---------|----------------------------------------------------|
| Integers (Signed)   | `i8`    | 8-bit signed integer                               |
|                     | `i16`   | 16-bit signed integer                              |
|                     | `i32`   | 32-bit signed integer                              |
|                     | `i64`   | 64-bit signed integer                              |
|                     | `isize` | Platform-dependent signed integer (pointer size)   |
| Integers (Unsigned) | `u8`    | 8-bit unsigned integer                             |
|                     | `u16`   | 16-bit unsigned integer                            |
|                     | `u32`   | 32-bit unsigned integer                            |
|                     | `u64`   | 64-bit unsigned integer                            |
|                     | `usize` | Platform-dependent unsigned integer (pointer size) |
| Floating Point      | `f32`   | 32-bit floating-point number                       |
|                     | `f64`   | 64-bit floating-point number                       |
| Other               | `bool`  | Boolean (`true` or `false`)                        |
|                     | `char`  | Unicode character                                  |
|                     | `void`  | Represents no value                                |

### Special Values

- `undefined`: Represents an uninitialized value

### Compound Types

#### Tuples

Ordered collections of values with different types:

```brim
let point: (i32, i32) = (10, 20)
let record: (string, i32, bool) = ("Alice", 25, true)
```

#### Arrays

Fixed-size collections of same-type values:
`Vec` is a dynamic array that can change its size. It's a part of the standard library.

```brim
let numbers: i32[5] = [1, 2, 3, 4, 5]
```

> Arrays are useful when you know the size of the collection at compile time and know that it won't change.

- Accessing elements in an array is done using the `[]` operator.

```brim
let array: int[5] = [1, 2, 3, 4, 5]
let first = array[0]
```

⚠️ **Array safety**:

- Bounds checking is enforced at runtime
- Out-of-bounds access triggers a panic
- Size must be known at compile time

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
fn add(a: int, b: int) -> int {
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

### Errors

`!` after the return type denotes that the function can return an error.

```brim
fn parse(input: string) -> string !MyError {
     if input.len() <= 0 {
         return @err(MyError::ParsingError {
             message: "Input is empty",
             span: Span::new(0, 0)
         })
     }
     
     return @ok(input)
}
```

When returning from a non-error function, you can simply return the value.

```brim
fn add(a: int, b: int) -> int {
    return a + b
}
```

But when returning from an error function, you need to use the `@ok` to return a success value and `@err` to return an
error.

```brim
fn div(a: int, b: int) -> int !string {
    if b == 0 {
        return @err("Division by zero")
    }
     
    return @ok(a / b)
}
```

> You can specify the error type after the `!` symbol.

### Built-in functions

Built-in functions are functions that are provided by the compiler itself. They are prefixed with the `@` symbol.

| Function | Description                                      |
|----------|--------------------------------------------------|
| `@print` | Prints a value to the provided descriptor.       |
| `@ok`    | Creates a success value with the provided value. |
| `@err`   | Creates an error with the provided value.        |
| `@panic` | Panics the program with the provided message.    |

## Errors

### Errors types

You can return error of any type. It can be a simple string or a custom error type. You can do that using the [
`@err`](#built-in-functions)
built-in function.

```brim
enum MyError {
    ParsingError {
        message: string,
        span: Span
    }
}

fn parse(input: string) -> string !MyError {
     ...
}
```

```brim
fn parse(input: string) -> string !string {
     ...
}
```

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

## Enums

I have a warm place in my heart for rust enums. They are simple and powerful.

Enums can have associated values.

```brim
enum Result<T, E> {
    Ok(T),
    Err(E)
}
```

or just a simple enum.

```brim
enum Color {
    Red,
    Green,
    Blue
}
```