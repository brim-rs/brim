# This file specifies how the language looks.

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
