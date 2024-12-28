# Whitespace in Brim

## Definition

Whitespace refers to any non-empty string containing only specific Unicode characters with the [
`Pattern_White_Space`](https://www.unicode.org/reports/tr31/)
property.

## Supported Whitespace Characters

| Character | Unicode | Description         | Common Notation |
|-----------|---------|---------------------|-----------------|
| `' '`     | U+0020  | Space               | `' '`           |
| `\t`      | U+0009  | Horizontal Tab      | `'\t'`          |
| `\n`      | U+000A  | Line Feed           | `'\n'`          |
| `\r`      | U+000D  | Carriage Return     | `'\r'`          |
| `\v`      | U+000B  | Vertical Tab        |                 |
| `\f`      | U+000C  | Form Feed           |                 |
|           | U+0085  | Next Line           |                 |
|           | U+200E  | Left-to-Right Mark  |                 |
|           | U+200F  | Right-to-Left Mark  |                 |
|           | U+2028  | Line Separator      |                 |
|           | U+2029  | Paragraph Separator |                 |

## Usage in Brim

Brim is a "free-form" language, which means:

1. Whitespace serves only to separate tokens in the grammar
2. Whitespace has no semantic significance
3. Programs have identical meaning regardless of which whitespace characters are used

### Examples

The following code snippets are semantically identical:

```brim
// Compact formatting
fn main(){@typeof("Hello")}

// Standard formatting
fn main() {
    @typeof("Hello")
}

// Excessive whitespace
fn    main     ( )    {
    @typeof(  "Hello"  )
}
```