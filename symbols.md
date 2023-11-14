
This is the rough specification for now.

# Symbols

- `-`  swap
- `+`  over
- `*`  copy
- `~`  drop
- `>`  ptr-read
- `<`  ptr-write
- `//` one line comment
- `/*` inline comment
- `#`  ???
- `$`  formatting
- `%`  ???
- `&`  addr-of
- `?`  type-of
- `!`  size-of
- `:`  access
- `.`  addr-of-word
- `,`  ignored, just sugar

# Brackets

- `<>` ???
- `()` inline tuple
- `{}` code block
- `[]` attribute

# Keywords

- `fn` fn definition
- `do` anonymus fn definition
- `new` new named type
- `impl` named type implementation
- `if` if condition
- `elif` else-if condition
- `else` else condition
- `for` for loop
- `loop` while-true loop
- `break` break out of loop
- `ret` return from fn
- `[move]` move read variable
- `[cascading]` define cascadig variable
- `constructor` constructor fn definition
- `destructor` destructor fn definition

# Types

## Basic types

- `int` unsigned integer
- `bool` boolean
- `char` byte sized character
- `byte` single byte
- `ptr` ptr to another type

## Concretely sized types

- `u8`
- `u16`
- `u32`
- `u64`

- `i8`
- `i16`
- `i32`
- `i64`

## Compound types

- `array` collection of homogenus values
- `tuple` collection of different types
- `struct` tuple with named fields
- `variant` tagged union of different types
- `enum` tagged union with named fields
- `union` unchecked variant type

## Function pointers

- `Fn` a function pointer

A function pointer can be concretely typed.
`Fn of (int int -> int)`
`Fn` will return it's signature as it's generics list, including the `->`.

## Other builtin types

- `Type` the type of types (typeof(int))
- `zero` monostate zero-sized type

## Hint types for signatures

- `any` (any) signal to `sign` that any type is valid here
- `variadic` (variadic) signal to `sign` that this signature is typechecked by the user
- `->` (arrow) signal to `sign` that the following values are the retured
- `of` (of) signal to `sign` that the following value are the generics

The values of `any, variadic, arrow and of` are all their own types, which have exactly
one state and are all zero-sized.

## Self type

- `self` defined by the type implemented inside `impl`

### Example

new any (zero)

impl any {
    constructor new (-> self) {
        zero
    }
}

new Box (struct of (
    ptr of any [member] = data
    ptr of Allocator comptime-only [member] = allocator
))

impl Box {
    constructor new (any -> self) {
        ... member= data
        allocator~ member= allocator
        struct:from-members
    }
    fn from-1 (any -> self) {
        self:new
    }
    destructor (self) {
        // deallocate memory
        // something like this
        data move~ allocator move~ :dealloc
    }
}

42 Box:new
Box:repr
Box:into-inner

# Notes

All of this is work in progress.

