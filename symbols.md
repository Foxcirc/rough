
This is the rough specification for now.

# Symbols

- `-`  swap
- `+`  over
- `*`  copy
- `~`  ptr-read
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

# Brackets

- `<>` inline tuple
- `()` ???
- `{}` code block
- `[]` ???

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
- `move` move read variable (move~)
- `cascading` define cascadig variable (cascading=)
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

## Other builtin types

- `type` the type of types (typeof(int))
- `zero` monostate zero-sized type

## Hint types for signatures

- `any` signal to `sign` that any type is valid here
- `variadic` signal to `sign` that this signature is typechecked by the user
- `->` signal to `sign` that the following values are the retured

## Self type

- `self` defined by the type implemented inside `impl`

new any <zero>

impl any {
    constructor new <-> self> {
        zero
    }
}

new Box <struct<
    ptr<any> member= data
    ptr<Allocator> comptime member= allocator
>>

impl Box {
    constructor new <any -> self> {
        ... member= data
        allocator~ member= allocator
        struct:from-members
    }
    fn from-1 <any -> self> {
        Box:new
    }
    fn repr <int -> str> {
        
    },
    destructor <self> {
        // deallocate memory
        // something like this
        data move~ allocator move~ :dealloc
    }
}

42 Box:new
Box:repr
Box:into-inner

