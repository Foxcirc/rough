
# How to add a new Token

1. Add a new variant to the Token enum

2. Add a new variant to the TokenKind enum

3. Go into `src/lexer/possible.rs` and:
   - Make the internal array of `Possible` bigger.
   - Update the comment above `Possible`'s declaration.
   - Edit the `Index` implementations.
   - If the token could have more then one character. Edit the `multichar` function.

4. Modify the `build` function in `src/lexer/lexer.rs` so the `TokenKind` can be parsed.

5. Add a new rule inside `src/lexer/check.rs` for the `TokenKind`.

6. Make sure to edit the `terminator` function and read through it, because the valid terminators may have to be modified.

7. Change the token systhesis bihaviour inside `lexer.rs` (currently line 130) if this is a multiline token.

8. *Hope* that it works.

*Note: As long as no changes to `check.rs` are made, nothing should happen.*
