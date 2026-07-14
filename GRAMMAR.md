# Boolean Expression Grammer

```text
EXPR -> TERM ("+" EXPR)?
TERM -> LIT ("*"? TERM)?
LIT -> "-" LIT | VAR
VAR -> [_a-zA-Z0-9]+ | "(" EXPR ")"
```
