# Boolean Expression Grammer

```text
EXPR -> DISJ ({"<-", "->", "<->"} EXPR)?
DISJ -> CONJ ("+" DISJ)?
CONJ -> LIT ("*"? CONJ)?
LIT  -> "-" LIT | VAR
VAR  -> [_a-zA-Z0-9]+ | "(" EXPR ")"
```
