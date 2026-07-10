# Boolean Expression Grammer

```text
E -> T ("+" E)?
T -> F ("*"? T)?
F -> L
L -> "-"? V
V -> [_a-zA-Z0-9]+ | "(" E ")"
```
