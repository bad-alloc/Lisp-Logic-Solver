Lisp-Logic-Solver
=================

Lisp program which finds solutions for arbitrary logical statements.

Run find-solution with any logical statement as a s-expression to get solutions:

```
CL-USER> (find-solution '(or (and a (not b)) (and b c)))
solution found: A = F, B = T, C = T
solution found: A = T, B = F, C = F
solution found: A = T, B = F, C = T
solution found: A = T, B = T, C = T
NIL
CL-USER> (find-solution '(implies a b))
solution found: A = F, B = F
solution found: A = F, B = T
solution found: A = T, B = T
NIL
```
