LTL formula - Automata converter
====

## Build

just run

```
make
```

## Run

```
./ltlac
```

Input arbitrary LTL formula. Syntax is as follows:

```
t ::= T | ! | [a-z]+ | ~t | t/\t | t\/t | Xt | Ft | Gt | tUt | tRt
```

where
```
  T      : top
  !      : bottom
  [a-z]+ : atomic propositional variable
  ~t     : not
  t/\t   : and
  t\/t   : or
  Xt     : next
  Ft     : future
  Gt     : globally
  tUt    : until
  tRt    : release
```

Also '(', ')' (parenthesis) can be used.
