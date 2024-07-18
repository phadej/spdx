1.1

- Support GHC-9.6.5...9.10.1
- Use better algorithm for expression equivalence and preorder
  (It's still very slow compared to the state of the art SAT solvers:
   it cannot solve the sudoku example in reasonable time;
   but it's still a lot faster than the naive algorithm used previously)
- Remove "Distribution.SPDX.Extra.Internal" module

1

- Move to use `Distribution.SPDX` (from `Cabal`) types.

0.2.2.0

- Update license list to v2.6
- GHC-8.2 compatible release
