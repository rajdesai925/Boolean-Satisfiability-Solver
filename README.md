# Boolean-Satisfiability-Solver

This program builds a parse tree given a boolean formula. It further converts the parse tree into an abstract syntax tree and uses this tree to return a list of all value assignments which would make the formula evaluate to true.

The grammar for the logical formula is defined as:

  < S > ::= TRUE
  < S > ::= FALSE
  < S > ::= a | b | · · · | z
  < S > ::= ( <T> )
  < T > ::= not <S>
  < T > ::= and <S> <S>
  < T > ::= or <S> <S>
