> ⚠️ Work in progress.

## Introduction

This project aims to implement generic versions of the two most common classes of type checking algorithms — bidirectional typing and Hindley-Milner-style type inference — within the [Free Foil](https://github.com/fizruk/free-foil) framework [^1].

Current goals are:

1. Design and implement generic type system notions (such as typed terms, type schemes, typing contexts) within the Free Foil framework [^1].
2. Implement generic bidirectional typing over user-defined typing rules for the signature of an object language.
3. Implement the _Pfenning recipe_ [^3] [^4], automating the selection of checking and synthesis judgements and simplifying the user-defined typing rules.
4. Generalize Hindley-Milner type system [^5] [^6] and Damas-Milner type inference [^7] to SOAS.
5. Implement an efficient (and language-agnostic) level-based `let`-generalization à la Rémy [^8].
6. Implement the constraint handling of $HM(X)$ [^9] withing the Free Foil framework, with generic instances for type class and subtyping constraints.
7. Demonstrate algorithms by implementing typecheckers for several languages (such as simplified Haskell [^10], Tog [^11], and Stella [^2]), benchmarking our implementation against existing typecheckers.

## Current progress

Our work in this repository contains implementations of bidirectional typing (System F) and Hindley-Milner-style type inference. Currently, we're working on implementing type checking and type inference algorithms for a concrete small language.

The repository has the following directory structure:
- `grammar/` — BNFC grammars describing concrete language syntaxes.
- `src/` — source code of type inference and type checking algorithms.
- `app/` — source code of REPL and interpreter binaries for playing with the current implementations.
- `test/` — scripts and test-cases with input programs for testing type checkers functionality.
- `bench/` — a benchmark of generalisation in Hindley–Milner inference.

Contents of the mentioned directories are divided for Hindley-Milner, MiniML and System F implementations. MiniML has no REPL or interpreter in `app/`.

## Level-based generalisation in Hindley–Milner inference

The Hindley–Milner type inference (`src/FreeFoilTypecheck/HindleyMilner/Inference.hs`) generalises the types of `let`-bound expressions using levels, following Rémy [^8] and the presentation by Kiselyov [^12]. Instead of scanning the typing environment for free unification variables at every `let`, the inference works as follows:

- every unification variable records the level at which it was created;
- the bound expression of a `let` is inferred one level deeper (`enterLevel`);
- when unification binds a variable to a type, it performs an occurs check and lowers the levels of the variables in that type to the level of the bound variable;
- after the bound expression is inferred, its type is generalised over the unification variables whose level is above the current one.

Intuitively, a unification variable whose level is above the current one does not occur in the typing environment, so it is safe to generalise it.

The test programs in `test/FreeFoilTypecheck/HindleyMilner/files/` include the examples from Kiselyov's article (`kiselyov_*.lam`). Each well-typed program has its expected type in a `*.expected.lam` file.

## Generic Hindley–Milner engine

`src/FreeFoilTypecheck/GeneralTypecheck.hs` implements Hindley–Milner type inference once, for any object language whose syntax is generated with Free Foil. A language provides the signatures of its terms and types, one typing rule per node of its term signature (an instance of `HMTypingSig`) and the typing rules of its patterns (an instance of `HMTypingPattern`): a pattern checked against a type gives the types of its variables. The engine provides unification variables, unification with an occurs check, instantiation of type schemes and level-based generalisation.

Typing rules receive the children of a node as suspended computations, so a rule decides when, and at which level, each child is inferred. For example, the rule for `let` infers the bound term inside `generalizeHM`, one level deeper. The documentation of the class `HMTypingSig` has the details and the alternative design that we did not choose. Naive generalisation (quantifying the variables that do not occur in the typing environment, as in Damas and Milner [^7]) is available as `Generalization = Naive`, for comparison.

Two languages use the engine:

- the Hindley–Milner language (`src/FreeFoilTypecheck/HindleyMilner/Rules.hs`). Its REPL and interpreter still use the language-specific inference described in the previous section (`HindleyMilner/Inference.hs`). The generic engine is run by the tests and the benchmark;
- MiniML (`grammar/miniml.cf`, `src/FreeFoilTypecheck/MiniML/`), with patterns, `case`, pairs, sums, lists, `fix`, `letrec`, `let`, λ-abstractions, `if`, naturals, booleans and type annotations. One pattern language (the wildcard, variables, `inl`, `inr`, `[]`, `::` and pairs, nested arbitrarily) serves the branches of `case` and the binders of λ, `let`, `letrec` and `fix`. The branches of `case` are in braces: `case e of { p1 -> e1 | p2 -> e2 }`. Its name, its core (λ, `let`, `letrec`, `if` and pairs) and the patterns of λ, `let` and `letrec` follow Mini-ML [^13], and the other features are the simple extensions of the typed λ-calculus in Pierce's book [^14] (chapter 11). Adding it needed the grammar, the Free Foil syntax (`MiniML/Syntax.hs`) and the typing rules (`MiniML/Rules.hs`). The syntax is generated with Free Foil's `mkFreeFoil`.

MiniML has no REPL or interpreter. To infer the type of a MiniML program, load the library in GHCi:

```sh
stack ghci free-foil-typecheck:lib
```

```haskell
ghci> :m FreeFoilTypecheck.GeneralTypecheck FreeFoilTypecheck.MiniML.Rules
ghci> either id showHMType (inferMiniML LevelBased "letrec map = λf. λl. case l of { [] -> [] | x :: xs -> f x :: map f xs } in map")
"forall x0 . (forall x1 . (x0 -> x1) -> List x0 -> List x1)"
```

(`:m` is needed because both `HindleyMilner/Rules.hs` and `MiniML/Rules.hs` define `showHMType`.) The test programs are in `test/FreeFoilTypecheck/MiniML/files/`. Each well-typed program has its expected type in a `*.expected.ml` file.

The tests of the generic engine run every Hindley–Milner and every MiniML test program in both generalisation modes (`GeneralTypecheckSpec` and `MiniML/RulesSpec`). Differential tests check that the generic engine with levels, the generic engine with naive generalisation, the language-specific inference and the specialised engine (see below) agree on every Hindley–Milner test program and on random Hindley–Milner terms. For MiniML, which has no language-specific inference, they check that the two generalisation modes agree on random terms. To run only the MiniML tests or only the differential tests:

```sh
stack test free-foil-typecheck:spec --test-arguments='--match MiniML'
stack test free-foil-typecheck:spec --test-arguments='--match Differential'
```

`src/FreeFoilTypecheck/HindleyMilner/SpecializedInference.hs` is the generic engine with levels, specialised by hand to the Hindley–Milner language: the same algorithm, on a first-order type of its own. It is a baseline for the cost of genericity, since the language-specific inference (the original implementation) lacks several optimisations of the generic engine.

The benchmark `generalization` (`bench/Main.hs`) times the four Hindley–Milner engines with [tasty-bench](https://hackage.haskell.org/package/tasty-bench) on programs with many nested `let`s (three families of programs, `nested-let`, `wide-env` and `let-chain`, with 160 to 1280 `let`s). It reports the mean time of each engine with twice the standard deviation, and the times of the other three engines relative to the generic engine with levels on the same program (e.g. `5.54x`). Run it with `stack bench`, or choose a family with a pattern and save the results as CSV:

```sh
stack bench free-foil-typecheck:bench:generalization --benchmark-arguments='-p nested-let --csv bench.csv'
```

## Building and testing

The project is built with [Stack](https://docs.haskellstack.org/):

```sh
stack build
stack test
stack bench
```

The REPLs read one expression per line, and the interpreters read a program from the standard input:

```sh
stack run repl-hm                # Hindley–Milner
stack run repl-sf                # System F
stack run interpreter-hm < test/FreeFoilTypecheck/HindleyMilner/files/well-typed/kiselyov_16.lam
```

---

[^1]: Nikolai Kudasov, Renata Shakirova, Egor Shalagin, and Karina Tyulebaeva. 2024. Free Foil: Generating Efficient and Scope-Safe Abstract Syntax. In 2024 4th International Conference on Code Quality (ICCQ). 1–16. https://doi.org/10.1109/ICCQ60895.2024.10576867
[^2]: Abdelrahman Abounegm, Nikolai Kudasov, and Alexey Stepanov. 2024. Teaching Type Systems Implementation with Stella, an Extensible Statically Typed Programming Language. In Proceedings of the Thirteenth Workshop on Trends in Functional Programming in Education, South Orange, New Jersey, USA, 9th January 2024 (Electronic Proceedings in Theoretical Computer Science, Vol. 405), Stephen Chang (Ed.). Open Publishing Association, 1–19. https://doi.org/10.4204/EPTCS.405.1
[^3]: Jana Dunfield and Neel Krishnaswami. 2021. Bidirectional Typing. ACM Comput. Surv. 54, 5, Article 98 (May 2021), 38 pages. https://doi.org/10.1145/3450952
[^4]: Jana Dunfield and Frank Pfenning. 2004. Tridirectional typechecking. SIGPLAN Not. 39, 1 (Jan. 2004), 281–292. https://doi.org/10.1145/ 982962.964025
[^5]: R. Hindley. 1969. The Principal Type-Scheme of an Object in Combinatory Logic. Trans. Amer. Math. Soc. 146 (1969), 29–60. http://www.jstor.org/stable/1995158
[^6]: Robin Milner. 1978. A theory of type polymorphism in programming. J. Comput. System Sci. 17, 3 (1978), 348–375. https://doi.org/10.1016/
0022-0000(78)90014-4
[^7]: Luis Damas and Robin Milner. 1982. Principal type-schemes for functional programs. In Proceedings of the 9th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (Albuquerque, New Mexico) (POPL ’82). Association for Computing Machinery, New York, NY, USA, 207–212. https://doi.org/10.1145/582153.582176
[^8]: Didier Rémy. 1992. Extension of ML type system with a sorted equation theory on types. Research Report RR-1766. INRIA. https://inria.hal.science/inria-00077006 Projet FORMEL.
[^9]: Martin Odersky, Martin Sulzmann, and Martin Wehr. 1999. Type inference with constrained types. Theory and practice of object systems 5, 1 (1999), 35–55.
[^10]: Mark P Jones. 1999. Typing Haskell in Haskell. In _Haskell workshop_, Vol. 7.
[^11]: Francesco Mazzoli and Andreas Abel. 2016. Typechecking through unification. arXiv:1609.09709 [cs.PL] https://arxiv.org/abs/1609.09709
[^12]: Oleg Kiselyov. 2013. How OCaml type checker works – or what polymorphism and garbage collection have in common. https://okmij.org/ftp/ML/generalization.html
[^13]: Dominique Clément, Joëlle Despeyroux, Thierry Despeyroux, and Gilles Kahn. 1986. A simple applicative language: Mini-ML. In Proceedings of the 1986 ACM Conference on LISP and Functional Programming (LFP ’86). Association for Computing Machinery, New York, NY, USA, 13–27. https://doi.org/10.1145/319838.319847
[^14]: Benjamin C. Pierce. 2002. Types and Programming Languages. MIT Press.
