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

[^1]: Nikolai Kudasov, Renata Shakirova, Egor Shalagin, and Karina Tyulebaeva. 2024. Free Foil: Generating Efficient and Scope-Safe Abstract Syntax. In 2024 4th International Conference on Code Quality (ICCQ). 1–16. https://doi.org/10.1109/ICCQ60895.2024.10576867
[^2]: Abdelrahman Abounegm, Nikolai Kudasov, and Alexey Stepanov. 2024. Teaching Type Systems Implementation with Stella, an Extensible Statically Typed Programming Language. In Proceedings of the Thirteenth Workshop on Trends in Functional Programming in Education, South Orange, New Jersey, USA, 9th January 2024 (Electronic Proceedings in Theoretical Computer Science, Vol. 405), Stephen Chang (Ed.). Open Publishing Association, 1–19. https://doi.org/10.4204/EPTCS.405.1
[^3]: Jana Dunfield and Neel Krishnaswami. 2021. Bidirectional Typing. ACM Comput. Surv. 54, 5, Article 98 (May 2021), 38 pages. https: //doi.org/10.1145/3450952
[^4]: Jana Dunfield and Frank Pfenning. 2004. Tridirectional typechecking. SIGPLAN Not. 39, 1 (Jan. 2004), 281–292. https://doi.org/10.1145/ 982962.964025
[^5]: R. Hindley. 1969. The Principal Type-Scheme of an Object in Combinatory Logic. Trans. Amer. Math. Soc. 146 (1969), 29–60. http: //www.jstor.org/stable/1995158
[^6]: Robin Milner. 1978. A theory of type polymorphism in programming. J. Comput. System Sci. 17, 3 (1978), 348–375. https://doi.org/10.1016/
0022-0000(78)90014-4
[^7]: Luis Damas and Robin Milner. 1982. Principal type-schemes for functional programs. In Proceedings of the 9th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (Albuquerque, New Mexico) (POPL ’82). Association for Computing Machinery, New York, NY, USA, 207–212. https://doi.org/10.1145/582153.582176
[^8]: Didier Rémy. 1992. Extension of ML type system with a sorted equation theory on types. Research Report RR-1766. INRIA. https://inria.hal.science/inria-00077006 Projet FORMEL.
[^9]: Martin Odersky, Martin Sulzmann, and Martin Wehr. 1999. Type inference with constrained types. Theory and practice of object systems 5, 1 (1999), 35–55.
[^10]: Mark P Jones. 1999. Typing Haskell in Haskell. In _Haskell workshop_, Vol. 7.
[^11]: Francesco Mazzoli and Andreas Abel. 2016. Typechecking through unification. arXiv:1609.09709 [cs.PL] https://arxiv.org/abs/1609.09709
