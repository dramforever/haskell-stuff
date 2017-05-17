# haskell-stuff
My Haskell snippets

- `de-bruijn-a-la-carte` a.k.a Compiling Combinators if you heard it from @MarisaKirisame
- `negation-kanren` is an implementation of miniKanren with inequality constraints (And it interprets syntax trees so nested negation just, damn, works (At least it should))
- `tiny-compiler` is a [Tiny 3-pass compiler](https://www.codewars.com/kata/tiny-three-pass-compiler)
- `smiley` is a POC for using the GHC optimizer to prove equality of Haskell expressions. **Not for anything other than a POC.** Inspired by [ghc-proofs]( https://github.com/nomeata/ghc-proofs), but uses rewrite rules so it's more lightweight, albeit less powerful