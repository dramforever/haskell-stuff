#!/usr/bin/env stack
-- stack runghc

-- Idea by MarisaKirisame

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

import Data.Proxy

data SKI = S | K | I | SKI :@ SKI

instance Show SKI where
  show S = "S"
  show K = "K"
  show I = "I"
  show (a :@ b) = "(" ++ show a ++ " " ++ show b ++ ")"

-- An expression with an extra variable in scope
-- It's implemented in the underlying representation 'repr'
-- as a function from the variable to the expression
-- (Think 'Reader')
-- By enforcing proper nesting (by exporting less functions), we can
-- get a statically checked properly scoped lambda expression EDSL

-- Example: The 'f x y' part in '\f -> \x -> \y -> f x y' is implemented
-- as (Scope (Scope (Scope repr))) for some repr (There may be more
-- scopes outside)
data Scope repr = Scope { getScope :: repr }

-- We can get operations lifted into Scope (Like lifted into Reader)
-- using a typeclass.
class Lambda repr where
  (@-) :: repr -> repr -> repr
  s, k, i :: repr

infixl 9 @-

instance Lambda SKI where
  (@-) = (:@)
  s = S
  k = K
  i = I

instance Lambda repr => Lambda (Scope repr) where
  -- Use S combinator to lift application
  -- (Just like using '<*>' of Reader)
  Scope f @- Scope x = Scope (s @- f @- x)

  -- These three are like using 'pure' of Reader
  s = Scope (k @- s)
  k = Scope (k @- k)
  i = Scope (k @- i)

-- These two functions implement de Bruijn indices
var :: Lambda repr => Scope repr
var = Scope i

suc :: Lambda repr => repr -> Scope repr
suc x = Scope (k @- x)

-- Now we want something nice. We'd like to write this:
c :: SKI
c = lam $ \f -> lam $ \x -> lam $ \y -> f @- y @- x
-- So that each variable is passed in, like in HOAS, but are really
-- de Bruijn indexed variables, automatically adding 'suc' as needed.
-- We would need to pass in a pre-lifted variable, which changes among
-- 'var', 'suc var', 'suc (suc var)', etc. depending on how I use it.
--
-- Although each variable is used only once in 'c', using the same variable
-- with different lambdas in scope should be supported

-- If we had a pre-lifting function 'liftVar', this should do it:
lam f = case f $ liftVar (var :: Scope repr) of Scope r -> r
--                                     ^^^^ Bound through ScopedTypeVariables
--                                                       (^^^^^^ Nice pun!)

-- And since I already explained how 'lam' should work, let's behold its type:
lam :: forall repr. Lambda repr
    => ((forall k. LiftVar (Scope repr) k => k) -> Scope repr) -> repr
--      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--       You might want to ask, 'WTF is this?'
-- Well, as you can see, it's possible to tell how many 'suc's we need to use
-- by looking at how many more nesting the use site has, i.e. how many
-- lambdas are in between. LiftVar implements this counting process with
-- a typeclass.

-- Example: \f -> \x -> \y -> f x y
--           f-----^-----^----f      <- f is lifted through 2 lambdas
--                 x-----^------x    <- x is lifted through 1 lambda

-- And here's the typeclass

-- LiftVar u v means 'v' is 0 or more 'Scope's wrapped over 'u'
-- For example, LiftVar u (Scope (Scope (Scope u))) holds

type LiftVar u v = LiftVar' (Equal u v) u v

type family Equal a b :: Bool where
  Equal a a = 'True 
  Equal a b = 'False

class LiftVar' (check :: Bool) u v where
  liftVar' :: proxy check -> u -> v

instance LiftVar' 'True a a where
  liftVar' _ = id

instance (Lambda v, LiftVar u v) => LiftVar' 'False u (Scope v) where
  liftVar' _ = suc . liftVar

liftVar :: forall u v. LiftVar u v => u -> v
liftVar = liftVar' (Proxy :: Proxy (Equal u v))

-- We can indeed check that all of these work as intended
main = do
  print c
  let expected = "((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) I))))) ((S (K K)) (K I)))))) ((S ((S (K S)) ((S (K K)) (K K)))) (K I)))"
  putStrLn (if expected == show c then "Okay" else "Not okay")
