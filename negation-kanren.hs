#!/usr/bin/env stack
{- stack exec ghci
--package free
--package logict
--package microlens-platform
--package mtl
-}

{-# OPTIONS -Wall #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module NegationKanren where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Free

import Control.Monad.State
import Control.Monad.Logic

import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl

import Control.Applicative
import Data.Foldable
import Data.Maybe
import Data.String

newtype YieldT m a = YieldT { getYieldT :: m (Maybe a) }

instance Functor m => Functor (YieldT m) where
    fmap f = YieldT . (fmap . fmap) f . getYieldT

instance Applicative m => Applicative (YieldT m) where
    pure = YieldT . pure . pure
    YieldT f <*> YieldT x = YieldT $ liftA2 (<*>) f x

instance (Monad m) => Monad (YieldT m) where
    YieldT m >>= f = YieldT $ m >>= maybe (pure Nothing) (getYieldT . f)

instance Alternative m => Alternative (YieldT m) where
    empty = YieldT empty
    YieldT f <|> YieldT x = YieldT (f <|> x)

instance (Monad m, Alternative m) => MonadPlus (YieldT m)

instance MonadState s m => MonadState s (YieldT m) where
    state f = YieldT $ state (\u -> let (a, v) = f u in (Just a, v))

instance MonadLogic m => MonadLogic (YieldT m) where
    msplit (YieldT x) = YieldT (go x)
        where
            go m = msplit m >>= \case
                Nothing -> pure $ Just Nothing
                Just (Nothing, m') -> go m'
                Just (Just u, m') -> pure $ Just $ Just (u, YieldT m')
    interleave (YieldT a) (YieldT b) = YieldT (interleave a b)

-- By marking recursive cases with @yield@, we can allow other alternatives
-- to have a chance in case they succeed before we do
class MonadLogic m => MonadYield m where
    yield :: m a -> m a

instance MonadLogic m => MonadYield (YieldT m) where
    yield (YieldT m) = YieldT $ pure Nothing <|> m

instance MonadYield m => MonadYield (StateT s m) where
    yield (StateT m) = StateT $ \u -> yield (m u)

runYieldT :: (Monad m, Alternative m) => YieldT m a -> m a
runYieldT (YieldT m) = m >>= maybe empty pure

class ExactZip f where
    exactZipWith :: (a -> b -> c) -> f a -> f b -> Maybe (f c)

exactZip :: ExactZip f => f a -> f b -> Maybe (f (a, b))
exactZip = exactZipWith (,)

data ExprF lbl a
    = Cons lbl [a]
    deriving (Eq, Functor, Foldable, Traversable)

instance (Show lbl, Show a) => Show (ExprF lbl a) where
    show (Cons lbl []) = show lbl
    show (Cons lbl args) = show lbl ++ show args

instance (Eq lbl) => ExactZip (ExprF lbl) where
    exactZipWith f (Cons lbl1 args1) (Cons lbl2 args2)
        | lbl1 == lbl2 && length args1 == length args2 =
            Just $ Cons lbl1 (zipWith f args1 args2)
        | otherwise =
            Nothing

data Var = Var Int
    deriving (Eq, Ord)

instance Show Var where
    show (Var n) = "#" ++ show n

type HoleExpr f = Free f Var

data Res f
    = NotEqualTo (S.Set Var)
    | Complex (f Var)

deriving instance Eq (f Var) => Eq (Res f)
deriving instance Show (f Var) => Show (Res f)

data Link f
    = UpLink Var
    | NoLink (Res f)

deriving instance Eq (f Var) => Eq (Link f)
deriving instance Show (f Var) => Show (Link f)

data UnificationState f
    = UState
    { _usSupply :: Int
    , _usVarMap :: M.Map Var (Link f)
    }

deriving instance Show (f Var) => Show (UnificationState f)

usSupply :: Lens' (UnificationState f) Int
usSupply f us@UState {_usSupply = n} =
    (\newn -> us {_usSupply = newn}) <$> f n

usVarMap :: Lens' (UnificationState f) (M.Map Var (Link f))
usVarMap f us@UState {_usVarMap = vm} =
    (\newvm -> us {_usVarMap = newvm}) <$> f vm

type MonadUFS f m = MonadState (UnificationState f) m

initialState :: UnificationState f
initialState = UState {_usSupply = 0, _usVarMap = M.empty}

freshVar :: MonadUFS f m => m Var
freshVar = Var <$> (usSupply <%= (+1))

findRep :: MonadUFS f m => Var -> m (Var, Maybe (Res f))
findRep u = use (usVarMap . at u) >>= \case
    Nothing -> pure (u, Nothing)
    Just (NoLink res) -> pure (u, Just res)
    Just (UpLink next) -> findRep next >>= \case
        (root, res) -> do
            usVarMap . at u ?= UpLink root
            pure (root, res)

repNotEqual
    :: (MonadUFS f m, Alternative m, ExactZip f, Foldable f)
    => Var -> Res f -> m ()
repNotEqual u rv =
    case rv of
        NotEqualTo nev -> traverse_ (disunify u) nev
        _ -> pure ()

unify
    :: (MonadUFS f m, Alternative m, ExactZip f, Foldable f)
    => Var -> Var -> m ()
unify u1 v1 = do
    (u, ru) <- findRep u1
    (v, rv) <- findRep v1
    if u == v
        then pure ()
        else do
            traverse_ (repNotEqual u) rv
            traverse_ (repNotEqual v) ru
            case (ru, rv) of
                (Nothing, _) ->
                    usVarMap . at u ?= UpLink v
                (_, Nothing) ->
                    usVarMap . at v ?= UpLink u
                (Just (Complex fu), Just (Complex fv)) -> do
                    -- TODO Refactor this
                    maybe empty sequence_ (exactZipWith unify fu fv)
                    usVarMap . at u ?= UpLink v
                (Just (Complex _), _) ->
                    usVarMap . at v ?= UpLink u
                (_, Just (Complex _)) ->
                    usVarMap . at u ?= UpLink v
                (Just (NotEqualTo neu), Just (NotEqualTo nev)) -> do
                    usVarMap . at u ?= UpLink v
                    usVarMap . at v ?= NoLink (NotEqualTo (neu `S.union` nev))

addNotEqual :: Var -> Maybe (Res f) -> Res f
addNotEqual v Nothing = NotEqualTo (S.singleton v)
addNotEqual v (Just (NotEqualTo neu)) = NotEqualTo (v `S.insert` neu)
addNotEqual _v (Just (Complex fu)) = Complex fu

disunify
    :: (MonadUFS f m, Alternative m, ExactZip f, Foldable f)
    => Var -> Var -> m ()
disunify u1 v1 = do
    (u, ru) <- findRep u1
    (v, rv) <- findRep v1
    if u == v
        then empty
        else do
            case (ru, rv) of
                (Just (Complex fu), Just (Complex fv)) ->
                    maybe (pure ()) sequence_ (exactZipWith disunify fu fv)
                _ -> do
                    usVarMap . at u ?= NoLink (addNotEqual v ru)
                    usVarMap . at v ?= NoLink (addNotEqual u rv)

data Rule f
    = Equal (Free f Var) (Free f Var)
    | Fresh (Var -> Rule f)
    | And (Rule f) (Rule f)
    | Or (Rule f) (Rule f)
    | Yes
    | No
    | Not (Rule f)
    | Yield (Rule f)

interpret
    :: (MonadUFS f m, MonadYield m, Traversable f, ExactZip f)
    => Rule f -> m ()
interpret (Equal a b) = do
    va <- record a
    vb <- record b
    unify va vb
interpret (Fresh f) = freshVar >>= interpret . f
interpret (And a b) = interpret a *> interpret b
interpret (Or a b) = interpret a `interleave` interpret b
interpret Yes = pure ()
interpret No = empty
interpret (Not a) = interpretNeg a
interpret (Yield a) = yield $ interpret a

interpretNeg
    :: (MonadUFS f m, MonadYield m, Traversable f, ExactZip f)
    => Rule f -> m ()
interpretNeg (Equal a b) = do
    va <- record a
    vb <- record b
    disunify va vb
interpretNeg (Fresh f) = freshVar >>= interpretNeg . f    
interpretNeg (And a b) = interpretNeg a `interleave` interpretNeg b
interpretNeg (Or a b) = interpretNeg a *> interpretNeg b
interpretNeg Yes = empty
interpretNeg No = pure ()
interpretNeg (Not a) = interpret a
interpretNeg (Yield a) = yield $ interpretNeg a

record
    :: (MonadUFS f m, Traversable f)
    => Free f Var -> m Var
record = iterA $ \fpa -> do
    fvs <- sequence fpa
    v <- freshVar
    usVarMap . at v ?= NoLink (Complex fvs)
    pure v

report
    :: (MonadUFS f m, Traversable f)
    => Var -> m (Free f Var)
report = unfoldM $ \v1 -> do
    (v, rv) <- findRep v1
    case rv of
        Just (Complex fv) -> pure (Right fv)
        _ -> pure (Left v)

data Constraint f
    = Var :==. (f Var)
    | Var :/=. [Var]

deriving instance Show (f Var) => Show (Constraint f)

findTop :: (MonadUFS f m) => Var -> m Var
findTop v = fst <$> findRep v

dumpConstraints :: (MonadUFS f m, Traversable f) => m [Constraint f]
dumpConstraints = do
    m <- use usVarMap
    let go v =
            use (usVarMap . at v) >>= \case
                Nothing -> pure Nothing
                Just (UpLink _) -> pure Nothing
                Just (NoLink (NotEqualTo nev)) ->
                    (\k -> Just $ v :/=. S.toList (S.fromList k)) <$> traverse findTop (S.toList nev)
                Just (NoLink (Complex fv)) ->
                    (\k -> Just $ v :==. k) <$> traverse findTop fv

    catMaybes <$> traverse go (M.keys m)

cons :: lbl -> [Free (ExprF lbl) a] -> Free (ExprF lbl) a
cons lbl xs = Free (Cons lbl xs)

atom :: lbl -> Free (ExprF lbl) a
atom s = cons s []

var :: Var -> Free f Var
var = Pure

newtype Label
    = Label String
    deriving (Eq)

instance Show Label where
    show (Label s) = s

instance IsString Label where
    fromString = Label

type Expr = Free (ExprF Label) Var
type F = ExprF Label

class Fresh a where
    fresh :: (a -> Rule f) -> Rule f

instance Fresh Var where
    fresh = Fresh

instance (Fresh x, Fresh y) => Fresh (x, y) where
    fresh f = fresh $ \x -> fresh $ \y -> f (x, y)

instance (Fresh x, Fresh y, Fresh z) => Fresh (x, y, z) where
    fresh f = fresh $ \(x, y) -> fresh $ \z -> f (x, y, z)

instance (Fresh x, Fresh y, Fresh z, Fresh w) => Fresh (x, y, z, w) where
    fresh f = fresh $ \(x, y, z) -> fresh $ \w -> f (x, y, z, w)

run
    :: (MonadLogic m, ExactZip f, Traversable f)
    => (Var -> Rule f) -> m (Var, [Constraint f])
run x = runYieldT $ evalStateT go initialState
    where
        go = do
            v <- freshVar
            interpret (x v)
            (\k -> (v,k)) <$> dumpConstraints

appendo :: Expr -> Expr -> Expr -> Rule F
appendo xs ys zs =
    Or  
        (fresh $ \(u, v, qs) ->
            And (Equal xs (cons "c" [var u, var v]))
                (And    (Yield $ appendo (var v) ys (var qs))
                        (Equal zs (cons "c" [var u, var qs]))))
        (And    (Equal ys zs)
                (Equal xs (atom "n")))


main :: IO ()
main = mapM_ print (take 5 (run go))
    where
        go res =
            fresh $ \(a, b, c) ->
                Equal (var res) (cons "res" [var a, var b, var c])
                `And` Not (Equal (var b) (var c))
                `And` appendo (var a) (var b) (var c)

{-
(#1,[#3 :/=. [#11],#5 :==. res[#9,#3,#11],#9 :==. c[#6,#10],#10 :==. n,#11 :==. c[#6,#3]])
(#1,[#3 :/=. [#16],#5 :==. res[#9,#3,#16],#9 :==. c[#6,#13],#13 :==. c[#10,#14],#14 :==. n,#15 :==. c[#10,#3],#16 :==. c[#6,#15]
])
(#1,[#3 :/=. [#21],#5 :==. res[#9,#3,#21],#9 :==. c[#6,#13],#13 :==. c[#10,#17],#17 :==. c[#14,#18],#18 :==. n,#19 :==. c[#14,#3
],#20 :==. c[#10,#19],#21 :==. c[#6,#20]])
(#1,[#3 :/=. [#26],#5 :==. res[#9,#3,#26],#9 :==. c[#6,#13],#13 :==. c[#10,#17],#17 :==. c[#14,#21],#21 :==. c[#18,#22],#22 :==.
 n,#23 :==. c[#18,#3],#24 :==. c[#14,#23],#25 :==. c[#10,#24],#26 :==. c[#6,#25]])
(#1,[#3 :/=. [#31],#5 :==. res[#9,#3,#31],#9 :==. c[#6,#13],#13 :==. c[#10,#17],#17 :==. c[#14,#21],#21 :==. c[#18,#25],#25 :==.
 c[#22,#26],#26 :==. n,#27 :==. c[#22,#3],#28 :==. c[#18,#27],#29 :==. c[#14,#28],#30 :==. c[#10,#29],#31 :==. c[#6,#30]])
-}