-- stack exec ghci

module DfaRegex where

import Data.Array
import Data.List

data Regex
    = C_0 | C_1
    | Or [Regex]
    | Concat [Regex]
    | Star Regex

epsilon, none :: Regex
epsilon = Concat []
none = Or []

mkOr :: [Regex] -> Regex
mkOr as = case as >>= expandOr of
    [a] -> a
    rs -> Or rs
    where
        expandOr (Or as) = as
        expandOr a = [a]

mkStar :: Regex -> Regex
mkStar (Or []) = epsilon
mkStar (Concat []) = epsilon
mkStar a = Star a

mkConcat :: [Regex] -> Regex
mkConcat as = case as >>= expandConcat of
    as | any isNone as -> Or []
    [a] -> a
    as -> Concat as
    where
        expandConcat (Concat as) = as
        expandConcat a = [a]

        isNone (Or []) = True
        isNone _ = False


instance Show Regex where
    show regex = "/^" ++ go regex ++ "$/"
        where
            go C_0 = "0"
            go C_1 = "1"
            go (Or []) = "-"
            go (Or as) = "(" ++ intercalate "|" (go <$> as) ++ ")"
            go (Concat as) = w as
                where
                    w (C_0 : Star C_0 : xs) = "0+" ++ w xs
                    w (Star C_0 : C_0 : xs) = "0+" ++ w xs
                    w (x : xs) = go x ++ w xs
                    w [] = ""
            go (Star m) = optParens m ++ "*"

            optParens :: Regex -> String
            optParens C_0 = "0"
            optParens C_1 = "1"
            optParens a@Or{} = go a
            optParens a = "(" ++ go a ++ ")"

type Matrix = Array (Int, Int)

infix 2 -->
(-->) :: a -> b -> (a, b)
(-->) = (,)

floyd :: Matrix Regex -> Matrix Regex
floyd mat = foldl' update mat ixs
    where
        bnds@((minIx, _), (maxIx, _)) = bounds mat
        pairs = indices mat
        ixs = [minIx .. maxIx]

        update m k = listArray bnds $ (\(i, j) -> build m i j k) <$> pairs

        build m i j k = mkOr [i ~> j, mkConcat [i ~> k, mkStar (k ~> k), k ~> j]]
            where u ~> v = m ! (u, v)

makeMatrix :: (Int, Int) -> [((Int, Int), Regex)] -> Matrix Regex
makeMatrix (minIx, maxIx) ts = mkOr <$> accumArray (flip (:)) [] ((minIx, minIx), (maxIx, maxIx)) ts

divisibleDfa :: Int -> [((Int, Int), Regex)]
divisibleDfa m = [0 .. m - 1] >>= (\i ->
    [ (i --> (i * 2) `mod` m, C_0)
    , (i --> (i * 2 + 1) `mod` m, C_1)])

divisibleRegex :: Int -> Regex
divisibleRegex n = (! (0, 0)) . floyd . makeMatrix (0, n - 1) $ divisibleDfa n

divisibleBy7 :: String
divisibleBy7 = show (divisibleRegex 7)