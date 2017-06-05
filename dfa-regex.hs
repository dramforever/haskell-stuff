-- stack exec ghci

module DfaRegex where

import Data.Array
import Data.List
data Regex
    = C_0 | C_1
    | Epsilon
    | None
    | Regex `Or` Regex
    | Regex `Then` Regex
    | Star Regex

instance Show Regex where
    show regex = "/^" ++ go regex ++ "$/"
        where
            go C_0 = "0"
            go C_1 = "1"
            go Epsilon = ""
            go None = "-"
            go a@Or{} = "(" ++ intercalate "|" (go <$> getOr a) ++ ")"
                where
                    getOr (a `Or` b) = getOr a ++ getOr b
                    getOr a = [a]
            go (a `Then` b) = go a ++ go b
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

simplify :: Regex -> Regex
simplify regex = go regex
    where
        go (a `Then` b) =
            case (go a, go b) of
                (Epsilon, v) -> v
                (u, Epsilon) -> u
                (None, _) -> None
                (_, None) -> None
                (u, v) -> u `Then` v
        go (a `Or` b) =
            case (go a, go b) of
                (None, v) -> v
                (u, None) -> u
                (u, v) -> u `Or` v
        go (Star a) =
            case go a of
                None -> Epsilon
                Epsilon -> Epsilon
                u -> Star u
        go r = r

floyd :: Matrix Regex -> Matrix Regex
floyd mat = foldl' update mat ixs
    where
        bnds@((minIx, _), (maxIx, _)) = bounds mat
        pairs = indices mat
        ixs = [minIx .. maxIx]

        -- matWithEps = listArray bnds $ go <$> pairs
        --     where
        --         go (i, j)
        --             | i == j = (mat ! (i, j)) `Or` Epsilon
        --             | otherwise = mat ! (i, j)

        update m k = listArray bnds $ (\(i, j) -> build m i j k) <$> pairs

        build m i j k = (i ~> j) `Or` ((i ~> k) `Then` Star (k ~> k) `Then` (k ~> j))
            where u ~> v = m ! (u, v)

makeMatrix :: (Int, Int) -> [((Int, Int), Regex)] -> Matrix Regex
makeMatrix (minIx, maxIx) = accumArray Or None ((minIx, minIx), (maxIx, maxIx))

divisibleDfa :: Int -> [((Int, Int), Regex)]
divisibleDfa m = [0 .. m - 1] >>= (\i ->
    [ (i --> (i * 2) `mod` m, C_0)
    , (i --> (i * 2 + 1) `mod` m, C_1)])

divisibleRegex :: Int -> Regex
divisibleRegex n = (! (0, 0)) . floyd . makeMatrix (0, n - 1) $ divisibleDfa n

divisibleBy7 :: String
divisibleBy7 = show . simplify $ divisibleRegex 7