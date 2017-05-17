module Main where

data Prop a
    = Yes
    | Huh -- Make Atry happy
    deriving (Show)

(===) :: a -> a -> Prop a
u === v = Huh -- Make Atry happy
{-# NOINLINE (===) #-}
{-# RULES "Prop" forall a. a === a = Yes #-}

mapMaybe f (Just u) = Just (f u)
mapMaybe f Nothing = Nothing

prop_Maybe
    :: Prop ((b -> c) -> (a -> b) -> Maybe a -> Maybe c)
prop_Maybe =
    (\f g -> mapMaybe f . mapMaybe g)
    === (\f g -> mapMaybe (f . g))

main = print prop_Maybe
-- Yes

