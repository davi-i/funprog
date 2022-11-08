module ExUsing where

import Prelude hiding
    ( filter
    )

type Pred a = (a -> Bool)

-- using concat
filter :: Pred a -> [a] -> [a]
filter p = concat . map f
  where
    f x
      | p x       = [x]
      | otherwise = []

-- using zipWith
sorted :: Ord a => [a] -> Bool
sorted xs = and $ zipWith (<=) xs (drop 1 xs)

-- using zipWith
fibs :: Integral i => [i]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)


