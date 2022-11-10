module Origami where

import Prelude hiding
    ( foldl , foldl1 , foldr , foldr1
    , scanr, scanl
    , sum , product
    , length
    , concat
    , filter
    , map
    , any , all
    , and , or
    , takeWhile , dropWhile
    )

import qualified Prelude as P

--
-- define the following folds:
--

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr _  base []     = base
foldr op base (x:xs) = x `op` foldr op base xs

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op base []     = base
foldl op base (x:xs) = foldl op (base `op` x) xs

-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 op []     = error "cannot fold empty list"
foldr1 op [x]    = x
foldr1 op (x:xs) = x `op` foldr1 op xs

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 op []      = error "cannot fold empty list"
foldl1 op (x:xs) = foldl op x xs


--
-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
--
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
--

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _  acc []     = [acc]
scanl op acc (x:xs) = acc : scanl op (acc `op` x) xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _  acc []     = [acc]
scanr op acc (x:xs) = case scanr op acc xs of
                        ys@(y:_) -> x `op` y : ys

--
-- Define all of the following functions as folds:
--

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

concat :: [[a]] -> [a]
concat = foldr (++) []

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

minimum :: Ord a => [a] -> a
minimum = foldr1 min

maximum :: Ord a => [a] -> a
maximum = foldr1 max

length :: Integral i => [a] -> i
length = foldr (const (+ 1)) 0

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (consif p) []
    where
        consif p x xs
            | p x       = x : xs
            | otherwise = xs

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x xs -> if p x then x : xs else []) []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p = snd . foldl dropWhile' (True, [])
    where
        dropWhile' (dropping, xs) x
          | dropping && p x = (True, xs)
          | otherwise       = (False, xs ++ [x])
 
-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
semo :: Integral i => [i] -> (i, Maybe i)
semo = foldr semo' (0, Nothing)
    where
        semo' n (s, m)
          | even n    = (n + s, m)
          | otherwise = (s    , max m $ Just n)

-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: Eq a => [a] -> [a]
remdups = foldr func []
    where
        func v [] = [v]
        func v xs@(x:_)
          | v == x    = xs
          | otherwise = v : xs

safeLast :: [a] -> Maybe a
safeLast = foldl (const Just) Nothing

-- dec2int [1,9,9,2] = 1992
dec2int :: Integral i => [i] -> i
dec2int = snd . foldr (\x -> \(k, y) -> (k * 10, x * k + y)) (1, 0)

