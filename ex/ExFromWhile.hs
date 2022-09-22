module ExFromWhile where

fromWhile :: Int -> (a -> Bool) -> [a] -> [a]
fromWhile n p = takeWhile p . drop n

fromFor :: Int -> Int -> [a] -> [a]
fromFor n m = take m . drop n

fromTo :: Int -> Int -> [a] -> [a]
fromTo n m = fromFor n (m - n)

fromToThat :: Int -> Int -> (a -> Bool) -> [a] -> [a]
fromToThat n m p = filter p . fromTo n m

