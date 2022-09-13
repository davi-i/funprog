module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..), Maybe(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head [] = undefined
head (x:_) = x

tail :: [a] -> [a]
tail [] = []
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr _  base []     = base
foldr op base (x:xs) = x `op` (foldr op base xs)

length :: Integral i => [a] -> i
length = foldr (P.const (+ 1)) 0

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

reverse :: [a] -> [a]
reverse xs = reverse' xs []
    where
        reverse' []       exs = exs
        reverse' (sx:sxs) exs = reverse' sxs (sx:exs)

(++) :: [a] -> [a] -> [a]
[]     ++ l = l
(x:xs) ++ l = x : (xs ++ l)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc v xs = xs ++ [v]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "cannot compute min of empty list"
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = error "cannot compute max of empty list"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

-- take
take :: Integral a => a -> [b] -> [b]
take _ [] = []
take 0 _ = []
take n (x:xs) = x : take (n - 1) xs
-- drop
drop :: Integral a => a -> [b] -> [b]
drop _ [] = []
drop 0 xs = xs
drop n (x:xs) = drop (n - 1) xs

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs')
  | p x       = dropWhile p xs'
  | otherwise = xs

-- tails
tails :: [a] -> [[a]]
tails []         = [[]]
tails xs@(x:xs') = xs : tails xs'
-- init
init :: [a] -> [a]
init []     = []
init [x]    = []
init (x:xs) = x : init xs

-- inits
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True
subset (x:xs) ys = x `elem` ys && xs `subset` ys

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = xs `subset` ys && ys `subset` xs


-- subsequences
subsequences :: [a] -> [[a]]
subsequences []     = [[]]
subsequences (x:xs) = xss ++ map (x:) xss
    where
        xss = subsequences xs

subsequences' :: [a] -> [[a]]
subsequences' [] = [[]]
subsequences' (x:xs) = join xss (map (x:) xss)
    where
        xss = subsequences' xs
        join ys [] = []
        join [] zs = []
        join (y:ys) (z:zs) = y : z : join ys zs

-- any
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p
-- all
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- and
and :: [Bool] -> Bool
and = foldr (&&) True
-- or
or :: [Bool] -> Bool
or = foldr (||) False

-- concat
concat :: [[a]] -> [a]
concat = foldr (++) []

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x = any (x ==)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' x []     = False
elem' x (y:ys) = x == y || elem' x ys

-- (!!)
(!!) :: Integral i => [a] -> i -> a
[]     !! _ = error "index out of range"
(x:_)  !! 0 = x
(_:xs) !! n
    | n < 0     = error "negative index"
    | otherwise = xs !! (n - 1)

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
-- map
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

-- cycle
cycle :: [a] -> [a]
cycle []     = []
cycle (x:xs) = x : cycle (xs <: x)
-- repeat
repeat :: a -> [a]
repeat x = x : repeat x
-- replicate
replicate :: Integral i => i -> a -> [a]
replicate n = take n . repeat

-- isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _          = True
isPrefixOf _ []          = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
-- isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf xs ys = isPrefixOf xs ys
    || case ys of
         []     -> False
         (_:ys) -> isInfixOf xs ys
-- isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

-- zip
zip :: [a] -> [b] -> [(a, b)]
zip _ []          = []
zip [] _          = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys
-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ []          = []
zipWith _ [] _          = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- intercalate
intercalate :: [a] -> [[a]] -> [a]
intercalate _ []        = []
intercalate _ [ys]      = ys
intercalate xs (ys:yss) = ys ++ xs ++ intercalate xs yss
-- intercalate :: [a] -> [a] -> [a]
-- intercalate xs [] = xs
-- intercalate [] ys = ys
-- intercalate (x:xs) (y:ys) = x : y : intercalate xs ys
-- nub
nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : nub (filter (/= x) (xs))

-- splitAt
splitAt :: Integral i => i -> [a] -> ([a], [a])
splitAt 0 xs     = ([], xs)
splitAt _ []     = ([], [])
splitAt n (x:xs) = (x : xs1, xs2)
    where 
        (xs1, xs2) = splitAt (n - 1) xs
-- splitAt n xs = (take n xs, drop n xs)
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break
break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break p xs@(x:xs')
  | p x       = ([], xs)
  | otherwise = (x : ls, rs)
    where
        (ls, rs) = break p xs'

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x:xs)
  | p x       = (x : ls, rs)
  | otherwise = (ls, x : rs)
    where
        (ls, rs) = partition p xs

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x xs = xs1 : case xs2 of
                     []    -> []
                     _:xs2 -> split x xs2
             where
                 (xs1, xs2) = break (== x) xs 
-- lines
lines :: String -> [String]
lines = split '\n'
-- lines "" = []
-- lines (x:xs)
--     | x == '\n' = "" : lines xs
--     | otherwise = case lines xs of
--                     [] -> [[x]]
--                     (ys:yss) -> (x : ys) : yss
-- words
words :: String -> [String]
words = split ' '

-- unlines
unlines :: [String] -> String
unlines = (++ "\n") . intercalate "\n" 
-- unwords
unwords :: [String] -> String
unwords = (++ " ") . intercalate " " 

-- transpose
transpose :: [[a]] -> [[a]]
transpose []       = []
transpose (xs:xss) = transpose' xs (transpose xss)
    where
        transpose' []     xss       = xss
        transpose' xs     []        = map (:[]) xs
        transpose' (x:xs) (xs':xss) = (x:xs') : transpose' xs xss

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

firstEqual :: Eq a => [a] -> Maybe a
firstEqual []         = Nothing
firstEqual xs@(x:xs') = P.fmap P.fst $ safeHead $ dropWhile (uncurry (/=)) (zip xs xs')

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome s = lowerLettersS == reverse lowerLettersS
    where lowerLettersS = filter C.isLetter (map C.toLower s)

fromWhile :: Integral i => i -> (a -> Bool) -> [a] -> [a]
fromWhile n p = takeWhile p . drop n

fromFor :: Integral i => i -> i -> [a] -> [a]
fromFor n m = take m . drop n

fromTo :: Integral i => i -> i -> [a] -> [a]
fromTo n m = fromFor n (m - n)

fromToThat :: Integral i => i -> i -> (a -> Bool) -> [a] -> [a]
fromToThat n m p = filter p . fromTo n m

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

filter' :: (a -> Bool) -> [a] -> [a] 
filter' p xs = concat $ map (\x -> if p x then [x] else []) xs

-- {{ tests

prop_filterFilter' xs = filter P.even xs == filter' P.even xs
prop_break xs = break P.even xs == L.break P.even xs
prop_partition xs = partition P.even xs == L.partition P.even xs
prop_transpose xss = transpose xss == L.transpose xss

-- }}

