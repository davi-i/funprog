module ExHyper where

import Prelude hiding ( exp )

-- Nat datatype --------------------------------

data Nat = Z | S Nat
     deriving (Eq)

instance (Num Nat) where
    (+) = add
    (*) = mul
    abs = id
    fromInteger 0 = Z
    fromInteger n
      | n > 0     = S $ fromInteger (n-1)
      | otherwise = Z
    signum Z = Z
    signum n = S Z
    negate n = Z

toInt :: Integral i => Nat -> i
toInt Z = 0
toInt (S n) = toInt n + 1

instance (Show Nat) where
    show = show . toInt

instance (Ord Nat) where
    Z     <= _     = True
    (S _) <= Z     = False
    (S n) <= (S m) = n <= m

{- explicit definitions of add, mul, exp: 

add n Z     = n
add n (S m) = S (add m n)

mul n Z     = Z
mul n (S m) = add n (mul n m)

exp n Z     = S Z
exp n (S m) = mul n (exp n m)

-}

------------------------------------------------

-- substitute 'undefined' by the correct number
-- to define each of those functions:

add :: Nat -> Nat -> Nat
add = hyper 1

mul :: Nat -> Nat -> Nat
mul = hyper 2

exp :: Nat -> Nat -> Nat
exp = hyper 3

-- hyper n should return the n'th operation in the sequence:
-- (..?..), add, mul, exp, ...?

hyper :: Integral i => i -> (Nat -> Nat -> Nat)
hyper 0 _ m     = S m
hyper 1 n Z     = n
hyper 2 _ Z     = Z
hyper _ _ Z     = S Z
hyper k n (S m) = hyper (k - 1) n (hyper k n m)

