module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , Enum(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    , Maybe(..)
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show Zero     = "O"
    show (Succ n) = 'S' : show n

instance Eq Nat where

    Zero   == Zero   = True
    Succ n == Succ m = n == m
    _      == _      = False

instance Ord Nat where

    Zero   <= _      = True
    Succ _ <= Zero   = False
    Succ n <= Succ m = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min (Succ n) (Succ m) = Succ (min n m)
    min _        _        = Zero

    max Zero     m        = m
    max n        Zero     = n
    max (Succ n) (Succ m) = Succ (max n m)

isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
-- pred :: Nat -> Nat
-- pred Zero     = Zero
-- pred (Succ n) = n

even :: Nat -> Bool
even Zero     = True
even (Succ n) = odd n

odd :: Nat -> Bool
odd Zero     = False
odd (Succ n) = even n

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> Zero   = n
n <+> Succ m = Succ (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
n      <-> Zero   = n
Zero   <-> _      = Zero
Succ n <-> Succ m = n <-> m

-- multiplication
(<*>) :: Nat -> Nat -> Nat
n <*> Zero   = Zero
n <*> Succ m = n <+> (n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
n <^> Zero     = Succ Zero
n <^> (Succ m) = n <*> (n <^> m)

checkedSub :: Nat -> Nat -> Maybe Nat
checkedSub n        Zero     = Just n
checkedSub Zero     _        = Nothing
checkedSub (Succ n) (Succ m) = checkedSub n m

countJust :: (a -> Maybe a) -> a -> Nat
countJust f x = count' Zero x
    where
        count' c x = case f x of
                       Nothing -> c
                       Just x  -> count' (Succ c) x

lastJust :: (a -> Maybe a) -> a -> a
lastJust f x = case f x of
                 Nothing -> x
                 Just x  -> lastJust f x

-- quotient
(</>) :: Nat -> Nat -> Nat
n </> m = countJust (`checkedSub` m) n

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> m = lastJust (`checkedSub` m) n

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|> m = n <%> m == Zero

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff n        Zero     = n
absDiff Zero     m        = m
absDiff (Succ n) (Succ m) = absDiff n m

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero     = Succ Zero
factorial (Succ n) = (Succ n) <*> (factorial n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _    = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo b a = lo' Zero a
    where
        lo' fl a = let d = (a </> b) in
                       case d of
                         Zero -> fl
                         _    -> lo' (Succ fl) d

--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat x
    | x > 0 = Succ (toNat (x - 1))
    | x < 0 = error "Cannot convert a negative int to nat"

fromNat :: Integral a => Nat -> a
fromNat Zero     = 0
fromNat (Succ n) = fromNat n + 1


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger = toNat

instance (Enum Nat) where
    toEnum = toNat
    fromEnum = fromNat

    enumFrom n = n : enumFrom (Succ n)

    enumFromThen n t = n : enumFromThen t (t * 2 - n)

    enumFromTo n m
      | n <= m    = n : enumFromTo (Succ n) m
      | otherwise = []

    enumFromThenTo n t m
      | n <= m    = n : enumFromThenTo t (t * 2 - n) m
      | otherwise = []
