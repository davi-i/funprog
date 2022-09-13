module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

-- define Rat:
data Rat = Rat Integer Integer

instance Show Rat where
    show (Rat p q) = show p ++ "/" ++ show q

instance Eq Rat where
    Rat p1 q1 == Rat p2 q2 = p1 * q2 == p2 * q1

instance Num Rat where
    Rat p1 q1 + Rat p2 q2 = Rat (p1 * div d q1 + p2 * div d q2) d
        where
            d = lcm q1 q2
    Rat p1 q1 * Rat p2 q2 = Rat (p1 * p2) (q1 * q2)
    negate (Rat p q) = Rat (negate p) q
    abs (Rat p q) = Rat (abs p) (abs q)
    signum (Rat p q) = Rat (signum p * signum q) 1
    fromInteger n = Rat n 1

instance Ord Rat where
    Rat p1 q1 <= Rat p2 q2 = p1 * q2 <= p2 * q1

rat :: Integer -> Integer -> Rat
rat p 0 = error "denominator cannot be 0"
rat p q = Rat p q

(//) :: Rat -> Rat -> Rat
Rat p1 q1 // Rat p2 q2 = Rat (p1 * q2) (q1 * p2)

denominator :: Rat -> Integer
denominator (Rat _ q) = q

numerator :: Rat -> Integer
numerator (Rat p _) = p

