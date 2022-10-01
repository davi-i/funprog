module ExBox where

data Box a = Box a
    deriving ( Show , Eq )

-- Some examples follow.
-- Notice that we can box values of any type
-- (obviously(?)) this includes functions as well.

boxedTwo :: Num a => Box a
boxedTwo = Box 2

boxedEven :: Integral i => Box (i -> Bool)
boxedEven = Box even

boxedSucc :: Enum e => Box (e -> e)
boxedSucc = Box succ

boxOfPair :: Box (Int, Char)
boxOfPair = Box (2, 'a')

pairOfBoxes :: (Box Int, Box Char)
pairOfBoxes = (Box 2, Box 'a')

listOfBoxes :: [Box Int]
listOfBoxes = [Box 4, Box 2, Box 0]

boxOfList :: Box [Int]
boxOfList = Box [4,2,0]

-- Define the following functions:
-- (The types should be enough to understand
-- what each function is supposed to do.)

boxPlus :: Num a => Box a -> Box a -> Box a
boxPlus (Box x) (Box y) = Box (x + y)

boxPlusUnboxed :: Num a => Box a -> Box a -> a
boxPlusUnboxed (Box x) (Box y) = x + y

unbox :: Box a -> a
unbox (Box x) = x

applyBoxed :: Box (a -> b) -> Box a -> Box b
applyBoxed (Box f) (Box x) = Box (f x)

applyWithinBox :: Box (a -> b) -> a -> Box b
applyWithinBox (Box f) x = Box (f x)

composeBoxedFuns :: Box (b -> c) -> Box (a -> b) -> Box (a -> c)
composeBoxedFuns (Box f) (Box g) = Box (f . g)

headOfBoxed :: Box [a] -> a
headOfBoxed (Box xs) = head xs
