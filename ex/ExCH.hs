{-# LANGUAGE LambdaCase #-}
module ExCH where

-- Bottom is a (the?) type with no proper values
data Bottom

-- Unit is a (the?) type with a unique proper value
data Unit = Unit

-- What the hell is boom?!
type Boom a = a -> Bottom

-- Can you define proper inhabitants for the following types?

bb :: Bottom -> Bottom
bb = id

bu :: Bottom -> Unit
bu _ = Unit

ub :: Unit -> Bottom -- impossible, there is no way to construct a value of type bottom
ub = undefined

uu :: Unit -> Unit
uu = id

nni :: a -> Boom (Boom a)
nni x = \na -> na x

nne :: Boom (Boom a) -> a -- impossible, no value of a can be constructed
nne = undefined

nnn :: Boom (Boom (Boom a)) -> Boom a
nnn nnna = \x -> nnna (nni x)

lem :: Either a (Boom a) -- impossible, a value of a or Boom a cannot be constructed
lem = undefined

lemnne :: Either a (Boom a) -> (Boom (Boom a)) -> a
lemnne (Left x)   _   = x
lemnne (Right na) nna = exfq $ nna na

wlem :: Boom (Boom (Either a (Boom a)))
wlem = \nana -> nana (Right (\x -> nana (Left x))) 

acnaib :: (a, Boom a) -> Bottom
acnaib (x, na) = na x

invimp :: (a -> b) -> (b -> a) -- false
invimp = undefined

conpos :: (a -> b) -> (Boom b -> Boom a)
conpos f = \nb -> \x -> (nb (f x))

cpcp :: (Boom b -> Boom a) -> (Boom (Boom a) -> Boom (Boom b))
cpcp = conpos

climp :: (a -> b) -> Either (Boom a) b -- false
climp = undefined

climp' :: Either (Boom a) b -> (a -> b)
climp' (Left na) x = exfq (na x)
climp' (Right y) _ = y


nb :: (a, Boom a) -> Bottom
nb (x, na) = na x

bottom :: a
bottom = bottom

exfq :: Bottom -> b
exfq _ = bottom

dem1 :: Boom (a,b) -> Either (Boom a) (Boom b) -- false
dem1 = undefined

dem1' :: Either (Boom a) (Boom b) -> Boom (a,b)
dem1' (Left na)  = \(x, _) -> na x
dem1' (Right nb) = \(_, y) -> nb y

dem2 :: (Boom a, Boom b) -> Boom (Either a b)
dem2 (na, nb) = \case
                  Left x  -> na x
                  Right y -> nb y

dem2' :: Boom (Either a b) -> (Boom a, Boom b)
dem2' nab = (\x -> nab (Left x), \y -> nab (Right y))

dem3' :: Boom (Either a (Boom a)) -> (Boom a, Boom (Boom a))
dem3' nab = (\x -> nab (Left x), \na -> nab (Right na))


