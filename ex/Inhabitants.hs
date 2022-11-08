module Inhabitants where

-- Define *convergent* (not bottom-ish) inhabitants
-- of the following types.
-- For each type, if you can define more than one
-- inhabitant, define a few of them, using primes
-- to name the other inhabitants, for example:
-- f, f', f'', f''', f'''', etc.

cI :: a -> a
cI = id

cK :: a -> b -> a
cK = const

cS :: (a -> b -> c) -> (a -> b) -> (a -> c)
cS f g x = f x (g x)

cB :: (b -> c) -> ((a -> b) -> (a -> c))
cB = (.)

cW :: (a -> (a -> b)) -> (a -> b)
cW f x = (f x) x

pL :: ((a -> b) -> a) -> a -- No convergent function can be defined with this type
pL f = undefined

p1 :: (a,b) -> a
p1 = fst

p2 :: (a,b) -> b
p2 = snd

p3 :: (a,b) -> (b,a)
p3 (x,y) = (y,x)

p4 :: Either a b -> b -- No definitely convergent function can be defined with this type
p4 = undefined

p5 :: Either a b -> [a]
p5 (Left x)  = [x]
p5 (Right _) = []

p5' :: Either a b -> [a]
p5' _ = []

p6 :: Either a b -> (a,b) -- No definitely convergent function can be defined with this type
p6 = undefined

p7 :: (a,b) -> Either a b
p7 (x,_) = Left x

p8 :: (a,b) -> Either a b
p8 (_,y) = Right y

p9 :: a -> b -> (a,a,b)
p9 x y = (x, x, y)

q1 :: a -> a -> a
q1 x _ = x

q1' :: a -> a -> a
q1' _ y = y

q2 :: (a,a) -> (a,a)
q2 (x, y) = (y, x) 

q3 :: Either (a,b) (a,c) -> (a, Either b c)
q3 (Left (x, y))  = (x, Left y)
q3 (Right (x, z)) = (x, Right z)

l1 :: [a]
l1 = []

l2 :: [[a]]
l2 = []

l2' :: [[a]]
l2' = [[]]    -- and infinity functions, just keep adding elements []

l3 :: [a] -> [a]
l3 _ = []

l3' :: [a] -> [a]
l3' []    = []
l3' (x:_) = [x]

l3'' :: [a] -> [a]
l3'' []     = []
l3'' (_:xs) = xs

l3''' :: [a] -> [a]
l3''' []       = []
l3''' [x]      = [x]
l3''' (x:y:zs) = y:x:zs

-- and you are able to define infity functions also

l4 :: a -> [a]
l4 x = [x]

l4' :: a -> [a]
l4' _ = []

l5 :: [a] -> a -- No definitely convergent function can be defined with this type
l5 = undefined

l6 :: a -> [[a]]
l6 x = [[x]]

l6' :: a -> [[a]]
l6' x = [[x], []] -- and so goes

