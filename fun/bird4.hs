allPairs n = [(x, k - x) | k <- [0..n], x <- [0..k]]

disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint xs@(x:xs') ys@(y:ys') = case compare x y of
                           LT -> disjoint xs' ys
                           EQ -> False
                           GT -> disjoint xs ys'
disjoint _ _ = True

hardy n = [(a, b, c, d) | a <- [0..n], b <- [a..n], c <- [a + 1..n], d <- [c..n], a^3 + b^3 == c^3 + d^3]
--
--

     -- (0,0) (0,1) (1,0) (0,2)

-- (0,0) 0,0   0,1   0,2   0,3

-- (0,1) 1,0   1,1   1,2   1,3

-- (1,0) 2,0   2,1   2,2   2,3

-- (0,2) 3,0   3,1   3,2   3,3

-- 0,0 - 0,1 - 1,0 

-- (0,0,0,0)
-- (0,0,0,1)
-- (0,1,0,0)
-- (0,0,1,0)
-- (0,1,0,1)
-- (1,0,0,0)
-- (0,0,0,2)
-- (0,1,1,0)
-- (1,0,0,1)
-- (0,2,0,0)

data List a = Nil | Snoc (List a) a

instance Show a => Show (List a) where
    show xs = '[' : show' xs ++ "]"
        where
            show' Nil          = ""
            show' (Snoc Nil x) = show x
            show' (Snoc xs  x) = show' xs ++ ',' : show x

toList :: [a] -> List a
toList = toList' Nil
    where
        toList' xs' []     = xs'
        toList' xs' (x:xs) = toList' (Snoc xs' x) xs

fromList :: List a -> [a]
fromList = fromList' []
    where
        fromList' xs' Nil         = xs'
        fromList' xs' (Snoc xs x) = fromList' (x:xs') xs


class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

instance Bifunctor (,) where
    bimap f g (x,y) = (f x, g y)

instance Bifunctor Either where
    bimap f _ (Left x)  = Left (f x)
    bimap _ g (Right y) = Right (g y)
