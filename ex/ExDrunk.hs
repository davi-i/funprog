module ExDrunk
    ( atIndices
    , everyOther
    , disjoint
    , stretch
    , drunk
    ) where

atIndex :: Integral i => i -> [a] -> Maybe a
atIndex _ []     = Nothing
atIndex 0 (x:_)  = Just x
atIndex n (_:xs) = atIndex (n - 1) xs

-- example:
-- atIndices [1,4,5] "Tchauzinho"
-- = "cuz"
atIndices :: Integral i => [i] -> [a] -> [a]
atIndices []     _  = []
atIndices (n:ns) xs = case atIndex n xs of
                        Nothing -> atIndices ns xs
                        Just x  -> x : atIndices ns xs

newDrop :: Integral i => i -> [a] -> [a]
newDrop 0 xs     = xs
newDrop _ []     = []
newDrop n (x:xs) = newDrop (n - 1) xs

-- example:
-- everyOther 2 "Hello There"
-- = "HloTee"
everyOther :: Integral i => i -> [a] -> [a]
everyOther _ []     = []
everyOther 0 (x:_)  = repeat x
everyOther n (x:xs) = x : everyOther n (newDrop (n - 1) xs)

-- examples:
-- disjoint [1,5,9] [2 .. 6]
-- = False
-- disjoint [1,5,9] [2,4 ..]
-- = True
-- ASSUMPTIONS FOR disjoint xs ys:
--   xs and ys are sorted
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint []         _          = True
disjoint _          []         = True
disjoint xs@(x:xs') ys@(y:ys') = x < y && disjoint xs' ys
                              || x > y && disjoint xs ys'

repeatN :: Integral i => i -> a -> [a]
repeatN 0 _ = []
repeatN n x = x : repeatN (n - 1) x

-- example:
-- stretch 3 "Gustavo"
-- = "GGGuuussstttaaavvvooo"
stretch :: Integral i => i -> [a] -> [a]
stretch n = concat . map (repeatN n)

splitAt' :: Integral i => i -> [a] -> ([a], [a])
splitAt' _ []     = ([], [])
splitAt' 0 xs     = ([], xs)
splitAt' n (x:xs) = (x : lxs, rxs)
    where
        (lxs, rxs) = splitAt' (n - 1) xs

-- example:
-- drunk 3 "Gustavo"
-- = "GusGtuasvtoavo"
-- drunk 5 "Gustavo"
-- = "GustaGvuostavo"
-- To understand these string, either get drunk or look at the markings:
--       , , , , ,,,
--   "GusGtuasvtoavo"
--    ''' ' ' ' '
--         , , ,,,,,
--   "GustaGvuostavo"
--    ''''' ' '
drunk :: Integral i => i -> [a] -> [a]
drunk n xs = start ++ concat (zipWith join xs end)
    where
        (start, end') = splitAt' n xs
        end = map Just end' ++ repeat Nothing
        join x Nothing = [x]
        join x (Just y) = [x, y]
