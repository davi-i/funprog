halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where
        n = length xs `div` 2

third :: [a] -> a
third = head . tail . tail

third' :: [a] -> a
third' = (!! 2)

third'' :: [a] -> a
third'' (_:_:x:_) = x

safetail :: [a] -> [a]
safetail xs = if null xs
              then []
              else tail xs

safetail' :: [a] -> [a]
safetail' xs
    | null xs   = []
    | otherwise = tail xs
             
safetail'' :: [a] -> [a]
safetail'' []     = []
safetail'' (_:xs) = xs

luhnDouble :: Int -> Int
luhnDouble n
    | k > 9     = k - 9
    | otherwise = k
  where
      k = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0
