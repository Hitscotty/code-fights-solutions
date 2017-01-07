squareDigitsSequence n = l 0 n
  where
      l n m
         | r (take n $ ([m] ++ f m)) = n
         | otherwise = l (n + 1) m
      f n = [s n] ++ f (s n)
      s x = if x /= 0 then (x `mod` 10) ^ 2 + s (x `div` 10) else 0
          
r :: [Int] -> Bool
r [] = False
r [_] = False
r (h:t) = if elem h t then True else r t


