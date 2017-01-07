import Data.Ix

comfortableNumbers l r = pairsFromTo [l..r] r
    where
        pairsFromTo (x:xs) r 
           | xs == [] = 0
           | otherwise  = findPairs x r + pairsFromTo xs r   
        findPairs y = length . pairsWith y      
        pairsWith n r = filter (\y -> ((y > n) && (y <= r)) && (isComf y n)) $ pairRange n    
        pairRange a = filter (/=a) . range $ ((a - s a), (a + s a))
        s x = if x <= 0 then 0 else  (x `mod` 10) + s ((x - (x `mod` 10)) `div` 10)
        isComf n m = m `elem` pairRange n
