weakNumbers n = [weakest, nbr]
    where 
        weakArray = weaknesses $ weakness n
        weakest = maximum weakArray
        nbr = count weakest weakArray

d n = length [x | x <- [1..n], n `mod` x == 0]
getDivisors n = length [x | x <- [1..n], n `mod` x == 0]
weakness n = reverse [getDivisors x | x <- [1..n]]

-- | count occurrences of n in array
count n = length . filter (==n)

-- | creates array of weaknesses
weaknesses [] = []
weaknesses (x:xs) = [length . filter (>x) $ xs] ++ weaknesses xs
