import Data.List

awardedPrizes prizes guesses answer = undefined
	where findPrizes n = [x | x <- prizes , head x >= n]
              totalPrizes = concat $ map (\x -> replicate ((nbr x) + 1 ) x ) prizes
              winners = getWinners answer guesses 

-- | association list with (ties, (index, closeness)) used to get winning order
-- | flatten this list to (ties, index) in winning order 
getWinners n g = flattenTuple $ zip ties winners
	where
	    winners = ((sort $ map (\x -> (abs (n - (fst x)), snd x)) (filter ((<=n) . fst) g)) ++ (reverse . sort $ map (\x -> (abs (n - (fst x)), snd x) ) (filter ((>n) . fst)  g)))
	    ties = map (\x -> (count (fst x)) (map fst winners)) winners

-- | helper functions

getAwards [] _ = []
getAwards _ [] = []
getAwards (x:xs) (y:ys) 
	| fst y == 1 = slice ++ getAwards xs ys -- problem need to round
	| otherwise = sliceWith ++ getAwards (xs) ys
	where third [_,_,l] = l
	      slice = [( snd y, (third x `div` (fst y)))]
	      sliceWith = [(snd y, (    sum  ( map third (take (fst y) (x:xs) ))         ) `div` (fst y)  ) ]
              


count x = length . filter (==x)
nbr [x,y,_] = abs (x - y)
thr [_,_,x] = toInteger x
flattenTuple =  map (\(t,(_,i)) -> (t,i))
flattenWinners = undefined
getPrize n = [x | x <- prizes , head x >= n]

-- | testing debugging
test = getAwards totalPrizes win
totalPrizes = concat $ map (\x -> replicate ((nbr x) + 1 ) x ) prizes
win = getWinners 70 (zip guesses [0..])
prizes :: [[Int]]
prizes = [[1,1,100], [2,2,50], [3,4,25] ]
guesses = [65, 70, 78, 65, 72]
answer = 70
