import Data.Char
import Text.Printf (printf) 

arrayPacking arr = toDec . concat $ toBin arr

  -- fix8bit s = (replicate (8 - (length s)) '0') ++ s
toDec :: String -> Int   
toDec = foldl (\a x -> a * 2 + digitToInt x) 0

toBin :: [Int] -> [String]   
toBin arr = foldl (\a x -> printf "%.8b" x : a) [] arr

toBin' 0 = [(show 0)]
toBin' n 
	| n `mod` 2 == 1 = toBin' (n `div` 2) ++ [(show 1)]
        | n `mod` 2 == 0 = toBin' (n `div` 2) ++ [(show 0)]

-- | better solution
arrayPacking' [d,c,b,a] = a*2^24+b*2^16+c*2^8+d
arrayPacking' [d,c,b] = b*2^16+c*2^8+d
arrayPacking' [d,c] = c*2^8+d
arrayPacking' [d] = d

