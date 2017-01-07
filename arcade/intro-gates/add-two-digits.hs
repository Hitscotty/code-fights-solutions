import Data.Char

addTwoDigits n = foldr (\x y -> (digitToInt x) + y) 0 (show n)  
