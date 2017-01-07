import Data.List
import Data.List.Split
import Data.Tree
import Control.Monad

data Vendor a = Vendor { vendorNbr :: Integer
                       , deliveryTime :: Integer
                       , itemPrice :: Integer
                       } deriving (Show)

instance Eq (Vendor a) where
	(Vendor x y z) == (Vendor x' y' z') = x == x' && y == y' && z == z' 

showVendors v = putStrLn $ concat $ map (\x -> (concat $ map (\y -> show y ++ "\n") x) ++ "\n") v

populate :: [Integer] -> [[Integer]] -> [[Vendor a]]
populate d v = map (filter (\x -> itemPrice x /= (-1))) $ transpose (zipWith3 (\x y -> map (Vendor x y)) [0..] d v)

minimalBasketPrice mP vD vP = nub . get1 $ foldr (\x xs -> if get2 x < (get2 xs) then x else xs) (possible !! 0) possible
	where combos = sequence (populate vD vP) 
	      possible = filter (\(_,_,x) -> x <= mP) [((accu vendorNbr y), (total deliveryTime y), (total itemPrice y)) | y <- combos]

get1 (x,_,_) = x
get2 (_,x,_) = x
get3 (_,_,x) = x

total f = foldr (\x y -> (f x) + y) 0
accu f = foldr (\x y -> (f x) : y) []
-- | Debugging section
-- showVendors $ [[ Vendor 0 (total deliveryTime y) (total itemPrice y) | y <- combos]]
vendorsDelivery = [5, 4, 2, 3]
vendorsProducts = [[1, 1, 1],[3, -1, 3],[-1, 2, 2],[5, -1, -1]]

test = populate vendorsDelivery vendorsProducts 
