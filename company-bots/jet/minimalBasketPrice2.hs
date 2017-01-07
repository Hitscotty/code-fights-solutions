import Data.List
import Data.Array.Unboxed
import Data.List.Split
import Control.Monad
import Data.Maybe
import Control.Applicative
import Data.Ord

data Vendor a = Vendor { vendorNbr :: Int
                       , deliveryTime :: Int
                       , itemPrice :: Int
                       } deriving (Show)
                       

data Candidate a = Candidate
  { vendors :: [Int]
  , time :: Int
  , price :: Int 
  }

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
  
minimalBasketPrice mP vD
  = nub . vendors . head
  . foldr (foo mP) [Candidate [] 0 0]
  . map (filter ((/= -1) . itemPrice))
  . transpose
  . zipWith3 (\x y -> map (Vendor x y)) [0..] vD

foo :: Int -> [Vendor a] -> [Candidate a] -> [Candidate a]
foo mP choices
  = catMaybes . snd
  . mapAccumL bar (mP+1)
  . sortOn time
  . liftA2 addVendor choices

bar :: Int -> Candidate a -> (Int, Maybe (Candidate a))
bar bestprice candidate = if price candidate < bestprice
  then (price candidate, Just candidate)
  else (bestprice, Nothing)

addVendor (Vendor v t p) (Candidate vs ts ps) = Candidate (v:vs) (t+ts) (p+ps)
