import Data.List
import Data.Maybe

packageBoxing pkg boxes
    | (length fits) == 0 = -1
    | (length fits) == 1 = fst . head $ fits
    | otherwise = snd $ minimum volumes
    where fits = filter (\x -> compare' (sort pkg) (snd x)) (organize boxes)
          volumes = map (\x -> (product (snd x), fst x)) $ fits

organize n = zip [0..] $ map sort n
compare' [x,y,z] [x1,y1,z1] = x <= x1 && y <= y1 && z <= z1
