g n m = (n - 1) + ( length . takeWhile (>=0) . reverse $ foldr (\x y -> ((head y) - x ) : y) [m] (reverse (f n))) - 1
f n = map (length . show) [n..2000]
