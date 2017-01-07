import Data.Tree 

depthFirst :: Tree a -> [a]
depthFirst (Node r forest) = r : concat [depthFirst t | t <- forest]

add :: Tree Int -> Int
add (Node r forest) = r + sum [add t | t <- forest]
