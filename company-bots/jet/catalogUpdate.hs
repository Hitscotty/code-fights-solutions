import Data.List

catalogUpdate c u = lexSort updated
  where 
   -- f = map (\x -> isName (x !! 0)) u
   -- processed = zip u $ f <*> [c]
    updated = foldr (\x y -> if (isName (head x) c) then (update x y) else (insert x y)) c u

isName name catalog = any (\x -> (x !! 0) == name) catalog 

lexSort s = sort $ map (\x -> (head x) : (sort (tail x))) s

update _ [] = []
update up@[n,k] (x:xs)
    | n /= (head x) = x : (update [n,k] xs)
    | otherwise  = (x ++ [k]) : (update [n,k] xs)

insert' name catalog = name : catalog

catalog = [["Books", "Classics", "Fiction"],["Electronics", "Cell Phones", "Computers", "Ultimate item"],["Grocery", "Beverages", "Snacks"],["Snacks", "Chocolate", "Sweets"],["root", "Books", "Electronics", "Grocery"]]

updates = [["Snacks", "Marmalade"],["Fiction", "Harry Potter"],["root", "T-shirts"],["T-shirts", "CodeFights"]]

