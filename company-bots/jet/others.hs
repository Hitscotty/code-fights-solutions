import Data.List (sort, partition, nub)

catalogUpdate catalog us = flatTree $ modTree (parseTree catalog) us

data Tree = Branch String [Tree]

parseTree = fillBranch "root"
    where
        fillBranch name cat = Branch name $ subtrees name cat
        subtrees name cat = case partition ((== name) . head) cat of
            ([], _) -> []
            (((_:ns):_), c) -> map (flip fillBranch c) ns

modTree (Branch name twigs) upds =
    case partition ((== name) . head) upds of
        ([], _) -> Branch name $ map (flip modTree upds) twigs
        (rs, u) -> Branch name $ map (flip modTree u) $
            (map (flip Branch []) $
             filter (not . (`elem` children)) $
             map (!! 1) rs) ++ twigs
    where children = map (\(Branch n _) -> n) twigs

flatTree :: Tree -> [[String]]
flatTree = fixOrder . levels
    where
        levels (Branch name twigs) = if length twigs > 0
            then (name : map (\(Branch n _) -> n) twigs) : concatMap levels twigs
            else []
        fixOrder l = map (\(x:xs) -> x : (sort) xs) $
            -- uncurry (flip (++)) $ fmap sort $ partition ((== "root") . head) l
            sort l
