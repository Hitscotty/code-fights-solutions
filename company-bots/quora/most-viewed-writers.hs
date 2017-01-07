import Data.List
import Data.List.Split
import Data.Ord
import Data.Function

-- | need to sum duplicates
data Question a = Question { question :: Int,
                              topics :: [Int]
                             } deriving (Show)

data Writer a = Writer { answerId :: Int,
                           userId :: Int,
                            views :: Int 
                          } deriving (Show)

data Topic a = Topic { answersOfTopic :: [Writer a] } deriving (Show)
             
type Pair = [Int]
type PairList = [Pair]

insertQuestions :: PairList -> [Question a]
insertQuestions topicData = [Question (fst t) (snd t) | t <- (zip [0..] topicData)]

insertAnswers :: PairList -> [Writer a]
insertAnswers viewData = [ (Writer (a !! 0) (a !! 1) (a !! 2)) | a <- viewData]

most t a v = (map  concat) [ map ((map (get answerId)) .  (a !!) . fromInteger) x | x <- sortByTopic t]
  where 
    get f n = foldr (\x y -> if ((f  x) == n) then x else y) (Writer (-1) (-1) (-1)) $ insertAnswers v

sortByTopic t = flatten $ groupBy (\x y -> fst x == fst y) . sort . concat $ [map (\y -> (y, fst x )) (snd x) | x <- (zip [0..] t)]
    where flatten sift = [map snd y | y <- sift]


mostViewedWriters t a v = map (sortBy myCompare) unsorted
    where 
        unsorted = map (dupList . sort) $ helper t a v
        helper t a v = [map (\y -> [(userId y), (views y)]) x | x <- most t a v]


-- | helper functions

myCompare [x,y] [x1,y1]
     | y == y1 = x `compare` x1
     | otherwise = y1 `compare` y
 
stail [_,y] = y

dupList :: PairList -> PairList
dupList xs = ss
 where gs = groupBy dup xs
       ss = map sumGroup gs

sumGroup :: PairList -> Pair
sumGroup xs = [h,t]
  where h = head $ head xs
        t = sum $ concatMap tail xs

dup :: Pair -> Pair -> Bool
dup xs ys = head xs == head ys

printArray :: Show a => [a] -> IO ()
printArray arr = mapM_ (putStrLn . unwords) $ map (map show) $ chunksOf 1 arr

topicData :: PairList
topicData = [[1, 2, 3], [2, 3, 4], [1, 4], [2, 3]]

answerData :: PairList
answerData = [[6, 4], [1, 2], [5], [3]]

viewData :: PairList
viewData = [[2, 1, 2], [6, 3, 5], [3, 3, 0], [5, 1, 1], [4, 2, 3], [1, 4, 2]]



