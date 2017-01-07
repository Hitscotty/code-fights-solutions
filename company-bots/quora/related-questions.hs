import Data.Graph
import Data.List

relatedQuestions :: Int -> [Int] -> [[Int]] -> Int
relatedQuestions n t edges = snd . head . sort $ readings
   where
      first = createGraph n edges
      second = createGraph n (map reverse edges)
      readings = zip [totalReadings first second edges t n | n <- [0..(n - 1)]] [0..]
      
-- | helper functions
-- sum . map (t !!) $ reachable
-- look at first half then second half get reading time at question 3 
-- getMax ->  $ zip [0..] $ map f edges 
totalReadings :: Fractional t => Graph -> Graph -> [[Int]] -> [Int] -> Int -> t
totalReadings first second edge time l = fromIntegral (time !! l) + (readT first second)
   where total g x = sum . map (time !!) . tail $ reachable g x
         readT fhalf shalf = ((fromIntegral (total fhalf l)) / (fromIntegral (relate l))) + (( (fromIntegral (total shalf l)) / (fromIntegral (relate l))))
         relate n = count n (concat edge)
   --     second = total (map reverse g) x
     --   total graph = sum . map (t !!) . tail $ reachable graph

createGraph :: Int -> [[Int]] -> Graph
createGraph n e = buildG (0,(n-1)) $ conv e 

related :: Int -> [[Int]] -> [Int]
related n edges = map f [0..(n-1)]
    where f x = count x . concat $ edges

count :: Int -> [Int] -> Int
count x = length . filter (==x) 

conv :: [[Int]] -> [(Int, Int)]
conv = map (\[x,y] -> (x,y))

-- | test cases 
questions :: [(Int, Int)]
questions = zip [0..n] t

n :: Int
n = 5

t :: [Int]
t = [2, 2, 1, 2, 2]

sample1 :: [[Int]]
sample1 = [[0, 1], [1, 2], [2, 3], [3, 4]]

sample3 :: [[Int]]
sample3 = [[2,7], [0,9], [3,5], [4,7], [0,2], [8,5], [3,6], [2,1], [5,0]]

n2 :: Int
n2 = 5

t2 :: [Int]
t2 =  [3, 11, 3, 18, 3]

sample2 :: [[Int]]
sample2 = [[3,1],[4,2],[0,3],[4,1]]

test = zip [totalReadings first second sample1 t n | n <- [0..4]] [0..]
   where first = createGraph 5 sample1
         second = createGraph 5 (map reverse sample1)
main = do
   print "edges"
   print $ sample1 
   print "time to read"
   print t
   print "estimated totals"
   print test
   print "relatedQuestions"
   print $ relatedQuestions 5 t sample1

