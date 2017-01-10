import Data.Graph
import Data.List
import qualified Data.Vector as V

-- | code explanations
{- 
      A graph is directed so I create an undirected graph by making node 
   in the graph have an edge back to itself. This way edge [1,2] 
   is a node 1 with and edge to 2 but equivalently 2 with and edge to 1. 
   I did this by getting all permutations of an edge granted that its size is fixed of 2
   the only possible permutations are that of an edge back to itself -> [1,2],[2,1]. 
      
      The given equation for the best reading time is indexOfQuestion + relatedQuestions/directlyRelated.
   the # of relatedQuestions is found by traversing all possible nodes from the given node (reachable) which returns
   a list of relatedQuestions. Since the questionNumber relates to the index of its reading time I use this list of numbers
   to find it's corresponding readingTime and take the sum of that with the queried questionNumber's directlyRelated count.
   The directlyRelated count is found my concatenating the edges and counting how many times the given question appears.
      Ex: [[1,2][2,1]] -> [1,2,2,1] ; for queston 1 this appears twice so it has 2 directlyRelated questions namely 2 and 1.

      To reduce time complexity I use a list comphrension that walks through the questions and applies this logic concurrently
   to each question number. The end result is a list of tuples with totalReading times and corresponding index 
   location of that question; herefor I select the minimum and return the snd number of the minimal tuple which is the optimal 
   question nbr.

   Algorithm:
      zip times with indexes and for each (time,index) tuple find the price for index by summing its reachable nodes and dividing by 
      its directly related number, given by edges, add this new number to the first question aka index and continue until end of questions.
      find minimum total reading time and return its snd tuple pair the index of question.

   issue: 
      can i find a case where (a + a) / a + (a + a) / b  does not equal (a + a + a + a) / b
-}

relatedQuestions :: Int -> [c] -> [[Int]] -> Vertex
relatedQuestions n t edges
    | edges  == [] = snd $ minimum (zip t [0..])
    | otherwise    = snd ascList
      where
            ascList      = minimum [(price (snd q) (related (snd q) edges) (fst q) , snd q) | q <- zip t [0..]]
            price x d ti = (fromIntegral (reach x) / fromIntegral d) + fromIntegral ti
            reach q      = V.sum . V.tail $ V.fromList [ (V.fromList t) V.! x | x <- reachable graph q]
            graph        = createGraph n adjList
            adjList      = concat [permutations x | x <- edges]

n1 :: Int
n1 = 5 

t1 :: [Int]
t1 = [2, 2, 1, 2, 2] 

   
s1 :: [[Int]]
s1 = [[0, 1], [1, 2], [2, 3], [3, 4]]


main = do
        putStr "questions: "
	print n1
	putStr "times: "
        print t1
	putStr "edges: "
        print s1
	putStr "shortest: "
	print $ relatedQuestions n1 t1 s1

-- | helper functions

fst2 [x,_] = x

related :: Int -> [[Int]] -> Int
related n edges = count n (concat edges)

createGraph :: Int -> [[Int]] -> Graph
createGraph e = buildG (0, e-1) . conv

count :: Int -> [Int] -> Int
count x = length . filter (==x) 

conv :: [[Int]] -> [(Int, Int)]
conv = map (\[x,y] -> (x,y))

maxx xs = fst2 . maximum $ concat [permutations x | x <- xs]
minn xs = fst2 . minimum $ concat [permutations x | x <- xs]







