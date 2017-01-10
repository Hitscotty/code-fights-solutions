import Data.Graph
import Data.List
import Data.Tree
import Test.Hspec
import Control.Exception (evaluate)

relatedQuestions n t edges = 2

readingTotal = dfs graph1 [3]
 
printForest :: Forest Vertex -> IO ()
printForest [] = print "finished"
printForest (x:xs) = do 
		    	print x
		    	printForest (subForest x)


k = components graph1
pp t = [printForest x | x <- t]

createGraph :: Int -> [[Int]] -> Graph
createGraph n edges = buildG (0,n-1) . conv . concat $ [permutations x | x <- edges]

conv :: [[Int]] -> [(Int, Int)]
conv = map (\[x,y] -> (x,y))

graph1 = createGraph n1 edges1
graph2 = createGraph n2 edges2
graph3 = createGraph n3 edges3

-- | test cases 
main :: IO ()
main = hspec $ do
	describe "relatedQuestions" $ do 
          it "test case 1" $ do 
	    relatedQuestions n1 t1 edges1 `shouldBe` (3 :: Int)  
          it "test case 2" $ do 
	    relatedQuestions n2 t2 edges2 `shouldBe` (4 :: Int)  
          it "test case 3" $ do 
	    relatedQuestions n3 t3 edges3 `shouldBe` (2 :: Int)  

n1 :: Int  	
n1 =  5

t1 :: [Int]
t1 = [2, 1, 13, 1, 12]

edges1 :: [[Int]]
edges1 = [[3,0], [3,2], [4,1], [3,1]]

n2 :: Int
n2 = 5

t2 :: [Int]
t2 = [3, 11, 3, 18, 3]

edges2 :: [[Int]]
edges2 = [[3,1], [4,2], [0,3], [4,1]]

n3 :: Int
n3 = 10

t3 :: [Int]
t3 = [9, 2, 7, 14, 4, 26, 21, 18, 39, 33]

edges3 :: [[Int]t
edges3 = [[2,7], [0,9], [3,5], [4,7], [0,2], [8,5], [3,6], [2,1], [5,0]]
