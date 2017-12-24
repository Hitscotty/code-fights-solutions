
{-# LANGUAGE FlexibleContexts #-}


{-
uniqueCount :: Sentence -> Sentence -> [(Int, Sentence)]
uniqueCount [] _ = []
uniqueCount (c:cs) c2 = count--[(totalUnique, matches)]
    where 
        matches = (\x -> if x == [] then [] else (snd . last) x) $ filter (\(x,_) -> x /= 0) count
        totalUnique = fst $ foldr (\(x,xs) (y,ys) -> ((x + y), ys)) (0,[]) count 
        count = (c `elem'` c2, last' c c2 ) : uniqueCount cs c2
        elem' c' cs' = length $ filter (==c') cs' 
        last' c' [] = []
        last' c' (cs':css')
                        | c' == cs' = css'
                        | otherwise = last' c' css'
-}        

-- first matching word 

conversations = [["lets","have","some","fun"], ["i","never","get","it"], ["be","aware","of","this","house"], ["he","will","call","her"]]
currentConversation = ["can", "you", "please"]

type Conversation = [[String]]
type Sentence = [String]

chatBot :: Conversation -> Sentence -> Sentence
chatBot conversations currentConversation =  currentConversation ++ suggestions
    where x = map (occurs' currentConversation) conversations
          most = largest' $ isuck x
          suggestions = lastrest (map (\(x,_) -> x) (snd most)) (conversations !! fst most)

max' xs = foldr go (0,[]) xs
        where go x y 
                | fst x >= fst y = x
                | otherwise = y

occurs [] _ = []
occurs (x:xs) s2 = 
    let counts = (next' found s2, total) : occurs xs s2
        found = (x,total)
        total = length $ filter (==x) s2
    in filter (\x -> snd x /= 0) counts

occurs' [] _ = []
occurs' (x:xs) s2 = 
    let counts = (x, total) : occurs' xs s2
        found = (x,total)
        total = length $ filter (==x) s2
    in filter (\x -> snd x /= 0) counts

next' _ [] = []
next' (s,0) xs = xs
next' (s,i) (x:xs) = if s == x then next' (s, i - 1) xs else next' (s,i) xs

disp x = (mapM_ print) x

largest xs = foldr go [] xs
    where go x y 
            | length x >= length y = x 
            | otherwise = y


largest' xs = foldr go (0,[]) xs
    where 
        go x y 
            | ((length . snd) x) >= ((length . snd) y) = x 
            | otherwise = y
        
lastrest [] _ = []            
lastrest rs xs = reverse $ go (reverse xs) 
    where go [] = [] 
          go (y:ys)
            | (y `elem` rs)  = []
            | otherwise = y : go ys 

isuck arr = go 0 arr
    where go _ [] = []
          go n (x:xs) = (n, x) : go (n + 1) xs
