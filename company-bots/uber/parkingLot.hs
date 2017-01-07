parkingSpot car@[h,w] p loc@[x1,y1,x2,y2] = (notBlocked spot) && (any (==True) entrances) 
  where 
          indexed = getRange x1 x2 p
          spot = getInnerRange y1 y2 indexed
          entrances = filterEntrances car loc $ map notBlocked [left, top, bottom, right]
          top = getInnerRange y1 y2 $ getRange 0 x1 p
          left = getInnerRange 0 y1 $ getRange x1 x2 p
          bottom = getInnerRange y1 y2 $ getRange len len p
          right = getInnerRange y2 innerLen $ getRange x1 x2 p
          len = (length p) - 1
          innerLen = (length $ p !! 0) - 1
 
-- | handles edge cases and defines the right entrances to use    
filterEntrances [h,w] [x1,y1,x2,y2] entr@[l,t,b,r]
	| abs (y1 - y2) > abs (x1 - x2) = [l,r]
	| otherwise = [t,b]

-- | creates an array of the inner array given a range
getInnerRange i j arr = flattenTuple $ (filter (\x -> fst x >= i && fst x <= j)) . concat $ map (zip [0..]) arr

-- | creates an array of an array given a range 
getRange i j arr = flattenTuple $ takeWhile (\x -> fst x <= j) $ drop i $ zip [0..] arr

-- | flattens a array of tuples to just an array
flattenTuple = foldr (\x y -> snd x : y) []

-- | checks a single array 
notBlocked = foldr (\x y -> x == 0 && y) True






    

