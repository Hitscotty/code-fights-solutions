minimalNumberOfCoins coins price = find price (reverse coins)
  where
    find 0 _ = 0
    find _ [] = 0
    find n (x:xs) = coins + find left xs
      where coins = div n x 
            left = mod n x
