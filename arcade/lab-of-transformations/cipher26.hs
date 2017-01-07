


find x = fst . (!!0) $ filter ((==x) . snd) key
  where key = zip [0..25] ['a'..'z']
