f n
  | n < 0 = []
  | otherwise = [(div n 2)] ++ f ( n - 4)
