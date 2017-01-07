switch n = if n == 0 then 1 else 0

switchLights (x:[]) = []
switchLights (x:xs) = switch x : switchLights [switch y | y <- xs]
