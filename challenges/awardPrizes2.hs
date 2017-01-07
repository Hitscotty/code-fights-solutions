awardedPrizes p g a = [t (ceiling $ f p [i..i + m i - 1] / m i) / 100 | i <- l]
    where 
        -- transform the list of guesses into a list of places / positions
        l   = [h g a x | x <- g]
        
        -- for a given place / position count how many other people
        -- have made the same place (i.e. given the same guess)
        m i = sum [1 | y <- l, y == i]

-- Which place is x's guess?
h g a x = sum [1 | y <- g, x > a && (y <= a || y > a && y < x) || max x y <= a && y > x]

-- What is the accumulated prize for all the places given in l?
f p l = sum [x | x <- [100 * t c | [a,b,c] <- p, i <- l, i+2 > a, i < b]]

t = toEnum

