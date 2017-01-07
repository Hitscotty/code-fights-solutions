import Data.Bits

secondRightmostZeroBit n = undefined

helper 0 = []
helper m = (m `mod` 2) : helper (m `div` 2)
