isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

helper :: Int -> Int -> Int -> Int
helper num iter prevSum
    | iter >= div (isqrt num)  2 = -1
    | num == prevSum + iter^3 = iter
    | otherwise = helper num (iter + 1) (prevSum + iter^3)

findNb :: Int -> Int
findNb a = helper a 0 0
