helperSqDig :: Int -> [Int]
helperSqDig a 
    | a < 10 = [rem a 10 * rem a 10]
    |otherwise = rem a 10 * rem a 10 : helperSqDig (div a 10)

listToNum :: [Int] -> Int
listToNum [a] = a
listToNum (a:num) = a*10^(length num + 1) + listToNum num

squareDigits :: Int -> Int
squareDigits a = if a > 0 then listToNum (helperSqDig a) else (-1)*listToNum (helperSqDig (modOfNum a))

modOfNum :: Int -> Int
modOfNum a 
    | a<0 = a*(-1)
    | otherwise = a