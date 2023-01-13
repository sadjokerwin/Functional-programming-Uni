-- Using the higher-order function foldr define a function sumsq which takes an
-- integer n as its argument and returns the sum of the squares of the first n
-- integers. That is to say,
-- sumsq n = 1^2 + 2^2 + 3^2 + . . . + n^2.
sumsq :: Int -> Int
sumsq n = foldr (\curr acum -> curr^2 + acum) 0 [1..n]


-- Define length, which returns the number of elements in a list, using foldr . Rede-
-- fine it using foldl .
myLength :: (Ord a) => [a] -> Int
myLength  = foldr (\curr acum -> (acum + 1) ) 0 


-- Define minlist, which returns the smallest integer in a non-empty list of integers,
-- using foldr1 . Redefine it using foldl1 .
myMinList :: [Int] -> Int
myMinList = foldr (\curr min -> if min > curr then curr else min) (maxBound :: Int)


-- Define reverse, which reverses a list, using foldr
myReverseList :: (Ord a) => [a] -> [a]
myReverseList  = foldr (\ curr acum -> acum ++ [curr]) [] 


-- Using foldr , define a function remove which takes two strings as its arguments
-- and removes every letter from the second list that occurs in the first list. For
-- example, remove "first" "second" = "econd".
isSymInWord :: Char -> String -> Bool
isSymInWord sym = foldr (\ curr acum -> curr == sym || acum ) False

myRemove :: String -> String -> String
myRemove fst = foldr (\curr acum -> if isSymInWord curr fst then acum else curr : acum) []


