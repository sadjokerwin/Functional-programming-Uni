import System.IO
-- TASK 1 ------------------------------------------------------------
generatePairsFromNumber :: [Int] -> [Int] -> [[Int]]
generatePairsFromNumber (a : as) [] = []
generatePairsFromNumber (a : as) (b : bs) = if a > b then generatePairsFromNumber (a : as) bs else (a : [b]) : generatePairsFromNumber (a : as) bs

generateAllPairs :: [Int] -> [Int] -> [[Int]]
generateAllPairs [] _ = []
generateAllPairs (a : as) (b : bs) = generatePairsFromNumber (a : as) (b : bs) ++ generateAllPairs as (b : bs)

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x : xs) (y : ys)
  | x <= y = x : mergeLists xs (y : ys)
  | otherwise = mergeLists (x : xs) ys

generateConcat :: [Int] -> [[Int]] -> [Int]
generateConcat pair pairs
  | pairs == [] = []
  | tail pair < head (take 1 pairs) = pair ++ head pairs
  | otherwise = generateConcat pair (tail pairs)

generate :: [[Int]]-> [[Int]]
generate [pair]  = [pair]
generate (pair : pairs) = if generateConcat pair pairs == [] then pair : generate pairs  else pair : generateConcat pair pairs : generate pairs 

-- generate :: [[Int]] -> Int
main :: IO ()
main = do
  print (generate (generateAllPairs [10, 15, 25] [1, 5, 20, 30]) )

-- TASK 2 ------------------------------------------------------------
isWordGood :: String -> Int
isWordGood word = if (length word == 2) || (length word == 3) || (length word == 4) || (length word == 7) then 1 else 0

sumOfWords :: [String] -> Int
sumOfWords [string] = isWordGood string
sumOfWords (string : strings) = isWordGood string + sumOfWords strings

mainFunc :: [[String]] -> Int
mainFunc [strings] = subtract 4 (sumOfWords strings)
mainFunc (strings : stringss) = subtract 4 (sumOfWords strings) + mainFunc stringss

-- main = do
--   contents <- readFile "input.txt"
--   print (mainFunc (map words (lines contents)))
