import System.IO 
isWordGood :: String -> Int
isWordGood word = if (length word == 2) || (length word == 3) || (length word == 4) || (length word == 7) then 1 else 0

sumOfWords :: [String] -> Int
sumOfWords [string] = isWordGood string
sumOfWords (string:strings) = (isWordGood string) + (sumOfWords strings)

mainFunc :: [[String]] -> Int
mainFunc [strings] = (subtract 4 (sumOfWords strings))
mainFunc (strings:stringss) = (subtract 4 (sumOfWords strings)) + (mainFunc stringss) 

main = do
    contents <- readFile "input.txt"
    print (mainFunc (map words (lines contents)))


