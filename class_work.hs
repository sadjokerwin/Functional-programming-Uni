import Data.List
import Data.Char (ord)
rotate :: String -> Int -> String
rotate str 0 = str
rotate str a = (last str):(rotate (take ((length str) - 1) str) (a-1))

helper :: String -> Int -> [String]
helper word 0 = []
helper word counter = rotate word counter : helper word (counter - 1)

allRotations :: String -> [String]
allRotations word = mergeSort(word:helper word (length word - 1))


mergeLists :: (Ord a) => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
  | x <= y = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeLists (mergeSort f) (mergeSort s) where
  (f, s) = splitAt (length xs `div` 2) xs

data BTree = Nul | Node Char BTree BTree
-- helperConsecVert :: BTree -> String -> Int -> String
-- helperConsecVert :: (Node sym left right) word 0 = []
-- helperConsecVert :: (Node sym left right) word counter = [] 

-- tryForEver :: BTree -> String -> Bool
-- tryForEver (Node sym Nul Nul) word = False;
-- tryForEver (Node sym left Nul) word = tryForEver left word || mainFunc left word
-- tryForEver (Node sym Nul right) word = tryForEver right word || mainFunc right word
-- tryForEver (Node sym left right) word = mainFunc left word || mainFunc right word

-- mainFunc :: BTree -> String -> Bool
-- mainFunc (Node sym left right) (x:xs)
--     | sym == x = mainFunc left xs || mainFunc right xs
--     | xs == [] = True
--     | sym /= x= False

poopyTree = (Node 'p' (Node 'c' Nul  (Node 'a' Nul (Node 't' Nul  Nul ))) (Node 'd' Nul  Nul ))

sumOfChars :: String -> Int
sumOfChars [] = 0
sumOfChars (x:xs) = (ord x) + sumOfChars xs

sumOfCharsFold :: String -> Int
sumOfCharsFold word = foldr (\sym count -> count + ord sym) 0 word

wordTuples :: [String] -> ([String], [String])
wordTuples words = foldr (\str (e, f) -> if (mod (sumOfChars str) 2) == 0 then (e,str : f) else (str : e,f)) ([], []) words

