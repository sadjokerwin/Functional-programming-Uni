import Graphics.Win32 (bST_CHECKED)
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
  -- | pairs == [pair] = pair ++ generateConcat (head pairs) (tail pairs) 
  | tail pair < head (take 1 pairs) = pair ++ head pairs
  --  ++ generateConcat (head pairs) (tail pairs)
  | otherwise = generateConcat pair (tail pairs)


-- generateConcat2 :: [[Int]] -> [Int] -> [Int]
-- generateConcat2 ::

mainGenerateHelper :: [[Int]] -> Int -> [[Int]]
-- mainGenerateHelper pairs counter
--   | counter == 0 = []
--   | otherwise = take 1 pairs ++ mainGenerateHelper (tail pairs) (counter - 1)
mainGenerateHelper [pair] 0 = [pair]
mainGenerateHelper (pair : pairs) 0 = if generateConcat pair pairs == [] then pair : mainGenerateHelper pairs 0 else pair : generateConcat pair pairs : mainGenerateHelper pairs 0

-- generate :: [[Int]] -> Int
main :: IO ()
main = do
  -- print (generateConcat [10, 30] [[10, 20], [10, 30], [15, 20], [15, 30], [25, 30]])
  -- print (generatePairsFromNumber [10, 15, 25] [1, 5, 20, 30])
  --  print (head (generateAllPairs [10, 15, 25] [1, 5, 20, 30]))
  --  print (take 1 (head (generateAllPairs [10, 15, 25] [1, 5, 20, 30])))
  -- print (generateConcat (head (generateAllPairs [10, 15, 25] [1, 5, 20, 30])) (generateAllPairs [10, 15, 25] [1, 5, 20, 30]))
  print (mainGenerateHelper (generateAllPairs [10, 15, 25, 35] [1, 5, 20, 30, 40]) 0)
  -- print (mergeLists [10, 15, 25, 35] [1, 5, 20, 30, 40])

-- print (head (generateAllPairs [10, 15, 25] [1, 5, 20, 30]))

-- print(11:[22])

-- generate
-- building_blocks ми е [(10, 20), (10, 30), (15, 20), (15, 30), (25, 30)]

