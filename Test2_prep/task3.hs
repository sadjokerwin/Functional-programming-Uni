data Bracket = Bracket Char  Int
-- bool = 1 skoba [
-- bool = 0 skoba ]
helperStr :: String -> Int -> [Bracket]
helperStr str counter
    | str == []  = []
    | otherwise = if head str == '[' then Bracket (head str)  counter : helperStr (tail str) (counter + 1) else if head str == ']' then Bracket (head str) counter : helperStr (tail str) (counter + 1) else helperStr (tail str) (counter + 1) 

printBrackets :: [Bracket] -> [(Char, Int)]
printBrackets [] = []
printBrackets ((Bracket br index):brs) = (br, index) : printBrackets brs

printBracket :: Bracket -> (Char, Int)
printBracket (Bracket br index) = (br, index)

searchAppropriateBracket :: Bracket -> [Bracket] -> Int -> (Int, Int)
searchAppropriateBracket (Bracket br1 index1) ((Bracket br2 index2) : brackets) numOfOpeningBrackets
    | br2 == '[' = searchAppropriateBracket (Bracket br1 index1) brackets (numOfOpeningBrackets + 1)
    | otherwise = if numOfOpeningBrackets == 0 then (index1, index2) else searchAppropriateBracket (Bracket br1 index1) brackets (numOfOpeningBrackets - 1)


traverseList :: [Bracket] -> [(Int, Int)]
traverseList [] = []
traverseList ((Bracket br index) : brackets) = if br == '[' then searchAppropriateBracket (Bracket br index) brackets 0 : traverseList brackets else traverseList brackets

matching :: String -> [(Int, Int)]
matching str = traverseList (helperStr str 0)

-- matching "1234" ➝ []
-- matching ",[.[-],]" ➝ [(3, 5), (1, 7)]
-- matching ",+[-.,+]" ➝ [(2, 7)]
-- matching "[][]" ➝ [(0, 1), (2, 3)]