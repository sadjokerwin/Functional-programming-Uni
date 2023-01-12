dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g [] = True
dominates f g (x:xs) = if f x < g x then False else dominates f g xs
