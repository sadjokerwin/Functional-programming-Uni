data Stock = Stock String Int
helperOneSymbol :: [Stock] -> Char -> Int
helperOneSymbol [Stock str num] symbol = if head str == symbol then num else 0
helperOneSymbol ((Stock str num):stoks) symbol = if head str == symbol then num + helperOneSymbol stoks symbol else helperOneSymbol stoks symbol


stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist stoks [a] = [(a, helperOneSymbol stoks a)]
stocklist stoks (x : xs) = (x, helperOneSymbol stoks x):stocklist stoks xs
stocks = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]

