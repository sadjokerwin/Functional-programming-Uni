modOfNum :: Double -> Double
modOfNum a
  | a < 0 = a * (-1)
  | otherwise = a
type Point = (Double, Double)

distanceBetweenPoints :: Point -> Point -> Double
distanceBetweenPoints (x1, y1) (x2, y2)
    | x1 == x2 = modOfNum (y1 - y2)
    | y1 == y2 = modOfNum (x1 - x2)
    | otherwise = sqrt ((x1 - x2)^2+(y1 - y2)^2)

goodPoints :: Point -> Double -> [Point] -> [Point]
goodPoints _ _ [] = []
goodPoints centre r (current:rem)
    | distanceBetweenPoints centre current <= r = current : goodPoints centre r rem
    | otherwise = goodPoints centre r rem

badPoints :: Point -> Double -> [Point] -> [Point]
badPoints _ _ [] = []
badPoints centre r (current:rem)
    | distanceBetweenPoints centre current > r = current : badPoints centre r rem
    | otherwise = badPoints centre r rem

splitPoints :: Point -> Double -> [Point] -> ([Point],[Point])
splitPoints centre r points = (goodPoints centre r points, badPoints centre r points)


    
