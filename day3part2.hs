sToI :: String -> Int
sToI s = read s

isTriangle' (a:b:c:[]) = (a + b > c) && (a + c > b) && (b + c > a)

parseIntLine :: String -> [Int]
parseIntLine s = map sToI (words s)

parseIntGrid :: String -> [[Int]]
parseIntGrid s = map parseIntLine (lines s)

countTriangle :: [Int] -> Int
countTriangle t
  | isTriangle' t = 1
  | otherwise     = 0

countTriangle' :: [[Int]] -> Int -> Int
countTriangle' (r1:r2:r3:[]) idx = countTriangle [r1 !! idx, r2 !! idx, r3 !! idx]

countTriangles :: [[Int]] -> Int
countTriangles [] = 0
countTriangles (r1:r2:r3:rs) = countTriangle' g 0 + countTriangle' g 1 + countTriangle' g 2 + countTriangles rs
  where
    g = [r1, r2, r3]

solve s = countTriangles triangleGrid
  where
    triangleGrid = parseIntGrid s

main = do
  inputText <- readFile "inputs/day3"
  putStrLn (show (solve inputText))
