sToI :: String -> Int
sToI s = read s

isTriangle' (a:b:c:[]) = (a + b > c) && (a + c > b) && (b + c > a)

isTriangle s = isTriangle' is
  where
    ws = words s
    is = map sToI ws
    

solve s = length (filter id (map isTriangle (lines s)))

main = do
  inputText <- readFile "inputs/day3"
  putStrLn (show (solve inputText))
