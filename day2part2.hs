

keypad :: [[Char]]
keypad = [['.', '.', '1', '.', '.'],
          ['.', '2', '3', '4', '.'],
          ['5', '6', '7', '8', '9'],
          ['.', 'A', 'B', 'C', '.'],
          ['.', '.', 'D', '.', '.']]


keypadAt :: (Int, Int) -> Char
keypadAt (x, y) = (keypad !! x) !! y

data Instruction = U | R | D | L deriving (Show, Eq)

charToInstruction :: Char -> Instruction
charToInstruction 'U' = U
charToInstruction 'R' = R
charToInstruction 'D' = D
charToInstruction 'L' = L

moveUnsafe :: Instruction -> (Int, Int) -> (Int, Int)
moveUnsafe U (x, y) = (x-1, y+0)
moveUnsafe R (x, y) = (x+0, y+1)
moveUnsafe D (x, y) = (x+1, y+0)
moveUnsafe L (x, y) = (x+0, y-1)

returnSafe :: (Int, Int) -> (Int, Int) -> (Int, Int)
returnSafe old (x, y)
  | x < 0                  = old
  | y < 0                  = old
  | x > 4                  = old
  | y > 4                  = old
  | keypadAt (x, y) == '.' = old
  | otherwise              = (x, y)
  
move :: Instruction -> (Int, Int) -> (Int, Int)
move i p = returnSafe p (moveUnsafe i p)

move' :: [Instruction] -> (Int, Int) -> (Int, Int)
move' [] p = p
move' (x:xs) p = move' xs (move x p)

move'' :: [[Instruction]] -> (Int, Int) -> [Char]
move'' [] p = []
move'' (x:xs) p = (keypadAt newP) : move'' xs newP
  where
    newP = move' x p

parseInput' :: String -> [Instruction]
parseInput' s = map charToInstruction s

parseInput :: String -> [[Instruction]]
parseInput s = map parseInput' (lines s)

solve :: String -> [Char]
solve s = move'' (parseInput s) (1, 1)

main = do
  inputText <- readFile "inputs/day2"
  putStrLn (show (solve inputText))

