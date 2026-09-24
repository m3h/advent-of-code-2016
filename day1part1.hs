import qualified Data.Maybe;

data Direction = L | R deriving (Show, Eq)

data Command
  = Dir Direction
  | Steps Int
  deriving (Show, Eq)

charToDirection :: Char -> Maybe Direction
charToDirection 'R' = Just R
charToDirection 'L' = Just L
charToDirection _   = Nothing

parseSteps :: String -> Int
parseSteps s = read (init s)

parseCommand :: String -> (Direction, Int)
parseCommand s = (Data.Maybe.fromJust (charToDirection (head s)), parseSteps (tail s))

parseCommands :: String -> [(Direction, Int)]
parseCommands s = map parseCommand (words (s ++ ","))


data Cardinal = N | E | S | W deriving (Show, Eq)

addDirection :: Cardinal -> Direction -> Cardinal
addDirection N L = W
addDirection E L = N
addDirection S L = E
addDirection W L = S
addDirection N R = E
addDirection E R = S
addDirection S R = W
addDirection W R = N

data Position = Position Cardinal Int Int deriving (Show, Eq)

walkSteps :: Position -> Int -> Position
walkSteps (Position N x y) s = Position N (x + 0) (y + s)
walkSteps (Position E x y) s = Position E (x + s) (y + 0)
walkSteps (Position S x y) s = Position S (x + 0) (y - s)
walkSteps (Position W x y) s = Position W (x - s) (y + 0)

doCommand :: Position -> (Direction, Int) -> Position
doCommand (Position c x y) (dir, steps) = walkSteps (Position (addDirection c dir) x y) steps

positionDistance :: Position -> Position -> Int
positionDistance (Position _ x1 y1) (Position _ x2 y2) = abs (x1 - x2) + abs (y1 - y2)

march :: [(Direction, Int)] -> Position -> Position
march [] p = p
march (c:rem) p = march rem (doCommand p c)

day1Part1 :: String -> Int
day1Part1 s = positionDistance start end
  where
    start = Position N 0 0
    commands = parseCommands s
    end = march commands start

main = putStrLn (show (day1Part1 inputText))
  where
    inputText = "L1, L3, L5, L3, R1, L4, L5, R1, R3, L5, R1, L3, L2, L3, R2, R2, L3, L3, R1, L2, R1, L3, L2, R4, R2, L5, R4, L5, R4, L2, R3, L2, R4, R1, L5, L4, R1, L2, R3, R1, R2, L4, R1, L2, R3, L2, L3, R5, L192, R4, L5, R4, L1, R4, L4, R2, L5, R45, L2, L5, R4, R5, L3, R5, R77, R2, R5, L5, R1, R4, L4, L4, R2, L4, L1, R191, R1, L1, L2, L2, L4, L3, R1, L3, R1, R5, R3, L1, L4, L2, L3, L1, L1, R5, L4, R1, L3, R1, L2, R1, R4, R5, L4, L2, R4, R5, L1, L2, R3, L4, R2, R2, R3, L2, L3, L5, R3, R1, L4, L3, R4, R2, R2, R2, R1, L4, R4, R1, R2, R1, L2, L2, R4, L1, L2, R3, L3, L5, L4, R4, L3, L1, L5, L3, L5, R5, L5, L4, L2, R1, L2, L4, L2, L4, L1, R4, R4, R5, R1, L4, R2, L4, L2, L4, R2, L4, L1, L2, R1, R4, R3, R2, R2, R5, L1, L2"
