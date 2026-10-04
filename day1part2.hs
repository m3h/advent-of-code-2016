import qualified Data.Maybe;
import Data.List;
import Data.Char;

data Direction = L | R deriving (Show, Eq)

data Command
  = Dir Direction
  | Steps Int
  deriving (Show, Eq)

charToDirection :: Char -> Maybe Direction
charToDirection 'R' = Just R
charToDirection 'L' = Just L
charToDirection _   = Nothing

stripLeft :: String -> String
stripLeft [] = []
stripLeft (h:s)
  | isSpace h = stripLeft s
  | otherwise = h:s

stripRight :: String -> String
stripRight ws = case unsnoc ws of
  Just (s, t) -> if isSpace t then stripRight s else ws
  Nothing -> ws

strip :: String -> String
strip s = stripLeft (stripRight s)

parseSteps :: String -> Int
parseSteps s = read (init s)

parseCommand :: String -> (Direction, Int)
parseCommand s = (Data.Maybe.fromJust (charToDirection (head s)), parseSteps (tail s))

parseCommands :: String -> [(Direction, Int)]
parseCommands s = map parseCommand (words ((strip s) ++ ","))


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

addDirectionToPosition :: Position -> Direction -> Position
addDirectionToPosition (Position c x y) dir = Position (addDirection c dir) x y

samePlace :: Position -> Position -> Bool
samePlace (Position _ xa ya) (Position _ xb yb) = (xa == xb) && (ya == yb)

seenPlace :: [Position] -> Bool
seenPlace [] = False
seenPlace (x:[]) = False
seenPlace (x:xs) = any (samePlace x) xs

step :: Position -> Position
step (Position N x y) = Position N (x + 0) (y + 1)
step (Position E x y) = Position E (x + 1) (y + 0)
step (Position S x y) = Position S (x + 0) (y - 1)
step (Position W x y) = Position W (x - 1) (y + 0)


walkStep :: [Position] -> [Position]
walkStep trail
  | seenPlace trail = trail
  | otherwise       = step (head trail) : trail


walkSteps :: [Position] -> Int -> [Position]
walkSteps trail 0 = trail
walkSteps trail s = walkSteps (walkStep trail) (s - 1)

addDirectionToTrail :: [Position] -> Direction -> [Position]
-- important not to add a new item to the list, because that would break seenPlace
addDirectionToTrail (x:xs) dir = (addDirectionToPosition x dir) : xs

doCommand :: [Position] -> (Direction, Int) -> [Position]
doCommand trail (dir, steps) = walkSteps (addDirectionToTrail trail dir) steps
 
positionDistance :: Position -> Position -> Int
positionDistance (Position _ x1 y1) (Position _ x2 y2) = abs (x1 - x2) + abs (y1 - y2)

march :: [(Direction, Int)] -> [Position] -> [Position]
march [] t = t
march (c:rem) t = march rem (doCommand t c)

day1Part1 :: String -> Int
day1Part1 s = positionDistance start end
  where
    start = Position N 0 0
    commands = parseCommands s
    trail = march commands [start]
    end = head trail

main = do
  inputText <- readFile "inputs/day1"
  putStrLn (show (day1Part1 inputText))
