module MyLib (parseGames, isGamePossible, power) where

import Text.Parsec.String (Parser)
import Text.Parsec
--import Text.Parsec.Char (string, char, digit, oneOf, endOfLine)

data CubeSet = CubeSet {red :: Int, green :: Int, blue :: Int}
    deriving (Eq, Show)

data Color = Red | Green | Blue
    deriving (Eq, Show)

data Game = Game Int [CubeSet]
    deriving (Eq, Show)


regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""



color :: Parser Color
color = do 
    (string "red" >> return Red) 
    <|> (string "green" >> return Green)
    <|> (string "blue" >> return Blue)



num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

cubeSet :: Parser CubeSet
cubeSet = cubeSet' $ CubeSet 0 0 0
  where
    cubeSet' :: CubeSet -> Parser CubeSet
    cubeSet' s = do
        s' <- colorPart s
        c  <- optionMaybe $ oneOf ",;"
        case c of
            Just ',' -> cubeSet' s'
            Just ';' -> return s'
            Nothing  -> return s'


colorPart :: CubeSet -> Parser CubeSet
colorPart s = do 
    char ' '
    d <- num
    char ' '
    clr <- color
    return $ adjustSet s clr d 
  where
    adjustSet :: CubeSet -> Color -> Int -> CubeSet
    adjustSet s Red   d =  s { red = d }
    adjustSet s Blue  d =  s { blue = d }
    adjustSet s Green d =  s { green = d }

game :: Parser Game
game = do
    string "Game "
    d <- num
    char ':'
    sets <- many1 cubeSet
    return $ Game d sets

fileLines :: Parser a -> Parser [a]
fileLines p = p `sepBy` endOfLine >>= \ps -> eof >> return ps

gameList :: Parser [Game]
gameList = fileLines game

parseGames :: String -> Either ParseError [Game]
parseGames = regularParse gameList

-- part 1

isGamePossible :: Int -> Int -> Int -> Game -> Int
isGamePossible rd gr bl (Game d sets) = if isSetPossible `all` sets then d else 0
  where
    isSetPossible :: CubeSet -> Bool
    isSetPossible (CubeSet rd' gr' bl') = (rd' <= rd) && (gr' <= gr) && (bl' <= bl)

-- part 2

growSet :: CubeSet -> CubeSet -> CubeSet
growSet (CubeSet r g b) (CubeSet r' g' b') = CubeSet (max r r') (max g g') (max b b')

minimalSet :: Game -> CubeSet
minimalSet (Game d sets) = foldr growSet (CubeSet 0 0 0) sets

power :: Game -> Int
power gm = 
    let (CubeSet r g b) = minimalSet gm
    in r * g * b


