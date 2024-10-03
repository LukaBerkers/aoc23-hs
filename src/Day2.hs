module Day2 (
    -- * Data types
    Game,
    Reveal,

    -- * File reading
    readGameRecords,

    -- * Cube counting logic
    cubeThresholds,
    possibleGameIds,
) where

import Data.Either.Combinators (whenLeft)
import System.FilePath ((</>))
import System.Log.Logger (Priority (ERROR), logM)
import Text.Parsec (
    ParseError,
    char,
    digit,
    endBy,
    endOfLine,
    many1,
    parse,
    sepBy1,
    space,
    string,
    (<|>),
 )
import Text.Parsec.String (Parser)
import Utils (inputDir, readInputFile)

-- | A game with an id and a list of revealed cubes.
data Game = Game {gameId :: Int, gameReveals :: [Reveal]}
    deriving (Show)

-- | The number of red, green, and blue cubes revealed.
data Reveal = Reveal {revealRedCount :: Int, revealGreenCount :: Int, revealBlueCount :: Int}
    deriving (Show)

moduleName :: String
moduleName = "Day2"

gameRecordsFilePath :: FilePath
gameRecordsFilePath = inputDir </> "day2.txt"

{- | Reads the game records from the input file.

This function performs an IO operation to read game records.
If the file cannot be parsed, this returns a 'ParseError'.
If the file is parsed successfully, this returns a list of the recorded 'Game's.
-}
readGameRecords :: IO (Either ParseError [Game])
readGameRecords = do
    let logger = moduleName ++ ".readGameRecords"
    result <- readInputFile gameRecordsFilePath parseGameRecordsFile
    whenLeft result $ \err -> logM logger ERROR $ show err
    return result

parseGameRecordsFile :: String -> Either ParseError [Game]
parseGameRecordsFile = parse (gameP `endBy` endOfLine) ""

intP :: Parser Int
intP = read <$> many1 digit

gameP :: Parser Game
gameP = do
    identifier <- string "Game" *> space *> intP <* char ':' <* space
    reveals <- revealP `sepBy1` (char ';' <* space)
    return $ Game identifier reveals

revealP :: Parser Reveal
revealP = do
    counts <- colorCountP `sepBy1` (char ',' <* space)
    let redCount = sum [count | (count, "red") <- counts]
        greenCount = sum [count | (count, "green") <- counts]
        blueCount = sum [count | (count, "blue") <- counts]
    return $ Reveal redCount greenCount blueCount

colorCountP :: Parser (Int, String)
colorCountP = do
    count <- intP <* space
    color <- string "red" <|> string "green" <|> string "blue"
    return (count, color)

-- | The given thresholds for the number of cubes that can be revealed, for a game to be possible.
cubeThresholds :: Reveal
cubeThresholds = Reveal 12 13 14

{- | Returns the ids of the games that are possible with the given thresholds.

A game is possible if all of its reveals are possible.
A reveal is possible if for each color,
the number of cubes is less than or equal to the number of cubes with that color
revealed in the threshold.
-}
possibleGameIds :: Reveal -> [Game] -> [Int]
possibleGameIds thresholds = map gameId . filter (isPossible thresholds . gameReveals)

isPossible :: Reveal -> [Reveal] -> Bool
isPossible thresholds = all (isPossibleReveal thresholds)

isPossibleReveal :: Reveal -> Reveal -> Bool
isPossibleReveal (Reveal red green blue) (Reveal red' green' blue') =
    red' <= red && green' <= green && blue' <= blue
