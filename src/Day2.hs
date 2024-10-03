module Day2 (Game, readGameRecords) where

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

data Reveal = Reveal {revealRedCount :: Int, revealGreenCount :: Int, revealBlueCount :: Int}
    deriving (Show)

moduleName :: String
moduleName = "Day2"

gameRecordsFilePath :: FilePath
gameRecordsFilePath = inputDir </> "test.txt"

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
