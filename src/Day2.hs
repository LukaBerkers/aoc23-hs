{-
This file is part of my solutions for Advent of Code 2023.
Copyright (C) 2024  Luka Berkers

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

module Day2 (
    -- * Data types
    Game,
    CubeSet (..),
    InputError (..),

    -- * File reading
    readGameRecords,

    -- * Cube counting logic
    cubeThresholds,
    findPossibleGameIds,
    getMinimumCubeSets,
    getCubeSetPower,
) where

import System.FilePath ((</>))
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
data Game = Game {gameId :: Int, gameReveals :: [CubeSet]}
    deriving (Show, Eq)

-- | A set of red, green, and blue cubes.
data CubeSet = CubeSet {csRedCount :: Int, csGreenCount :: Int, csBlueCount :: Int}
    deriving (Show, Eq)

-- | Represents errors that can occur when reading and parsing a file.
data InputError = ReadError IOError | ParseError ParseError
    deriving (Show, Eq)

gameRecordsFilePath :: FilePath
gameRecordsFilePath = inputDir </> "day2.txt"

{- | Reads the game records from the input file.

This function performs an IO operation to read game records.
If the file cannot be read, this returns a v'ReadError'.
If the file cannot be parsed, this returns a v'ParseError'.
If the file is parsed successfully, this returns a list of the recorded 'Game's.
-}
readGameRecords :: IO (Either InputError [Game])
readGameRecords = do
    readResult <- readInputFile gameRecordsFilePath parseGameRecordsFile
    return $ case readResult of
        Left readErr -> Left $ ReadError readErr
        Right (Left parseErr) -> Left $ ParseError parseErr
        Right (Right games) -> Right games

parseGameRecordsFile :: String -> Either ParseError [Game]
parseGameRecordsFile = parse (gameP `endBy` endOfLine) ""

intP :: Parser Int
intP = read <$> many1 digit

gameP :: Parser Game
gameP = do
    identifier <- string "Game" *> space *> intP <* char ':' <* space
    reveals <- revealP `sepBy1` (char ';' <* space)
    return $ Game identifier reveals

revealP :: Parser CubeSet
revealP = do
    counts <- colorCountP `sepBy1` (char ',' <* space)
    let redCount = sum [count | (count, "red") <- counts]
        greenCount = sum [count | (count, "green") <- counts]
        blueCount = sum [count | (count, "blue") <- counts]
    return $ CubeSet redCount greenCount blueCount

colorCountP :: Parser (Int, String)
colorCountP = do
    count <- intP <* space
    color <- string "red" <|> string "green" <|> string "blue"
    return (count, color)

-- | The given thresholds for the number of cubes that can be revealed, for a game to be possible.
cubeThresholds :: CubeSet
cubeThresholds = CubeSet 12 13 14

{- | Returns the ids of the games that are possible with the given thresholds.

A game is possible if all of its reveals are possible.
A reveal is possible if for each color,
the number of cubes is less than or equal to the number of cubes with that color
revealed in the threshold.
-}
findPossibleGameIds :: CubeSet -> [Game] -> [Int]
findPossibleGameIds thresholds = map gameId . filter (isPossibleGame thresholds)

isPossibleGame :: CubeSet -> Game -> Bool
isPossibleGame thresholds = all (isPossibleReveal thresholds) . gameReveals

isPossibleReveal :: CubeSet -> CubeSet -> Bool
isPossibleReveal (CubeSet red green blue) (CubeSet red' green' blue') =
    red' <= red && green' <= green && blue' <= blue

{- | Calculates the minimum cube set for each game.

The minimum cube set for a game is the set with the fewest number of cubes of each color,
that still satisfies the all the game's reveals.
-}
getMinimumCubeSets :: [Game] -> [CubeSet]
getMinimumCubeSets = map getMinimumCubeSet

getMinimumCubeSet :: Game -> CubeSet
getMinimumCubeSet = foldr maxCubeValues (CubeSet 0 0 0) . gameReveals

maxCubeValues :: CubeSet -> CubeSet -> CubeSet
maxCubeValues (CubeSet red green blue) (CubeSet red' green' blue') =
    CubeSet (max red red') (max green green') (max blue blue')

-- | Returns the power value of a given cube set.
getCubeSetPower :: CubeSet -> Int
getCubeSetPower (CubeSet red green blue) = red * green * blue
