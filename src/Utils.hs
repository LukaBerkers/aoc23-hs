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

module Utils (inputDir, readInputLines, readInputFile) where

import Control.Exception (try)

-- | The directory where the input files are stored.
inputDir :: FilePath
inputDir = "input"

-- | Reads a file line by line and parses each line using the provided parser.
readInputLines :: FilePath -> (String -> a) -> IO (Either IOError [a])
readInputLines filePath lineParser = readInputFile filePath (map lineParser . lines)

-- | Reads a file and parses its contents using the provided parser.
readInputFile :: FilePath -> (String -> a) -> IO (Either IOError a)
readInputFile filePath fileParser = do
    readResult <- try $ readFile filePath :: IO (Either IOError String)

    return $ case readResult of
        Left err -> Left err
        Right fileContents -> Right $ fileParser fileContents
