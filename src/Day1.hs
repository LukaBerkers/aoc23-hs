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

module Day1 (readCalibrationDocument, recoverCalibrationValues) where

import Data.Char (isDigit)
import Data.Foldable (find)
import System.FilePath ((</>))
import Utils (inputDir, readInputLines)

calibrationDocumentFilePath :: FilePath
calibrationDocumentFilePath = inputDir </> "day1.txt"

-- | Reads the calibration document and returns the lines as a list of strings.
readCalibrationDocument :: IO (Either IOError [String])
readCalibrationDocument = readInputLines calibrationDocumentFilePath id

{- | Recovers the calibration values from the calibration document lines.

The calibration values are the first and last digit of each line in the calibration document.
The "digits" may be actual digits, or spelled out as letters in English.
There may be only one digit in a line, in which case the first and last digit are the same.
If any line does not contain a calibration value, 'Nothing' is returned.
Otherwise, the calibration values are returned as a list of integers.
-}
recoverCalibrationValues :: [String] -> Maybe [Int]
recoverCalibrationValues = traverse recoverCalibrationValue

recoverCalibrationValue :: String -> Maybe Int
recoverCalibrationValue s = do
    firstDigit <- findFirstDigit s
    lastDigit <- findLastDigit s
    pure $ read [firstDigit, lastDigit]

_findFirstDigit :: String -> Maybe Char
_findFirstDigit = find isDigit

_findLastDigit :: String -> Maybe Char
_findLastDigit = find isDigit . reverse

findFirstDigit :: String -> Maybe Char
findFirstDigit [] = Nothing
findFirstDigit ('o' : 'n' : 'e' : _) = Just '1'
findFirstDigit ('t' : 'w' : 'o' : _) = Just '2'
findFirstDigit ('t' : 'h' : 'r' : 'e' : 'e' : _) = Just '3'
findFirstDigit ('f' : 'o' : 'u' : 'r' : _) = Just '4'
findFirstDigit ('f' : 'i' : 'v' : 'e' : _) = Just '5'
findFirstDigit ('s' : 'i' : 'x' : _) = Just '6'
findFirstDigit ('s' : 'e' : 'v' : 'e' : 'n' : _) = Just '7'
findFirstDigit ('e' : 'i' : 'g' : 'h' : 't' : _) = Just '8'
findFirstDigit ('n' : 'i' : 'n' : 'e' : _) = Just '9'
findFirstDigit (c : cs)
    | isDigit c = Just c
    | otherwise = findFirstDigit cs

findLastDigit :: String -> Maybe Char
findLastDigit = findDigitBackwards . reverse

findDigitBackwards :: String -> Maybe Char
findDigitBackwards [] = Nothing
findDigitBackwards ('e' : 'n' : 'o' : _) = Just '1'
findDigitBackwards ('o' : 'w' : 't' : _) = Just '2'
findDigitBackwards ('e' : 'e' : 'r' : 'h' : 't' : _) = Just '3'
findDigitBackwards ('r' : 'u' : 'o' : 'f' : _) = Just '4'
findDigitBackwards ('e' : 'v' : 'i' : 'f' : _) = Just '5'
findDigitBackwards ('x' : 'i' : 's' : _) = Just '6'
findDigitBackwards ('n' : 'e' : 'v' : 'e' : 's' : _) = Just '7'
findDigitBackwards ('t' : 'h' : 'g' : 'i' : 'e' : _) = Just '8'
findDigitBackwards ('e' : 'n' : 'i' : 'n' : _) = Just '9'
findDigitBackwards (c : cs)
    | isDigit c = Just c
    | otherwise = findDigitBackwards cs
