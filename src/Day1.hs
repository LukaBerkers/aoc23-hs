module Day1 (readCalibrationDocument, recoverCalibrationValues) where

import Utils (inputDir, readInputFile)

import Data.Char (isDigit)
import Data.Foldable (find)
import System.FilePath ((</>))

calibrationDocumentFileName :: FilePath
calibrationDocumentFileName = inputDir </> "day1.txt"

-- | Reads the calibration document and returns the lines as a list of strings.
readCalibrationDocument :: IO [String]
readCalibrationDocument = readInputFile calibrationDocumentFileName id

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
findFirstDigit ('o':'n':'e':_) = Just '1'
findFirstDigit ('t':'w':'o':_) = Just '2'
findFirstDigit ('t':'h':'r':'e':'e':_) = Just '3'
findFirstDigit ('f':'o':'u':'r':_) = Just '4'
findFirstDigit ('f':'i':'v':'e':_) = Just '5'
findFirstDigit ('s':'i':'x':_) = Just '6'
findFirstDigit ('s':'e':'v':'e':'n':_) = Just '7'
findFirstDigit ('e':'i':'g':'h':'t':_) = Just '8'
findFirstDigit ('n':'i':'n':'e':_) = Just '9'
findFirstDigit (c : cs)
    | isDigit c = Just c
    | otherwise = findFirstDigit cs

findLastDigit :: String -> Maybe Char
findLastDigit = findDigitBackwards . reverse

findDigitBackwards :: String -> Maybe Char
findDigitBackwards [] = Nothing
findDigitBackwards ('e':'n':'o':_) = Just '1'
findDigitBackwards ('o':'w':'t':_) = Just '2'
findDigitBackwards ('e':'e':'r':'h':'t':_) = Just '3'
findDigitBackwards ('r':'u':'o':'f':_) = Just '4'
findDigitBackwards ('e':'v':'i':'f':_) = Just '5'
findDigitBackwards ('x':'i':'s':_) = Just '6'
findDigitBackwards ('n':'e':'v':'e':'s':_) = Just '7'
findDigitBackwards ('t':'h':'g':'i':'e':_) = Just '8'
findDigitBackwards ('e':'n':'i':'n':_) = Just '9'
findDigitBackwards (c : cs)
    | isDigit c = Just c
    | otherwise = findDigitBackwards cs
