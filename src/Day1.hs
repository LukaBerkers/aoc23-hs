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

findFirstDigit :: String -> Maybe Char
findFirstDigit = find isDigit

findLastDigit :: String -> Maybe Char
findLastDigit = find isDigit . reverse
