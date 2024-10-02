module Day1 (readCalibrationDocument, recoverCalibrationValues) where

import Utils (inputDir, readInputFile)

import Data.Char (isDigit)
import Data.Foldable (find)
import System.FilePath ((</>))

calibrationDocumentFileName :: FilePath
calibrationDocumentFileName = inputDir </> "day1.txt"

readCalibrationDocument :: IO [String]
readCalibrationDocument = readInputFile calibrationDocumentFileName id

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
