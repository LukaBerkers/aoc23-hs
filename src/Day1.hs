module Day1 (readCalibrationDocument, recoverCalibrationValues) where

import Utils (inputDir)

import Data.Char (isDigit)
import Data.Foldable (find)
import System.FilePath ((</>))

calibrationDocumentFile :: FilePath
calibrationDocumentFile = inputDir </> "day1.txt"

readCalibrationDocument :: IO [String]
readCalibrationDocument = lines <$> readFile calibrationDocumentFile

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
