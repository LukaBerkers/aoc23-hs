module Utils (readInputFile) where

import System.FilePath ((</>))

inputDir :: FilePath
inputDir = "input"

readInputFile :: FilePath -> (String -> a) -> IO [a]
readInputFile inputFileName lineParser = map lineParser . lines <$> readFile (inputDir </> inputFileName)