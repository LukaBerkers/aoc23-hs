module Utils (readInputFile, inputDir) where

inputDir :: FilePath
inputDir = "input"

readInputFile :: FilePath -> (String -> a) -> IO [a]
readInputFile filePath lineParser = map lineParser . lines <$> readFile filePath