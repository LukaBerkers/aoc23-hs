module Utils (readInputFile, inputDir) where

-- | The directory where the input files are stored.
inputDir :: FilePath
inputDir = "input"

-- | Reads a file line by line and parses each line using the provided parser.
readInputFile :: FilePath -> (String -> a) -> IO [a]
readInputFile filePath lineParser = map lineParser . lines <$> readFile filePath