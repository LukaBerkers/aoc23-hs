module Utils (inputDir, readInputLines, readInputFile) where

-- | The directory where the input files are stored.
inputDir :: FilePath
inputDir = "input"

-- | Reads a file line by line and parses each line using the provided parser.
readInputLines :: FilePath -> (String -> a) -> IO [a]
readInputLines filePath lineParser = map lineParser . lines <$> readFile filePath

-- | Reads a file and parses its contents using the provided parser.
readInputFile :: FilePath -> (String -> a) -> IO a
readInputFile filePath fileParser = fileParser <$> readFile filePath
