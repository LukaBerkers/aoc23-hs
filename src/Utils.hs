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
