module Main (main) where

import Day2 (cubeThresholds, findPossibleGameIds, readGameRecords)
import System.IO (stderr)
import System.Log (Priority (..))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger (logM, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)

moduleName :: String
moduleName = "Main"

configureLogger :: IO ()
configureLogger = do
    let formatter = simpleLogFormatter "$time [$prio] $loggername - $msg"
    handler <- streamHandler stderr INFO >>= \h -> return $ setFormatter h formatter
    updateGlobalLogger rootLoggerName (setLevel INFO . setHandlers [handler])

main :: IO ()
main = do
    configureLogger
    let logger = moduleName ++ ".main"

    gameRecordsResult <- readGameRecords
    case gameRecordsResult of
        Left err -> do
            logM logger CRITICAL $ show err
            putStrLn "Failed to read or parse the game records."
        Right gameRecords -> do
            let possibleGameIds = findPossibleGameIds cubeThresholds gameRecords
            putStrLn $ "Sum of possible game IDs: " ++ show (sum possibleGameIds)
