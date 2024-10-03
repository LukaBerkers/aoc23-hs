module Main (main) where

import Day2 (readGameRecords)
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger (
    Priority (..),
    logM,
    rootLoggerName,
    setHandlers,
    setLevel,
    updateGlobalLogger,
 )

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
        Left err -> logM logger CRITICAL $ "Failed to read game records: " ++ show err
        Right gameRecords -> logM logger INFO $ "Game records: " ++ show gameRecords
