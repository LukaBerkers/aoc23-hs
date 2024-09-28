module Main (main) where

import Day1 (readCalibrationDocument, recoverCalibrationValues)

import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger (Priority (..), logM, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)

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

    calibrationLines <- readCalibrationDocument
    case recoverCalibrationValues calibrationLines of
        Just calibrationValues -> do
            putStrLn $ "Sum of calibration values: " ++ show (sum calibrationValues)
        Nothing -> do
            logM logger CRITICAL "Not all lines in the calibration document contained a calibration value."
