{-
This file is part of my solutions for Advent of Code 2023.
Copyright (C) 2024  Luka Berkers

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

module Main (main) where

import Day2 (getCubeSetPower, getMinimumCubeSets, readGameRecords, gameRecordsFilePath)
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

    gameRecordsResult <- readGameRecords gameRecordsFilePath
    case gameRecordsResult of
        Left err -> do
            logM logger CRITICAL $ show err
            putStrLn "Failed to read or parse the game records."
        Right gameRecords -> do
            let minCubeSets = getMinimumCubeSets gameRecords
            let minCubeSetPowers = map getCubeSetPower minCubeSets
            logM logger INFO $ "Minimum cube set powers: " ++ show minCubeSetPowers
            putStrLn $ "Sum of minimum cube set powers: " ++ show (sum minCubeSetPowers)
