{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Data.Time
import qualified Data.Map.Strict as M

import Lens.Micro.Platform
import Text.Parsec

import Data
import Activities
import CLParser
import Pretty


main :: IO ()
main = do
  zonedTime <- getZonedTime
  let time = localDay $ zonedTimeToLocalTime zonedTime

  -- use ExceptT IO
  maybeActivities <- readActivities
  case maybeActivities of
    Left err -> putStrLn $ err <> " please look at the data.json file and try to repair it or delete it. If you delete it you'll lose all your data"
    Right activities -> go activities time

  where
    go :: Activities -> Day -> IO ()
    go activities time = do
      input <- getLine
      case parse parseCommand "" input of
        Left err -> putStrLn (show err) >> main
        Right command -> case command of
          ActivityDone name -> let activity = activityDone name time activities in
            putStrLn ("Activity " <> name <> " done " <> show ((( getActivities activity) M.! name) ^. counter) <> " times.") >> go activity time
          RenameActivity oldName newName -> case renameActivity oldName newName activities of
            Left err -> putStrLn (pretty err) >> go activities time
            Right newMap -> (putStrLn $ oldName <> " successfully renamed to " <> newName) >> go newMap time
          RemoveActivity name -> confirmation >>= (\ans -> if ans == False then go activities time
                                                                           else go (removeActivity name activities) time)
          AddActivity name -> go (addActivity name time activities) time
          Help -> undefined

{-'
-- commandCheck :: Command -> MaybeT Reader Map
oldmain :: IO ()
oldmain = do
  -- checking if acitvity aren't timed out
  input <- getLine
  case parse parseCommand "" input of
    Left err -> print err >> main
    Right command -> case command of
      _ -> undefined
--}
{-}}
-- fix localtime, take day
checkActivities :: [Activity] -> LocalTime -> [Activity]
checkActivities activities today = map checkActivity activities
  where
    checkActivity :: Activity -> Activity
    checkActivity activity = if wasOneDayAgo (localDay today) (activity ^. lastDone)
                             then activity
                             else activity & counter .~ 0


wasOneDayAgo :: Day -> Day -> Bool
wasOneDayAgo day1 day2 = (diffDays day1 day2) <= 1


madeUpActivities :: Day -> Day -> Day -> Activities
madeUpActivities today yesterday longAgo = Activities $ M.fromList [("jogging", (Activity 1 today))
                                                                   ,("prog", (Activity 2 yesterday))
                                                                   ,("study", (Activity 3 longAgo))
                                                                   ]

printActivity :: Activity -> String
printActivity activity = "Done " <> show (activity ^. counter) <> " times. Last time was " <> show (activity ^. lastTimeDone) <> ". \n"
-}
