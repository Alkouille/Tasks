{-# LANGUAGE TemplateHaskell #-}

module Activities where

import Lens.Micro.Platform

import Data.Time
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map.Strict as M



import Pretty

type Name = String


data RuntimeError
  = ActivityDoesNotExist Name


data ActivityData = ActivityData { _counter :: Integer
                                 , _lastDone :: Day
                                 }

data Activity = Activity { _activityName :: Name
                         , _activityData :: ActivityData
                         }

makeLenses ''ActivityData
makeLenses ''Activity
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ActivityData


lookupActivity :: Name -> Activities -> Maybe Activity
lookupActivity name activs = case M.lookup name (getActivities activs) of
  Nothing -> Nothing
  Just activity -> Just $ Activity name activity

newtype Activities = Activities { getActivities :: M.Map Name ActivityData }
deriveJSON defaultOptions ''Activities


resetActivity :: Name -> Activities -> Activities
resetActivity name activs = Activities $ M.adjust (\act -> (act & counter .~ 0)) name (getActivities activs)


activityDone :: Name -> Day -> Activities -> Activities
activityDone name today activs = Activities $ M.adjust (\act -> (act & counter %~ (+1) & lastDone .~ today )) name (getActivities activs)


addActivity :: Name -> Day -> Activities -> Activities
addActivity name today activs = Activities $ M.insert name (ActivityData 0 today) (getActivities activs)


removeActivity :: Name -> Activities -> Activities
removeActivity name activs = Activities $ M.delete name (getActivities activs)


renameActivity :: Name -> Name -> Activities -> Either RuntimeError Activities
renameActivity oldName newName (Activities activs) = case M.lookup oldName activs of
  Nothing -> Left $ ActivityDoesNotExist oldName
  Just activ -> Right . Activities $ M.insert newName activ (M.delete oldName activs)


checkActivities :: [Activity] -> Day -> [Activity]
checkActivities activities today = map checkActivity activities
  where
    checkActivity :: Activity -> Activity
    checkActivity activity = if wasOneDayAgo today (activity ^. activityData ^. lastDone)
                             then activity
                             else activity & activityData . counter .~ 0


wasOneDayAgo :: Day -> Day -> Bool
wasOneDayAgo day1 day2 = (diffDays day1 day2) <= 1


madeUpActivities :: Day -> Day -> Day -> Activities
madeUpActivities today yesterday longAgo = Activities $ M.fromList [("jogging", (ActivityData 1 today))
                                                                   ,("prog", (ActivityData 2 yesterday))
                                                                   ,("study", (ActivityData 3 longAgo)) -- find a way to get the day before yesterday
                                                                   ]

activityDataToActivity :: ActivityData -> Name -> Activity
activityDataToActivity activData name = Activity name activData


activitiesToList :: Activities -> [Activity]
activitiesToList activs = map fun (M.toList (getActivities activs))
  where
    fun :: (Name, ActivityData) -> Activity
    fun (name, activData) = Activity name activData


instance Pretty Activity where
  pretty activity = "activity " <> activity ^. activityName <> " done " <> show (activity ^. activityData ^. counter) <> " times."


instance Pretty Activities where
  pretty activities = (activitiesToList activities) >>= pretty


instance Pretty RuntimeError where
  pretty (ActivityDoesNotExist name) = "Error, activity \"" <> name <> "\" does not exist"
