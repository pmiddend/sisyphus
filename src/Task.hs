{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task
  ( Task (..),
    Repeater (..),
    RegularTask,
    RepeatingTask,
    TaskId (..),
    Weekday (..),
    LeisureProject (..),
    LeisureId (..),
    DisplayMode (..),
    Importance (..),
    TimeEstimate (..),
    mapRepeater,
    annealTasksInModel,
    fromJsWeekday,
    showMiso,
    increaseTaskId,
    mapTaskId,
    increaseLeisureId,
    isEveryNDays,
    isEveryWeekday,
    estimateInMinutes,
    weekdayToAllocationTime,
  )
where

import Data.Aeson
import Data.List (partition)
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Data.Time.Calendar (Day, diffDays)
import GHC.Generics (Generic)
import Miso.String (MisoString, toMisoString)
import Simanneal
import Prelude hiding (all)

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq, Generic, Bounded, Enum)

-- instance Show Weekday where
--   show Monday = "Montag"
--   show Tuesday = "Dienstag"
--   show Wednesday = "Mittwoch"
--   show Thursday = "Donnerstag"
--   show Friday = "Freitag"
--   show Saturday = "Samstag"
--   show Sunday = "Sonntag"

instance FromJSON Weekday

instance ToJSON Weekday

fromJsWeekday :: Int -> Maybe Weekday
fromJsWeekday i = case i of
  0 -> Just Sunday
  1 -> Just Monday
  2 -> Just Tuesday
  3 -> Just Wednesday
  4 -> Just Thursday
  5 -> Just Friday
  6 -> Just Saturday
  _ -> Nothing

newtype Importance = Importance Int deriving (Eq, Ord)

instance Show Importance where
  show (Importance x) = case x of
    0 -> "Unwichtig"
    1 -> "Wichtig"
    2 -> "Superwichtig"
    _ -> "Wichtigkeit(" <> show x <> ")"

importanceNumeric :: Importance -> Int
importanceNumeric (Importance x) = x

instance FromJSON Importance where
  parseJSON = withScientific "Importance" (pure . Importance . floor)

instance FromJSON TaskId where
  parseJSON = withScientific "TaskId" (pure . TaskId . floor)

instance FromJSON LeisureId where
  parseJSON = withScientific "LeisureId" (pure . LeisureId . floor)

newtype TimeEstimate = TimeEstimate Int deriving (Eq, Ord, Num, Enum, Integral, Real)

instance Semigroup TimeEstimate where
  TimeEstimate a <> TimeEstimate b = TimeEstimate (a + b)

instance Monoid TimeEstimate where
  mempty = TimeEstimate 0

instance Show TimeEstimate where
  show (TimeEstimate x) = case x of
    10 -> "<10min"
    30 -> "30min"
    60 -> "1h"
    120 -> ">1h"
    _ -> show x <> "min"

instance FromJSON TimeEstimate where
  parseJSON = withScientific "TimeEstimate" (pure . TimeEstimate . floor)

data Repeater = EveryNDays Int | EveryWeekday Weekday deriving (Eq, Show, Generic)

isEveryNDays :: Repeater -> Bool
isEveryNDays (EveryNDays _) = True
isEveryNDays _ = False

isEveryWeekday :: Repeater -> Bool
isEveryWeekday (EveryWeekday _) = True
isEveryWeekday _ = False

instance ToJSON Repeater

instance FromJSON Repeater

data Task idType repeaterType = Task
  { title :: MisoString,
    importance :: Importance,
    deadline :: Maybe Day,
    timeEstimate :: TimeEstimate,
    completionDay :: Maybe Day,
    taskId :: idType,
    repeater :: repeaterType
  }
  deriving (Show, Generic, Eq)

mapTaskId ::
  forall idTypeBefore idTypeAfter repeaterType.
  (idTypeBefore -> idTypeAfter) ->
  Task idTypeBefore repeaterType ->
  Task idTypeAfter repeaterType
mapTaskId f t = t {taskId = f (taskId t)}

mapRepeater ::
  forall idType repeaterTypeBefore repeaterTypeAfter.
  (repeaterTypeBefore -> repeaterTypeAfter) ->
  Task idType repeaterTypeBefore ->
  Task idType repeaterTypeAfter
mapRepeater f t = t {repeater = f (repeater t)}

-- mapTaskBoth ::
--   forall idTypeBefore idTypeAfter repeaterTypeBefore repeaterTypeAfter.
--   (idTypeBefore -> idTypeAfter) ->
--   (repeaterTypeBefore -> repeaterTypeAfter) ->
--   Task idTypeBefore repeaterTypeBefore ->
--   Task idTypeAfter repeaterTypeAfter
-- mapTaskBoth f g t = t {taskId = f (taskId t), repeater = g (repeater t)}

instance (FromJSON idType, FromJSON repeaterType) => FromJSON (Task idType repeaterType) where
  parseJSON = withObject "Task" $ \v -> Task <$> (v .: "title") <*> (v .: "importance") <*> (v .: "deadline") <*> (v .: "time-estimate") <*> (v .: "completion-day") <*> (v .: "id") <*> (v .: "repeater")

instance (ToJSON idType, ToJSON repeaterType) => ToJSON (Task idType repeaterType) where
  toJSON (Task title (Importance importance) deadline (TimeEstimate timeEstimate) completionDay taskId repeater) =
    object
      [ "title" .= title,
        "importance" .= importance,
        "time-estimate" .= timeEstimate,
        "completion-day" .= completionDay,
        "deadline" .= deadline,
        "id" .= taskId,
        "repeater" .= repeater
      ]

newtype TaskId = TaskId Int deriving (Eq, Show, Ord)

increaseTaskId :: TaskId -> TaskId
increaseTaskId (TaskId i) = TaskId (i + 1)

increaseLeisureId :: LeisureId -> LeisureId
increaseLeisureId (LeisureId i) = LeisureId (i + 1)

newtype LeisureId = LeisureId Int deriving (Eq, Show, Ord)

data LeisureProject a = LeisureProject
  { leisureTitle :: MisoString,
    leisureId :: a
  }
  deriving (Show, Eq, Generic, Functor)

instance FromJSON a => FromJSON (LeisureProject a)

instance ToJSON a => ToJSON (LeisureProject a)

data DisplayMode = DisplayWork | DisplayLeisure deriving (Show, Eq)

type RegularTask = Task TaskId (Maybe TaskId)

type RepeatingTask = Task TaskId Repeater

instance ToJSON TaskId where
  toJSON (TaskId i) = toJSON i

instance ToJSON LeisureId where
  toJSON (LeisureId i) = toJSON i

taskEstimateSum :: [Task idType repeaterType] -> TimeEstimate
taskEstimateSum ts = TimeEstimate (sum (estimateInMinutes . timeEstimate <$> ts))

annealTasksInModel :: forall idType repeaterType. Ord idType => Seed -> Day -> TimeEstimate -> [Task idType repeaterType] -> S.Set idType
annealTasksInModel seed' today' timeBudgetForToday tasks =
  S.fromList
    ( taskId
        <$> annealTasks
          seed'
          today'
          timeBudgetForToday
          (taskEstimateSum (filter (\t -> completionDay t == Just today') tasks))
          (filter (\t -> isNothing (completionDay t)) tasks)
    )

estimateInMinutes :: TimeEstimate -> Int
estimateInMinutes (TimeEstimate e) = e

showMiso :: Show a => a -> MisoString
showMiso = toMisoString . show

removeIndex :: Int -> [a] -> [a]
removeIndex i a = take i a ++ drop (i + 1) a

removeRandomElement :: [a] -> SimannealState ([a], [a]) (a, [a])
removeRandomElement xs = do
  randomIndex <- randomRS (0, length xs - 1)
  pure (xs !! randomIndex, removeIndex randomIndex xs)

annealTasks :: forall idType repeaterType. Seed -> Day -> TimeEstimate -> TimeEstimate -> [Task idType repeaterType] -> [Task idType repeaterType]
annealTasks seed today' timeBudgetForToday spentMinutes allTasks =
  let taskImportanceSum :: [Task idType repeaterType] -> Float
      taskImportanceSum ts = fromIntegral (sum ((importanceNumeric . importance) <$> ts))
      taskUrgency :: Task idType repeaterType -> Float
      taskUrgency t = case deadline t of
        Nothing -> 0.0
        Just d ->
          min 3.0 (fromIntegral (diffDays d today'))
      taskUrgencySum :: [Task idType repeaterType] -> Float
      taskUrgencySum ts = sum (taskUrgency <$> ts)
      allocated :: TimeEstimate
      allocated = max 0 (timeBudgetForToday - spentMinutes)
      totalEstimate :: TimeEstimate
      totalEstimate = taskEstimateSum allTasks
      maxDistanceToAllocated :: TimeEstimate
      maxDistanceToAllocated = max allocated totalEstimate
      distance x y = abs (x - y)
      (baseTasks, remainingTasks) = partition (\t -> deadline t == Just today') allTasks
      taskEnergy :: ([Task idType repeaterType], [Task idType repeaterType]) -> Energy
      taskEnergy (ts, _) =
        let closeToAllocated :: Float
            closeToAllocated = fromIntegral (distance allocated (taskEstimateSum (ts <> baseTasks))) / fromIntegral maxDistanceToAllocated
            sumImportance = taskImportanceSum ts
            sumUrgency = taskUrgencySum ts
         in Energy (closeToAllocated - 0.05 * sumImportance - 0.05 * sumUrgency)
      mutateTasks :: ([Task idType repeaterType], [Task idType repeaterType]) -> SimannealState ([Task idType repeaterType], [Task idType repeaterType]) ([Task idType repeaterType], [Task idType repeaterType])
      mutateTasks (chosenTasks, openTasks) = do
        removeOrAdd :: Int <- randomRS (1, 100)
        let thisIterationRemoves = removeOrAdd <= 50
        if length chosenTasks > 1 && thisIterationRemoves
          then do
            (removedTask, newChosenTasks) <- removeRandomElement chosenTasks
            pure (newChosenTasks, removedTask : openTasks)
          else
            if length openTasks >= 1
              then do
                (removedTask, newOpenTasks) <- removeRandomElement openTasks
                pure (removedTask : chosenTasks, newOpenTasks)
              else pure (chosenTasks, openTasks)
   in if length allTasks <= 1 || null remainingTasks
        then allTasks
        else
          let solution = simanneal seed (remainingTasks, []) mutateTasks taskEnergy 100.0 0.01 0.5
           in (baseTasks <> fst solution)

weekdayToAllocationTime :: Weekday -> TimeEstimate
weekdayToAllocationTime Saturday = TimeEstimate 180
weekdayToAllocationTime Sunday = TimeEstimate 180
weekdayToAllocationTime _ = TimeEstimate 80
