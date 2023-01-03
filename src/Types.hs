{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
  ( Task (..),
    Repeater (..),
    RegularTask,
    RepeatingTask,
    TaskId (..),
    Weekday (..),
    LeisureProject (..),
    LeisureCategory (..),
    LeisureId (..),
    DisplayMode (..),
    LeisureMode (..),
    Importance (..),
    leisureMode,
    newTaskFormOpen,
    importance,
    leisureCategory,
    mkLeisureCategory,
    leisureTitle,
    leisureId,
    repeater,
    deadline,
    TimeEstimate (..),
    ExplicitAllocation (..),
    Model (..),
    mapRepeater,
    completionDay,
    increaseTaskId,
    mapTaskId,
    increaseLeisureId,
    isEveryNDays,
    isEveryWeekday,
    seed,
    created,
    title,
    displayMode,
    explicitAllocation,
    numericImportance,
    taskId,
    timeEstimate,
    explicitAllocationChanging,
    leisureProjects,
    newLeisureProject,
    newTask,
    remainingTasksOpened,
    repeatingTasks,
    statusMessages,
    annealedTasks,
    tasks,
    today,
  )
where

import Control.Lens (Iso', iso, makeLenses)
import Data.Aeson
import Data.Bifunctor (first)
import qualified Data.Set as S
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Miso.String (MisoString)
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

newtype TaskId = TaskId Int deriving (Eq, Show, Ord)

newtype LeisureId = LeisureId Int deriving (Eq, Show, Ord)

newtype Importance = Importance Int deriving (Eq, Ord)

numericImportance :: Iso' Importance Int
numericImportance = iso (\(Importance i) -> i) Importance

instance Show Importance where
  show (Importance x) = case x of
    0 -> "Unwichtig"
    1 -> "Wichtig"
    2 -> "Superwichtig"
    _ -> "Wichtigkeit(" <> show x <> ")"

instance FromJSON Importance where
  parseJSON = withScientific "Importance" (pure . Importance . floor)

instance FromJSON TaskId where
  parseJSON = withScientific "TaskId" (pure . TaskId . floor)

instance FromJSON LeisureId where
  parseJSON = withScientific "LeisureId" (pure . LeisureId . floor)

newtype TimeEstimate = TimeEstimate Int deriving (Eq, Ord, Num, Enum, Integral, Real)

instance Semigroup TimeEstimate where
  TimeEstimate a <> TimeEstimate b = TimeEstimate (a + b)

instance Read TimeEstimate where
  readsPrec i s = first TimeEstimate <$> readsPrec i s

instance Monoid TimeEstimate where
  mempty = TimeEstimate 0

instance Show TimeEstimate where
  show (TimeEstimate x) = show x <> "min"

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
  { _title :: MisoString,
    _created :: Day,
    _importance :: Importance,
    _deadline :: Maybe Day,
    _timeEstimate :: TimeEstimate,
    _completionDay :: Maybe Day,
    _taskId :: idType,
    _repeater :: repeaterType
  }
  deriving (Show, Generic, Eq)

makeLenses ''Task

mapTaskId ::
  forall idTypeBefore idTypeAfter repeaterType.
  (idTypeBefore -> idTypeAfter) ->
  Task idTypeBefore repeaterType ->
  Task idTypeAfter repeaterType
mapTaskId f t = t {_taskId = f (_taskId t)}

mapRepeater ::
  forall idType repeaterTypeBefore repeaterTypeAfter.
  (repeaterTypeBefore -> repeaterTypeAfter) ->
  Task idType repeaterTypeBefore ->
  Task idType repeaterTypeAfter
mapRepeater f t = t {_repeater = f (_repeater t)}

-- mapTaskBoth ::
--   forall idTypeBefore idTypeAfter repeaterTypeBefore repeaterTypeAfter.
--   (idTypeBefore -> idTypeAfter) ->
--   (repeaterTypeBefore -> repeaterTypeAfter) ->
--   Task idTypeBefore repeaterTypeBefore ->
--   Task idTypeAfter repeaterTypeAfter
-- mapTaskBoth f g t = t {taskId = f (taskId t), repeater = g (repeater t)}

instance (FromJSON idType, FromJSON repeaterType) => FromJSON (Task idType repeaterType) where
  parseJSON = withObject "Task" $ \v -> Task <$> (v .: "title") <*> (v .: "created") <*> (v .: "importance") <*> (v .: "deadline") <*> (v .: "time-estimate") <*> (v .: "completion-day") <*> (v .: "id") <*> (v .: "repeater")

instance (ToJSON idType, ToJSON repeaterType) => ToJSON (Task idType repeaterType) where
  toJSON (Task title' created' (Importance importance') deadline' (TimeEstimate timeEstimate') completionDay' taskId' repeater') =
    object
      [ "title" .= title',
        "created" .= created',
        "importance" .= importance',
        "time-estimate" .= timeEstimate',
        "completion-day" .= completionDay',
        "deadline" .= deadline',
        "id" .= taskId',
        "repeater" .= repeater'
      ]

increaseTaskId :: TaskId -> TaskId
increaseTaskId (TaskId i) = TaskId (i + 1)

increaseLeisureId :: LeisureId -> LeisureId
increaseLeisureId (LeisureId i) = LeisureId (i + 1)

newtype LeisureCategory = LeisureCategory {_mkLeisureCategory :: MisoString} deriving (Eq, Show, Ord, Generic)

makeLenses ''LeisureCategory

instance FromJSON LeisureCategory

instance ToJSON LeisureCategory

data LeisureProject a = LeisureProject
  { _leisureTitle :: MisoString,
    _leisureId :: a,
    _leisureCategory :: LeisureCategory
  }
  deriving (Show, Eq, Generic, Functor)

makeLenses ''LeisureProject

instance FromJSON a => FromJSON (LeisureProject a)

instance ToJSON a => ToJSON (LeisureProject a)

data DisplayMode = DisplayWork | DisplayLeisure | DisplayDebug deriving (Show, Eq)

data LeisureMode = LeisureAll | LeisureSelected deriving (Show, Eq)

data TaskMode = TaskModeAll | TaskModeSelected deriving (Show, Eq)

type RegularTask = Task TaskId (Maybe TaskId)

type RepeatingTask = Task TaskId Repeater

instance ToJSON TaskId where
  toJSON (TaskId i) = toJSON i

instance ToJSON TimeEstimate where
  toJSON (TimeEstimate i) = toJSON i

instance ToJSON LeisureId where
  toJSON (LeisureId i) = toJSON i

data ExplicitAllocation = ExplicitAllocation
  { eaDate :: Day,
    eaValue :: TimeEstimate
  }
  deriving (Eq, Show, Generic)

instance FromJSON ExplicitAllocation

instance ToJSON ExplicitAllocation

data Model = Model
  { _newTask :: Task () (Maybe Repeater),
    _newTaskFormOpen :: Bool,
    _tasks :: [RegularTask],
    _repeatingTasks :: [RepeatingTask],
    _explicitAllocation :: Maybe ExplicitAllocation,
    _explicitAllocationChanging :: Maybe TimeEstimate,
    _remainingTasksOpened :: Bool,
    _annealedTasks :: S.Set TaskId,
    _statusMessages :: [MisoString],
    _today :: Day,
    _seed :: Int,
    _leisureProjects :: [LeisureProject LeisureId],
    _newLeisureProject :: LeisureProject (),
    _displayMode :: DisplayMode,
    _leisureMode :: LeisureMode
  }
  deriving (Show, Generic, Eq)

makeLenses ''Model
