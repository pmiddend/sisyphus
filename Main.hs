{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Aeson
import Data.List (maximumBy, partition, sortBy)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as S
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Miso
import Miso.String (MisoString, fromMisoString, fromMisoStringEither, toMisoString)
import Simanneal
import Prelude hiding (all)
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
-- import qualified Network.Wai.Handler.Warp         as Warp
-- import           Network.WebSockets
#endif

#ifndef __GHCJS__
getCurrentDay :: JSM MisoString
getCurrentDay = pure "2022-12-13"

getCurrentWeekday :: JSM Int
getCurrentWeekday = pure 1
#else
foreign import javascript unsafe "$r = new Date().toISOString().substr(0,10)"
  getCurrentDay :: JSM MisoString

foreign import javascript unsafe "$r = new Date().getDay()"
  getCurrentWeekday :: JSM Int
#endif

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)

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

newtype TimeEstimate = TimeEstimate Int deriving (Eq, Ord)

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

data Task a = Task
  { title :: MisoString,
    importance :: Importance,
    deadline :: Maybe Day,
    timeEstimate :: TimeEstimate,
    completionDay :: Maybe Day,
    taskId :: a
  }
  deriving (Show, Generic, Eq, Functor)

instance FromJSON a => FromJSON (Task a) where
  parseJSON = withObject "Task" $ \v -> Task <$> (v .: "title") <*> (v .: "importance") <*> (v .: "deadline") <*> (v .: "time-estimate") <*> (v .: "completion-day") <*> (v .: "id")

instance ToJSON a => ToJSON (Task a) where
  toJSON (Task title (Importance importance) deadline (TimeEstimate timeEstimate) completionDay taskId) =
    object
      [ "title" .= title,
        "importance" .= importance,
        "time-estimate" .= timeEstimate,
        "completion-day" .= completionDay,
        "deadline" .= deadline,
        "id" .= taskId
      ]

newtype TaskId = TaskId Int deriving (Eq, Show, Ord)

increaseTaskId :: TaskId -> TaskId
increaseTaskId (TaskId i) = TaskId (i + 1)

-- | Type synonym for an application model
data Model = Model
  { newTask :: Task (),
    tasks :: [Task TaskId],
    annealedTasks :: S.Set TaskId,
    statusMessages :: [MisoString],
    today :: Day,
    weekday :: Weekday,
    seed :: Int
  }
  deriving (Show, Generic, Eq)

localStorageKey :: MisoString
localStorageKey = "v4"

data LocalStorageModel = LocalStorageModel
  { lsTasks :: [Task TaskId]
  }
  deriving (Generic, Show, Eq)

instance ToJSON TaskId where
  toJSON (TaskId i) = toJSON i

instance FromJSON LocalStorageModel

instance ToJSON LocalStorageModel

-- | Sum type for application events
data Action
  = LocalStorageReceived (Either String LocalStorageModel)
  | Nop
  | Init
  | IncreaseSeed
  | AddTaskClicked
  | ToggleDone TaskId
  | LocalStorageUpdated
  | CurrentDayReceived MisoString
  | CurrentWeekDayReceived Int
  | NewTaskChanged (Task ())
  deriving (Show, Eq)

initialTask :: Task ()
initialTask =
  Task
    { title = "",
      importance = Importance 0,
      deadline = Nothing,
      timeEstimate = TimeEstimate 10,
      completionDay = Nothing,
      taskId = ()
    }

initialModel :: Model
initialModel =
  Model
    { newTask = initialTask,
      tasks = mempty,
      annealedTasks = mempty,
      statusMessages = mempty,
      today = toEnum 0,
      weekday = Monday,
      seed = 14
    }

modelToLocalStorage :: Model -> LocalStorageModel
modelToLocalStorage (Model {tasks = tasks}) = LocalStorageModel tasks

setLocalStorageFromModel :: Model -> JSM Action
setLocalStorageFromModel newModel = LocalStorageUpdated <$ setLocalStorage localStorageKey (modelToLocalStorage newModel)

taskEstimateSum :: [Task a] -> Float
taskEstimateSum ts = fromIntegral (sum (estimateInMinutes . timeEstimate <$> ts))

annealTasksInModel :: forall a. Ord a => Seed -> Day -> Weekday -> [Task a] -> S.Set a
annealTasksInModel seed' today' weekday' tasks =
  S.fromList
    ( taskId
        <$> annealTasks
          seed'
          today'
          weekday'
          (taskEstimateSum (filter (\t -> completionDay t == Just today') tasks))
          (filter (\t -> isNothing (completionDay t)) tasks)
    )

updateTask :: Model -> TaskId -> (Task TaskId -> Task TaskId) -> Model
updateTask m tid f =
  let possiblyEditTask t = if taskId t == tid then f t else t
      newTasks = foldr (\t prevTasks -> possiblyEditTask t : prevTasks) [] (tasks m)
   in m {tasks = newTasks}

parseDay :: MisoString -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" . fromMisoString

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel IncreaseSeed m = noEff (m {seed = seed m + 1, annealedTasks = annealTasksInModel (seed m + 1) (today m) (weekday m) (tasks m)})
updateModel (LocalStorageReceived l) m =
  case l of
    Left errorMessage -> m <# do consoleLog ("error receiving local storage: " <> toMisoString errorMessage) >> pure Nop
    Right v -> noEff (m {tasks = lsTasks v, annealedTasks = annealTasksInModel (seed m) (today m) (weekday m) (lsTasks v)})
updateModel Init m = batchEff m [LocalStorageReceived <$> getLocalStorage localStorageKey, CurrentDayReceived <$> getCurrentDay, CurrentWeekDayReceived <$> getCurrentWeekday]
updateModel Nop m = noEff m
updateModel (CurrentWeekDayReceived d) m =
  case fromJsWeekday d of
    Nothing -> noEff (m {statusMessages = "couldn't parse weekday number: " <> showMiso d : statusMessages m})
    Just wd -> noEff (m {weekday = wd})
updateModel (CurrentDayReceived d) m =
  case fromMisoStringEither d of
    Left _ -> noEff (m {statusMessages = "couldn't parse current day string" : statusMessages m})
    Right ddecoded -> case parseDay ddecoded of
      Nothing -> noEff (m {statusMessages = toMisoString ("couldn't parse \"" <> ddecoded <> "\"") : statusMessages m})
      Just todayParsed -> noEff (m {today = todayParsed})
updateModel LocalStorageUpdated m = noEff m
updateModel (NewTaskChanged nt) m = noEff (m {newTask = nt})
updateModel (ToggleDone tid) m =
  let newModel = updateTask m tid $ \t -> case completionDay t of
        Nothing -> t {completionDay = Just (today m)}
        Just _ -> t {completionDay = Nothing}
   in newModel <# setLocalStorageFromModel newModel
updateModel AddTaskClicked m =
  let maxId :: TaskId
      maxId = case tasks m of
        [] -> TaskId 0
        _ -> taskId (maximumBy (comparing taskId) (tasks m))
      addedTask :: Task TaskId
      addedTask = increaseTaskId maxId <$ newTask m
      newTasks = addedTask : tasks m
      newModel = m {newTask = initialTask, tasks = newTasks, annealedTasks = annealTasksInModel (seed m) (today m) (weekday m) newTasks}
   in newModel <# (LocalStorageUpdated <$ setLocalStorage localStorageKey (modelToLocalStorage newModel))

viewNewTaskForm :: Model -> View Action
viewNewTaskForm m =
  let nt = newTask m
      importances = (\i -> (showMiso (Importance i), Importance i)) <$> [0, 1, 2]
      timeEstimates = [("<10min", TimeEstimate 10), ("30min", TimeEstimate 30), ("1h", TimeEstimate 60), (">1h", TimeEstimate 120)]
      makeImportanceRadio :: (MisoString, Importance) -> [View Action]
      makeImportanceRadio (displayText, value) =
        [ input_ [class_ "btn-check", type_ "radio", name_ "importance", id_ displayText, value_ displayText, checked_ (importance nt == value), onClick (NewTaskChanged (nt {importance = value}))],
          label_ [for_ displayText, class_ "btn btn-outline-primary w-100"] [text displayText]
        ]
      makeTimeEstimateRadio :: (MisoString, TimeEstimate) -> [View Action]
      makeTimeEstimateRadio (displayValue, value) =
        [ input_ [class_ "btn-check", type_ "radio", name_ "time-estimate", id_ displayValue, value_ displayValue, checked_ (timeEstimate nt == value), onClick (NewTaskChanged (nt {timeEstimate = value}))],
          label_ [for_ displayValue, class_ "btn btn-outline-secondary w-100"] [text displayValue]
        ]
   in form_
        [class_ "mb-3"]
        [ div_
            [class_ "form-floating mb-3"]
            [ input_ [type_ "text", id_ "title", class_ "form-control", value_ (title nt), onInput (\i -> NewTaskChanged $ nt {title = i})],
              label_ [for_ "title"] [text "Titel der Aufgabe"]
            ],
          h5_ [] ["Wichtigkeit"],
          div_ [class_ "btn-group mb-3 d-flex"] (concatMap makeImportanceRadio importances),
          h5_ [] ["Zeitschätzung"],
          div_ [class_ "btn-group mb-3 d-flex"] (concatMap makeTimeEstimateRadio timeEstimates),
          div_
            [class_ "form-floating mb-3"]
            [ input_ [type_ "date", id_ "deadline", class_ "form-control", value_ (maybe "" showMiso (deadline nt)), onInput (\i -> NewTaskChanged $ nt {deadline = parseDay i})],
              label_ [for_ "deadline"] [text "Deadline"]
            ],
          button_ [type_ "button", class_ "btn btn-primary w-100", onClick AddTaskClicked] [viewIcon "save", text " Hinzufügen"]
        ]

weekdayToAllocationTime :: Weekday -> TimeEstimate
weekdayToAllocationTime Saturday = TimeEstimate 180
weekdayToAllocationTime Sunday = TimeEstimate 180
weekdayToAllocationTime _ = TimeEstimate 80

estimateInMinutes :: TimeEstimate -> Int
estimateInMinutes (TimeEstimate e) = e

showMiso :: Show a => a -> MisoString
showMiso = toMisoString . show

showDate :: Day -> Day -> MisoString
showDate today' d
  | today' == d = "heute"
  | succ today' == d = "morgen"
  | succ (succ today') == d = "übermorgen"
  | otherwise = showMiso d

importanceToIcon :: Importance -> View action
importanceToIcon i =
  let iconClass (Importance x)
        | x == 0 = "reception-2"
        | x == 1 = "reception-3"
        | otherwise = "reception-4"
      iconColor (Importance x)
        | x == 0 = ""
        | x == 1 = ""
        | otherwise = ""
   in i_ [class_ ("bi-" <> iconClass i <> " " <> iconColor i)] []

viewIcon :: MisoString -> View action
viewIcon desc = i_ [class_ ("bi-" <> desc)] []

viewTasksListGroup :: Day -> [Task TaskId] -> View Action
viewTasksListGroup today' all =
  let viewTaskItem :: Task TaskId -> View Action
      viewTaskItem t =
        div_
          [class_ "list-group-item"]
          [ div_
              [class_ "d-flex w-100 justify-content-between align-items-center"]
              [ div_
                  []
                  [ input_ [type_ "checkbox", class_ "btn-check", id_ (showMiso (taskId t) <> "-check"), checked_ (isJust (completionDay t)), onClick (ToggleDone (taskId t))],
                    label_
                      [for_ (showMiso (taskId t) <> "-check"), class_ "btn btn-sm btn-outline-secondary"]
                      [viewIcon "check-circle"],
                    span_
                      [class_ "ms-3 mb-1"]
                      [importanceToIcon (importance t), text (" " <> title t)]
                  ],
                div_
                  []
                  [ maybe
                      (text "")
                      (\dl -> span_ [class_ "badge rounded-pill text-bg-success me-2"] [viewIcon "calendar-date", text (" " <> showDate today' dl)])
                      (deadline t),
                    small_ [class_ "badge rounded-pill text-bg-info"] [text (showMiso (timeEstimate t))]
                  ]
              ]
          ]
   in div_ [class_ "list-group list-group-flush"] (viewTaskItem <$> all)

removeIndex :: Int -> [a] -> [a]
removeIndex i a = take i a ++ drop (i + 1) a

removeRandomElement :: [a] -> SimannealState ([a], [a]) (a, [a])
removeRandomElement xs = do
  randomIndex <- randomRS (0, length xs - 1)
  pure (xs !! randomIndex, removeIndex randomIndex xs)

annealTasks :: forall a. Seed -> Day -> Weekday -> Float -> [Task a] -> [Task a]
annealTasks seed today' weekday' spentMinutes allTasks =
  let taskImportanceSum :: [Task a] -> Float
      taskImportanceSum ts = fromIntegral (sum ((importanceNumeric . importance) <$> ts))
      taskUrgency :: Task a -> Float
      taskUrgency t = case deadline t of
        Nothing -> 0.0
        Just d ->
          min 3.0 (fromIntegral (diffDays d today'))
      taskUrgencySum :: [Task a] -> Float
      taskUrgencySum ts = sum (taskUrgency <$> ts)
      allocated :: Float
      allocated = max 0 (fromIntegral (estimateInMinutes (weekdayToAllocationTime weekday')) - spentMinutes)
      totalEstimate :: Float
      totalEstimate = taskEstimateSum allTasks
      maxDistanceToAllocated :: Float
      maxDistanceToAllocated = max allocated totalEstimate
      distance :: Float -> Float -> Float
      distance x y = abs (x - y)
      (baseTasks, remainingTasks) = partition (\t -> deadline t == Just today') allTasks
      taskEnergy :: ([Task a], [Task a]) -> Energy
      taskEnergy (ts, _) =
        let closeToAllocated :: Float
            closeToAllocated = (distance allocated (taskEstimateSum (ts <> baseTasks))) / maxDistanceToAllocated
            sumImportance = taskImportanceSum ts
            sumUrgency = taskUrgencySum ts
         in Energy (closeToAllocated - 0.05 * sumImportance - 0.05 * sumUrgency)
      mutateTasks :: ([Task a], [Task a]) -> SimannealState ([Task a], [Task a]) ([Task a], [Task a])
      mutateTasks (chosenTasks, openTasks) = do
        removeOrAdd :: Int <- randomRS (1, 100)
        let thisIterationRemoves = removeOrAdd <= 50
        if length chosenTasks > 1 && thisIterationRemoves
          then do
            (removedTask, newChosenTasks) <- removeRandomElement chosenTasks
            pure (newChosenTasks, removedTask : openTasks)
          else
            if length openTasks > 1
              then do
                (removedTask, newOpenTasks) <- removeRandomElement openTasks
                pure (removedTask : chosenTasks, newOpenTasks)
              else pure (chosenTasks, openTasks)
   in if length allTasks <= 1 || null remainingTasks
        then allTasks
        else
          let solution = simanneal seed (remainingTasks, []) mutateTasks taskEnergy 100.0 0.01 0.5
           in (baseTasks <> fst solution)

buildProgressBar :: [(Int, Maybe MisoString)] -> View action
buildProgressBar parts =
  let sumTotal = fromIntegral (sum (fst <$> parts))
      makePercentageString :: Int -> MisoString
      makePercentageString part =
        let integralResult :: Int
            integralResult = round (fromIntegral part / sumTotal * 100.0 :: Float)
         in showMiso integralResult <> "%"
      makePart :: (Int, Maybe MisoString) -> View action
      makePart (_, Nothing) = text ""
      makePart (part, Just background) = div_ [class_ ("progress-bar " <> background), style_ (M.singleton "width" (makePercentageString part))] [text (showMiso part <> "min")]
   in div_ [class_ "progress"] (makePart <$> parts)

viewProgressBar :: Day -> Weekday -> [Task TaskId] -> View Action
viewProgressBar today' weekday' all =
  let done :: Int
      done = estimateInMinutes (foldMap timeEstimate (filter (\t -> completionDay t == Just today') all))
      allocated :: Int
      allocated = estimateInMinutes (weekdayToAllocationTime weekday')
      overhang :: Int
      overhang = max 0 (done - allocated)
      leftover = max 0 (allocated - done)
      description =
        if leftover > 0
          then small_ [] [strong_ [] [text (showMiso leftover <> "min")], text " übrig"]
          else
            if overhang > 0
              then small_ [] [text (showMiso overhang <> "min drüber, gib auf dich acht!")]
              else text ""
   in div_
        [class_ "mb-3"]
        [ buildProgressBar
            [ (min allocated done, Just "bg-success"),
              (overhang, Just "bg-danger"),
              (leftover, Nothing)
            ],
          description
        ]

viewModel :: Model -> View Action
viewModel m =
  let uncompletedTasks :: [Task TaskId]
      uncompletedTasks = filter (\t -> maybe True id ((>= today m) <$> completionDay t)) (tasks m)
      taskIdsDoneToday :: S.Set TaskId
      taskIdsDoneToday = S.fromList (taskId <$> (filter (\t -> completionDay t == Just (today m)) uncompletedTasks))
      annealedIdsAndDoneToday :: S.Set TaskId
      annealedIdsAndDoneToday = annealedTasks m <> taskIdsDoneToday
      (todayTasks, remainingTasks) = partition (\t -> taskId t `S.member` annealedIdsAndDoneToday) uncompletedTasks
      deadlineDays :: Task t -> Integer
      deadlineDays t = case deadline t of
        Nothing -> 4
        Just d ->
          let difference = diffDays d (today m)
           in if difference < 0
                then 0
                else 1 + min 2 difference
      sortedRemainingTasks =
        sortBy
          (comparing deadlineDays <> comparing (Down . importance) <> comparing (Down . timeEstimate))
          remainingTasks
   in div_
        [class_ "container"]
        [ link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css", rel_ "stylesheet"],
          link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.2/font/bootstrap-icons.css", rel_ "stylesheet"],
          header_ [class_ "d-flex justify-content-center bg-info text-light mb-3"] [h1_ [class_ "mt-2 mb-2"] [i_ [class_ "bi-alarm"] [], " Sisyphus"]],
          if statusMessages m /= []
            then ol_ [] ((\sm -> li_ [] [text sm]) <$> statusMessages m)
            else text "",
          viewNewTaskForm m,
          hr_ [],
          viewProgressBar (today m) (weekday m) (tasks m),
          div_
            [class_ "d-flex justify-content-between align-items-center"]
            [ h5_ [] [text $ "Vorschlag (" <> showMiso (sum (estimateInMinutes . timeEstimate <$> todayTasks)) <> "min)"]
            -- div_ [] [button_ [type_ "button", class_ "btn btn-sm btn-outline-secondary", onClick IncreaseSeed] [i_ [class_ "bi-dice-5"] [], text $ " Neu würfeln"]]
            ],
          viewTasksListGroup (today m) (sortBy (comparing (Down . importance) <> comparing title) todayTasks),
          if null remainingTasks then text "" else h5_ [] [text "Andere Aufgaben"],
          if null remainingTasks then text "" else viewTasksListGroup (today m) sortedRemainingTasks
        ]

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 8080 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = Init -- initial action to be executed on application load
    model = initialModel
    update = updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off
