{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.State.Strict (MonadState, State, evalState, get, put)
import Data.Aeson
import Data.List (find, maximumBy, partition, sortBy)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..), comparing)
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Miso
import Miso.String (MisoString, fromMisoString, fromMisoStringEither, toMisoString)
import System.Random (Random, RandomGen, StdGen, mkStdGen, randomR)
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

-- importanceNumeric :: Importance -> Int
-- importanceNumeric (Importance x) = x

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
    selectedTasks :: [TaskId],
    statusMessages :: [MisoString],
    today :: Day,
    weekday :: Weekday,
    seed :: Int
  }
  deriving (Show, Generic, Eq)

localStorageKey :: MisoString
localStorageKey = "v3"

data LocalStorageModel = LocalStorageModel
  { lsTasks :: [Task TaskId],
    lsSelectedTasks :: [TaskId]
    -- , lsTimeSpent :: [ (Day, Int) ]
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
  | ToggleSelected TaskId
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
      tasks = [],
      selectedTasks = [],
      statusMessages = mempty,
      today = toEnum 0,
      weekday = Monday,
      seed = 14
    }

modelToLocalStorage :: Model -> LocalStorageModel
modelToLocalStorage (Model {tasks = tasks, selectedTasks}) = LocalStorageModel tasks selectedTasks

-- appendStatus :: Model -> MisoString -> Effect Action Model
-- appendStatus m message = noEff (m {statusMessages = message : statusMessages m})

setLocalStorageFromModel :: Model -> JSM Action
setLocalStorageFromModel newModel = LocalStorageUpdated <$ setLocalStorage localStorageKey (modelToLocalStorage newModel)

updateTask :: Model -> TaskId -> (Task TaskId -> Task TaskId) -> Model
updateTask m tid f =
  let possiblyEditTask t = if taskId t == tid then f t else t
   in m {tasks = foldr (\t prevTasks -> possiblyEditTask t : prevTasks) [] (tasks m)}

parseDay :: MisoString -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" . fromMisoString

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel IncreaseSeed m = noEff (m {seed = seed m + 1})
updateModel (LocalStorageReceived l) m =
  case l of
    Left errorMessage -> m <# do consoleLog ("error receiving local storage: " <> toMisoString errorMessage) >> pure Nop
    Right v -> noEff (m {newTask = newTask m, tasks = lsTasks v, selectedTasks = lsSelectedTasks v})
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
updateModel (ToggleSelected tid) m =
  let newSelected =
        if tid `elem` selectedTasks m
          then filter (/= tid) (selectedTasks m)
          else tid : selectedTasks m
      newModel = m {selectedTasks = newSelected}
   in newModel <# setLocalStorageFromModel newModel
updateModel LocalStorageUpdated m = noEff m
updateModel (NewTaskChanged nt) m = noEff (m {newTask = nt})
updateModel (ToggleDone tid) m =
  noEff $
    updateTask m tid $ \t -> case completionDay t of
      Nothing -> t {completionDay = Just (today m)}
      Just _ -> t {completionDay = Nothing}
updateModel AddTaskClicked m =
  let maxId :: TaskId
      maxId = case tasks m of
        [] -> TaskId 0
        _ -> taskId (maximumBy (comparing taskId) (tasks m))
      addedTask :: Task TaskId
      addedTask = increaseTaskId maxId <$ newTask m
      newModel = m {newTask = initialTask, tasks = addedTask : tasks m}
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
          button_ [type_ "button", class_ "btn btn-primary w-100", onClick AddTaskClicked] [text "Hinzufügen"]
        ]

weekdayToAllocationTime :: Weekday -> TimeEstimate
weekdayToAllocationTime Saturday = TimeEstimate 180
weekdayToAllocationTime Sunday = TimeEstimate 180
weekdayToAllocationTime _ = TimeEstimate 80

estimateInMinutes :: TimeEstimate -> Int
estimateInMinutes (TimeEstimate e) = e

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

viewProgressBar :: Day -> Weekday -> [TaskId] -> [Task TaskId] -> View Action
viewProgressBar today' weekday' selectedIds all =
  let done :: Int
      done = estimateInMinutes (foldMap timeEstimate (filter (\t -> completionDay t == Just today') all))
      allocated :: Int
      allocated = estimateInMinutes (weekdayToAllocationTime weekday')
      selectedTotal :: Int
      selectedTotal = estimateInMinutes (foldMap timeEstimate (filter (\t -> taskId t `elem` selectedIds) all))
      overhang :: Int
      overhang = max 0 (selectedTotal - allocated)
      leftover = max 0 (allocated - selectedTotal)
      description =
        if leftover > 0
          then small_ [] [strong_ [] [text (showMiso leftover <> "min")], text " übrig"]
          else
            if overhang > 0
              then small_ [] [text (showMiso overhang <> "min drüber, gib auf dich acht!")]
              else text ""
   in div_
        []
        [ buildProgressBar
            [ (min allocated done, Just "bg-success"),
              (min allocated (selectedTotal - done), Just "bg-info"),
              (overhang, Just "bg-danger"),
              (leftover, Nothing)
            ],
          description
        ]

showMiso :: Show a => a -> MisoString
showMiso = toMisoString . show

taskIsDone :: TaskId -> [Task TaskId] -> Bool
taskIsDone i ts = case find ((== i) . taskId) ts of
  Nothing -> False
  Just t -> isJust (completionDay t)

viewTasks :: [TaskId] -> [Task TaskId] -> View Action
viewTasks selectedIds all =
  let viewTaskLine t =
        tr_
          []
          [ td_
              []
              [ div_
                  []
                  [ input_
                      [ class_ "form-check-input",
                        type_ "checkbox",
                        id_ ("select-" <> showMiso (taskId t)),
                        onClick (ToggleSelected (taskId t)),
                        checked_ (taskId t `elem` selectedIds)
                      ]
                  ]
              ],
            td_
              []
              [ div_
                  []
                  [ input_
                      [ class_ "form-check-input",
                        type_ "checkbox",
                        id_ ("done-" <> showMiso (taskId t)),
                        onClick (ToggleDone (taskId t)),
                        checked_ (taskIsDone (taskId t) all),
                        disabled_ (not $ taskId t `elem` selectedIds)
                      ]
                  ]
              ],
            td_
              []
              [div_ [] [text (title t)]],
            td_
              []
              [div_ [] [text (maybe "" showMiso (deadline t))]],
            td_
              []
              [div_ [] [text (showMiso (importance t))]],
            td_
              []
              [div_ [] [text (showMiso (timeEstimate t))]]
          ]
   in div_
        []
        [ -- h2_ [] [text ("current day/ws: " <> showMiso today' <> "/" <> showMiso weekday')],
          table_
            [class_ "table"]
            [ thead_
                []
                [ tr_
                    []
                    [ th_ [] [text "Select"],
                      th_ [] [text "Done"],
                      th_ [] [text "Title"],
                      th_ [] [text "Deadline"],
                      th_ [] [text "Importance"],
                      th_ [] [text "Estimate"]
                    ]
                ],
              tbody_ [] (viewTaskLine <$> all)
            ]
        ]

type Metric = Float

type Temperature = Float

type RandomState = State StdGen

liftRandom :: MonadState s m => (s -> (b, s)) -> m b
liftRandom f = do
  gen <- get
  let (result, newGen) = f gen
  put newGen
  pure result

randomRS :: (MonadState s m, Random b, RandomGen s) => (b, b) -> m b
randomRS range = liftRandom (randomR range)

-- -- https://medium.com/swlh/how-to-implement-simulated-annealing-algorithm-in-python-ab196c2f56a0
-- -- https://oleg.fi/gists/posts/2020-06-02-simulated-annealing.html
simanneal :: s -> Metric -> (s -> RandomState s) -> (s -> Metric) -> Temperature -> Temperature -> RandomState s
simanneal startState startMetric chooseNeighbor metric startTemp tempDiff = simanneal' startTemp startState startMetric
  where
    simanneal' temp currentState currentMetric =
      if temp <= 0.0
        then do
          pure currentState
        else do
          newNeighbor <- chooseNeighbor currentState
          let newNeighborMetric = metric newNeighbor
              nextTemp = temp - tempDiff
          if newNeighborMetric > currentMetric
            then do
              simanneal' nextTemp newNeighbor newNeighborMetric
            else do
              r <- randomRS (0.0, 1.0)
              let decider = exp (-(currentMetric - newNeighborMetric) / temp)
              if r < decider
                then simanneal' nextTemp newNeighbor newNeighborMetric
                else simanneal' nextTemp currentState currentMetric

removeIndex :: Int -> [a] -> [a]
removeIndex i a = take i a ++ drop (i + 1) a

removeRandomElement :: [a] -> RandomState (a, [a])
removeRandomElement xs = do
  randomIndex <- randomRS (0, length xs - 1)
  pure (xs !! randomIndex, removeIndex randomIndex xs)

annealTasks :: forall a. Day -> Weekday -> [Task a] -> RandomState [Task a]
annealTasks today' weekday' allTasks =
  let allocated :: Float
      allocated = fromIntegral (estimateInMinutes (weekdayToAllocationTime weekday'))
      totalEstimate :: Float
      totalEstimate = fromIntegral (sum (estimateInMinutes . timeEstimate <$> allTasks))
      maxDistanceToAllocated :: Float
      maxDistanceToAllocated = max allocated totalEstimate
      (baseTasks, remainingTasks) = partition (\t -> deadline t == Just today') allTasks
      taskMetric :: ([Task a], [Task a]) -> Metric
      taskMetric (ts, _) =
        let closeToAllocated :: Float
            closeToAllocated = maxDistanceToAllocated - abs (allocated - fromIntegral (sum ((estimateInMinutes . timeEstimate) <$> (ts <> baseTasks)))) / maxDistanceToAllocated
         in --            sumImportants = sum ((importanceNumeric . importance) <$> (ts <> baseTasks))
            --         in closeToAllocated + 0.05 * fromIntegral sumImportants
            closeToAllocated
      mutateTasks :: ([Task a], [Task a]) -> RandomState ([Task a], [Task a])
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
        then pure allTasks
        else do
          (chosenTasks, _) <-
            simanneal
              (remainingTasks, [])
              (taskMetric (remainingTasks, []))
              mutateTasks
              taskMetric
              200.0
              0.01
          pure (baseTasks <> chosenTasks)

viewModel :: Model -> View Action
viewModel m =
  let isOldTask :: Task t -> Bool
      isOldTask t = maybe False (\cd -> cd < (today m)) (completionDay t)
      oldTasks :: [Task TaskId]
      oldTasks = filter isOldTask (tasks m)
      newTasks :: [Task TaskId]
      newTasks = filter (not . isOldTask) (tasks m)
      annealed = evalState (annealTasks (today m) (weekday m) (filter (\t -> isNothing (completionDay t)) (tasks m))) (mkStdGen (seed m))
      deadlineDays :: Task t -> Integer
      deadlineDays t = case deadline t of
        Nothing -> 4
        Just d ->
          let difference = diffDays d (today m)
           in if difference < 0
                then 0
                else 1 + min 2 difference
      sortedTasks =
        sortBy (comparing deadlineDays <> comparing (Down . importance) <> comparing (Down . timeEstimate)) newTasks
   in div_
        [class_ "container"]
        [ link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css", rel_ "stylesheet"],
          link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.2/font/bootstrap-icons.css", rel_ "stylesheet"],
          header_ [class_ "d-flex justify-content-center bg-info text-light mb-3"] [h1_ [class_ "mt-2 mb-2"] [i_ [class_ "bi-alarm"] [], " Sisyphus"]],
          if statusMessages m /= []
            then ol_ [] ((\sm -> li_ [] [text sm]) <$> statusMessages m)
            else text "",
          div_ [] [button_ [type_ "button", class_ "btn btn-primary w-100", onClick IncreaseSeed] [text $ "Seed+1 (at " <> showMiso (seed m) <> ")"]],
          viewNewTaskForm m,
          hr_ [],
          viewProgressBar (today m) (weekday m) (selectedTasks m) (tasks m),
          h5_ [] [text $ "Vorschlag (" <> showMiso (sum (estimateInMinutes . timeEstimate <$> annealed)) <> ")"],
          viewTasks
            (selectedTasks m)
            annealed,
          viewTasks (selectedTasks m) sortedTasks,
          if null oldTasks then text "" else h5_ [] [text "Old tasks"],
          if null oldTasks then text "" else viewTasks (selectedTasks m) oldTasks
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
