{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Aeson
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Miso
import Miso.String hiding (concatMap, filter)
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
  getCurrentWeekDay :: JSM Int
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

newtype Importance = Importance Int deriving (Eq, Show)

instance FromJSON Importance where
  parseJSON = withScientific "Importance" (pure . Importance . floor)

newtype TimeEstimate = TimeEstimate Int deriving (Eq, Show)

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

-- | Type synonym for an application model
data Model = Model
  { newTask :: Task (),
    tasks :: [Task Int],
    selectedTasks :: [Int],
    statusMessages :: [MisoString],
    today :: Day,
    weekday :: Weekday
    -- , timeSpent :: [ (Day, Int) ]
  }
  deriving (Show, Generic, Eq)

localStorageKey :: MisoString
localStorageKey = "v3"

data LocalStorageModel = LocalStorageModel
  { lsTasks :: [Task Int],
    lsSelectedTasks :: [Int]
    -- , lsTimeSpent :: [ (Day, Int) ]
  }
  deriving (Generic, Show, Eq)

instance FromJSON LocalStorageModel

instance ToJSON LocalStorageModel

-- | Sum type for application events
data Action
  = LocalStorageReceived (Either String LocalStorageModel)
  | Nop
  | Init
  | AddTaskClicked
  | LocalStorageUpdated
  | CurrentDayReceived MisoString
  | CurrentWeekDayReceived Int
  | ToggleSelected Int
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
      weekday = Monday
    }

modelToLocalStorage :: Model -> LocalStorageModel
modelToLocalStorage (Model {tasks = tasks, selectedTasks}) = LocalStorageModel tasks selectedTasks

setLocalStorageFromModel :: Model -> JSM Action
setLocalStorageFromModel newModel = LocalStorageUpdated <$ setLocalStorage localStorageKey (modelToLocalStorage newModel)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
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
    Right ddecoded -> case parseTimeM True defaultTimeLocale "%Y-%m-%d" ddecoded of
      Nothing -> noEff (m {statusMessages = toMisoString ("couldn't parse \"" <> ddecoded <> "\"") : statusMessages m})
      Just todayParsed -> noEff (m {today = todayParsed})
updateModel (ToggleSelected tid) m =
  let newSelected =
        if tid `elem` selectedTasks m
          then filter (/= tid) (selectedTasks m)
          else tid : selectedTasks m
      newModel = m {selectedTasks = newSelected}
   in newModel <# setLocalStorageFromModel newModel
updateModel LocalStorageUpdated m = noEff (m {statusMessages = "local storage set" : statusMessages m})
updateModel (NewTaskChanged nt) m = noEff (m {newTask = nt})
updateModel AddTaskClicked m =
  let maxId :: Int
      maxId = case tasks m of
        [] -> 0
        _ -> taskId (maximumBy (comparing taskId) (tasks m))
      addedTask :: Task Int
      addedTask = maxId + 1 <$ newTask m
      newModel = m {newTask = initialTask, tasks = addedTask : tasks m}
   in newModel <# (LocalStorageUpdated <$ setLocalStorage localStorageKey (modelToLocalStorage newModel))

viewNewTaskForm :: Model -> View Action
viewNewTaskForm m =
  let nt = newTask m
      importances = [("Unwichtig", Importance 0), ("Wichtig", Importance 1), ("Superwichtig", Importance 2)]
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
        []
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
            [ input_ [type_ "date", id_ "deadline", class_ "form-control", value_ (maybe "" showMiso (deadline nt))],
              label_ [for_ "deadline"] [text "Deadline"]
            ],
          button_ [type_ "button", class_ "btn btn-primary w-100", onClick AddTaskClicked] [text "Hinzufügen"]
        ]

showMiso :: Show a => a -> MisoString
showMiso = toMisoString . show

viewTasks :: Model -> View Action
viewTasks m =
  let viewTaskLine t =
        tr_
          []
          [ td_
              []
              [ div_
                  []
                  [ input_ [class_ "form-check-input", type_ "checkbox", id_ ("select-" <> showMiso (taskId t)), onClick (ToggleSelected (taskId t))]
                  ]
              ],
            td_
              []
              [div_ [] [text "done button here"]]
          ]
   in div_
        []
        [ hr_ [],
          h2_ [] [text ("current day/ws: " <> showMiso (today m) <> "/" <> showMiso (weekday m))],
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
                      th_ [] [text "Time Estimate"]
                    ]
                ],
              tbody_ [] (viewTaskLine <$> tasks m)
            ]
        ]

viewModel :: Model -> View Action
viewModel m =
  div_
    [class_ "container"]
    [ link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css", rel_ "stylesheet"],
      link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.2/font/bootstrap-icons.css", rel_ "stylesheet"],
      header_ [class_ "d-flex justify-content-center bg-info text-light mb-3"] [h1_ [class_ "mt-2 mb-2"] [i_ [class_ "bi-alarm"] [], " Taskie"]],
      if statusMessages m /= []
        then ol_ [] ((\sm -> li_ [] [text sm]) <$> statusMessages m)
        else text "",
      viewNewTaskForm m,
      viewTasks m
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
