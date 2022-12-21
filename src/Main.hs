{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- This is 1.8.0.4

import Data.Aeson
import Data.List (maximumBy, partition, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as S
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Miso
import Miso.String (MisoString, fromMisoString, fromMisoStringEither, toMisoString)
import Task
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

data Model = Model
  { newTask :: Task () (Maybe Repeater),
    tasks :: [RegularTask],
    repeatingTasks :: [RepeatingTask],
    annealedTasks :: S.Set TaskId,
    statusMessages :: [MisoString],
    today :: Day,
    weekday :: Weekday,
    seed :: Int,
    leisureProjects :: [LeisureProject LeisureId],
    newLeisureProject :: LeisureProject (),
    displayMode :: DisplayMode
  }
  deriving (Show, Generic, Eq)

localStorageKey :: MisoString
localStorageKey = "v6"

data LocalStorageModel = LocalStorageModel
  { lsTasks :: [Task TaskId (Maybe TaskId)],
    lsRepeatingTasks :: [Task TaskId Repeater],
    lsLeisureProjects :: [LeisureProject LeisureId]
  }
  deriving (Generic, Show, Eq)

instance FromJSON LocalStorageModel

instance ToJSON LocalStorageModel

-- | Sum type for application events
data Action
  = LocalStorageReceived (Either String LocalStorageModel)
  | Nop
  | Init
  | IncreaseSeed
  | AddTaskClicked
  | AddLeisureProjectClicked
  | ToggleLeisureProject LeisureId
  | ToggleDone TaskId
  | ToggleRepeatingDone TaskId
  | ToggleMode
  | LocalStorageUpdated
  | CurrentDayReceived MisoString
  | CurrentWeekDayReceived Int
  | NewTaskChanged (Task () (Maybe Repeater))
  | NewLeisureProjectChanged (LeisureProject ())
  deriving (Show, Eq)

initialTask :: Task () (Maybe Repeater)
initialTask =
  Task
    { title = "",
      importance = Importance 0,
      deadline = Nothing,
      timeEstimate = TimeEstimate 10,
      completionDay = Nothing,
      taskId = (),
      repeater = Nothing
    }

initialModel :: Model
initialModel =
  Model
    { newTask = initialTask,
      tasks = mempty,
      repeatingTasks = mempty,
      annealedTasks = mempty,
      statusMessages = mempty,
      today = toEnum 0,
      weekday = Monday,
      seed = 15,
      displayMode = DisplayWork,
      leisureProjects = mempty,
      newLeisureProject = initialLeisureProject
    }

initialLeisureProject :: LeisureProject ()
initialLeisureProject = LeisureProject {leisureTitle = "", leisureId = ()}

modelToLocalStorage :: Model -> LocalStorageModel
modelToLocalStorage (Model {tasks = tasks, repeatingTasks = repeatingTasks, leisureProjects = leisureProjects}) = LocalStorageModel tasks repeatingTasks leisureProjects

setLocalStorageFromModel :: Model -> JSM Action
setLocalStorageFromModel newModel = LocalStorageUpdated <$ setLocalStorage localStorageKey (modelToLocalStorage newModel)

updateTask :: Model -> TaskId -> (RegularTask -> RegularTask) -> Model
updateTask m tid f =
  let possiblyEditTask t = if taskId t == tid then f t else t
      newTasks = foldr (\t prevTasks -> possiblyEditTask t : prevTasks) [] (tasks m)
   in m {tasks = newTasks}

updateRepeatingTask :: Model -> TaskId -> (RepeatingTask -> RepeatingTask) -> Model
updateRepeatingTask m tid f =
  let possiblyEditTask t = if taskId t == tid then f t else t
      newTasks = foldr (\t prevTasks -> possiblyEditTask t : prevTasks) [] (repeatingTasks m)
   in m {repeatingTasks = newTasks}

parseDay :: MisoString -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" . fromMisoString

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum xs = Just (maximum xs)

calculateWeekday :: Day -> Weekday
calculateWeekday d =
  let (_, _, dow) = toWeekDate d
   in case dow of
        1 -> Monday
        2 -> Tuesday
        3 -> Wednesday
        4 -> Thursday
        5 -> Friday
        6 -> Saturday
        _ -> Sunday

goBackUntilWeekdayMatches :: Weekday -> Day -> Day
goBackUntilWeekdayMatches repeatOn lastClosing =
  if calculateWeekday lastClosing == repeatOn
    then lastClosing
    else goBackUntilWeekdayMatches repeatOn (pred lastClosing)

createRepeatingTasks :: Day -> [RegularTask] -> [RepeatingTask] -> [RegularTask]
createRepeatingTasks today' regularTasks = concatMap possiblyRepeat
  where
    createTask rt = [const (Just (taskId rt)) `mapRepeater` rt]
    possiblyRepeat :: RepeatingTask -> [RegularTask]
    possiblyRepeat rt
      | isJust (completionDay rt) = []
      | otherwise =
        let hasOpenCandidate = any (\t -> repeater t == Just (taskId rt) && isNothing (completionDay t)) regularTasks
         in if hasOpenCandidate
              then []
              else
                let lastClosing = safeMaximum (mapMaybe (\t -> if repeater t == Just (taskId rt) then (completionDay t) else Nothing) regularTasks)
                 in case lastClosing of
                      Nothing -> createTask rt
                      Just lc ->
                        case repeater rt of
                          EveryNDays n ->
                            if diffDays today' lc >= fromIntegral n
                              then createTask rt
                              else []
                          EveryWeekday repeatOn ->
                            let previousToBeClosed = goBackUntilWeekdayMatches repeatOn lc
                             in if diffDays previousToBeClosed today' >= 7
                                  then createTask rt
                                  else []

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel IncreaseSeed m = noEff (m {seed = seed m + 1, annealedTasks = annealTasksInModel (seed m + 1) (today m) (weekdayToAllocationTime (weekday m)) (tasks m)})
updateModel (ToggleLeisureProject projectId) m =
  let newModel = m {leisureProjects = filter (\lp -> leisureId lp /= projectId) (leisureProjects m)}
   in newModel <# setLocalStorageFromModel newModel
updateModel ToggleMode m =
  noEff
    ( m
        { displayMode =
            case displayMode m of
              DisplayWork -> DisplayLeisure
              DisplayLeisure -> DisplayWork
        }
    )
updateModel (LocalStorageReceived l) m =
  case l of
    Left errorMessage -> m <# do consoleLog ("error receiving local storage: " <> toMisoString errorMessage) >> pure Nop
    Right v ->
      let tasksAndRepeated = lsTasks v <> createRepeatingTasks (today m) (lsTasks v) (lsRepeatingTasks v)
       in noEff
            ( m
                { leisureProjects = lsLeisureProjects v,
                  tasks = tasksAndRepeated,
                  repeatingTasks = lsRepeatingTasks v,
                  annealedTasks = annealTasksInModel (seed m) (today m) (weekdayToAllocationTime (weekday m)) tasksAndRepeated
                }
            )
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
updateModel (NewLeisureProjectChanged lp) m = noEff (m {newLeisureProject = lp})
updateModel (ToggleDone tid) m =
  let newModel = updateTask m tid $ \t -> case completionDay t of
        Nothing -> t {completionDay = Just (today m)}
        Just _ -> t {completionDay = Nothing}
   in newModel <# setLocalStorageFromModel newModel
updateModel (ToggleRepeatingDone tid) m =
  let newModel = updateRepeatingTask m tid $ \t -> case completionDay t of
        Nothing -> t {completionDay = Just (today m)}
        Just _ -> t {completionDay = Nothing}
   in newModel <# setLocalStorageFromModel newModel
updateModel AddTaskClicked m =
  let maxId :: TaskId
      maxId = fromMaybe (TaskId 0) (safeMaximum ((taskId <$> tasks m) <> (taskId <$> repeatingTasks m)))
      newTaskWithId :: Task TaskId (Maybe Repeater)
      newTaskWithId = const (increaseTaskId maxId) `mapTaskId` newTask m
      newModel =
        case repeater (newTask m) of
          Nothing ->
            let newNonrepeatingTask :: RegularTask
                newNonrepeatingTask = (const Nothing) `mapRepeater` newTaskWithId
                newTasks = newNonrepeatingTask : tasks m
             in m
                  { newTask = initialTask,
                    tasks = newTasks,
                    annealedTasks = annealTasksInModel (seed m) (today m) (weekdayToAllocationTime (weekday m)) newTasks
                  }
          Just repeating ->
            let newRepeatingTask :: RepeatingTask
                newRepeatingTask = (const repeating) `mapRepeater` newTaskWithId
                newRepeatingTasks = newRepeatingTask : repeatingTasks m
             in m
                  { newTask = initialTask,
                    repeatingTasks = newRepeatingTasks
                  }
   in newModel <# (LocalStorageUpdated <$ setLocalStorage localStorageKey (modelToLocalStorage newModel))
updateModel AddLeisureProjectClicked m =
  let maxId :: LeisureId
      maxId = case leisureProjects m of
        [] -> LeisureId 0
        lps -> leisureId (maximumBy (comparing leisureId) lps)
      addedLeisureProject :: LeisureProject LeisureId
      addedLeisureProject = increaseLeisureId maxId <$ newLeisureProject m
      newLeisureProjects = addedLeisureProject : leisureProjects m
      newModel = m {newLeisureProject = initialLeisureProject, leisureProjects = newLeisureProjects}
   in newModel <# (LocalStorageUpdated <$ setLocalStorage localStorageKey (modelToLocalStorage newModel))

viewNewTaskForm :: Model -> View Action
viewNewTaskForm m =
  let nt = newTask m
      importances = (\i -> (showMiso (Importance i), Importance i)) <$> [0, 1, 2]
      timeEstimates = [("<10min", TimeEstimate 10), ("30min", TimeEstimate 30), ("1h", TimeEstimate 60), (">1h", TimeEstimate 120)]
      makeWeekdayRadio :: Weekday -> Weekday -> [View Action]
      makeWeekdayRadio curWd wd =
        [ input_ [class_ "btn-check", type_ "radio", name_ "weekday-repeater", id_ ("repeat-wd-" <> showMiso wd), value_ (showMiso wd), checked_ (curWd == wd), onClick (NewTaskChanged (nt {repeater = Just (EveryWeekday wd)}))],
          label_ [for_ ("repeat-wd-" <> showMiso wd), class_ "btn btn-outline-primary w-100"] [text (showMiso wd)]
        ]
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
      formForRepeater = case repeater nt of
        Nothing ->
          div_
            [class_ "form-floating mb-3"]
            [ input_ [type_ "date", id_ "deadline", class_ "form-control", value_ (maybe "" showMiso (deadline nt)), onInput (\i -> NewTaskChanged $ nt {deadline = parseDay i})],
              label_ [for_ "deadline"] [text "Deadline"]
            ]
        Just (EveryNDays n) ->
          div_
            [class_ "form-floating mb-3"]
            [ input_ [type_ "number", id_ "every-n-days-input", class_ "form-control", value_ (showMiso n), onInput (\i -> NewTaskChanged $ nt {repeater = Just (EveryNDays (read (fromMisoString i)))})],
              label_ [for_ "every-n-days-input"] [text "Tage"]
            ]
        Just (EveryWeekday wd) ->
          div_
            [class_ "mb-3"]
            [ h6_ [] [text "Wochentag"],
              div_
                [class_ "btn-group d-flex"]
                (concatMap (makeWeekdayRadio wd) (enumFromTo Monday Sunday))
            ]
   in form_
        [class_ "mb-3"]
        [ h3_ [] [viewIcon "plus-lg", text " Neue Aufgabe"],
          div_
            [class_ "form-floating mb-3"]
            [ input_ [type_ "text", id_ "title", class_ "form-control", value_ (title nt), onInput (\i -> NewTaskChanged $ nt {title = i})],
              label_ [for_ "title"] [text "Titel der Aufgabe"]
            ],
          h5_ [] ["Wichtigkeit"],
          div_ [class_ "btn-group mb-3 d-flex"] (concatMap makeImportanceRadio importances),
          h5_ [] ["Zeitschätzung"],
          div_ [class_ "btn-group mb-3 d-flex"] (concatMap makeTimeEstimateRadio timeEstimates),
          h5_ [] ["Deadline/Wiederholung"],
          div_
            [class_ "btn-group mb-3 d-flex"]
            [ input_ [class_ "btn-check", type_ "radio", name_ "repeater", id_ "has-deadline", value_ "has-deadline", checked_ (isNothing (repeater nt)), onClick (NewTaskChanged (nt {repeater = Nothing}))],
              label_ [for_ "has-deadline", class_ "btn btn-outline-secondary w-100"] [text "Nicht wiederholend"],
              input_ [class_ "btn-check", type_ "radio", name_ "repeater", id_ "every-n-days", value_ "every-n-days", checked_ (maybe False isEveryNDays (repeater nt)), onClick (NewTaskChanged (nt {repeater = Just (EveryNDays 1)}))],
              label_
                [for_ "every-n-days", class_ "btn btn-outline-secondary w-100"]
                [text "Alle N Tage"],
              input_
                [class_ "btn-check", type_ "radio", name_ "repeater", id_ "every-weekday", value_ "every-weekday", checked_ (maybe False isEveryWeekday (repeater nt)), onClick (NewTaskChanged (nt {repeater = Just (EveryWeekday Monday)}))],
              label_ [for_ "every-weekday", class_ "btn btn-outline-secondary w-100"] [text "Bestimmter Wochentag"]
            ],
          formForRepeater,
          button_
            [type_ "button", class_ "btn btn-primary w-100", onClick AddTaskClicked]
            [viewIcon "save", text " Hinzufügen"]
        ]

showDate :: Day -> Day -> MisoString
showDate today' d
  | today' == d = "heute"
  | succ today' == d = "morgen"
  | succ (succ today') == d = "übermorgen"
  | otherwise = showMiso d

importanceToIcon :: Importance -> View action
importanceToIcon (Importance 0) = span_ [style_ (M.singleton "visibility" "hidden")] [text "😟"]
importanceToIcon (Importance 1) = text "❗"
importanceToIcon (Importance _) = text "🔥"

-- importanceToIcon i =
--   let iconClass (Importance x)
--         | x == 0 = "reception-2"
--         | x == 1 = "reception-3"
--         | otherwise = "reception-4"
--       iconColor (Importance x)
--         | x == 0 = ""
--         | x == 1 = ""
--         | otherwise = ""
--    in i_ [class_ ("bi-" <> iconClass i <> " " <> iconColor i)] []

viewIcon :: MisoString -> View action
viewIcon desc = i_ [class_ ("bi-" <> desc)] []

viewRepeatingTasks :: Model -> View Action
viewRepeatingTasks m =
  let viewRepeater :: Repeater -> View action
      viewRepeater r = small_ [class_ "badge rounded-pill text-bg-warning"] [viewRepeater' r]
      viewRepeater' :: Repeater -> View action
      viewRepeater' (EveryWeekday wd) = text ("jeden " <> showMiso wd)
      viewRepeater' (EveryNDays 1) = text ("jeden Tag")
      viewRepeater' (EveryNDays d) = text ("alle " <> showMiso d <> " Tage")
      viewRepeatTaskItem :: RepeatingTask -> View Action
      viewRepeatTaskItem t =
        div_
          [class_ "list-group-item"]
          [ div_
              [class_ "d-flex w-100 justify-content-between align-items-center"]
              [ div_
                  []
                  [ input_ [type_ "checkbox", class_ "btn-check", id_ (showMiso (taskId t) <> "-check"), onClick (ToggleRepeatingDone (taskId t))],
                    label_
                      [for_ (showMiso (taskId t) <> "-check"), class_ "btn btn-sm btn-outline-secondary"]
                      [viewIcon "check-circle"],
                    span_
                      [class_ "ms-3 mb-1"]
                      [importanceToIcon (importance t), text (" " <> title t)]
                  ],
                div_
                  [class_ "hstack gap-1"]
                  [ viewRepeater (repeater t),
                    small_ [class_ "badge rounded-pill text-bg-info"] [text (showMiso (timeEstimate t))]
                  ]
              ]
          ]
      notDoneRepeating = filter (\t -> isNothing (completionDay t)) (repeatingTasks m)
   in div_
        []
        [ h3_ [] [viewIcon "arrow-clockwise", text "Wiederkehrende Aufgaben"],
          div_ [class_ "list-group list-group-flush"] (viewRepeatTaskItem <$> notDoneRepeating)
        ]

viewTasksListGroup :: Day -> [RegularTask] -> View Action
viewTasksListGroup today' all =
  let viewTaskItem :: RegularTask -> View Action
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

viewProgressBar :: Day -> Weekday -> [RegularTask] -> View Action
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
          then small_ [] [strong_ [] [text (showMiso leftover <> "min")], text (" übrig (" <> showMiso done <> "min fertig)")]
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

viewModeSwitcher :: Model -> View Action
viewModeSwitcher m =
  div_
    [class_ "btn-group d-flex mb-3"]
    [ input_ [class_ "btn-check", type_ "radio", name_ "display-mode", id_ "display-mode-work", value_ "work", checked_ (displayMode m == DisplayWork), onClick ToggleMode],
      label_ [for_ "display-mode-work", class_ "btn btn-lg btn-outline-secondary w-100"] [text "🏢 Arbeit"],
      input_ [class_ "btn-check", type_ "radio", name_ "display-mode", id_ "display-mode-leisure", value_ "leisure", checked_ (displayMode m == DisplayLeisure), onClick ToggleMode],
      label_ [for_ "display-mode-leisure", class_ "btn btn-lg btn-outline-secondary w-100"] ["😌 Freizeit"]
    ]

viewModel :: Model -> View Action
viewModel m =
  let content = case displayMode m of
        DisplayWork -> viewModelWork m
        DisplayLeisure -> viewModelLeisure m
   in div_
        [class_ "container"]
        ( [ link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css", rel_ "stylesheet"],
            link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.2/font/bootstrap-icons.css", rel_ "stylesheet"],
            header_ [class_ "d-flex justify-content-center bg-info text-light mb-3"] [h1_ [class_ "mt-2 mb-2"] ["⏰ Sisyphus"]],
            viewModeSwitcher m,
            if statusMessages m /= []
              then ol_ [] ((\sm -> li_ [] [text sm]) <$> statusMessages m)
              else text ""
          ]
            ++ [content]
        )

viewModelLeisure :: Model -> View Action
viewModelLeisure m =
  let viewNewLeisureForm nt =
        form_
          []
          [ h3_ [] [viewIcon "plus-lg", text " Neues Projekt"],
            div_
              [class_ "form-floating mb-3"]
              [ input_ [type_ "text", id_ "title", class_ "form-control", value_ (leisureTitle nt), onInput (\i -> NewLeisureProjectChanged $ nt {leisureTitle = i})],
                label_ [for_ "title"] [text "Titel des Projekts"]
              ],
            button_ [type_ "button", class_ "btn btn-primary w-100", onClick AddLeisureProjectClicked] [viewIcon "save", text " Hinzufügen"]
          ]
      viewLeisureProject :: LeisureProject LeisureId -> View Action
      viewLeisureProject p =
        div_
          [class_ "list-group-item"]
          [ div_
              [class_ "hstack gap-3"]
              [ button_
                  [ type_ "button",
                    class_ "btn btn-outline-secondary btn-sm",
                    onClick (ToggleLeisureProject (leisureId p))
                  ]
                  [viewIcon "check-circle"],
                span_ [] [text (leisureTitle p)]
              ]
          ]
   in div_
        []
        [h3_ [] [text "🌴 Deine Freizeitprojekte"], div_ [class_ "list-group list-group-flush"] (viewLeisureProject <$> leisureProjects m), viewNewLeisureForm (newLeisureProject m)]

viewModelWork :: Model -> View Action
viewModelWork m =
  let uncompletedTasks :: [RegularTask]
      uncompletedTasks = filter (\t -> maybe True id ((>= today m) <$> completionDay t)) (tasks m)
      taskIdsDoneToday :: S.Set TaskId
      taskIdsDoneToday = S.fromList (taskId <$> (filter (\t -> completionDay t == Just (today m)) uncompletedTasks))
      annealedIdsAndDoneToday :: S.Set TaskId
      annealedIdsAndDoneToday = annealedTasks m <> taskIdsDoneToday
      (todayTasks, remainingTasks) = partition (\t -> taskId t `S.member` annealedIdsAndDoneToday) uncompletedTasks
      deadlineDays :: Task idType repeaterType -> Integer
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
        []
        [ viewProgressBar (today m) (weekday m) (tasks m),
          div_
            [class_ "d-flex justify-content-between align-items-center"]
            [ h5_ [] [text $ "Vorschlag (" <> showMiso (sum (estimateInMinutes . timeEstimate <$> todayTasks)) <> "min)"]
            -- div_ [] [button_ [type_ "button", class_ "btn btn-sm btn-outline-secondary", onClick IncreaseSeed] [i_ [class_ "bi-dice-5"] [], text $ " Neu würfeln"]]
            ],
          viewTasksListGroup (today m) (sortBy (comparing (Down . importance) <> comparing title) todayTasks),
          if null remainingTasks then text "" else h5_ [] [text "Andere Aufgaben"],
          if null remainingTasks
            then text ""
            else
              viewTasksListGroup
                (today m)
                sortedRemainingTasks,
          hr_ [],
          viewNewTaskForm
            m,
          hr_ [],
          viewRepeatingTasks m
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
