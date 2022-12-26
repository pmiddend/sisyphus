{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Lens (Getter, to, use, (%=), (+=), (.=), (^.))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get)
import Data.Aeson hiding ((.=))
import Data.List (maximumBy, partition, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as S
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Miso
import Miso.String (MisoString, fromMisoString, fromMisoStringEither, toMisoString)
import Task
import Types
import Prelude hiding (all)
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
#endif

#ifndef __GHCJS__
getCurrentDay :: JSM MisoString
getCurrentDay = pure "2022-12-13"
#else
foreign import javascript unsafe "$r = new Date().toISOString().substr(0,10)"
  getCurrentDay :: JSM MisoString
#endif

weekday :: Getter Model Weekday
weekday = today . to calculateWeekday

localStorageKey :: MisoString
localStorageKey = "v6"

data LocalStorageModel = LocalStorageModel
  { lsTasks :: [Task TaskId (Maybe TaskId)],
    lsRepeatingTasks :: [Task TaskId Repeater],
    lsLeisureProjects :: [LeisureProject LeisureId],
    lsExplicitAllocation :: Maybe ExplicitAllocation
  }
  deriving (Generic, Show, Eq)

instance FromJSON LocalStorageModel

instance ToJSON LocalStorageModel

-- | Sum type for application events
data Action
  = LocalStorageReceived (Either String LocalStorageModel)
  | Nop
  | RequestRefresh
  | Init
  | ToggleAdaptAllocation
  | CancelAdaptAllocation
  | AdaptAllocationChange TimeEstimate
  | ToggleRemainingTasksOpened
  | IncreaseSeed
  | AddTaskClicked
  | AddLeisureProjectClicked
  | ToggleLeisureProject LeisureId
  | ToggleDone TaskId
  | ToggleRepeatingDone TaskId
  | ToggleMode
  | LocalStorageUpdated
  | CurrentDayReceived MisoString
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
    { _newTask = initialTask,
      _tasks = mempty,
      _repeatingTasks = mempty,
      _annealedTasks = mempty,
      _statusMessages = mempty,
      _explicitAllocation = Nothing,
      _explicitAllocationChanging = Nothing,
      _remainingTasksOpened = False,
      _today = toEnum 0,
      _seed = 15,
      _displayMode = DisplayWork,
      _leisureProjects = mempty,
      _newLeisureProject = initialLeisureProject
    }

initialLeisureProject :: LeisureProject ()
initialLeisureProject = LeisureProject {leisureTitle = "", leisureId = ()}

modelToLocalStorage :: Model -> LocalStorageModel
modelToLocalStorage Model {_tasks = tasks', _repeatingTasks = repeatingTasks', _leisureProjects = leisureProjects', _explicitAllocation = explicitAllocation'} = LocalStorageModel tasks' repeatingTasks' leisureProjects' explicitAllocation'

setLocalStorageFromModel :: Transition Action Model ()
setLocalStorageFromModel = do
  m <- get
  scheduleIO (LocalStorageUpdated <$ setLocalStorage localStorageKey (modelToLocalStorage m))

updateTask :: TaskId -> (RegularTask -> RegularTask) -> Transition Action Model ()
updateTask tid f = do
  let possiblyEditTask t = if taskId t == tid then f t else t
  tasks %= (possiblyEditTask <$>)

updateRepeatingTask :: TaskId -> (RepeatingTask -> RepeatingTask) -> Transition Action Model ()
updateRepeatingTask tid f = do
  let possiblyEditTask t = if taskId t == tid then f t else t
  repeatingTasks %= (possiblyEditTask <$>)

parseDay :: MisoString -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" . fromMisoString

weekdayAllocationTime' :: Model -> TimeEstimate
weekdayAllocationTime' m =
  case m ^. explicitAllocation of
    Nothing -> weekdayToAllocationTime (m ^. weekday)
    Just (ExplicitAllocation explicitDate dateAllocation) -> do
      if explicitDate == (m ^. today)
        then dateAllocation
        else weekdayToAllocationTime (m ^. weekday)

weekdayAllocationTime :: Transition Action Model TimeEstimate
weekdayAllocationTime = do
  s <- get
  pure (weekdayAllocationTime' s)

reanneal :: Transition Action Model ()
reanneal = do
  tasks' <- use tasks
  today' <- use today
  repeatingTasks' <- use repeatingTasks
  seed' <- use seed
  let newTasks = tasks' <> createRepeatingTasks today' tasks' repeatingTasks'
  allocationTime' <- weekdayAllocationTime
  annealedTasks .= annealTasksInModel seed' today' allocationTime' newTasks

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Action Model ()
updateModel (AdaptAllocationChange newValue) = explicitAllocationChanging .= Just newValue
updateModel RequestRefresh = scheduleIO (CurrentDayReceived <$> getCurrentDay)
updateModel IncreaseSeed = do
  seed += 1
  tasks' <- use tasks
  today' <- use today
  seed' <- use seed
  weekdayAllocation' <- weekdayAllocationTime
  annealedTasks .= annealTasksInModel seed' today' weekdayAllocation' tasks'
updateModel CancelAdaptAllocation = explicitAllocationChanging .= Nothing
updateModel ToggleAdaptAllocation = do
  explicitAllocChanging' <- use explicitAllocationChanging
  case explicitAllocChanging' of
    Nothing -> do
      allocTime <- weekdayAllocationTime
      explicitAllocationChanging .= Just allocTime
    Just explicitValue -> do
      explicitAllocationChanging .= Nothing
      today' <- use today
      let newEa = ExplicitAllocation today' explicitValue
      explicitAllocation .= Just newEa
      reanneal
      setLocalStorageFromModel
updateModel ToggleRemainingTasksOpened = do
  rto <- use remainingTasksOpened
  remainingTasksOpened .= not rto
updateModel (ToggleLeisureProject projectId) = do
  lps <- use leisureProjects
  leisureProjects .= filter (\lp -> leisureId lp /= projectId) lps
  setLocalStorageFromModel
updateModel ToggleMode =
  displayMode
    %= ( \dm -> case dm of
           DisplayWork -> DisplayLeisure
           DisplayLeisure -> DisplayWork
       )
updateModel (LocalStorageReceived l) =
  case l of
    Left errorMessage -> do
      scheduleIO_ (consoleLog ("error receiving local storage: " <> toMisoString errorMessage))
    Right v -> do
      leisureProjects .= lsLeisureProjects v
      repeatingTasks .= lsRepeatingTasks v
      explicitAllocation .= lsExplicitAllocation v
      tasks .= lsTasks v
      reanneal
updateModel Init = do
  scheduleIO (LocalStorageReceived <$> getLocalStorage localStorageKey)
  scheduleIO (CurrentDayReceived <$> getCurrentDay)
updateModel Nop = pure ()
updateModel (CurrentDayReceived d) =
  case fromMisoStringEither d of
    Left _ -> do
      sms <- use statusMessages
      statusMessages .= ("couldn't parse current day string" : sms)
    Right ddecoded -> case parseDay ddecoded of
      Nothing -> do
        sms <- use statusMessages
        statusMessages .= toMisoString ("couldn't parse \"" <> ddecoded <> "\"") : sms
      Just todayParsed -> do
        today .= todayParsed
        reanneal
updateModel LocalStorageUpdated = pure ()
updateModel (NewTaskChanged nt) = newTask .= nt
updateModel (NewLeisureProjectChanged lp) = newLeisureProject .= lp
updateModel (ToggleDone tid) = do
  today' <- use today
  updateTask tid $ \t -> case completionDay t of
    Nothing -> t {completionDay = Just today'}
    Just _ -> t {completionDay = Nothing}
  setLocalStorageFromModel
updateModel (ToggleRepeatingDone tid) = do
  today' <- use today
  updateRepeatingTask tid $ \t -> case completionDay t of
    Nothing -> t {completionDay = Just today'}
    Just _ -> t {completionDay = Nothing}
  setLocalStorageFromModel
updateModel AddTaskClicked = do
  tasks' <- use tasks
  rtasks' <- use repeatingTasks
  newTask' <- use newTask
  let maxId :: TaskId
      maxId = fromMaybe (TaskId 0) (safeMaximum ((taskId <$> tasks') <> (taskId <$> rtasks')))
      newTaskWithId :: Task TaskId (Maybe Repeater)
      newTaskWithId = const (increaseTaskId maxId) `mapTaskId` newTask'
  case repeater newTask' of
    Nothing ->
      let newNonrepeatingTask :: RegularTask
          newNonrepeatingTask = const Nothing `mapRepeater` newTaskWithId
          newTasks = newNonrepeatingTask : tasks'
       in do
            newTask .= initialTask
            tasks .= newTasks
            reanneal
    Just repeating -> do
      rts <- use repeatingTasks
      let newRepeatingTask :: RepeatingTask
          newRepeatingTask = const repeating `mapRepeater` newTaskWithId
          newRepeatingTasks = newRepeatingTask : rts
      newTask .= initialTask
      repeatingTasks .= newRepeatingTasks
      setLocalStorageFromModel
updateModel AddLeisureProjectClicked =
  do
    lps <- use leisureProjects
    newLp <- use newLeisureProject
    let maxId :: LeisureId
        maxId = case lps of
          [] -> LeisureId 0
          lps' -> leisureId (maximumBy (comparing leisureId) lps')
        addedLeisureProject :: LeisureProject LeisureId
        addedLeisureProject = increaseLeisureId maxId <$ newLp
        newLeisureProjects = addedLeisureProject : lps
    newLeisureProject .= initialLeisureProject
    leisureProjects .= newLeisureProjects
    setLocalStorageFromModel

viewNewTaskForm :: Model -> View Action
viewNewTaskForm m =
  let nt = m ^. newTask
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
        [class_ "mb-3 mt-3"]
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
      viewRepeater' (EveryNDays 1) = text "jeden Tag"
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
      notDoneRepeating = filter (isNothing . completionDay) (m ^. repeatingTasks)
   in div_
        [class_ "mt-3"]
        [ h3_ [] [viewIcon "arrow-clockwise", text " Wiederkehrende Aufgaben"],
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
   in div_ [class_ "progress w-100", style_ (M.singleton "height" "2.7em")] (makePart <$> parts)

viewAdapterSlider :: Model -> TimeEstimate -> View Action
viewAdapterSlider _m currentValue =
  div_
    []
    [ div_
        [class_ "hstack gap-1"]
        [ input_
            [ type_ "range",
              class_ "form-range",
              min_ "0",
              max_ "360",
              step_ "10",
              id_ "adapter-slider",
              value_ (showMiso (estimateInMinutes currentValue)),
              onInput (AdaptAllocationChange . read . fromMisoString)
            ],
          button_
            [class_ "btn btn-sm btn-danger", type_ "button", onClick CancelAdaptAllocation]
            [text "Abbruch"],
          button_
            [class_ "btn btn-sm btn-primary", type_ "button", onClick ToggleAdaptAllocation]
            [text "Ok"]
        ],
      small_ [] [strong_ [] [text (showMiso currentValue)]]
    ]

viewProgressBar :: Day -> TimeEstimate -> [RegularTask] -> View Action
viewProgressBar today' allocated' all =
  let description
        | leftover > 0 = small_ [] [strong_ [] [text (showMiso leftover <> "min"), text " übrig"]]
        | overhang > 0 = small_ [] [text (showMiso overhang <> "min drüber, gib auf dich acht!")]
        | otherwise = text ""
      done :: Int
      done = estimateInMinutes (foldMap timeEstimate (filter (\t -> completionDay t == Just today') all))
      allocated :: Int
      allocated = estimateInMinutes allocated'
      overhang :: Int
      overhang = max 0 (done - allocated)
      leftover = max 0 (allocated - done)
   in div_
        [class_ "mb-3"]
        [ div_
            [class_ "hstack gap-1"]
            [ buildProgressBar
                [ (min allocated done, Just "bg-success"),
                  (overhang, Just "bg-danger"),
                  (leftover, Nothing)
                ],
              button_ [type_ "button", class_ "btn btn-primary btn-sm", onClick ToggleAdaptAllocation] [text "Anpassen"]
            ],
          description
        ]

viewModeSwitcher :: Model -> View Action
viewModeSwitcher m =
  div_
    [class_ "btn-group d-flex mb-3"]
    [ input_ [class_ "btn-check", type_ "radio", name_ "display-mode", id_ "display-mode-work", value_ "work", checked_ ((m ^. displayMode) == DisplayWork), onClick ToggleMode],
      label_ [for_ "display-mode-work", class_ "btn btn-lg btn-outline-secondary w-100"] [text "🏢 Arbeit"],
      input_ [class_ "btn-check", type_ "radio", name_ "display-mode", id_ "display-mode-leisure", value_ "leisure", checked_ ((m ^. displayMode) == DisplayLeisure), onClick ToggleMode],
      label_ [for_ "display-mode-leisure", class_ "btn btn-lg btn-outline-secondary w-100"] ["😌 Freizeit"]
    ]

viewModel :: Model -> View Action
viewModel m =
  let content = case m ^. displayMode of
        DisplayWork -> viewModelWork m
        DisplayLeisure -> viewModelLeisure m
   in div_
        [class_ "container"]
        ( [ link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css", rel_ "stylesheet"],
            link_ [href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.2/font/bootstrap-icons.css", rel_ "stylesheet"],
            header_ [class_ "d-flex justify-content-center bg-info text-light mb-3"] [h1_ [class_ "mt-2 mb-2"] ["⏰ Sisyphus"]],
            viewModeSwitcher m,
            if (m ^. statusMessages) /= []
              then ol_ [] ((\sm -> li_ [] [text sm]) <$> (m ^. statusMessages))
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
        [h3_ [] [text "🌴 Deine Freizeitprojekte"], div_ [class_ "list-group list-group-flush"] (viewLeisureProject <$> (m ^. leisureProjects)), viewNewLeisureForm (m ^. newLeisureProject)]

viewModelWork :: Model -> View Action
viewModelWork m =
  let uncompletedTasks :: [RegularTask]
      uncompletedTasks = filter (\t -> Data.Maybe.fromMaybe True ((>= (m ^. today)) <$> completionDay t)) (m ^. tasks)
      taskIdsDoneToday :: S.Set TaskId
      taskIdsDoneToday = S.fromList (taskId <$> filter (\t -> completionDay t == Just (m ^. today)) uncompletedTasks)
      annealedIdsAndDoneToday :: S.Set TaskId
      annealedIdsAndDoneToday = (m ^. annealedTasks) <> taskIdsDoneToday
      (todayTasks, remainingTasks) = partition (\t -> taskId t `S.member` annealedIdsAndDoneToday) uncompletedTasks
      deadlineDays :: Task idType repeaterType -> Integer
      deadlineDays t = case deadline t of
        Nothing -> 4
        Just d ->
          let difference = diffDays d (m ^. today)
           in if difference < 0
                then 0
                else 1 + min 2 difference
      sortedRemainingTasks =
        sortBy
          (comparing deadlineDays <> comparing (Down . importance) <> comparing (Down . timeEstimate))
          remainingTasks
      viewRemainingTasks =
        div_
          [class_ "accordion accordion-flush"]
          [ div_
              [class_ "accordion-item"]
              [ h2_
                  [class_ "accordion-header"]
                  [ button_ [class_ ("accordion-button" <> if m ^. remainingTasksOpened then "" else " collapsed"), type_ "button", onClick ToggleRemainingTasksOpened] [text "Weitere Aufgaben"]
                  ],
                div_
                  [class_ ("accordion-collapse collapse" <> if m ^. remainingTasksOpened then " show" else "")]
                  [ div_ [class_ "accordion-body"] [viewTasksListGroup (m ^. today) sortedRemainingTasks]
                  ]
              ]
          ]
      progressOrAdaptation =
        case m ^. explicitAllocationChanging of
          Nothing -> viewProgressBar (m ^. today) (weekdayAllocationTime' m) (m ^. tasks)
          Just currentValue -> viewAdapterSlider m currentValue
   in div_
        []
        [ progressOrAdaptation,
          div_
            [class_ "d-flex justify-content-between align-items-center"]
            [ h5_ [] [text $ "Vorschlag (" <> showMiso (sum (estimateInMinutes . timeEstimate <$> todayTasks)) <> "min)"]
            -- div_ [] [button_ [type_ "button", class_ "btn btn-sm btn-outline-secondary", onClick IncreaseSeed] [i_ [class_ "bi-dice-5"] [], text $ " Neu würfeln"]]
            ],
          viewTasksListGroup (m ^. today) (sortBy (comparing (Down . importance) <> comparing title) todayTasks),
          if null remainingTasks then text "" else viewRemainingTasks,
          viewNewTaskForm m,
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
main =
  runApp $
    startApp
      App
        { initialAction = Init,
          model = initialModel,
          update = fromTransition . updateModel,
          view = viewModel,
          events = defaultEvents,
          subs = [timer],
          mountPoint = Nothing,
          logLevel = Off
        }
  where
    refreshAction :: Sink Action -> IO ()
    refreshAction sink =
      forever $ do
        let refreshSeconds = 30
        threadDelay (1000 * 1000 * refreshSeconds)
        sink RequestRefresh
    timer :: Sub Action
    timer sink = liftIO $ void $ forkIO $ refreshAction sink
