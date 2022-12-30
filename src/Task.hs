{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    applyN,
    succN,
    calculateWeekday,
    createRepeatingTasks,
    safeMaximum,
    annealTasksInModel,
    showMiso,
    increaseTaskId,
    mapTaskId,
    increaseLeisureId,
    isEveryNDays,
    isEveryWeekday,
    estimateInMinutes,
    weekdayToAllocationTime,
    equating,
    readIntegral,
  )
where

import Control.Lens (Getter, folded, from, sumOf, to, traversed, view, (^.))
import Data.Function (on)
import Data.List (partition)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set as S
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Miso.String (MisoString, toMisoString)
import Simanneal
import Types
import Prelude hiding (all)

taskEstimateSum :: [Task idType repeaterType] -> TimeEstimate
--taskEstimateSum ts = TimeEstimate (sum (estimateInMinutes . timeEstimate <$> ts))
taskEstimateSum = sumOf (folded . timeEstimate)

annealTasksInModel :: forall idType repeaterType. Ord idType => Seed -> Day -> TimeEstimate -> [Task idType repeaterType] -> S.Set idType
annealTasksInModel seed' today' timeBudgetForToday tasks' =
  S.fromList
    ( view taskId
        <$> annealTasks
          seed'
          today'
          timeBudgetForToday
          (taskEstimateSum (filter (\t -> (t ^. completionDay) == Just today') tasks'))
          (filter (isNothing . view completionDay) tasks')
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
annealTasks seed' today' timeBudgetForToday spentMinutes allTasks =
  let taskUrgency :: Getter (Task idType repeaterType) Float
      taskUrgency =
        to
          ( \t -> case t ^. deadline of
              Nothing -> 0.0
              Just d ->
                min 3.0 (max 0.0 (fromIntegral (diffDays d today')))
          )
      taskAge :: Getter (Task idType repeaterType) Float
      taskAge = to (\t -> min 7.0 (abs (fromIntegral (diffDays (t ^. created) today'))))
      allocated :: TimeEstimate
      allocated = max 0 (timeBudgetForToday - spentMinutes)
      totalEstimate :: TimeEstimate
      totalEstimate = taskEstimateSum allTasks
      maxDistanceToAllocated :: TimeEstimate
      maxDistanceToAllocated = max allocated totalEstimate
      distance x y = abs (x - y)
      (baseTasks, remainingTasks) = partition (\t -> maybe False (<= today') (t ^. deadline)) allTasks
      taskEnergy :: Task idType repeaterType -> Energy
      taskEnergy t =
        ((t ^. importance . numericImportance . to fromIntegral . from energyFloat) ^* 0.05)
          + ((t ^. taskUrgency . from energyFloat) ^* 0.05)
          + ((t ^. taskAge . from energyFloat) ^* 0.01)
      taskGroupEnergy :: ([Task idType repeaterType], [Task idType repeaterType]) -> Energy
      taskGroupEnergy (ts, _) =
        let closeToAllocated :: Energy
            closeToAllocated = Energy (fromIntegral (distance allocated (taskEstimateSum (ts <> baseTasks))) / fromIntegral maxDistanceToAllocated)
         in closeToAllocated - sumOf (traversed . to taskEnergy) ts
      mutateTasks :: ([Task idType repeaterType], [Task idType repeaterType]) -> SimannealState ([Task idType repeaterType], [Task idType repeaterType]) ([Task idType repeaterType], [Task idType repeaterType])
      mutateTasks (chosenTasks, openTasks) = do
        removeOrAdd :: Int <- randomRS (1, 100)
        let thisIterationRemoves = removeOrAdd <= 50
        if length chosenTasks > 1 && thisIterationRemoves
          then do
            (removedTask, newChosenTasks) <- removeRandomElement chosenTasks
            pure (newChosenTasks, removedTask : openTasks)
          else
            if not (null openTasks)
              then do
                (removedTask, newOpenTasks) <- removeRandomElement openTasks
                pure (removedTask : chosenTasks, newOpenTasks)
              else pure (chosenTasks, openTasks)
   in if length allTasks <= 1 || null remainingTasks
        then allTasks
        else
          let solution = simanneal seed' (remainingTasks, []) mutateTasks taskGroupEnergy 100.0 0.01 0.5
           in (baseTasks <> fst solution)

weekdayToAllocationTime :: Weekday -> TimeEstimate
weekdayToAllocationTime Saturday = TimeEstimate 180
weekdayToAllocationTime Sunday = TimeEstimate 180
weekdayToAllocationTime _ = TimeEstimate 80

goBackUntilWeekdayMatches :: Weekday -> Day -> Day
goBackUntilWeekdayMatches repeatOn lastClosing =
  if calculateWeekday lastClosing == repeatOn
    then lastClosing
    else goBackUntilWeekdayMatches repeatOn (pred lastClosing)

createRepeatingTasks :: Day -> [RegularTask] -> [RepeatingTask] -> [RegularTask]
createRepeatingTasks today' regularTasks = concatMap possiblyRepeat
  where
    createTask rt = [const (Just (rt ^. taskId)) `mapRepeater` rt]
    possiblyRepeat :: RepeatingTask -> [RegularTask]
    possiblyRepeat rt
      | isJust (rt ^. completionDay) = []
      | otherwise =
        let hasOpenCandidate = any (\t -> (t ^. repeater) == Just (rt ^. taskId) && isNothing (t ^. completionDay)) regularTasks
         in if hasOpenCandidate
              then []
              else
                let lc = fromMaybe (rt ^. created) (safeMaximum (mapMaybe (\t -> if t ^. repeater == Just (rt ^. taskId) then t ^. completionDay else Nothing) regularTasks))
                 in case rt ^. repeater of
                      EveryNDays n ->
                        if diffDays today' lc >= fromIntegral n
                          then createTask rt
                          else []
                      EveryWeekday repeatOn ->
                        let previousToBeClosed = goBackUntilWeekdayMatches repeatOn lc
                         in if diffDays previousToBeClosed today' >= 7
                              then createTask rt
                              else []

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

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum xs = Just (maximum xs)

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating f = (==) `on` f

applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)

succN :: Enum a => Int -> a -> a
succN n = applyN n succ

readIntegral :: Num a => String -> Maybe a
readIntegral s =
  case reads s of
    (i, _) : _ -> Just $ fromInteger i
    [] -> Nothing
