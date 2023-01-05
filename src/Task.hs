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
    daysSinceCreation,
    calculateNewId,
    applyN,
    succN,
    predN,
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

import Control.Lens (Getter, folded, from, sumOf, to, traversed, view, (&), (.~), (^.))
import Control.Monad (join)
import Control.Monad.State.Strict (State, evalState, get, put)
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
-- taskEstimateSum ts = TimeEstimate (sum (estimateInMinutes . timeEstimate <$> ts))
taskEstimateSum = sumOf (folded . timeEstimate)

annealTasksInModel :: forall idType repeaterType. Ord idType => Seed -> Day -> TimeEstimate -> [Task idType repeaterType] -> (AnnealMetadata, S.Set idType)
annealTasksInModel seed' today' timeBudgetForToday tasks' =
  let (resultingEnergy, resultingTasks) =
        annealTasks
          seed'
          today'
          timeBudgetForToday
          (filter (\t -> isNothing (t ^. completionDay) || (t ^. completionDay) == Just today') tasks')
   in (resultingEnergy, S.fromList (view taskId <$> resultingTasks))

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

daysSinceCreation :: Num a => Day -> Task idType repeaterType -> a
daysSinceCreation today' t = abs (fromIntegral (diffDays (t ^. created) today'))

data AnnealMetadata = AnnealMetadata
  { _annealFinalEnergy :: Energy,
    _annealMaxDistanceToAllocated :: TimeEstimate,
    _annealDistanceMeasure :: Energy,
    _annealEnergyMeasure :: Energy
  }

instance Show AnnealMetadata where
  show (AnnealMetadata fe mdta d e) = "energy: " <> show fe <> ", mdta: " <> show mdta <> ", d: " <> show d <> ", e: " <> show e

annealTasks :: forall idType repeaterType. Seed -> Day -> TimeEstimate -> [Task idType repeaterType] -> (AnnealMetadata, [Task idType repeaterType])
annealTasks seed' today' allocated allTasks =
  let taskUrgency :: Getter (Task idType repeaterType) Float
      taskUrgency =
        to
          ( \t -> case t ^. deadline of
              Nothing -> 0.0
              Just d ->
                min 3.0 (max 0.0 (fromIntegral (diffDays d today')))
          )
      taskAge :: Getter (Task idType repeaterType) Float
      taskAge = to (\t -> min 7.0 (daysSinceCreation today' t))
      totalEstimate :: TimeEstimate
      totalEstimate = taskEstimateSum allTasks
      maxDistanceToAllocated :: TimeEstimate
      maxDistanceToAllocated = max allocated (max 0 (totalEstimate - allocated))
      distance x y = abs (x - y)
      (baseTasks, remainingTasks) = partition (\t -> maybe False (<= today') (t ^. deadline) || (t ^. completionDay) == Just today') allTasks
      taskEnergy :: Task idType repeaterType -> Energy
      taskEnergy t =
        ((t ^. importance . numericImportance . to fromIntegral . from energyFloat) ^* 0.05)
          + ((t ^. taskUrgency . from energyFloat) ^* 0.05)
          + ((t ^. taskAge . from energyFloat) ^* 0.01)
      distanceFromMax ts = Energy (fromIntegral (distance allocated (taskEstimateSum ts)) / fromIntegral maxDistanceToAllocated)
      taskGroupEnergy :: ([Task idType repeaterType], [Task idType repeaterType]) -> Energy
      taskGroupEnergy (ts, _) =
        distanceFromMax (ts <> baseTasks) - sumOf (traversed . to taskEnergy) ts
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
        then (AnnealMetadata (taskGroupEnergy (allTasks, [])) 0 0 0, allTasks)
        else
          let solution = simanneal seed' (remainingTasks, []) mutateTasks taskGroupEnergy 100.0 0.01 0.5
              finalTasks = baseTasks <> fst solution
              distanceMeasure = distanceFromMax finalTasks
              energyMeasure = sumOf (traversed . to taskEnergy) finalTasks
           in (AnnealMetadata (taskGroupEnergy (finalTasks, [])) maxDistanceToAllocated distanceMeasure energyMeasure, finalTasks)

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
createRepeatingTasks today' regularTasks' repeatingTasks' = evalState (createRepeatingTasks' today' regularTasks' repeatingTasks') (calculateNewId regularTasks' repeatingTasks')

concatMapM :: (Monad t, Traversable t, Applicative m) => (a -> m (t b)) -> t a -> m (t b)
concatMapM f t = join <$> traverse f t

createRepeatingTasks' :: Day -> [RegularTask] -> [RepeatingTask] -> State TaskId [RegularTask]
createRepeatingTasks' today' regularTasks' repeatingTasks' = concatMapM possiblyRepeat repeatingTasks'
  where
    createTask :: RepeatingTask -> State TaskId [RegularTask]
    createTask rt = do
      newId <- get
      put (increaseTaskId newId)
      pure [rt & repeater .~ (Just (rt ^. taskId)) & taskId .~ newId & created .~ today']
    possiblyRepeat :: RepeatingTask -> State TaskId [RegularTask]
    possiblyRepeat rt
      | isJust (rt ^. completionDay) = pure []
      | otherwise =
          let hasOpenCandidate = any (\t -> (t ^. repeater) == Just (rt ^. taskId) && isNothing (t ^. completionDay)) regularTasks'
           in if hasOpenCandidate
                then pure []
                else
                  let lc = fromMaybe (rt ^. created) (safeMaximum (mapMaybe (\t -> if t ^. repeater == Just (rt ^. taskId) then t ^. completionDay else Nothing) regularTasks'))
                   in case rt ^. repeater of
                        EveryNDays n ->
                          if diffDays today' lc >= fromIntegral n
                            then createTask rt
                            else pure []
                        EveryWeekday repeatOn ->
                          let previousToBeClosed = goBackUntilWeekdayMatches repeatOn lc
                           in if diffDays previousToBeClosed today' >= 7
                                then createTask rt
                                else pure []

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

predN :: Enum a => Int -> a -> a
predN n = applyN n pred

readIntegral :: Num a => String -> Maybe a
readIntegral s =
  case reads s of
    (i, _) : _ -> Just $ fromInteger i
    [] -> Nothing

calculateNewId :: [RegularTask] -> [RepeatingTask] -> TaskId
calculateNewId tasks' rtasks' =
  increaseTaskId (fromMaybe (TaskId 0) (safeMaximum (((^. taskId) <$> tasks') <> ((^. taskId) <$> rtasks'))))
