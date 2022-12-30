{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import Data.Time.Calendar (Day)
import Simanneal
import Task
import Test.Tasty
import Test.Tasty.HUnit

baseDay :: Day
baseDay = toEnum 0

baseSeed :: Seed
baseSeed = 14

notImportant = Importance 0

important = Importance 1

tenMin = TimeEstimate 10

defaultTask :: TaskId -> Task TaskId (Maybe repeaterType)
defaultTask tid =
  Task
    { _title = (showMiso tid),
      _importance = notImportant,
      _created = baseDay,
      _deadline = Nothing,
      _timeEstimate = tenMin,
      _completionDay = Nothing,
      _taskId = tid,
      _repeater = Nothing
    }

-- noDeadline importance' timeEstimate' created' tid' =
--   defaultTask {
--       importance = importance',
--       created = created',
--       deadline = Nothing,
--       timeEstimate = timeEstimate',
--       completionDay = Nothing,
--       taskId = tid',
--       repeater = Nothing
--     }

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

testsDailyTasks =
  let dailyTask = (defaultTask (TaskId 1)) {_repeater = EveryNDays 1}
   in testGroup
        "daily tasks"
        [ testCase "task should be created" $ createRepeatingTasks (toEnum 0) mempty [dailyTask] @?= [dailyTask {_repeater = Just (TaskId 1)}],
          testCase "task shouldn't be recreated if not done yet" $
            let todaysTasks = createRepeatingTasks (toEnum 0) mempty [dailyTask]
             in createRepeatingTasks (toEnum 1) todaysTasks [dailyTask]
                  @?= [],
          testCase
            "task should be recreated on the next day once it's done on the first day"
            $ let todaysTasks = createRepeatingTasks (toEnum 0) mempty [dailyTask]
                  completedToday = (\t -> t {_completionDay = Just (toEnum 0)}) <$> todaysTasks
               in assertBool "should not be empty" (createRepeatingTasks (toEnum 1) completedToday [dailyTask] /= [])
        ]

testEveryNDayTasks :: TestTree
testEveryNDayTasks =
  let everyDays = (defaultTask (TaskId 1)) {_repeater = EveryNDays 3}
   in testGroup
        "Every N day tasks"
        [ testCase "should create task" $ createRepeatingTasks (toEnum 0) mempty [everyDays] @?= [everyDays {_repeater = Just (TaskId 1)}],
          testCase "should not create task twice" $
            let todaysTasks = createRepeatingTasks (toEnum 0) mempty [everyDays]
             in createRepeatingTasks (toEnum 1) todaysTasks [everyDays] @?= [],
          testCase "should not recreate task once done immediately" $
            let todaysTasks = createRepeatingTasks (toEnum 0) mempty [everyDays]
                completedToday = (\t -> t {_completionDay = Just (toEnum 0)}) <$> todaysTasks
             in createRepeatingTasks (toEnum 1) completedToday [everyDays]
                  @?= [],
          testCase
            "should recreate task after repetition time"
            $ let todaysTasks = createRepeatingTasks (toEnum 0) mempty [everyDays]
                  completedToday = (\t -> t {_completionDay = Just (toEnum 0)}) <$> todaysTasks
               in assertBool "shouldn't create task" (createRepeatingTasks (toEnum 4) completedToday [everyDays] /= [])
        ]

testsAnnealing :: TestTree
testsAnnealing =
  testGroup
    "Annealing"
    [ testCase "should fill up the space" $
        annealTasksInModel
          baseSeed
          baseDay
          (TimeEstimate 80)
          (defaultTask . TaskId <$> [1 .. 8])
          @?= (S.fromList (TaskId <$> [1 .. 8])),
      testCase
        "should fill up the space until limit is reached"
        $ ( S.size
              ( annealTasksInModel
                  baseSeed
                  baseDay
                  (TimeEstimate 80)
                  (defaultTask . TaskId <$> [1 .. 20])
              )
          )
          @?= 8,
      testCase
        "should prioritize importance"
        $ assertBool "important should be in it" $
          S.member
            (TaskId 7)
            ( annealTasksInModel
                baseSeed
                baseDay
                (TimeEstimate 30)
                [ defaultTask (TaskId 1),
                  defaultTask (TaskId 2),
                  defaultTask (TaskId 3),
                  defaultTask (TaskId 4),
                  defaultTask (TaskId 5),
                  defaultTask (TaskId 6),
                  (defaultTask (TaskId 7)) {_importance = important},
                  defaultTask (TaskId 8),
                  defaultTask (TaskId 9)
                ]
            ),
      testCase
        "should prioritize old tasks"
        $ assertBool "old should be in it" $
          S.member
            (TaskId 11)
            ( annealTasksInModel
                baseSeed
                baseDay
                (TimeEstimate 30)
                (((defaultTask . TaskId) <$> [1 .. 10]) <> [(defaultTask (TaskId 11)) {_created = pred (pred (pred baseDay))}])
            )
    ]

testsRepeatingTasks :: TestTree
testsRepeatingTasks = testGroup "Repeating tasks" [testsDailyTasks, testEveryNDayTasks]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [testsRepeatingTasks, testsAnnealing]
