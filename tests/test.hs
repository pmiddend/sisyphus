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

noDeadline title' importance' timeEstimate' tid' =
  Task
    { title = title',
      importance = importance',
      deadline = Nothing,
      timeEstimate = timeEstimate',
      completionDay = Nothing,
      taskId = tid',
      repeater = Nothing
    }

dailyTask :: RepeatingTask
dailyTask =
  Task
    { title = "daily",
      importance = notImportant,
      deadline = Nothing,
      timeEstimate = tenMin,
      completionDay = Nothing,
      taskId = TaskId 1,
      repeater = EveryNDays 1
    }

everyDays :: RepeatingTask
everyDays =
  Task
    { title = "everydays",
      importance = notImportant,
      deadline = Nothing,
      timeEstimate = tenMin,
      completionDay = Nothing,
      taskId = TaskId 1,
      repeater = EveryNDays 3
    }

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

testsDailyTasks =
  let dailyTask =
        Task
          { title = "daily",
            importance = notImportant,
            deadline = Nothing,
            timeEstimate = tenMin,
            completionDay = Nothing,
            taskId = TaskId 1,
            repeater = EveryNDays 1
          }
   in testGroup
        "daily tasks"
        [ testCase "task should be created" $ createRepeatingTasks (toEnum 0) mempty [dailyTask] @?= [dailyTask {repeater = Just (taskId dailyTask)}],
          testCase "task shouldn't be recreated if not done yet" $
            let todaysTasks = createRepeatingTasks (toEnum 0) mempty [dailyTask]
             in createRepeatingTasks (toEnum 1) todaysTasks [dailyTask]
                  @?= [],
          testCase
            "task should be recreated on the next day once it's done on the first day"
            $ let todaysTasks = createRepeatingTasks (toEnum 0) mempty [dailyTask]
                  completedToday = (\t -> t {completionDay = Just (toEnum 0)}) <$> todaysTasks
               in assertBool "should not be empty" (createRepeatingTasks (toEnum 1) completedToday [dailyTask] /= [])
        ]

testEveryNDayTasks :: TestTree
testEveryNDayTasks =
  let everyDays =
        Task
          { title = "everydays",
            importance = notImportant,
            deadline = Nothing,
            timeEstimate = tenMin,
            completionDay = Nothing,
            taskId = TaskId 1,
            repeater = EveryNDays 3
          }
   in testGroup
        "Every N day tasks"
        [ testCase "should create task" $ createRepeatingTasks (toEnum 0) mempty [everyDays] @?= [everyDays {repeater = Just (taskId everyDays)}],
          testCase "should not create task twice" $
            let todaysTasks = createRepeatingTasks (toEnum 0) mempty [everyDays]
             in createRepeatingTasks (toEnum 1) todaysTasks [everyDays] @?= [],
          testCase "should not recreate task once done immediately" $
            let todaysTasks = createRepeatingTasks (toEnum 0) mempty [everyDays]
                completedToday = (\t -> t {completionDay = Just (toEnum 0)}) <$> todaysTasks
             in createRepeatingTasks (toEnum 1) completedToday [everyDays]
                  @?= [],
          testCase
            "should recreate task after repetition time"
            $ let todaysTasks = createRepeatingTasks (toEnum 0) mempty [everyDays]
                  completedToday = (\t -> t {completionDay = Just (toEnum 0)}) <$> todaysTasks
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
          ((\tid -> noDeadline ("t" <> showMiso tid) notImportant tenMin tid) <$> [1 .. 8])
          @?= (S.fromList [1 .. 8]),
      testCase
        "should fill up the space until limit is reached"
        $ ( S.size
              ( annealTasksInModel
                  baseSeed
                  baseDay
                  (TimeEstimate 80)
                  ((\tid -> noDeadline ("t" <> showMiso tid) notImportant tenMin tid) <$> [1 .. 20])
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
                [ noDeadline "t1" notImportant tenMin (TaskId 1),
                  noDeadline "t2" notImportant tenMin (TaskId 2),
                  noDeadline "t3" notImportant tenMin (TaskId 3),
                  noDeadline "t4" notImportant tenMin (TaskId 4),
                  noDeadline "t5" notImportant tenMin (TaskId 5),
                  noDeadline "t6" notImportant tenMin (TaskId 6),
                  noDeadline "t7" important tenMin (TaskId 7),
                  noDeadline "t8" notImportant tenMin (TaskId 8),
                  noDeadline "t9" notImportant tenMin (TaskId 9)
                ]
            )
    ]

testsRepeatingTasks :: TestTree
testsRepeatingTasks = testGroup "Repeating tasks" [testsDailyTasks, testEveryNDayTasks]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [testsRepeatingTasks, testsAnnealing]
