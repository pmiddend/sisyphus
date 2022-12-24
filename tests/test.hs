{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import Data.Time.Calendar (Day)
import Simanneal
import Task
import Test.Hspec

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
main = hspec $ do
  describe "createRepeatingTasks" $ do
    describe "daily" $ do
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
      it "should create task" $ do
        createRepeatingTasks (toEnum 0) mempty [dailyTask] `shouldBe` [dailyTask {repeater = Just (taskId dailyTask)}]
      it "should not create task twice" $ do
        let todaysTasks = createRepeatingTasks (toEnum 0) mempty [dailyTask]
        createRepeatingTasks (toEnum 1) todaysTasks [dailyTask] `shouldBe` []
      it "should recreate task once done" $ do
        let todaysTasks = createRepeatingTasks (toEnum 0) mempty [dailyTask]
            completedToday = (\t -> t {completionDay = Just (toEnum 0)}) <$> todaysTasks
        createRepeatingTasks (toEnum 1) completedToday [dailyTask] `shouldNotBe` []
    describe "every n days" $ do
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
      it "should create task" $ do
        createRepeatingTasks
          (toEnum 0)
          mempty
          [everyDays]
          `shouldBe` [everyDays {repeater = Just (taskId everyDays)}]
      it "should not create task twice" $ do
        let todaysTasks = createRepeatingTasks (toEnum 0) mempty [everyDays]
        createRepeatingTasks (toEnum 1) todaysTasks [everyDays] `shouldBe` []
      it "should not recreate task once done immediately" $ do
        let todaysTasks = createRepeatingTasks (toEnum 0) mempty [everyDays]
            completedToday = (\t -> t {completionDay = Just (toEnum 0)}) <$> todaysTasks
        createRepeatingTasks (toEnum 1) completedToday [everyDays] `shouldBe` []
      it "should recreate task after repetition time" $ do
        let todaysTasks = createRepeatingTasks (toEnum 0) mempty [everyDays]
            completedToday = (\t -> t {completionDay = Just (toEnum 0)}) <$> todaysTasks
        createRepeatingTasks (toEnum 4) completedToday [everyDays] `shouldNotBe` []
  describe "annealTasksInModel" $ do
    it "should fill up the space" $ do
      annealTasksInModel
        baseSeed
        baseDay
        (TimeEstimate 80)
        ((\tid -> noDeadline ("t" <> showMiso tid) notImportant tenMin tid) <$> [1 .. 8])
        `shouldBe` (S.fromList [1 .. 8])

    it "should fill up the space until limit is reached" $ do
      ( S.size
          ( annealTasksInModel
              baseSeed
              baseDay
              (TimeEstimate 80)
              ((\tid -> noDeadline ("t" <> showMiso tid) notImportant tenMin tid) <$> [1 .. 20])
          )
        )
        `shouldBe` 8

    it "should prioritize importance" $ do
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
        `shouldSatisfy` (S.member (TaskId 7))
