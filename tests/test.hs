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

main :: IO ()
main = hspec $ do
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
