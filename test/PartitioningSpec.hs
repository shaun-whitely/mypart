{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PartitioningSpec where

import           AppError
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S
import           Data.Time.Calendar   (fromGregorian)
import           Data.Time.Clock      (UTCTime (..), secondsToDiffTime)
import           Models               (MonthPartition, Months,
                                       utcTimeToMonthPartition)
import           MonadTime
import qualified Partitioning
import           Queries
import           Test.Hspec

spec :: Spec
spec =
  describe "Partitioning.run" $ do

    context "when there are no existing partitions" $ do
      it "returns TableNotPartitioned" $ do
        runPartitioning testTime [] `shouldBe` (Left TableNotPartitioned, [])

    context "when the partitions are at least desired number of months ahead" $ do
      let ps = [ utcTimeToMonthPartition testTime
               ..utcTimeToMonthPartition test3MonthsFromNow
               ]

      it "does not modify the partitions" $ do
        execPartitioning testTime ps `shouldBe` ps

      it "returns empty list" $ do
        evalPartitioning testTime ps `shouldBe` (Right [])

    context "when the partitions are less than desired number of months ahead" $ do
      let ps = [ utcTimeToMonthPartition testTime
               ..utcTimeToMonthPartition test1MonthFromNow
               ]

      it "should create the necessary partitions" $ do
        let expected = [ utcTimeToMonthPartition testTime
                       ..utcTimeToMonthPartition test3MonthsFromNow
                       ]
        execPartitioning testTime ps `shouldBe` expected

      it "returns the created partitions" $ do
        let expected = [ utcTimeToMonthPartition test2MonthsFromNow
                       ..utcTimeToMonthPartition test3MonthsFromNow
                       ]
        evalPartitioning testTime ps `shouldBe` (Right expected)

testTime :: UTCTime
testTime = UTCTime (fromGregorian 2020 3 1) (secondsToDiffTime 0)

test1MonthFromNow :: UTCTime
test1MonthFromNow = UTCTime (fromGregorian 2020 4 1) (secondsToDiffTime 0)

test2MonthsFromNow :: UTCTime
test2MonthsFromNow = UTCTime (fromGregorian 2020 5 1) (secondsToDiffTime 0)

test3MonthsFromNow :: UTCTime
test3MonthsFromNow = UTCTime (fromGregorian 2020 6 1) (secondsToDiffTime 0)

monthsAhead :: Months
monthsAhead = 3

evalPartitioning
  :: UTCTime
  -> [MonthPartition]
  -> Either AppError [MonthPartition]
evalPartitioning time initialState = fst $ runPartitioning time initialState

execPartitioning
  :: UTCTime
  -> [MonthPartition]
  -> [MonthPartition]
execPartitioning time initialState = snd $ runPartitioning time initialState

runPartitioning
  :: UTCTime
  -> [MonthPartition]
  -> (Either AppError [MonthPartition], [MonthPartition])
runPartitioning time initialState
  = runTestStack (Partitioning.run monthsAhead) time initialState

runTestStack
  :: TestM a
  -> UTCTime
  -> [MonthPartition]
  -> (a, [MonthPartition])
runTestStack m time s = S.runState (R.runReaderT (runTestM m) time) s

newtype TestM a
  = TestM
  { runTestM :: R.ReaderT UTCTime (S.State [MonthPartition]) a
  }
  deriving
  ( Functor
  , Applicative
  , Monad
  , R.MonadReader UTCTime
  , S.MonadState [MonthPartition]
  )

instance MonadQueries TestM where
  getPartitions = S.get
  createInitialPartition = undefined
  createPartitions ps = S.modify (++ps)

instance MonadTime TestM where
  currentTime = R.ask
