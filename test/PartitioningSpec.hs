{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PartitioningSpec where

import           AppError
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S
import           Data.Time.Calendar   (fromGregorian)
import           Data.Time.Clock      (UTCTime (..), secondsToDiffTime)
import           Models               (MonthPartition, utcTimeToMonthPartition)
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

    context "when the partitions are at least 3 months ahead" $ do
      it "does not modify the partitions" $ do
        let ps = [ utcTimeToMonthPartition testTime
                 ..utcTimeToMonthPartition test3MonthsFromNow
                 ]
        runPartitioning testTime ps `shouldBe` (Right (), ps)

    context "when the partitions are less than 3 months ahead" $ do
      it "should create the necessary partitions" $ do
        let ps = [ utcTimeToMonthPartition testTime
                 ..utcTimeToMonthPartition test1MonthFromNow
                 ]
        let expected = [ utcTimeToMonthPartition testTime
                       ..utcTimeToMonthPartition test3MonthsFromNow
                       ]
        runPartitioning testTime ps `shouldBe` (Right (), expected)

testTime :: UTCTime
testTime = UTCTime (fromGregorian 2020 3 1) (secondsToDiffTime 0)

test1MonthFromNow :: UTCTime
test1MonthFromNow = UTCTime (fromGregorian 2020 4 1) (secondsToDiffTime 0)

test3MonthsFromNow :: UTCTime
test3MonthsFromNow = UTCTime (fromGregorian 2020 6 1) (secondsToDiffTime 0)

runPartitioning
  :: UTCTime
  -> [MonthPartition]
  -> (Either AppError (), [MonthPartition])
runPartitioning time initialState
  = runTestStack Partitioning.run time initialState

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
