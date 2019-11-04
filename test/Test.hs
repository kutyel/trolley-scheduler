module Test where

import Lib
import Test.Hspec
import System.Random
import Control.Monad.IO.Class (liftIO)

main = hspec $ do
  describe "something" $ do
    it "goes" $ do
      liftIO $ setStdGen $ mkStdGen 1234
      let days = 3 -- TODO: get days length from month (ask with prompt)
          people = ["Ben", "Emily", "Kate", "Flavio", "Rob", "Caty"]
          av :: Availability
          av =
            [ ("Ben", "TW")
            , ("Emily", "MH")
            , ("Kate", "MW")
            , ("Flavio", "TWH")
            , ("Rob", "MTH")
            ]
          shifts = "MTWHFS" -- Each char is a seperate shift
      sched <- generateSchedule days people shifts av

      sched `shouldBe` [ [ Day 'M' 1 [ Shift "Ben"
                                     , Shift "Flavio"
                                     ]
                         , Day 'M' 2 [ Shift "Emily" ]
                         , Day 'M' 3 [ Shift "Rob"
                                     , Shift "Ben"
                                     , Shift "Kate"
                                     , Shift "Emily"
                                     ]
                         ]
                       ]
