module Main where


import Lib
-- import           Control.Conditional   (if')
-- import           Control.Monad         (liftM2)
-- import           Data.List
-- import           Data.Maybe
-- import           System.Random
-- import           System.Random.Shuffle (shuffleM)

main :: IO ()
main = do
  let days = 30 -- TODO: get days length from month (ask with prompt)
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
  print =<< generateSchedule days people shifts av
