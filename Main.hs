module Main where

import Lib

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
