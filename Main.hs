module Main where

import           Control.Conditional   (if')
import           Control.Monad         (liftM2)
import           Data.List
import           Data.Maybe
import           System.Random
import           System.Random.Shuffle (shuffleM)

type ShiftList = String

type Person = String

type Schedule = [Day]

type Availability = [(Person, ShiftList)]

data Shift =
  Shift Char Person -- TODO: [Person] 😱
  deriving (Eq)

data Day =
  Day Int [Shift]
  deriving (Eq)

instance Show Day where
  show (Day n xs) = "Day " ++ show n ++ ": \n" ++ show xs

instance Show Shift where
  show (Shift c p) = "Shift " ++ [c] ++ ", Volunteer: " ++ p ++ "\n"

zipWithPredicate :: (a -> b -> c) -> (a -> b -> Bool) -> [a] -> [b] -> [c]
zipWithPredicate f g xs ys = fmap (uncurry f) $ filter (uncurry g) $ zip xs ys

getPeopleFromShifts :: Availability -> Char -> Person -> Bool
getPeopleFromShifts av c p = ifAble $ lookup p av
  where
    ifAble (Just s) = c `elem` s
    ifAble Nothing  = False

randomFillShifts :: [Person] -> ShiftList -> Availability -> IO [Shift]
randomFillShifts people shifts av = do
  shuffledPeople <- shuffleM people
  pure $ zipWithPredicate Shift (getPeopleFromShifts av) shifts shuffledPeople

randomDay :: Int -> [Person] -> ShiftList -> Availability -> IO Day
randomDay day people shifts av =
  fmap (Day day) (randomFillShifts people shifts av)

getRandomSchedule :: Int -> [Person] -> ShiftList -> Availability -> IO Schedule
getRandomSchedule days people shifts av =
  sequence [randomDay day people shifts av | day <- [1 .. days]]

generateSchedule ::
     Int -> [Person] -> ShiftList -> Availability -> IO [Schedule]
generateSchedule days people shifts av =
  catMaybes <$> (getRandomSchedule days people shifts av >>= applyPred)
  where
    pred = noonesWorkingLongerThen 7
    applyPred :: (Applicative f) => Schedule -> f [Maybe Schedule]
    applyPred = flip (liftM2 if' pred (pure . return . Just)) (pure [Nothing])

getStaffList :: Schedule -> [Person]
getStaffList [] = []
getStaffList (Day _ shifts:_) = map person shifts
  where
    person (Shift _ p) = p

hasOff :: Person -> Day -> Bool
hasOff p (Day _ shifts) = all isOff shifts
  where
    isOff (Shift _ q) = p /= q

longestStreak :: Eq e => e -> [e] -> Int
longestStreak e =
  maximum . (0 :) . map length . filter (e `matchesGroup`) . group
  where
    matchesGroup _ []     = False
    matchesGroup e (x:xs) = e == x

getStretchList :: Schedule -> [Int]
getStretchList sched =
  map (\person -> longestStreak False $ map (hasOff person) sched) staffList
  where
    staffList = getStaffList sched -- TODO: Restrictions

noonesWorkingLongerThen :: Int -> Schedule -> Bool
noonesWorkingLongerThen daysLong sched =
  not . any (> daysLong) $ getStretchList sched

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
