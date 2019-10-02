module Main where

import           Data.List
import           Data.Maybe
import           System.Random
import           System.Random.Shuffle (shuffleM)

type ShiftList = String

type Person = String

type Schedule = [Day] --Adds Xs ("is-off") to the shift list to pair up with non-working people

type Availability = (Person, ShiftList)

data Day =
  Day Int [Shift]
  deriving (Eq)

instance Show Day where
  show (Day n xs) = "Day " ++ show n ++ ": \n" ++ show xs

data Shift =
  Shift Char Person
  deriving (Eq)

instance Show Shift where
  show (Shift c p) = "Shift " ++ [c] ++ ", Volunteer: " ++ p ++ "\n"

padShifts :: Int -> ShiftList -> ShiftList
padShifts nPeople shifts = shifts ++ replicate (nPeople - length shifts) 'X' --Randomly assigns people shifts

randomFillShifts :: [Person] -> ShiftList -> IO [Shift]
randomFillShifts people shifts = do
  let paddedShifts = padShifts (length people) shifts
  shuffledPeople <- shuffleM people
  let schedule = zipWith Shift paddedShifts shuffledPeople
  pure $ filter ignoreDaysOff schedule
  where
    ignoreDaysOff :: Shift -> Bool
    ignoreDaysOff (Shift x _) = x /= 'X'

randomDay :: Int -> [Person] -> ShiftList -> IO Day
randomDay day people shifts = fmap (Day day) (randomFillShifts people shifts)

getRandomSchedule :: Int -> [Person] -> ShiftList -> IO Schedule
getRandomSchedule days people shifts =
  sequence [randomDay day people shifts | day <- [1 .. days]]

generateSchedule :: Int -> [Person] -> ShiftList -> IO [Schedule]
generateSchedule days people shifts =
  catMaybes <$>
  (getRandomSchedule days people shifts >>= \s ->
     if pred s
       then pure [Just s]
       else pure [Nothing])
  where
    pred = noonesWorkingLongerThen 7

getStaffList :: Schedule -> [Person]
getStaffList [] = []
getStaffList (Day _ shifts:_) = map person shifts
  where
    person (Shift _ p) = p

hasOff :: Person -> Day -> Bool
hasOff p (Day _ shifts) = all isOff shifts
  where
    isOff (Shift s q) = p /= q || s == 'X'

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
  let days = 30 -- The full rotation length
      people = ["Ben", "Emily", "Kate", "Flavio", "Rob", "Caty"] -- Volunteer list
      av -- TODO: Availability :: (Volunteer, Shifts)
       = [("B", "23"), ("E", "1"), ("K", "13"), ("F", "3"), ("R", "2")]
      shifts = "MTW" -- Each char is a seperate shift
  found <- generateSchedule days people shifts
  print found
