module Main where

import           Data.List
import           Data.Maybe
import           System.Random
import           System.Random.Shuffle (shuffleM)

type ShiftList = String

type Person = String

type Schedule = [Day]

type Availability = [(Person, ShiftList)]

data Shift =
  Shift Char Person
  deriving (Eq)

data Day =
  Day Int [Shift]
  deriving (Eq)

instance Show Day where
  show (Day n xs) = "Day " ++ show n ++ ": \n" ++ show xs

instance Show Shift where
  show (Shift c p) = "Shift " ++ [c] ++ ", Volunteer: " ++ p ++ "\n"

zipWithPredicate :: (a -> b -> c) -> (b -> a -> Bool) -> [a] -> [b] -> [c]
zipWithPredicate f g xs ys = [f x y | x <- xs, y <- ys, g y x]

padShifts :: Int -> ShiftList -> ShiftList
padShifts nPeople shifts = shifts ++ replicate (nPeople - length shifts) 'X' --Randomly assigns people shifts

getPeopleFromShifts :: Availability -> Person -> Char -> Bool
getPeopleFromShifts av p c = ifAble $ lookup p av
  where
    ifAble (Just s) = c `elem` s
    ifAble Nothing  = False

randomFillShifts :: [Person] -> ShiftList -> Availability -> IO [Shift]
randomFillShifts people shifts av = do
  let paddedShifts = padShifts (length people) shifts
  shuffledPeople <- shuffleM people
  let schedule =
        zipWithPredicate
          Shift
          (getPeopleFromShifts av)
          paddedShifts
          shuffledPeople
  pure $ filter ignoreDaysOff schedule
  where
    ignoreDaysOff :: Shift -> Bool
    ignoreDaysOff (Shift x _) = x /= 'X'

randomDay :: Int -> [Person] -> ShiftList -> Availability -> IO Day
randomDay day people shifts av =
  fmap (Day day) (randomFillShifts people shifts av)

getRandomSchedule :: Int -> [Person] -> ShiftList -> Availability -> IO Schedule
getRandomSchedule days people shifts av =
  sequence [randomDay day people shifts av | day <- [1 .. days]]

generateSchedule ::
     Int -> [Person] -> ShiftList -> Availability -> IO [Schedule]
generateSchedule days people shifts av =
  catMaybes <$>
  (getRandomSchedule days people shifts av >>= \s ->
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
      av :: Availability
      av =
        [ ("Ben", "TW")
        , ("Emily", "M")
        , ("Kate", "MW")
        , ("Flavio", "W")
        , ("Rob", "T")
        ]
      shifts = "MTW" -- Each char is a seperate shift
  found <- generateSchedule days people shifts av
  print found
