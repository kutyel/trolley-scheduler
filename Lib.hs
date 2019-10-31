module Lib where

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
  Shift Person -- TODO: [Person] 😱
  deriving (Eq)

data Day =
  Day Char Int [Shift]
  deriving (Eq)

-- data DayOfWeek
--     = Monday
--     | Tuesday
--     | Wednesday
--     | Thursday
--     | Friday
--     | Saturday
--     | Sunday
--     deriving (Eq, Show, Read)

-- -- | \"Circular\", so for example @[Tuesday ..]@ gives an endless sequence.
-- -- Also: 'fromEnum' gives [1 .. 7] for [Monday .. Sunday], and 'toEnum' performs mod 7 to give a cycle of days.
-- instance Enum DayOfWeek where
--     toEnum i =
--         case mod i 7 of
--             0 -> Sunday
--             1 -> Monday
--             2 -> Tuesday
--             3 -> Wednesday
--             4 -> Thursday
--             5 -> Friday
--             _ -> Saturday
--     fromEnum Monday = 1
--     fromEnum Tuesday = 2
--     fromEnum Wednesday = 3
--     fromEnum Thursday = 4
--     fromEnum Friday = 5
--     fromEnum Saturday = 6
--     fromEnum Sunday = 7
--     enumFromTo wd1 wd2
--         | wd1 == wd2 = [wd1]
--     enumFromTo wd1 wd2 = wd1 : enumFromTo (succ wd1) wd2
--     enumFromThenTo wd1 wd2 wd3
--         | wd2 == wd3 = [wd1, wd2]
--     enumFromThenTo wd1 wd2 wd3 = wd1 : enumFromThenTo wd2 (toEnum $ (2 * fromEnum wd2) - (fromEnum wd1)) wd3

-- m2s :: [Char]
-- m2s = "MTWHFSU" ++ m2s

-- charToDOW :: Char -> Maybe DayOfWeek
-- charToDOW c =
--   lookup x (zip m2s [Monday .. Sunday])

instance Show Day where
  show (Day c n xs) = "Day " ++ [c] ++ " " ++ show n ++ ": \n" ++ show xs

instance Show Shift where
  show (Shift p) = "Shift Volunteer: " ++ p ++ "\n"

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
  pure $ ((Shift . snd) <$> zipWithPredicate (,) (getPeopleFromShifts av) shifts shuffledPeople)

randomDay :: Int -> [Person] -> ShiftList -> Availability -> IO Day
randomDay day people shifts av =
  -- TODO figure out how to set the real date here
  fmap (Day 'M' day) (randomFillShifts people shifts av)

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
getStaffList ((Day _ _ shifts):_) = map person shifts
  where
    person (Shift p) = p

hasOff :: Person -> Day -> Bool
hasOff p (Day _ _ shifts) = all isOff shifts
  where
    isOff (Shift q) = p /= q

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
