--module Meetup (Weekday(..), Schedule(..), meetupDay) where
--import Data.Time.Calendar

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
data Schedule = First | Second | Third | Fourth | Last | Teenth

monthKey :: Int -> Int
monthKey m = case m of
	1 -> 0
	2 -> 3
	3 -> 3
	4 -> 6
	5 -> 1
	6 -> 4
	7 -> 6
	8 -> 2
	9 -> 5
	10 -> 0
	11 -> 3
	12 -> 5

-- This only works in years under 2000 and 2099
knowday :: Int -> Int -> Int -> Int
knowday d m y = mod (d + (monthKey m) + last2 + (last2 `div` 4) + 6) 7 where
	last2 = (y `mod` 10) + (((y `div` 10) `mod` 10) * 10)

weekDayToNumber :: Weekday -> Int
weekDayToNumber day = case day of
	Sunday -> 0
	Monday -> 1
	Tuesday -> 2
	Wednesday -> 3
	Thursday -> 4
	Friday -> 5
	Saturday -> 6

--meetupDay :: Schedule -> Weekday -> Int -> Int -> Day
--from