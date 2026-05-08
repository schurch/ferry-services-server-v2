{-# LANGUAGE OverloadedStrings #-}

module Utility
  ( convertScottishLocalTimeToUTC,
    stringToDay,
    splitOn,
    trim,
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Time.Calendar
  ( Day,
    addDays,
    fromGregorian,
    fromGregorianValid,
    toGregorian,
  )
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    localTimeToUTC,
    minutesToTimeZone,
  )
import Text.Read (readMaybe)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

stringToDay :: String -> Maybe Day
stringToDay dateString = case splitOn '-' dateString of
  [year, month, day] -> do
    validYear <- readMaybe year
    validMonth <- readMaybe month
    validDay <- readMaybe day
    fromGregorianValid validYear validMonth validDay
  _ -> Nothing

convertScottishLocalTimeToUTC :: LocalTime -> UTCTime
convertScottishLocalTimeToUTC localTime =
  localTimeToUTC timezone localTime
  where
    timezone =
      minutesToTimeZone $
        if isBritishSummerTime localTime
          then 60
          else 0

isBritishSummerTime :: LocalTime -> Bool
isBritishSummerTime (LocalTime day timeOfDay)
  | day < startDay = False
  | day > endDay = False
  | day == startDay = timeOfDay >= TimeOfDay 2 0 0
  | day == endDay = timeOfDay < TimeOfDay 2 0 0
  | otherwise = True
  where
    (year, _, _) = toGregorian day
    startDay = lastSundayOfMonth year 3
    endDay = lastSundayOfMonth year 10

lastSundayOfMonth :: Integer -> Int -> Day
lastSundayOfMonth year month =
  case [day | offset <- [0 .. 6], let day = addDays (-offset) firstOfNextMonth, dayOfWeek day == 0] of
    day : _ -> day
    [] -> error "Could not determine last Sunday of month"
  where
    firstOfNextMonth =
      if month == 12
        then fromGregorian (year + 1) 1 1
        else fromGregorian year (month + 1) 1

dayOfWeek :: Day -> Integer
dayOfWeek day =
  let (_, _, weekday) = toWeekDate day
   in fromIntegral weekday `mod` 7
