{-# LANGUAGE OverloadedStrings #-}

module Utility
  ( stringToDay,
    splitOn,
  )
where

import Data.Time.Calendar
  ( Day,
    fromGregorianValid,
  )
import Text.Read (readMaybe)

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
