{-# LANGUAGE OverloadedStrings #-}

module Utility
  ( splitOn
  ) where

import           Text.Read                      ( readMaybe )

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter = foldr f [[]]
 where
  f c l@(x : xs) | c == delimiter = [] : l
                 | otherwise      = (c : x) : xs
