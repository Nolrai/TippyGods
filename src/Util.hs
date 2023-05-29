{-# LANGUAGE NoImplicitPrelude #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( splitEvery,
  )
where

import Data.List (splitAt)
import RIO

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = front : splitEvery n back
  where
    (front, back) = splitAt n xs
