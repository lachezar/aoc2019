module Lib
  ( day1
  ) where

import Control.Monad (forever)

import Control.Monad.IO.Class (liftIO)
import GHC.IO.Handle (isEOF)

day1 :: IO ()
day1 = do
  lines <- getLines
  day1Solution 0 lines

day1Solution :: Int -> [String] -> IO ()
day1Solution total [] = print total
day1Solution total (line:lines) = day1Solution (total + calcFuel mass) lines
  where
    mass = read line :: Int

calcFuel :: Int -> Int
calcFuel mass
  | fuel < 0 = 0
  | otherwise = fuel + calcFuel fuel
  where
    fuel = mass `div` 3 - 2

getLines :: IO [String]
-- getLines = return $ map show [12, 14, 1969, 100756]
getLines = lines <$> getContents