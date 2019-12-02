{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( day2
  ) where

import qualified Data.Text as T

import Control.Monad (forever)

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Maybe (fromMaybe)
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

day2 :: IO ()
day2 = do
  [line] <- getLines
  let seq = (map (read . T.unpack) $ T.splitOn (T.pack ",") (T.pack line)) :: [Int]
  let pairs = [(i, j) | i <- [0 .. 99], j <- [0 .. 99]]
  let (i, j) =
        fromMaybe (-1, -1) $ find (\(i, j) -> 19690720 == day2Solution 0 (updateList (updateList seq 1 i) 2 j)) pairs
  print $ 100 * i + j

day2Solution :: Int -> [Int] -> Int
day2Solution cp seq
  | seq !! cp == 99 = head seq
  | otherwise =
    let [code, raddr1, raddr2, waddr] = take 4 $ drop cp seq
     in let val = calc seq code raddr1 raddr2
         in let seq' = updateList seq waddr val
             in day2Solution (cp + 4) seq'

calc :: [Int] -> Int -> Int -> Int -> Int
calc seq 1 addr1 addr2 = seq !! addr1 + seq !! addr2
calc seq 2 addr1 addr2 = seq !! addr1 * seq !! addr2

updateList :: [a] -> Int -> a -> [a]
updateList l i v = take i l ++ [v] ++ drop (i + 1) l

getLines :: IO [String]
-- getLines = return ["2,4,4,5,99,0"]
getLines = lines <$> getContents