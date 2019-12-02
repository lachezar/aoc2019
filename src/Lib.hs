{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( day2
  ) where

import qualified Data.Text as Text
import Data.Text.Read (decimal)

import Control.Monad (forever)

import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Data.List (find)
import Data.Maybe (fromMaybe)
import GHC.IO.Handle (isEOF)
import System.Exit (die)

day1 :: IO ()
day1 = do
  lines <- getLines
  day1Solution 0 lines

day1Solution :: Int -> [Text.Text] -> IO ()
day1Solution total [] = print total
day1Solution total (line:lines) =
  case decimal line of
    Left _ -> die "Couldn't parse the input"
    Right (mass, _) -> day1Solution (total + calcFuel mass) lines

calcFuel :: Int -> Int
calcFuel mass
  | fuel < 0 = 0
  | otherwise = fuel + calcFuel fuel
  where
    fuel = mass `div` 3 - 2

day2 :: IO ()
day2 = do
  [line] <- getLines
  case mapM decimal $ Text.splitOn (Text.pack ",") line of
    Left _ -> die "Couldn't parse the input"
    Right seq ->
      let seq' = map fst seq
       in let inputParams = find (\(i, j) -> 19690720 == day2Solution 0 (updateList (updateList seq' 1 i) 2 j)) pairs
           in case inputParams of
                Nothing -> print "Couldn't find the input params"
                Just (i, j) -> print $ 100 * i + j
  where
    pairs = [(i, j) | i <- [0 .. 99], j <- [0 .. 99]]

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

getLines :: IO [Text.Text]
-- getLines = return ["2,4,4,5,99,0"]
getLines = Text.lines . Text.pack <$> getContents