{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( day1
  , day2
  , day3
  , day4
  , day5
  ) where

import Data.List (find)
import qualified Data.Text as Text
import Data.Text.Read (decimal, signed)
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

day3 :: IO ()
day3 = do
  lines <- getLines
  day3Solution lines

day3Solution :: [Text.Text] -> IO ()
day3Solution lines =
  let makeSequence =
        mapM
          (\direction ->
             case decimal $ Text.tail direction of
               Left x -> Left x
               Right (i, _) -> Right (Text.head direction, i)) .
        Text.splitOn (Text.pack ",")
   in case mapM makeSequence lines of
        Left _ -> die "Couldn't parse the input"
        Right wires ->
          let [wire1, wire2] =
                map
                  (makeLines .
                   reverse .
                   foldl (\pts (direction, offset) -> relativePoint (head pts) direction offset : pts) [Point 0 0])
                  wires
           in print $
              minimum .
              map
                (\(l1, l2) ->
                   lineLength (Line (p1 l1) (crossPoint l1 l2)) + lineLength (Line (p1 l2) (crossPoint l1 l2)) +
                   (sum . map lineLength $ takeWhile (\l -> l1 /= l) wire1) +
                   (sum . map lineLength $ takeWhile (\l -> l2 /= l) wire2)) $
              filter (uncurry checkLinesCross) [(l1, l2) | l1 <- tail wire1, l2 <- tail wire2]

data Point =
  Point
    { x :: Int
    , y :: Int
    }
  deriving (Show, Eq)

data Line =
  Line
    { p1 :: Point
    , p2 :: Point
    }
  deriving (Show, Eq)

relativePoint :: Point -> Char -> Int -> Point
relativePoint (Point x y) 'L' offset = Point (x - offset) y
relativePoint (Point x y) 'R' offset = Point (x + offset) y
relativePoint (Point x y) 'U' offset = Point x (y + offset)
relativePoint (Point x y) 'D' offset = Point x (y - offset)

makeLines :: [Point] -> [Line]
makeLines [p1, p2] = [Line p1 p2]
makeLines (p1:p2:pts) = Line p1 p2 : makeLines (p2 : pts)

checkLinesCross :: Line -> Line -> Bool
checkLinesCross (Line (Point a1 b1) (Point a2 b2)) (Line (Point x1 y1) (Point x2 y2)) =
  (between a1 x1 a2 || between a1 x2 a2 || between x1 a1 x2 || between x1 a2 x2) &&
  (between b1 y1 b2 || between b1 y2 b2 || between y1 b1 y2 || between y1 b2 y2)

between :: Int -> Int -> Int -> Bool
between x c y = (x <= c && c <= y) || (y <= c && c <= x)

manhattanDistance :: Line -> Line -> Int
manhattanDistance (Line (Point a1 b1) (Point a2 b2)) (Line (Point x1 y1) (Point x2 y2))
  | a1 == a2 && y1 == y2 = abs a1 + abs y1
  | b1 == b2 && x1 == x2 = abs b1 + abs x1

crossPoint :: Line -> Line -> Point
crossPoint (Line (Point a1 b1) (Point a2 b2)) (Line (Point x1 y1) (Point x2 y2))
  | a1 == a2 && y1 == y2 = Point a1 y1
  | b1 == b2 && x1 == x2 = Point x1 b1

lineLength :: Line -> Int
lineLength (Line (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) + abs (y1 - y2)

day4 :: IO ()
day4 = do
  [line] <- getLines
  day4Solution line

day4Solution :: Text.Text -> IO ()
day4Solution line =
  case mapM decimal $ Text.splitOn (Text.pack "-") line of
    Left _ -> die "Couldn't parse the input"
    Right [(low, _), (high, _)] ->
      print $
      length $ filter (== True) [elem 2 (islands (show i) '@' []) && checkForIncreasing (show i) | i <- [low .. high]]

checkForDouble :: String -> Bool
checkForDouble [_] = False
checkForDouble (a:b:rest)
  | a == b = True
  | otherwise = checkForDouble (b : rest)

islands :: String -> Char -> [Int] -> [Int]
islands "" _ acc = acc
islands (a:rest) _ [] = islands rest a [1]
islands (a:rest) c (n:acc)
  | a == c = islands rest c ((n + 1) : acc)
  | otherwise = islands rest a (1 : n : acc)

checkForIncreasing :: String -> Bool
checkForIncreasing [_] = True
checkForIncreasing (a:b:rest)
  | a <= b = checkForIncreasing (b : rest)
  | otherwise = False

day5 :: IO ()
day5 = do
  [line] <- getLines
  day5Solution line

day5Solution :: Text.Text -> IO ()
day5Solution line =
  case mapM (signed decimal) $ Text.splitOn (Text.pack ",") line of
    Left _ -> die "Couldn't parse the input"
    Right seq -> processInstruction 0 $ map fst seq

processInstruction :: Int -> [Int] -> IO ()
processInstruction pc seq =
  case seq !! pc of
    99 -> print "end"
    104 -> do
      print $ seq !! (pc + 1)
      processInstruction (pc + 2) seq
    4 -> do
      print $ seq !! (seq !! (pc + 1))
      processInstruction (pc + 2) seq
    3 -> do
      x <- getLine
      let seq' = updateList seq (seq !! (pc + 1)) (read x)
      processInstruction (pc + 2) seq'
    other -> uncurry processInstruction $ processComplexInstruction pc seq (pad (show other) '0' 5)

processComplexInstruction :: Int -> [Int] -> String -> (Int, [Int])
processComplexInstruction pc seq (_a:b:c:de)
  | de == "05" || de == "06" =
    let nextPc =
          if instructionType' de (getValueUsingMode seq c (seq !! (pc + 1))) 0
            then getValueUsingMode seq b (seq !! (pc + 2))
            else pc + 3
     in (nextPc, seq)
  | de == "07" || de == "08" =
    let v =
          if instructionType' de (getValueUsingMode seq c (seq !! (pc + 1))) (getValueUsingMode seq b (seq !! (pc + 2)))
            then 1
            else 0
     in (pc + 4, updateList seq (seq !! (pc + 3)) v)
  | otherwise =
    let result =
          instructionType de (getValueUsingMode seq c (seq !! (pc + 1))) (getValueUsingMode seq b (seq !! (pc + 2)))
     in (pc + 4, updateList seq (seq !! (pc + 3)) result)

getValueUsingMode :: [Int] -> Char -> Int -> Int
getValueUsingMode _seq '1' input = input
getValueUsingMode seq '0' input = seq !! input

instructionType :: Num a => String -> (a -> a -> a)
instructionType "02" = (*)
instructionType "01" = (+)

instructionType' ::
     Eq a
  => Ord a =>
       String -> (a -> a -> Bool)
instructionType' "08" = (==)
instructionType' "07" = (<)
instructionType' "06" = (==)
instructionType' "05" = (/=)

pad :: String -> Char -> Int -> String
pad s c n = replicate (n - length s) c ++ s

getLines :: IO [Text.Text]
-- getLines = Text.lines . Text.pack <$> getContents
-- getLines = return ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
-- getLines = return ["1002,4,3,4,33"]
getLines =
  return
    [ "3,225,1,225,6,6,1100,1,238,225,104,0,1102,78,40,225,1102,52,43,224,1001,224,-2236,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1,191,61,224,1001,224,-131,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,86,74,225,1102,14,76,225,1101,73,83,224,101,-156,224,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,1102,43,82,225,2,196,13,224,101,-6162,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1001,161,51,224,101,-70,224,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,102,52,187,224,1001,224,-832,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,1102,19,79,225,101,65,92,224,1001,224,-147,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,16,90,225,1102,45,44,225,1102,92,79,225,1002,65,34,224,101,-476,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,226,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,344,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,359,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,374,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,389,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,404,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,419,1001,223,1,223,7,677,226,224,102,2,223,223,1005,224,434,101,1,223,223,1007,677,677,224,102,2,223,223,1005,224,449,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,464,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,479,101,1,223,223,107,226,677,224,102,2,223,223,1006,224,494,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,509,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,539,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,569,101,1,223,223,1107,677,677,224,102,2,223,223,1006,224,584,1001,223,1,223,1108,226,226,224,1002,223,2,223,1006,224,599,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,614,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1007,677,226,224,102,2,223,223,1006,224,644,101,1,223,223,8,677,677,224,1002,223,2,223,1006,224,659,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226"
    ]
