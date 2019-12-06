{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( day1
  , day2
  , day3
  , day4
  , day5
  , day6
  ) where

import Data.List (find, nub)
import Data.Map (Map)
import qualified Data.Map as Map
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

day6 :: IO ()
day6 = do
  lines <- getLines
  day6Solution lines

day6Solution :: [Text.Text] -> IO ()
day6Solution lines = print $ minSeq - 2
  where
    seq = map (Text.splitOn (Text.pack ")")) lines
    vertices = nub $ concat seq
    graph = foldl (\acc [v1, v2] -> Map.insert v1 (v2 : Map.findWithDefault [] v1 acc) acc) Map.empty seq
    seqToYou = map (traverseCountSteps (-1) graph "YOU") vertices
    seqToSan = map (traverseCountSteps (-1) graph "SAN") vertices
    minSeq = minimum $ map (uncurry (+)) $ filter (\(i, j) -> i /= 0 && j /= 0) $ zip seqToYou seqToSan

traverseCountSteps :: Int -> Map Text.Text [Text.Text] -> Text.Text -> Text.Text -> Int
traverseCountSteps c graph ev v
  | v == ev = n
  | otherwise = sum (map (traverseCountSteps n graph ev) seq)
  where
    seq = Map.findWithDefault [] v graph
    n = c + 1

getLines :: IO [Text.Text]
getLines = Text.lines . Text.pack <$> getContents
-- getLines = return ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
-- getLines = return ["1002,4,3,4,33"]
-- getLines = return ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]