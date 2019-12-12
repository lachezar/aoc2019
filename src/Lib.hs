{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( day1
  , day2
  , day3
  , day4
  , day5
  , day6
  , day7
  , day8
  , day9
  , day10
  ) where

import Data.Foldable (minimumBy)
import Data.List (find, groupBy, maximumBy, nub, permutations, sort)
import Data.List.Index (indexed)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Read (decimal, signed)
import GHC.List (foldl')
import IntCode
  ( Input(..)
  , Output(..)
  , ProgramCounter(..)
  , RelativeBase(..)
  , Sequence(..)
  , State(..)
  , runInstructions
  )
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
    Right seq -> processInstruction 0 0 $ map fst seq

processInstruction :: Int -> Int -> [Int] -> IO ()
processInstruction rb pc seq =
  case seq !! pc of
    99 -> print "end"
    204 -> do
      print $ getValueUsingMode rb seq '2' (pc + 1)
      processInstruction rb (pc + 2) seq
    104 -> do
      print $ getValueUsingMode rb seq '1' (pc + 1)
      processInstruction rb (pc + 2) seq
    4 -> do
      print $ getValueUsingMode rb seq '0' (pc + 1)
      processInstruction rb (pc + 2) seq
    203 -> do
      x <- getLine
      let seq' = setValueUsingMode rb seq '2' (pc + 1) (read x)
      processInstruction rb (pc + 2) seq'
    3 -> do
      x <- getLine
      let seq' = setValueUsingMode rb seq '0' (pc + 1) (read x)
      processInstruction rb (pc + 2) seq'
    other -> do
      let (rb', pc', seq') = processComplexInstruction rb pc seq (pad (show other) '0' 5)
      processInstruction rb' pc' seq'

processComplexInstruction :: Int -> Int -> [Int] -> String -> (Int, Int, [Int])
processComplexInstruction rb pc seq (a:b:c:de)
  | de == "05" || de == "06" =
    let nextPc =
          if instructionType' de (getValueUsingMode rb seq c (pc + 1)) 0
            then getValueUsingMode rb seq b (pc + 2)
            else pc + 3
     in (rb, nextPc, seq)
  | de == "07" || de == "08" =
    let v =
          if instructionType' de (getValueUsingMode rb seq c (pc + 1)) (getValueUsingMode rb seq b (pc + 2))
            then 1
            else 0
     in (rb, pc + 4, setValueUsingMode rb seq a (pc + 3) v)
  | de == "09" =
    let rb' = getValueUsingMode rb seq c (pc + 1)
     in (rb + rb', pc + 2, seq)
  | otherwise =
    let result = instructionType de (getValueUsingMode rb seq c (pc + 1)) (getValueUsingMode rb seq b (pc + 2))
     in (rb, pc + 4, setValueUsingMode rb seq a (pc + 3) result)
processComplexInstruction _ _ _ _ = undefined

getValueUsingMode :: Int -> [Int] -> Char -> Int -> Int
getValueUsingMode rb seq '2' index = seq !! (rb + seq !! index)
getValueUsingMode _rb seq '1' index = seq !! index
getValueUsingMode _rb seq '0' index = seq !! (seq !! index)
getValueUsingMode _ _ _ _ = undefined

setValueUsingMode :: Int -> [Int] -> Char -> Int -> Int -> [Int]
setValueUsingMode rb seq '2' index value = updateList seq (rb + seq !! index) value
setValueUsingMode _rb seq '1' index value = updateList seq index value
setValueUsingMode _rb seq '0' index value = updateList seq (seq !! index) value
setValueUsingMode _ _ _ _ _ = undefined

instructionType :: Num a => String -> (a -> a -> a)
instructionType "02" = (*)
instructionType "01" = (+)
instructionType _ = undefined

instructionType' ::
     Eq a
  => Ord a =>
       String -> (a -> a -> Bool)
instructionType' "08" = (==)
instructionType' "07" = (<)
instructionType' "06" = (==)
instructionType' "05" = (/=)
instructionType' _ = undefined

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

day7 :: IO ()
day7 = do
  [line] <- getLines
  day7Solution line

day7Solution :: Text.Text -> IO ()
day7Solution line =
  case mapM (signed decimal) $ Text.splitOn (Text.pack ",") line of
    Left _ -> die "Couldn't parse the input"
    Right seq -> do
      results <- mapM (combineProcessInstruction' (replicate 5 (0, map fst seq)) [0]) settings
      print $ maximum results
  where
    settings = permutations [5 .. 9]

combineProcessInstruction' :: [(Int, [Int])] -> [Int] -> [Int] -> IO Int
combineProcessInstruction' ((pc, seq):rest) input [] =
  if pc == (-999)
    then return $ head input
    else do
      (pc', seq', output) <- processInstruction' pc seq input []
      combineProcessInstruction' (rest ++ [(pc', seq')]) output []
combineProcessInstruction' ((pc, seq):rests) input (setting:rest) = do
  (pc', seq', output) <- processInstruction' pc seq (setting : input) []
  combineProcessInstruction' (rests ++ [(pc', seq')]) output rest

processInstruction' :: Int -> [Int] -> [Int] -> [Int] -> IO (Int, [Int], [Int])
processInstruction' pc seq input output =
  case seq !! pc of
    99 -> do
      print output
      return (-999, seq, output)
    104 -> do
      let newSignal = seq !! (pc + 1)
      processInstruction' (pc + 2) seq input (output ++ [newSignal])
    4 -> do
      let newSignal = seq !! (seq !! (pc + 1))
      processInstruction' (pc + 2) seq input (output ++ [newSignal])
    3 ->
      if null input
        then return (pc, seq, output)
        else do
          let seq' = updateList seq (seq !! (pc + 1)) $ head input
          processInstruction' (pc + 2) seq' (tail input) output
    other -> do
      let (rb, pc', seq') = processComplexInstruction 0 pc seq (pad (show other) '0' 5)
      processInstruction' pc' seq' input output

day8 :: IO ()
day8 = do
  [line] <- getLines
  day8Solution $ Text.unpack line

day8Solution :: String -> IO ()
day8Solution line = do
  print $ ones * twos
  print canvas
  mapM_ print $ segment 25 canvas
  where
    segments = segment (25 * 6) line
    zeros = map (\(i, s) -> (i, length $ filter (== '0') s)) $ indexed segments
    (mostZerosSegment, _) = minimumBy (\(ia, a) (ib, b) -> compare a b) zeros
    ones = length $ filter (== '1') $ segments !! mostZerosSegment
    twos = length $ filter (== '2') $ segments !! mostZerosSegment
    canvas = blend (head segments) (tail segments)

segment :: Int -> String -> [String]
segment _ [] = []
segment n l = s : segment n r
  where
    (s, r) = splitAt n l

blend :: String -> [String] -> String
blend canvas [] = canvas
blend canvas (top:layers) = blend canvas' layers
  where
    canvas' = zipWith blendPixels canvas top

blendPixels :: Char -> Char -> Char
blendPixels '2' lp = lp
blendPixels cp _ = cp

day9 :: IO ()
day9 = do
  [line] <- getLines
  day9Solution line

day9Solution :: Text.Text -> IO ()
day9Solution line =
  case mapM (signed decimal) $ Text.splitOn (Text.pack ",") line of
    Left _ -> die "Couldn't parse the input"
    Right seq -> processInstruction 0 0 $ map fst seq ++ replicate 4000 0

day10 :: IO ()
day10 = do
  lines <- getLines
  day10Solution lines

day10Solution :: [Text.Text] -> IO ()
day10Solution lines = do
  print (maxVisibleAsteroids, maxVisiblePoint)
  print twoHundredAsteroid
  print $ x twoHundredAsteroid * 100 + y twoHundredAsteroid
  where
    asteroids =
      concatMap
        (\(i, s) ->
           foldl'
             (\acc (j, c) ->
                if c == '#'
                  then Point j i : acc
                  else acc)
             [] $
           indexed s) $
      indexed $ map Text.unpack lines
    lf = map location asteroids
    visibleAsteroids = map (\a -> (length $ nub $ lf <*> [a], a)) asteroids
    (maxVisibleAsteroids, maxVisiblePoint) = maximumBy (comparing fst) visibleAsteroids
    relativeLocations =
      sort $
      filter (\(AsteroidLocation rl _) -> rl /= Self) $
      map (\a -> AsteroidLocation (location maxVisiblePoint a) (Line maxVisiblePoint a)) asteroids
    groupedLocations = groupBy (\(AsteroidLocation rl1 _) (AsteroidLocation rl2 _) -> rl1 == rl2) relativeLocations
    AsteroidLocation _ (Line _ twoHundredAsteroid) = destroyAsteroids 199 groupedLocations

data RelativeLocation
  = TopLeft Rational
  | TopRight Rational
  | BottomLeft Rational
  | BottomRight Rational
  | Top
  | Bottom
  | Left'
  | Right'
  | Self
  deriving (Show, Eq)

instance Ord RelativeLocation where
  compare Top Top = EQ
  compare Top _ = GT
  compare (TopRight _) Top = LT
  compare (TopRight a) (TopRight b) = compare a b
  compare (TopRight _) _ = GT
  compare Right' Top = LT
  compare Right' (TopRight _) = LT
  compare Right' Right' = EQ
  compare Right' _ = GT
  compare (BottomRight _) Top = LT
  compare (BottomRight _) (TopRight _) = LT
  compare (BottomRight _) Right' = LT
  compare (BottomRight a) (BottomRight b) = compare a b
  compare (BottomRight _) _ = GT
  compare Bottom Top = LT
  compare Bottom (TopRight _) = LT
  compare Bottom Right' = LT
  compare Bottom (BottomRight _) = LT
  compare Bottom Bottom = EQ
  compare Bottom _ = GT
  compare (BottomLeft _) Top = LT
  compare (BottomLeft _) (TopRight _) = LT
  compare (BottomLeft _) Right' = LT
  compare (BottomLeft _) (BottomRight _) = LT
  compare (BottomLeft _) Bottom = LT
  compare (BottomLeft a) (BottomLeft b) = compare a b
  compare (BottomLeft _) _ = GT
  compare Left' (TopLeft _) = GT
  compare Left' Left' = EQ
  compare Left' _ = LT
  compare (TopLeft a) (TopLeft b) = compare a b
  compare (TopLeft _) _ = LT
  compare Self Self = EQ
  compare Self _ = GT

location :: Point -> Point -> RelativeLocation
location (Point x y) (Point cx cy)
  | cx < x && cy < y = TopLeft $ dx % dy
  | cx < x && cy > y = BottomLeft $ dx % dy
  | cx > x && cy < y = TopRight $ dx % dy
  | cx > x && cy > y = BottomRight $ dx % dy
  | cx == x && cy < y = Top
  | cx == x && cy > y = Bottom
  | cx < x && cy == y = Left'
  | cx > x && cy == y = Right'
  | cx == x && cy == y = Self
  where
    dx = toInteger $ abs (cx - x)
    dy = toInteger $ abs (cy - y)

instance Ord Line where
  compare l1 l2 = lineLength l1 `compare` lineLength l2

data AsteroidLocation =
  AsteroidLocation RelativeLocation Line
  deriving (Show, Eq)

instance Ord AsteroidLocation where
  compare (AsteroidLocation rl1 l1) (AsteroidLocation rl2 l2)
    | rlCompare == EQ = lCompare
    | otherwise = rlCompare
    where
      rlCompare = rl2 `compare` rl1
      lCompare = l1 `compare` l2

destroyAsteroids :: Int -> [[AsteroidLocation]] -> AsteroidLocation
destroyAsteroids n [] = undefined
destroyAsteroids n ([]:rest) = destroyAsteroids n rest
destroyAsteroids 0 (h:_rest) = head h
destroyAsteroids n (h:rest) = destroyAsteroids (n - 1) (rest ++ [tail h])

day11 :: IO ()
day11 = do
  [line] <- getLines
  day11Solution line

day11Solution :: Text.Text -> IO ()
day11Solution line =
  case mapM (signed decimal) $ Text.splitOn (Text.pack ",") line of
    Left _ -> die "Couldn't parse the input"
    Right seq ->
      runInstructions
        (State
           (ProgramCounter 0)
           (RelativeBase 0)
           (Sequence $ map fst seq ++ replicate 10000 0)
           (Input (repeat 1))
           (Output Nothing))

getLines :: IO [Text.Text]
getLines = Text.lines <$> TextIO.readFile "input.txt"
-- getLines = Text.lines . Text.pack <$> getContents
-- getLines = return ["104,1125899906842624,99"]
-- getLines = return ["1002,4,3,4,33"]
-- getLines = return ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]