{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}

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
  , day11
  , day12
  , day13
  , day14
  , day15
  , day16
  , day17
  ) where

import Control.Concurrent (threadDelay)
import Data.Char (chr, ord)
import Data.Foldable (minimumBy)
import Data.List (find, findIndices, groupBy, maximumBy, nub, permutations, sort, sortBy)
import Data.List.Index (indexed)
import Data.Map (Map, (!), toList)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Read (decimal, signed)
import Debug.Trace (traceShowId)
import GHC.IO.Handle (hFlush)
import GHC.List (foldl')
import IntCode
  ( Input(..)
  , Output(..)
  , ProgramCounter(..)
  , RelativeBase(..)
  , Sequence(..)
  , State(..)
  , runInstruction
  , runInstructions
  )
import System.Exit (die)
import System.IO (stdout)

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
calc _ _ _ _ = undefined

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
relativePoint _ _ _ = undefined

makeLines :: [Point] -> [Line]
makeLines [p1, p2] = [Line p1 p2]
makeLines (p1:p2:pts) = Line p1 p2 : makeLines (p2 : pts)
makeLines _ = undefined

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
manhattanDistance _ _ = undefined

crossPoint :: Line -> Line -> Point
crossPoint (Line (Point a1 b1) (Point a2 b2)) (Line (Point x1 y1) (Point x2 y2))
  | a1 == a2 && y1 == y2 = Point a1 y1
  | b1 == b2 && x1 == x2 = Point x1 b1
crossPoint _ _ = undefined

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
    Right _ -> undefined

checkForDouble :: String -> Bool
checkForDouble [_] = False
checkForDouble (a:b:rest)
  | a == b = True
  | otherwise = checkForDouble (b : rest)
checkForDouble _ = undefined

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
checkForIncreasing _ = undefined

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

pad :: [a] -> a -> Int -> [a]
pad es e n = replicate (n - length es) e ++ es

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
combineProcessInstruction' _ _ _ = undefined

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
    Right seq ->
      runInstructions
        (State
           (ProgramCounter 0)
           (RelativeBase 0)
           (Sequence (Map.fromList $ indexed $ map fst seq ++ replicate 4000 0))
           (Input [2])
           (Output Nothing))

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
location _ _ = undefined

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
      runRobotInstructions
        (State
           (ProgramCounter 0)
           (RelativeBase 0)
           (Sequence $ Map.fromList $ indexed $ map fst seq)
           (Input [])
           (Output Nothing))
        (RobotState (0, 0) DUp DirectionInstruction)
        (Map.insert (0, 0) 1 Map.empty)

type Field = Map (Int, Int) Int

data Direction
  = DLeft
  | DRight
  | DUp
  | DDown
  deriving (Show)

data RobotInstruction
  = ColorInstruction
  | DirectionInstruction
  deriving (Show)

data RobotState =
  RobotState
    { robotLoc :: (Int, Int)
    , robotDir :: Direction
    , robotInst :: RobotInstruction
    }
  deriving (Show)

moveRobot :: Int -> RobotState -> RobotState
moveRobot 0 (RobotState (x, y) DLeft ColorInstruction) = RobotState (x, y + 1) DDown DirectionInstruction
moveRobot 0 (RobotState (x, y) DDown ColorInstruction) = RobotState (x + 1, y) DRight DirectionInstruction
moveRobot 0 (RobotState (x, y) DRight ColorInstruction) = RobotState (x, y - 1) DUp DirectionInstruction
moveRobot 0 (RobotState (x, y) DUp ColorInstruction) = RobotState (x - 1, y) DLeft DirectionInstruction
moveRobot 1 (RobotState (x, y) DLeft ColorInstruction) = RobotState (x, y - 1) DUp DirectionInstruction
moveRobot 1 (RobotState (x, y) DDown ColorInstruction) = RobotState (x - 1, y) DLeft DirectionInstruction
moveRobot 1 (RobotState (x, y) DRight ColorInstruction) = RobotState (x, y + 1) DDown DirectionInstruction
moveRobot 1 (RobotState (x, y) DUp ColorInstruction) = RobotState (x + 1, y) DRight DirectionInstruction
moveRobot _ _ = undefined

runRobot :: RobotState -> Field -> Maybe Int -> (RobotState, Field)
runRobot robotState field (Just x) =
  let lastInst = robotInst robotState
   in case lastInst of
        ColorInstruction ->
          let robotState' = moveRobot x robotState
           in (robotState', field)
        DirectionInstruction ->
          let robotState' = robotState {robotInst = ColorInstruction}
           in let location = robotLoc robotState
               in let currentColor = Map.findWithDefault 0 location field
                   in if currentColor == x
                        then (robotState', field)
                        else let field' = Map.insert location x field
                              in (robotState', field')
runRobot robotState field Nothing = (robotState, field)

printField :: Field -> IO ()
printField field =
  let canvas = replicate 20 $ replicate 50 '.'
   in mapM_ print $
      foldl' (\acc ((i, j), c) -> updateList acc (10 + j) (updateList (acc !! (10 + j)) (0 + i) $ printColor c)) canvas $
      Map.toList field

printColor :: Int -> Char
printColor 0 = '.'
printColor 1 = '#'
printColor _ = undefined

runRobotInstructions :: State -> RobotState -> Field -> IO ()
runRobotInstructions End _ _ = print "end"
runRobotInstructions state@(State pc rb seq input output) robotState field = do
  let compInput = Map.findWithDefault 0 (robotLoc robotState) field
  let state' = state {stateInput = Input [compInput]}
  let instruction = unSequence seq ! unProgramCounter pc
  case runInstruction state' (pad (show instruction) '0' 5) of
    End -> printField field
    state''@(State pc _rb _seq _input output') -> do
      let (robotState', field') = runRobot robotState field (unOutput output')
      -- consume the program output and reset it
      let state''' = state'' {stateOutput = Output Nothing}
      runRobotInstructions state''' robotState' field'

data Triple =
  Triple
    { tripleX :: Int
    , tripleY :: Int
    , tripleZ :: Int
    }
  deriving (Eq)

instance Show Triple where
  show (Triple a b c) = "<x=" <> show a <> ", y=" <> show b <> ", z=" <> show c <> ">"

listToTriple :: [Int] -> Triple
listToTriple [a, b, c] = Triple a b c
listToTriple _ = undefined

velocityChange :: [Triple] -> [Triple] -> [Triple]
velocityChange pos vel = vel'
  where
    xs = map tripleX pos
    ys = map tripleY pos
    zs = map tripleZ pos
    vxs = map tripleX vel
    vys = map tripleY vel
    vzs = map tripleZ vel
    vel' =
      zipWith3
        Triple
        (zipWith3
           (\a b c -> a - b + c)
           (map (length . (\e -> filter (> e) xs)) xs)
           (map (length . (\e -> filter (< e) xs)) xs)
           vxs)
        (zipWith3
           (\a b c -> a - b + c)
           (map (length . (\e -> filter (> e) ys)) ys)
           (map (length . (\e -> filter (< e) ys)) ys)
           vys)
        (zipWith3
           (\a b c -> a - b + c)
           (map (length . (\e -> filter (> e) zs)) zs)
           (map (length . (\e -> filter (< e) zs)) zs)
           vzs)

gravityChange :: [Triple] -> [Triple] -> [Triple]
gravityChange pos vel = pos'
  where
    xs = map tripleX pos
    ys = map tripleY pos
    zs = map tripleZ pos
    vxs = map tripleX vel
    vys = map tripleY vel
    vzs = map tripleZ vel
    pos' = zipWith3 Triple (zipWith (+) xs vxs) (zipWith (+) ys vys) (zipWith (+) zs vzs)

moonPositions :: [Triple] -> [Triple] -> Int -> ([Triple], [Triple])
moonPositions pos vel 0 = (pos, vel)
moonPositions pos vel n = moonPositions pos' vel' (n - 1)
  where
    vel' = velocityChange pos vel
    pos' = gravityChange pos vel'

moonPositionsList :: [Triple] -> [Triple] -> [[Triple]]
moonPositionsList pos vel = pos' : moonPositionsList pos' vel'
  where
    vel' = velocityChange pos vel
    pos' = gravityChange pos vel'

energy :: Triple -> Int
energy (Triple x y z) = abs x + abs y + abs z

findTimeSteps :: [Triple] -> [Triple] -> Int
findTimeSteps pos vel =
  (dx * dy * dz) `div` foldl' (*) 1 (take 2 $ sortBy (flip compare) [gcd dx dy, gcd dx dz, gcd dy dz])
  where
    seq = take 1000000 $ moonPositionsList pos vel
    xs = map tripleX pos
    ys = map tripleY pos
    zs = map tripleZ pos
    ix = findIndices (\m -> map tripleX m == xs) seq
    iy = findIndices (\m -> map tripleY m == ys) seq
    iz = findIndices (\m -> map tripleZ m == zs) seq
    dx = ix !! 4 - ix !! 2
    dy = iy !! 4 - iy !! 2
    dz = iz !! 4 - iz !! 2

day12 :: IO ()
day12 = do
  lines <- getLines
  day12Solution lines

day12Solution :: [Text.Text] -> IO ()
day12Solution lines =
  case mapM
         (mapM (signed decimal . Text.filter (\c -> c `elem` ('-' : ['0' .. '9']))) . Text.splitOn (Text.pack ","))
         lines of
    Left _ -> die "Couldn't parse the input"
    Right seq ->
      let pos = map (listToTriple . map fst) seq
       in let vel = replicate 4 (Triple 0 0 0)
           in print $ findTimeSteps pos vel

drawTile :: Int -> Char
drawTile 0 = '.'
drawTile 1 = '@'
drawTile 2 = '#'
drawTile 3 = '='
drawTile 4 = 'o'
drawTile _ = undefined

drawArcade :: Field -> IO ()
drawArcade field =
  mapM_ print $ foldl' (\c ((x, y), t) -> updateList c y (updateList (c !! y) x $ drawTile t)) canvas tiles
  where
    canvas = replicate 24 (replicate 40 '.')
    tiles = toList field

findTileOnX :: Field -> Int -> Int
findTileOnX field tile = x
  where
    tiles = toList field
    Just ((x, _), _) = find (\((x, y), t) -> t == tile) tiles

runArcadeInstructions :: State -> Field -> [Int] -> IO ()
runArcadeInstructions End field _ = return ()
runArcadeInstructions state@(State pc rb seq _input output) field aggregatedOutput = do
  let instruction = unSequence seq ! unProgramCounter pc
  state' <-
    if instruction `mod` 10 == 3
      then do
        putStr "\ESC[2J"
        -- clear the terminal
        drawArcade field
        hFlush stdout
        threadDelay $ 1000000 `div` 30
        let ballX = findTileOnX field 4
        let paddleX = findTileOnX field 3
        let input
              | ballX < paddleX = -1
              | ballX == paddleX = 0
              | otherwise = 1
        return $ State pc rb seq (Input [input]) output
      else return state
  case runInstruction state' (pad (show instruction) '0' 5) of
    End -> runArcadeInstructions End field []
    state''@(State _pc _rb _seq _input output')
  -- consume the program output and reset it
     ->
      let state''' = state'' {stateOutput = Output Nothing}
       in case aggregatedOutput ++ maybeToList (unOutput output') of
            [-1, 0, score] -> do
              print $ "score: " <> show score
              runArcadeInstructions state''' field []
            [x, y, t] -> runArcadeInstructions state''' (Map.insert (x, y) t field) []
            outputBuffer -> runArcadeInstructions state''' field outputBuffer

day13 :: IO ()
day13 = do
  [line] <- getLines
  day13Solution line

day13Solution :: Text.Text -> IO ()
day13Solution line =
  case mapM (signed decimal) $ Text.splitOn (Text.pack ",") line of
    Left _ -> die "Couldn't parse the input"
    Right seq ->
      runArcadeInstructions
        (State
           (ProgramCounter 0)
           (RelativeBase 0)
           (Sequence $ Map.insert 0 2 $ Map.fromList $ indexed $ map fst seq)
           (Input [])
           (Output Nothing))
        Map.empty
        []

data Ingredient =
  Ingredient Int String
  deriving (Show)

type IngredientsStash = Map String Int

type IngredientConversionRules = Map String (Int, [Ingredient])

purchaseIngredient ::
     IngredientConversionRules -> IngredientsStash -> [Ingredient] -> Int -> (IngredientsStash, [Ingredient], Int)
purchaseIngredient icr leftovers [] ore = (leftovers, [], ore)
purchaseIngredient icr leftovers (ing@(Ingredient amount "ORE"):stack) ore =
  purchaseIngredient icr leftovers stack (ore + amount)
purchaseIngredient icr leftovers (ingredient@(Ingredient amount ingType):stack) ore =
  let (leftovers', ingredient'@(Ingredient amount' _)) = getFromStash leftovers ingredient
   in if amount' == 0
        then purchaseIngredient icr leftovers' stack ore
        else let Just (exchangeAmount, exchangeIngredients) = Map.lookup ingType icr
              in let m = (amount' - 1) `div` exchangeAmount + 1
                  in let (purchaseMultiplier, sendToStash)
                           | exchangeAmount > amount' = (1, exchangeAmount - amount')
                           | exchangeAmount < amount' = (m, m * exchangeAmount - amount')
                           | otherwise = (1, 0)
                      in let leftovers'' = addToStash leftovers' (Ingredient sendToStash ingType)
                          in let stack' =
                                   map
                                     (\(Ingredient a it) -> Ingredient (purchaseMultiplier * a) it)
                                     exchangeIngredients ++
                                   stack
                              in purchaseIngredient icr leftovers'' stack' ore

addToStash :: IngredientsStash -> Ingredient -> IngredientsStash
addToStash leftovers (Ingredient amount ingredientType) = Map.insertWith (+) ingredientType amount leftovers

getFromStash :: IngredientsStash -> Ingredient -> (IngredientsStash, Ingredient)
getFromStash stash input@(Ingredient amount ingredientType) =
  if availableAmount >= amount
    then (Map.insert ingredientType (availableAmount - amount) stash, Ingredient 0 ingredientType)
    else (Map.insert ingredientType 0 stash, Ingredient (amount - availableAmount) ingredientType)
  where
    availableAmount = Map.findWithDefault 0 ingredientType stash

parseIngredient :: String -> Ingredient
parseIngredient term = Ingredient (read amount) ingredientType
  where
    [amount, ingredientType] = words term

day14 :: IO ()
day14 = do
  lines <- getLines
  day14Solution lines

day14Solution :: [Text.Text] -> IO ()
day14Solution lines = do
  let fuelForOneTrillionOre =
        takeWhile
          (\a ->
             let (_, _, oreAmount) = purchaseIngredient ingredientConversionRules leftovers [Ingredient a "FUEL"] 0
              in oreAmount < 1000000000000)
          [(6216590 - 100) .. (6216590 + 100)]
  print fuelForOneTrillionOre
  let res@(_, _, oreAmount) = purchaseIngredient ingredientConversionRules leftovers [Ingredient 6216590 "FUEL"] 0
  print res
  where
    parsedLines =
      map
        ((\[input, output] ->
            ( map (parseIngredient . Text.unpack) (Text.splitOn (Text.pack ", ") input)
            , (parseIngredient . Text.unpack) output)) .
         Text.splitOn (Text.pack " => "))
        lines
    ingredientConversionRules =
      foldl'
        (\icr (input, Ingredient amount ingredientType) -> Map.insert ingredientType (amount, input) icr)
        Map.empty
        parsedLines
    leftovers = Map.empty

drawDroidTile :: Int -> Char
drawDroidTile 0 = '#'
drawDroidTile 1 = '.'
drawDroidTile 2 = 'X'
drawDroidTile 9 = 'D'
drawDroidTile _ = undefined

drawDroidField :: (Int, Int) -> Field -> IO ()
drawDroidField (dx, dy) field =
  mapM_ print $
  foldl' (\c ((x, y), t) -> updateList c (y + 30) (updateList (c !! (y + 30)) (x + 80) $ drawDroidTile t)) canvas tiles
  where
    canvas = replicate 60 (replicate 160 ' ')
    tiles = toList field ++ [((dx, dy), 9)]

moveDroid :: (Int, Int, Int) -> (Int, Int)
moveDroid (x, y, 1) = (x, y - 1)
moveDroid (x, y, 2) = (x, y + 1)
moveDroid (x, y, 3) = (x - 1, y)
moveDroid (x, y, 4) = (x + 1, y)
moveDroid _ = undefined

changeDirection :: Int -> Int
changeDirection 1 = 2
changeDirection 2 = 1
changeDirection 3 = 4
changeDirection 4 = 3
changeDirection _ = undefined

runDroid :: State -> Field -> [Int] -> (Int, Int, Int) -> Int -> (State, Field, Int)
runDroid _ field [] _ sto = (End, field, sto)
runDroid End field _ _ sto = (End, field, sto)
runDroid state@(State pc rb seq _input output) field steps location@(x, y, d) stepsToOxygen =
  let instruction = unSequence seq ! unProgramCounter pc
   in if instruction `mod` 10 == 3
        then let state' = State pc rb seq (Input [d]) output
              in let state'' = runInstruction state' (pad (show instruction) '0' 5)
                  in runDroid state'' field steps location stepsToOxygen
        else if instruction `mod` 10 == 4
               then case runInstruction state (pad (show instruction) '0' 5) of
                      End -> (End, field, stepsToOxygen)
                      state'@(State _pc _rb _seq input output') ->
                        let (x', y') = moveDroid location
                         in case unOutput output' of
                              Just 0 ->
                                let field' = Map.insert (x', y') 0 field
                                 in let notVisitedNeighbours =
                                          filter (\l -> not (Map.member (moveDroid l) field')) $ map (x, y, ) [1 .. 4]
                                     in if null notVisitedNeighbours
                                          then runDroid
                                                 state'
                                                 field'
                                                 (tail steps)
                                                 (x, y, changeDirection $ head steps)
                                                 stepsToOxygen
                                          else runDroid state' field' steps (head notVisitedNeighbours) stepsToOxygen
                              Just n ->
                                let stepsToOxygen'
                                      | n == 2 = length steps
                                      | otherwise = stepsToOxygen
                                 in let isCurrentLocationVisited = Map.member (x', y') field
                                     in let steps'
                                              | isCurrentLocationVisited = steps
                                              | otherwise = d : steps
                                         in let field' = Map.insert (x', y') n field
                                             in let notVisitedNeighbours =
                                                      filter (\l -> not (Map.member (moveDroid l) field')) $
                                                      map (x', y', ) [1 .. 4]
                                                 in if null notVisitedNeighbours
                                                      then runDroid
                                                             state'
                                                             field'
                                                             (tail steps)
                                                             (x', y', changeDirection $ head steps)
                                                             stepsToOxygen'
                                                      else runDroid
                                                             state'
                                                             field'
                                                             steps'
                                                             (head notVisitedNeighbours)
                                                             stepsToOxygen'
                              _ -> undefined
               else let state' = runInstruction state (pad (show instruction) '0' 5)
                     in runDroid state' field steps location stepsToOxygen

oxygenFlowNeighbours :: Field -> [(Int, Int)] -> (Field, [(Int, Int)])
oxygenFlowNeighbours field locations = (field', neighbours)
  where
    neighbours =
      concatMap
        (\(x, y) -> filter (\(x', y') -> Map.member (x', y') field) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)])
        locations
    field' = foldl' (flip Map.delete) field neighbours

oxygenFlow :: Field -> [(Int, Int)] -> Int
oxygenFlow field [] = 0
oxygenFlow field locations = uncurry oxygenFlow (oxygenFlowNeighbours (traceShowId field) locations) + 1

day15 :: IO ()
day15 = do
  [line] <- getLines
  day15Solution line

day15Solution :: Text.Text -> IO ()
day15Solution line =
  case mapM (signed decimal) $ Text.splitOn (Text.pack ",") line of
    Left _ -> die "Couldn't parse the input"
    Right seq -> do
      let state =
            State
              (ProgramCounter 0)
              (RelativeBase 0)
              (Sequence $ Map.fromList $ indexed $ map fst seq ++ replicate 0 1000)
              (Input [])
              (Output Nothing)
      let field = Map.empty
      let (_, field', stepsToOxygen) = runDroid state field [4] (0, 0, 4) 0
      drawDroidField (0, 0) field'
      print field'
      print stepsToOxygen
      case find (\((x, y), t) -> t == 2) $ toList field' of
        Just ((x, y), 2) -> print $ oxygenFlow (Map.filter (> 0) field') [(x, y)] - 1
        _ -> print "Can't flow the maze with oxygen"

generateFFTPattern :: Int -> [Int]
generateFFTPattern n = tail $ cycle $ concatMap (replicate n) [0, 1, 0, -1]

calcPhaseResult :: Int -> Int -> [Int] -> [Int]
calcPhaseResult maxSteps step digits
  | maxSteps + 1 == step = []
  | otherwise = phaseResult : calcPhaseResult maxSteps (step + 1) digits
  where
    phaseResult = (`mod` 10) $ abs $ sum $ zipWith (*) digits $ generateFFTPattern step

calcPhaseResult' :: [Int] -> [Int]
calcPhaseResult' digits = init $ foldr (\d (h:r) -> ((d + h) `mod` 10) : h : r) [0] digits

day16 :: IO ()
day16 = do
  [line] <- getLines
  day16Solution line

day16Solution :: Text.Text -> IO ()
day16Solution line = do
  let digits = map (\c -> ord c - asciiZero) $ Text.unpack line
  print $
    map (\x -> chr $ asciiZero + x) $
    take 8 $ foldl' (\d n -> pad (calcPhaseResult (length d) 1 d) 0 (length digits)) digits [1 .. 100]
  let digits10k = take (length digits * 10000) $ cycle digits
  let offset = read $ take 7 $ Text.unpack line
  let digits' = drop offset digits10k
  print $ map (\x -> chr $ asciiZero + x) $ take 8 $ foldl' (\acc _ -> calcPhaseResult' acc) digits' [1 .. 100]
  where
    asciiZero = ord '0'

drawVRField :: [[Int]] -> IO ()
drawVRField field = do
  putStr "\ESC[2J"
  mapM_ (print . map chr) field
  hFlush stdout
  threadDelay $ 1000000 `div` 30

checkIntersection :: [[Int]] -> (Int, Int) -> Int
checkIntersection field (x, y)
  | x >= length (head field) - 2 = checkIntersection field (1, y + 1)
  | y >= length field - 2 = 0
  | otherwise =
    if all (\(x', y') -> ((field !! y') !! x') == sc) [(x - 1, y), (x, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      then (x * y) + checkIntersection field (x + 1, y)
      else 0 + checkIntersection field (x + 1, y)
  where
    sc = ord '#'

runVR :: State -> [[Int]] -> (Int, Int) -> (State, [[Int]], (Int, Int))
runVR End field _ = (End, field, (0, 0))
runVR state@(State pc rb seq _input _output) field loc@(x, y) = do
  let instruction = unSequence seq ! unProgramCounter pc
  case runInstruction state (pad (show instruction) '0' 5) of
    End -> (End, field, loc)
    state'@(State _pc _rb _seq _input output) ->
      let state'' = state' {stateOutput = Output Nothing}
       in case unOutput output of
            Just 10 -> runVR state'' (field ++ [[]]) (0, y + 1)
            Just c -> runVR state'' (init field ++ [last field ++ [c]]) (x + 1, y)
            Nothing -> runVR state'' field (x, y)

day17 :: IO ()
day17 = do
  [line] <- getLines
  day17Solution line

day17Solution :: Text.Text -> IO ()
day17Solution line =
  case mapM (signed decimal) $ Text.splitOn (Text.pack ",") line of
    Left _ -> die "Couldn't parse the input"
    Right seq -> do
      let state =
            State
              (ProgramCounter 0)
              (RelativeBase 0)
              (Sequence $ Map.insert 0 2 $ Map.fromList $ indexed $ map fst seq)
              (Input $
               map ord $
               unlines ["C,A,C,A,B,C,A,B,C,B", "R,8,R,12,L,8,L,8", "L,10,R,6,R,6,L,8", "R,6,L,10,R,8", "n", ""])
              (Output Nothing)
      let field = [[]]
      let (state', field', loc) = runVR state field (0, 0)
      drawVRField $ init field'
      print $ head $ last field'

getLines :: IO [Text.Text]
getLines = Text.lines <$> TextIO.readFile "input.txt"