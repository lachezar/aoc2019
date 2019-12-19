{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module IntCode
  ( runInstructions
  , runInstruction
  , State(..)
  , ProgramCounter(..)
  , RelativeBase(..)
  , Sequence(..)
  , Input(..)
  , Output(..)
  ) where

import Data.Foldable (forM_)
import Debug.Trace (traceShowId)
import Data.Map (Map, insert, findWithDefault)

newtype RelativeBase =
  RelativeBase
    { unRelativeBase :: Int
    }
  deriving (Show)

newtype ProgramCounter =
  ProgramCounter
    { unProgramCounter :: Int
    }
  deriving (Show)

newtype Sequence =
  Sequence
    { unSequence :: Map Int Int
    }
  deriving (Show)

newtype Input =
  Input
    { unInput :: [Int]
    }
  deriving (Show)

newtype Output =
  Output
    { unOutput :: Maybe Int
    }
  deriving (Show)

data State
  = State
      { statePc :: ProgramCounter
      , stateRb :: RelativeBase
      , stateSeq :: Sequence
      , stateInput :: Input
      , stateOutput :: Output
      }
  | End
  deriving (Show)

data Mode
  = Relative
  | Absolute
  | Immediate deriving (Show)

mode :: Char -> Mode
mode '2' = Relative
mode '1' = Immediate
mode '0' = Absolute
mode _ = undefined

(!) :: Ord k => Integral v => Map k v -> k -> v
(!) m k = findWithDefault 0 k m

get :: State -> Mode -> Int -> Int
get (State pc rb seq _ _) Relative index = unSequence seq ! (unRelativeBase rb + unSequence seq ! index)
get (State pc rb seq _ _) Immediate index = unSequence seq ! index
get (State pc rb seq _ _) Absolute index = unSequence seq ! (unSequence seq ! index)
get _ _ _ = undefined

set :: State -> Mode -> Int -> Int -> State
set state@(State pc rb seq input output) Relative index value =
  state {stateSeq = Sequence $ insert (unRelativeBase rb + unSequence seq ! index) value (unSequence seq)}
set state@(State pc rb seq input output) Immediate index value =
  state {stateSeq = Sequence $ insert index value (unSequence seq)}
set state@(State pc rb seq input output) Absolute index value =
  state {stateSeq = Sequence $ insert (unSequence seq ! index) value (unSequence seq)}
set _ _ _ _ = undefined

updateList :: [a] -> Int -> a -> [a]
updateList l i v = take i l ++ [v] ++ drop (i + 1) l

upProgramCounter :: State -> Int -> State
upProgramCounter End _ = End
upProgramCounter state@(State pc rb seq input output) n = state {statePc = ProgramCounter (unProgramCounter pc + n)}

runInstruction :: State -> String -> State
runInstruction End _ = End
runInstruction state@(State pc rb seq input output) (a:b:c:de)
  | de == "01" =
    let x = get state (mode c) (unProgramCounter pc + 1)
     in let y = get state (mode b) (unProgramCounter pc + 2)
         in let state' = set state (mode a) (unProgramCounter pc + 3) (x + y)
             in upProgramCounter state' 4
  | de == "02" =
    let x = get state (mode c) (unProgramCounter pc + 1)
     in let y = get state (mode b) (unProgramCounter pc + 2)
         in let state' = set state (mode a) (unProgramCounter pc + 3) (x * y)
             in upProgramCounter state' 4
  | de == "03" =
    let state' = set state (mode c) (unProgramCounter pc + 1) $ head $ unInput input
     in let state'' = state' {stateInput = Input $ tail $ unInput input}
         in upProgramCounter state'' 2
  | de == "04" =
    let x = get state ( mode c) ( unProgramCounter pc + 1)
     in let state' = state {stateOutput = Output (Just x)}
         in upProgramCounter state' 2
  | de == "05" =
    let x = get state (mode c) (unProgramCounter pc + 1)
     in let relativePcMove =
              if x /= 0
                then get state (mode b) (unProgramCounter pc + 2) - unProgramCounter pc
                else 3
         in upProgramCounter state relativePcMove
  | de == "06" =
    let x = get state (mode c) (unProgramCounter pc + 1)
     in let relativePcMove =
              if x == 0
                then get state (mode b) (unProgramCounter pc + 2) - unProgramCounter pc
                else 3
         in upProgramCounter state relativePcMove
  | de == "07" =
    let x = get state (mode c) (unProgramCounter pc + 1)
     in let y = get state (mode b) (unProgramCounter pc + 2)
         in let value =
                  if x < y
                    then 1
                    else 0
             in let state' = set state (mode a) (unProgramCounter pc + 3) value
                 in upProgramCounter state' 4
  | de == "08" =
    let x = get state (mode c) (unProgramCounter pc + 1)
     in let y = get state (mode b) (unProgramCounter pc + 2)
         in let value =
                  if x == y
                    then 1
                    else 0
             in let state' = set state (mode a) (unProgramCounter pc + 3) value
                 in upProgramCounter state' 4
  | de == "09" =
    let x = get state (mode c) (unProgramCounter pc + 1)
     in let state' = state {stateRb = RelativeBase (unRelativeBase rb + x)}
         in upProgramCounter state' 2
  | de == "99" = End
runInstruction _ _ = undefined

runInstructions :: State -> IO ()
runInstructions End = print "end"
runInstructions state@(State pc rb seq input output) = do
  let instruction = unSequence seq ! unProgramCounter pc
  case runInstruction state (pad (show instruction) '0' 5) of
    End -> print "end"
    state' -> do
      Data.Foldable.forM_ (unOutput $ stateOutput state') print
      runInstructions state' {stateOutput = Output Nothing}

pad :: String -> Char -> Int -> String
pad s c n = replicate (n - length s) c ++ s