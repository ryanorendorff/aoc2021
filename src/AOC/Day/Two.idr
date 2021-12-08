module AOC.Day.Two

import AOC.Solution
import Data.String

%default total

----------------------------------------------------------------------------
-- Types -------------------------------------------------------------------
----------------------------------------------------------------------------

data Direction = Forward Integer
               | Up Integer
               | Down Integer

InputType : Type
InputType = List Direction


----------------------------------------------------------------------------
-- Interfaces --------------------------------------------------------------
----------------------------------------------------------------------------

-- These interfaces not strictly necessary, but are helpful for debugging.

partial
Cast String (Maybe Direction) where
    cast x = case (words x) of
        ("forward" :: (a :: [])) => Just (Forward (cast a))
        ("up" :: (a :: [])) => Just (Up (cast a))
        ("down" :: (a :: [])) => Just (Down (cast a))
        _ => Nothing

Show Direction where
    show (Forward x) = "forward " ++ (show x)
    show (Up x) = "up " ++ (show x)
    show (Down x) = "down " ++ (show x)


----------------------------------------------------------------------------
-- Helper Functions --------------------------------------------------------
----------------------------------------------------------------------------

direction_helper : (Integer, Integer) -> List Direction -> (Integer, Integer)
direction_helper vals [] = vals
direction_helper (f, ud) ((Forward x) :: dirs) = direction_helper (f + x, ud) dirs
direction_helper (f, ud) ((Up x) :: dirs) = direction_helper (f, ud - x) dirs
direction_helper (f, ud) ((Down x) :: dirs) = direction_helper (f, ud + x) dirs

direction : List Direction -> Integer
direction dirs = let
  (f, ud) = direction_helper (0, 0) dirs
  in f * ud

direction_helper2 : (Integer, Integer, Integer) -> List Direction -> (Integer, Integer, Integer)
direction_helper2 vals [] = vals
direction_helper2 (f, ud, aim) ((Forward x) :: dirs) = direction_helper2 (f + x, ud + aim * x, aim) dirs
direction_helper2 (f, ud, aim) ((Up x) :: dirs) = direction_helper2 (f, ud, aim - x) dirs
direction_helper2 (f, ud, aim) ((Down x) :: dirs) = direction_helper2 (f, ud, aim + x) dirs


direction2 : List Direction -> Integer
direction2 dirs = let
  (f, ud, aim) = direction_helper2 (0, 0, 0) dirs
  in f * ud


----------------------------------------------------------------------------
-- Solutions ---------------------------------------------------------------
----------------------------------------------------------------------------

part1 : InputType -> Integer
part1 = direction

part2 : InputType -> Integer
part2 = direction2


----------------------------------------------------------------------------
-- Interface for input/output ----------------------------------------------
----------------------------------------------------------------------------

-- Why is this partial?
partial
parser : String -> Maybe InputType
parser = sequence . map (the (Maybe Direction) . cast) . lines

partial
export
day2 : (inputType ** AOCSolution inputType)
day2 = solutionToDP (MkAOCSolution {
    day = 2
  , parser = parser
  , part1 = show . part1
  , part2 = show . part2
  })
