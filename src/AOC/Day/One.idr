module AOC.Day.One

import AOC.Solution

import Data.String
import Data.List

%default total

----------------------------------------------------------------------------
-- Types -------------------------------------------------------------------
----------------------------------------------------------------------------

InputType : Type
InputType = List Integer

----------------------------------------------------------------------------
-- Helper Functions --------------------------------------------------------
----------------------------------------------------------------------------

ascending : Ord a => List a -> Nat
ascending list = foldr (\(x, y), acc => if x < y then S acc else acc) Z (zip list (drop 1 list))

window : (Num a, Ord a) => List a -> List a
window list = zipWith3 (\a, b, c => a + b + c) list (drop 1 list) (drop 2 list)


----------------------------------------------------------------------------
-- Solutions ---------------------------------------------------------------
----------------------------------------------------------------------------

part1 : InputType -> Nat
part1 = ascending

part2 : InputType -> Nat
part2 = ascending . window


----------------------------------------------------------------------------
-- Interface for input/output ----------------------------------------------
----------------------------------------------------------------------------

parser : String -> Maybe (InputType)
parser = sequence . map (parseInteger {a=Integer}) . lines

export
day1 : (inputType ** AOCSolution inputType)
day1 = solutionToDP (MkAOCSolution {
    day = 1
  , parser = parser
  , part1 = show . part1
  , part2 = show . part2
  })
