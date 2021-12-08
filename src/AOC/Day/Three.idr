module AOC.Day.Three

import AOC.Solution
import Data.Vect
import Data.String
import Data.Nat

%default total

----------------------------------------------------------------------------
-- Types -------------------------------------------------------------------
----------------------------------------------------------------------------

data Bit : Type where
  Low : Bit
  High : Bit

Binary : (n : Nat) -> Type
Binary n = Vect n Bit

InputType : Type
InputType = (m ** List (Binary m))


----------------------------------------------------------------------------
-- Interfaces --------------------------------------------------------------
----------------------------------------------------------------------------

Show Bit where
  show Low = "0"
  show High = "1"

-- Could be more efficient with fold and cons but eh.
[BinaryShow] Show (Binary n) where
  show [] = ""
  show (x :: xs) = show x ++ show @{BinaryShow} xs

Cast Bit Nat where
  cast Low = 0
  cast High = 1

Cast Bit Integer where
  cast x = the Integer (cast (the Nat (cast x)))

Cast Nat (Maybe Bit) where
  cast 0 = Just Low
  cast 1 = Just High
  cast _ = Nothing

Cast Bool Bit where
  cast False = Low
  cast True  = High

Cast (Binary n) Nat where
  cast = foldl (\acc, x => acc * 2 + x) 0 . map cast

Cast (Binary n) Integer where
  cast x = the Integer (cast (the Nat (cast x)))

----------------------------------------------------------------------------
-- Helper Functions --------------------------------------------------------
----------------------------------------------------------------------------

countBit : Bit -> Integer -> Integer
countBit Low x = x - 1
countBit High x = x + 1

moreOf : {n : Nat} -> List (Binary n) -> Vect n Integer
moreOf {n} list = foldr (\x, acc => zipWith countBit x acc) (replicate n 0) list

mostCommon : Vect n Integer -> Binary n
mostCommon = map (cast . (> 0))

leastCommon : Vect n Integer -> Binary n
leastCommon = map (cast . (< 0))


----------------------------------------------------------------------------
-- Solutions ---------------------------------------------------------------
----------------------------------------------------------------------------

part1 : InputType -> String
part1 (m ** list) = show $ (cast . mostCommon $ mf) * (cast . leastCommon $ mf)
  where
    mf : Vect m Integer
    mf = moreOf list


----------------------------------------------------------------------------
-- Parsing Helper Functions ------------------------------------------------
----------------------------------------------------------------------------

toBinaryDep' : (s : List Char) -> Maybe (n ** Binary n)
toBinaryDep' [] = Just (0 ** [])
toBinaryDep' ('0' :: str) = (toBinaryDep' str) >>= (\(n ** bin) => Just $ (S n ** Low :: bin))
toBinaryDep' ('1' :: str) = (toBinaryDep' str) >>= (\(n ** bin) => Just $ (S n ** High :: bin))
toBinaryDep' _ = Nothing

toBinaryDep : (s : String) -> Maybe (n ** Binary n)
toBinaryDep s = toBinaryDep' (unpack s)

-- Stolen from https://github.com/jumper149/AoC2021/blob/main/03/src/Main.idr
exactLengths : (len : Nat) -> Traversable t => t (n ** Binary n) -> Maybe (t (Binary len))
exactLengths len xs = traverse (\ x => exactLength len x.snd) xs

-- I thought one could define validateBinaryList recursively without
-- exactLengths, but while that compiles, it results in returning Nothing
-- always. Not precisely sure which Nothing either (from the recursive call or
-- from the call to exactLength).
validateBinaryList : List (n ** Binary n) -> Maybe (InputType)
validateBinaryList [] = Just (0 ** [])
validateBinaryList ((len ** bin) :: ys) = do
  list <- exactLengths len ys
  pure (len ** bin :: list)


----------------------------------------------------------------------------
-- Interface for input/output ----------------------------------------------
----------------------------------------------------------------------------

parser : String -> Maybe (InputType)
parser = join . map validateBinaryList . sequence . map toBinaryDep . lines

export
day3 : (inputType ** AOCSolution inputType)
day3 = solutionToDP (MkAOCSolution {
    day = 3
  , parser = parser
  , part1 = part1
  , part2 = \x => "part2"
  })
