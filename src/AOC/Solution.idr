module AOC.Solution

%default total

public export
record AOCSolution inputType where
  constructor MkAOCSolution
  day : Nat
  parser : String -> Maybe inputType
  part1 : inputType -> String
  part2 : inputType -> String

export
solutionToDP : {a : Type} -> (x : AOCSolution a) -> (inputType ** AOCSolution inputType)
solutionToDP {a} x = (a ** x)
