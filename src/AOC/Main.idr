module AOC.Main

import AOC.Day
import AOC.Solution
import AOC.Solution.IO

import Data.List

main : IO ()
main = do
  sequence_ $ intersperse (putStrLn "") $ map printSolutionDP $ days
