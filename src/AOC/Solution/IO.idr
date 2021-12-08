module AOC.Solution.IO

import AOC.Solution

import System.File

printSolution : forall a. AOCSolution a -> IO ()
printSolution sol = do
  file <- readFile ("inputs/day-" ++ (show sol.day) ++ ".txt")
  inputString <- case file of
                   Left err => do
                     printLn err
                     pure ""
                   Right x => pure x

  case (sol.parser inputString) of
      Just input => do
        let part1 = sol.part1 input
            part2 = sol.part2 input

        putStrLn $ "Day " ++ show sol.day ++ ":"
        putStrLn $ "Answer 1: " ++ part1
        putStrLn $ "Answer 2: " ++ part2

      Nothing => do
        printLn $ "Could not parse string for day " ++ show sol.day

export
printSolutionDP : (a ** AOCSolution a) -> IO ()
printSolutionDP x = printSolution (x.snd)
