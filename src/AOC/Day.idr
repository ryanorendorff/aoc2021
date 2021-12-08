module AOC.Day

import AOC.Solution

import AOC.Day.One
import AOC.Day.Two
import AOC.Day.Three

%default total

export
days : List (inputType ** AOCSolution inputType)
days = [ day1
       , day2
       , day3
       ]
