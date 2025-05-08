# Set Calculator

A command-line calculator for evaluating set expressions, written in Haskell.

## Features

- **Set literals**: Define sets using curly braces, e.g., `{1, 2, 3}`
- **Union**: `A union B`
- **Intersection**: `A intersect B` 
- **Difference**: `A minut B` 
- **Symmetric difference**: `A symdiff B` 
- **Cartesian product**: `A cross B` 
- **Power set**: `powerset A` returns a set of all possible subsets of A
- **Subset test**: `A subset B` use keyword subset
- **Membership test**: `x in A`
- **Cardinality**: `card(A)` (ASCII keywordword)
- **Interactive REPL**: Type expressions, view results, use `:help` and `:quit`


## Files

- **Calc.lhs**: The file that contains all of the important stuff. This file handles the specifics of the parsing and the interpretor. 
Most of what we did is in this file
- **Parsing2.hs**: The file that contains a lot of the behind the scenes stuff that relates to the parser
- **CalcREPL.hs**: The file that contains the loop for running the stack the calculator operates in

## Effort

- This project mostly resembles the structure of the modules and the last two projects we did in class. The code is parsed into 
the relevant datatypes, interpreted, and returns the answer or the appropriate error message

### Installing: 

Clone the repository and run "stack build" in the terminal. After that runs, run "stack run" in the terminal. After that the 
calculator REPL will provide you with appropriate instructions. 