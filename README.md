# Project in Compuer Science Modelling F21


## Download project
To down load this project, follow these steps:
1. Open a command promt
2. Navigate to where you want to put the project
3. Execute the following command: ```git clone git@github.com:JoeInator/ComputerScienceModeling.git```
4. Type ```cd ComputerScienceModeling```

## Execute the project
To execute this project, you'll have to generate the lexer and the parser using the following commands:
```
FsLexYacc.10.0.0/build/fslex/net46/fslex.exe CalculatorLexer.fsl --unicode
FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe CalculatorParser.fsp --module CalculatorParser
```
That will generate the three files ```CalculatorLexer.fs, CalculatorParser.fs``` and ```CalculatorParser.fsi``` which are crucial for the progam to execute.

Lastly, run the following command to start the program
```
fsi Calculator.fsx
```
You can all the relevant commands for generating the parser and starting the program by typing ```./run.sh``` in the terminal
## Assignment 2

### Important files.
For this this assignment, two new files has been added to the project:
1. ProgramGraph.fsx - This file takes the input evaluated by the parser, and turns it into an array of edges by implementing the edges function from the book "formal methods"
2. PopulateFile.fsx - This file takes the output array from ProgramGraph.fsx and uses it to write a graphviz file that can vusialize the program graph
### Known probems/issues:
1. The matching type problem need to be fixed - This has been sorted out by turning everything into a string. I am not convinced that this is a good way to go tho.
2. We haven't implemented two different PG's for the graphviz file. At the moment, only non-deterministic PG's can be printed into the graphviz file
3. multigauded commands for loops e.g. do..od crashes the program in the PG generations with the DONE[GC] as we haven't been able to porperly implement it. Yet
## Assignment 1
### Examine the project
For this assignment, there 4 important files.
1. ```CalculatorParser.fs``` which handles the input based on defined patterns with the specified tokens
2. ```CalculatorLexer.fs``` which matches the input keywords and special characters to a specified token
3. ```CalculatorTypesAST.fs``` which contains the abstract syntax that are assigned the appropiate pattern in ```CalculatorParser.fs```
4. ```Calculator.fsx``` which reads the given input, and initiates the parser and the lexer to evaluate it.