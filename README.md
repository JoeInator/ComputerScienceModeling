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

## Examine the project
For this assignment, there 4 important files.
1. ```CalculatorParser.fs``` which handles the input based on defined patterns with the specified tokens
2. ```CalculatorLexer.fs``` which matches the input keywords and special characters to a specified token
3. ```CalculatorTypesAST.fs``` which contains the abstract syntax that are assigned the appropiate pattern in ```CalculatorParser.fs```