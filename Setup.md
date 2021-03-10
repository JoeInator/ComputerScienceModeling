# Project in Compuer Science Modelling F21

To execute this project, you'll have to generate the lexer and the parser using the following commands:
```
FsLexYacc.10.0.0/build/fslex/net46/fslex.exe CalculatorLexer.fsl --unicode
FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe CalculatorParser.fsp --module CalculatorParser
fsi Calculator.fsx
```