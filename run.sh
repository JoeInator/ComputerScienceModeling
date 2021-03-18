FsLexYacc.10.0.0/build/fslex/net46/fslex.exe CalculatorLexer.fsl --unicode
FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe CalculatorParser.fsp --module CalculatorParser
dotnet fsi Calculator.fsx