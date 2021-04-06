// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

#load "CalculatorTypesAST.fs"
open CalculatorTypesAST
#load "CalculatorParser.fs"
open CalculatorParser
#load "CalculatorLexer.fs"
open CalculatorLexer
#load "ProgramGraph.fs"
open ProgramGraph
#load "PopulateFile.fs"
open PopulateFile
#load "Interpreter.fs"
open Interpreter

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)

// We
let parse input =
 // translate string into a buffer of characters
 let lexbuf = LexBuffer<char>.FromString input
 // translate the buffer into a stream of tokens and parse them
 let res =
     CalculatorParser.start CalculatorLexer.tokenize lexbuf
 // return the result of parsing (i.e. value of type "expr")
 res

// We implement here the function that interacts with the user
let rec compute n =
 if n = 0 then
  printfn "Bye bye"
 else
  printf "Enter a command: "

  try
   // We parse the input string
   //printfn "MAP: %A" (dom)
   printfn
       "write the initial variable configuration separating them with a semicolon e.g\n
a = 2; b = 5; A=[1, 5, 8, 2, 5, 6]"

   let variables = Console.ReadLine()
   printfn "Now write your program to be executed"
   let program = parse (Console.ReadLine())
   // and print the result of evaluating it -- For now, the type of e can be set on the last line of CalculatorParser.fs

   //The following is for Assignment 2
   let programGraph = genPG (program)
   let CMD, TEST = programGraph
   printfn "Arraylength: %d" (CMD.Length + TEST.Length)
   printfn "Grapf: %A" (programGraph)
   
   //InterpretPG(TEST, CMD, variables)
   // do a>0 -> b:=b+A[a]; a:=a-1 [] a=0 -> b:=1000 od
   let vars = "a = 5; b = 5; A=[1, 5, 8, 2, 5, 6]"

   let testProg =
    ExecuteLoop(
     ExecuteCondition(
      LargerThanOrEqualsExpr(Variable "a", Num 0.0),
      CommandSequence(
       AssignVariableCommand("b", PlusExpr(Variable "b", ArrayValue("A", Variable "a"))),
       AssignVariableCommand("a", MinusExpr(Variable "a", Num 1.0))
      )
     )
    )

   let testPG =
    ([ (1, AssignVariableCommand("b", PlusExpr(Variable "b", ArrayValue("A", Variable "a"))), 2);
       (2, AssignVariableCommand("a", MinusExpr(Variable "a", Num 1.0)), 0)],
     [ (0, LargerThanExpr(Variable "a", Num 0.0), 1);
       (0, NOTExpr (LargerThanExpr (Variable "a", Num 0.0)), -1);
       (6, NOTExpr (LargerThanExpr (Variable "a", Num 0.0)), -1)])
   //WriteFile2(programGraph)
   let CMD, TEST = testPG
   InterpretPG(programGraph, vars)
   printfn "Thats a valid program"
   compute n
  with err ->
   printf "Syntax error\n"
   compute (n - 1)

// Start interacting with the user
compute 2

//Fixes: 