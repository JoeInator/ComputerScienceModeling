// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FSharp.Text.Lexing
open CalculatorTypesAST
open CalculatorLexer
open CalculatorParser
open ProgramGraph
open PopulateFile
open Interpreter

let parse input =
 // translate string into a buffer of characters
 let lexbuf = LexBuffer<char>.FromString input
 // translate the buffer into a stream of tokens and parse them
 let res =
     CalculatorParser.start CalculatorLexer.tokenize lexbuf
 // return the result of parsing (i.e. value of type "expr")
 res

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
   WriteFile2(programGraph)
   
   (* printfn "Arraylength: %d" (CMD.Length + TEST.Length)
   printfn "Grapf: %A" (programGraph) *)
   let vars = "a = 5; b = 5; A=[1, 5, 8, 2, 5, 6]"
   InterpretPG(programGraph, variables)
   
  with err ->
   printfn "Internal error"
 printfn "Do you want to go again?? y/n"
 if Console.ReadLine() = "y" then compute n


[<EntryPoint>]
let main argv =
    printfn "HI"
    compute 2
    0 // return an integer exit code