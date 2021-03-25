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
#load "ProgramGraph.fsx"
open ProgramGraph
#load "PopulateFile.fsx"
open PopulateFile
#load "Interpreter.fsx"
open Interpreter

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = CalculatorParser.start CalculatorLexer.tokenize lexbuf
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
        printfn "write the initial variable configuration separating them with a semicolon e.g\n
        a = 2; b = 5; A=[1, 5, 8, 2, 5, 6]"
        let variables = Console.ReadLine()
        printfn "Now write your program to be executed"
        let program = parse (Console.ReadLine())
        // and print the result of evaluating it -- For now, the type of e can be set on the last line of CalculatorParser.fs

        //The following is for Assignment 2
        let programGraph = genPG(program)
        let CMD, TEST = programGraph
        printfn "Arraylength: %d" (CMD.Length + TEST.Length)
        printfn "Grapf: %A" (programGraph)
        (*//WriteFile(programGraph) *)
        //Interpreter.execInterpreter(program, variables)
        
        printfn "Thats a valid program"
        //evalCmd(e) |>
        compute n
        with err -> 
            printf "Syntax error\n"
            compute (n-1)

// Start interacting with the user
compute 2
