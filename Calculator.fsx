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

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
(* 
let rec eval e =
  match e with
    | Num(x) -> x
    | TimesExpr(x,y) -> eval(x) * eval (y)
    | DivExpr(x,y) -> eval(x) / eval (y)
    | PlusExpr(x,y) -> eval(x) + eval (y)
    | MinusExpr(x,y) -> eval(x) - eval (y)
    | PowExpr(x,y) -> eval(x) ** eval (y)
    | SqrtExpr(x) -> sqrt(eval(x)) //NEW
    | CubeExpr(x) -> eval(x) ** 3.0 //NEW
    | LogExpr(x) -> log(eval(x)) //NEW
    | Log10Expr(x) -> log10(eval(x)) //NEW
    | UPlusExpr(x) -> eval(x)
    | UMinusExpr(x) -> - eval(x)
    // | Variable(x) -> x
    // | ArrayValue(x,y) -> x[y]

let rec evalBool e =
  match e with
    | EqualsExpr(x,y) -> eval(x) = eval(y)
    | NotEqualsExpr(x,y) -> eval(x) <> eval(y)
    | LargerThanExpr(x,y) -> eval(x) > eval(y)
    | LargerThanOrEqualsExpr(x,y) -> eval(x) >= eval(y)
    | SmallerThanExpr(x,y) -> eval(x) < eval(y)
    | SmallerThanOrEqualsExpr(x,y) -> eval(x) <= eval(y)
    | NOTExpr(x) -> not(evalBool(x))
    | BoolLogicOrExpr(x,y) -> evalBool(x) || evalBool(y)
    | LogicOrExpr(x,y) -> evalBool(x) || evalBool(y)
    | BoolLogicAndExpr(x,y) -> evalBool(x) && evalBool(y)
    | LogicAndExpr(x,y) -> evalBool(x) && evalBool(y)
 *)
 
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
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it -- For now, the type of e can be set on the last line of CalculatorParser.fs
        
        //The following is for Assignment 2
        let programGraph = edgesCmd(0, -1, e)
        printfn "Arraylength: %d" (programGraph.Length)
        printfn "Grapf: %A" (programGraph)
        WriteFile(programGraph)
        printfn "Thats a valid program"

        compute n
        with err -> 
            printf "Syntax error\n"
            compute (n-1)

// Start interacting with the user
compute 2
