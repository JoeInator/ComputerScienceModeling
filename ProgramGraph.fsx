
open System
#load "CalculatorTypesAST.fs"
open CalculatorTypesAST
#load "CalculatorParser.fs"
open CalculatorParser
#load "CalculatorLexer.fs"
open CalculatorLexer

let mutable nodeCount = 0
let node="q"

let increNode = nodeCount<-nodeCount+1
let merge a b = a @ b |> List.distinct

let rec edges2 start stop program =
 match program with
  | Num(_) -> failwith "Not Implemented"
  | PlusExpr(_) ->failwith "Not Implemented" //"start -> stop [label = \"%f + %f\"]"(x y)
  | TimesExpr(_) -> failwith "Not Implemented"
  | DivExpr(_) -> failwith "Not Implemented"
  | MinusExpr(_) -> failwith "Not Implemented"
  | PowExpr(_) -> failwith "Not Implemented"
  | SqrtExpr(_) -> failwith "Not Implemented"
  | CubeExpr(_) -> failwith "Not Implemented"
  | LogExpr(_) -> failwith "Not Implemented"
  | Log10Expr(_) -> failwith "Not Implemented"
  | UPlusExpr(_) -> failwith "Not Implemented"
  | UMinusExpr(_) -> failwith "Not Implemented"
  | Variable(_) -> failwith "Not Implemented"
  | ArrayValue(_) -> failwith "Not Implemented"

let rec edgesCmd src sink command =
 match command with
  | AssignVariableCommand(x,y) -> [(src, command, sink)]
  | AssignArrayValue(x,y,z) -> [(src, command, sink)]
  | SkipOperation -> [(src, command, sink)]
  | ExecuteLoop(_) -> failwith "Not Implemented"
  | ExecuteIf(x) -> failwith "Not Implemented" //edgesGC(src, sink, x)
  | CommandSequence(x,y) -> let edges1 = edgesCmd(src, nodeCount<-nodeCount+1, x)  
                            let edges2 = edgesCmd(nodeCount, sink, y)
                            in List.concat [edges1; edges2]

let rec edgesGC src sink GC =
 match GC with
  | ExecuteCondition(x,y) -> failwith "Not Implemented"
  | ExecuteChoice(_) -> failwith "Not Implemented" //[(src, "x", )]

//variable, array, bool
//F# type expression
// Set of tuples(start end label)

//Node gen: let mutable count := 0 count <- count+1