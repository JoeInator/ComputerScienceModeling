
open System
#load "CalculatorTypesAST.fs"
open CalculatorTypesAST
#load "CalculatorParser.fs"
open CalculatorParser
#load "CalculatorLexer.fs"
open CalculatorLexer

let mutable nodeCount = 1
let node="q"

let increNode = nodeCount<-nodeCount+1
let merge a b = a @ b |> List.distinct

let rec edges2 start stop program =
 match program with
  | Num(_) -> failwith "Not Implemented"
  | PlusExpr(_) -> failwith "Not Implemented" //"start -> stop [label = \"%f + %f\"]"(x y)
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

let rec edgesCmd (src:int, sink:int, commands) =
 match commands with
  | AssignVariableCommand(x,y) -> [(src, commands, sink)]
  | AssignArrayValue(x,y,z) -> [(src, commands, sink)]
  | SkipOperation -> [(src, commands, sink)]
  | ExecuteLoop(_) -> failwith "Not Implemented"
  | ExecuteIf(x) -> failwith "Not Implemented" //edgesGC(src, sink, x)
  | CommandSequence(x,y) -> nodeCount<-nodeCount+1
                            let edges1 = edgesCmd(src, nodeCount, x)  
                            let edges2 = edgesCmd(nodeCount, sink, y)
                            in List.concat [edges1; edges2]

let rec edgesGC src sink GC =
 match GC with
  | ExecuteCondition(x,y) -> failwith "Not Implemented" //[(src, "x", )]
  | ExecuteChoice(_) -> failwith "Not Implemented" 

//variable, array, bool
//F# type expression
// Set of tuples(start end label)

//Node gen: let mutable count := 0 count <- count+1