
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

let rec edgesExpr (src:int, sink:int, expr) =
 match expr with
  | Num(_) -> [(src, expr, sink)]
  | PlusExpr(_) -> [(src, expr, sink)]
  | TimesExpr(_) -> [(src, expr, sink)]
  | DivExpr(_) -> [(src, expr, sink)]
  | MinusExpr(_) -> [(src, expr, sink)]
  | PowExpr(_) -> [(src, expr, sink)]
  | SqrtExpr(_) -> [(src, expr, sink)]
  | CubeExpr(_) -> [(src, expr, sink)]
  | LogExpr(_) -> [(src, expr, sink)]
  | Log10Expr(_) -> [(src, expr, sink)]
  | UPlusExpr(_) -> [(src, expr, sink)]
  | UMinusExpr(_) -> [(src, expr, sink)]
  | Variable(_) -> [(src, expr, sink)]
  | ArrayValue(_) -> [(src, expr, sink)]

let rec edgesBool (src:int, sink:int, BE) =
 match BE with
  | EqualsExpr(x,y) -> [(src, BE, sink)]
  | NotEqualsExpr(_) -> failwith "Not Implemented"
  | LargerThanExpr(_) -> failwith "Not Implemented"
  | LargerThanOrEqualsExpr(_) -> failwith "Not Implemented"
  | SmallerThanExpr(_) -> failwith "Not Implemented"
  | SmallerThanOrEqualsExpr(_) -> failwith "Not Implemented"
  | NOTExpr(_) -> failwith "Not Implemented"
  | BoolLogicOrExpr(_) -> failwith "Not Implemented"
  | BoolLogicAndExpr(_) -> failwith "Not Implemented"
  | LogicOrExpr(_) -> failwith "Not Implemented"
  | LogicAndExpr(_) -> failwith "Not Implemented"
  | TrueOrFalse(_) -> failwith "Not Implemented"

let rec edgesCmd (src:int, sink:int, commands) =
 match commands with
  | AssignVariableCommand(x,y) -> [(src, commands, sink)]
  | AssignArrayValue(x,y,z) -> [(src, commands, sink)]
  | SkipOperation -> [(src, commands, sink)]
  | ExecuteLoop(_) -> failwith "Not Implemented"
  | ExecuteIf(x) -> edgesGC(src, sink, x)
  | CommandSequence(x,y) -> nodeCount<-nodeCount+1
                            let edges1 = edgesCmd(src, nodeCount, x)  
                            let edges2 = edgesCmd(nodeCount, sink, y)
                            in List.concat [edges1;edges2]

and edgesGC (src:int, sink:int, GC) =
 match GC with
  | ExecuteCondition(x,y) -> //[(src, GC, sink)]
                             nodeCount<-nodeCount+1;
                             let edges1 = edgesBool(src, nodeCount, x)
                             let edges2 = edgesCmd(nodeCount, sink, y)
                             in List.concat [edges2]  //TODO: We need to append the edges1 somehow, but it's has a bad type (boolExpr)
  | ExecuteChoice(x,y) ->   let edges1 = edgesGC(src, sink, x)  
                            let edges2 = edgesGC(src, sink, y)
                            in List.concat [edges1; edges2] 

//variable, array, bool
//F# type expression
// Set of tuples(start end label)

//Node gen: let mutable count := 0 count <- count+1