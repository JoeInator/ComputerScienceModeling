
open System
#load "CalculatorTypesAST.fs"
open CalculatorTypesAST
#load "CalculatorParser.fs"
open CalculatorParser
#load "CalculatorLexer.fs"
open CalculatorLexer

let mutable nodeCount = 1
let array:List<int * 'T *int> = []

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
  | NotEqualsExpr(_) -> [(src, BE, sink)]
  | LargerThanExpr(_) -> [(src, BE, sink)]
  | LargerThanOrEqualsExpr(_) -> [(src, BE, sink)]
  | SmallerThanExpr(_) -> [(src, BE, sink)]
  | SmallerThanOrEqualsExpr(_) -> [(src, BE, sink)]
  | NOTExpr(_) -> [(src, BE, sink)]
  | BoolLogicOrExpr(_) -> [(src, BE, sink)]
  | BoolLogicAndExpr(_) -> [(src, BE, sink)]
  | LogicOrExpr(_) -> [(src, BE, sink)]
  | LogicAndExpr(_) -> [(src, BE, sink)]
  | TrueOrFalse(_) -> [(src, BE, sink)]

let rec edgesCmd (src:int, sink:int, commands) =
 match commands with
  | AssignVariableCommand(x,y) -> [(src, commands, sink)]
  | AssignArrayValue(x,y,z) -> [(src, commands, sink)]
  | SkipOperation -> [(src, commands, sink)]
  | ExecuteLoop(x) -> List.concat [edgesGC(src, src, x); [(src, DONEGUARD(x), sink)]]
  | ExecuteIf(x) -> edgesGC(src, sink, x) //failwith "Not Implemented"
  | CommandSequence(x,y) -> nodeCount<-nodeCount+1
                            let edges1 = edgesCmd(src, nodeCount, x)  
                            let edges2 = edgesCmd(nodeCount, sink, y)
                            in List.concat [edges1;edges2]

and edgesGC (src:int, sink:int, GC) =
 match GC with
  | ExecuteCondition(x,y) -> //edgesCmd(nodeCount, sink, y)
                             nodeCount<-nodeCount+1;
                             let edges1 = edgesBool(src, nodeCount, x)
                             let edges2 = edgesCmd(nodeCount, sink, y)
                             in List.concat [[(src, x, nodeCount)]; edges2]  //TODO: We need to append the edges1 somehow, but it's has a bad type (boolExpr)
                             //edges2
  | ExecuteChoice(x,y) ->   let edges1 = edgesGC(src, sink, x)  
                            let edges2 = edgesGC(src, sink, y)
                            in List.concat [edges1; edges2] 

and DONEGUARD (GC) =
 match GC with
 | ExecuteCondition(x,y) -> NOTExpr(x)
 | ExecuteChoice(_) -> failwith "Not Implemented"
//variable, array, bool
//F# type expression
// Set of tuples(start end label)

//Node gen: let mutable count := 0 count <- count+1