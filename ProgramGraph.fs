module ProgramGraph
open System
open CalculatorTypesAST

let mutable nodeCount = 0
let mutable CmdArray:List<int * commands *int> = []
let mutable TestArray:List<int * boolExpr * int> = []

let rec edgesExpr (expr) =
 match expr with
  | Num(x) -> x.ToString()
  | PlusExpr(x,y) -> edgesExpr(x)+"+"+edgesExpr(y)
  | TimesExpr(x,y) -> edgesExpr(x)+"*"+edgesExpr(y)
  | DivExpr(x,y) -> edgesExpr(x)+"/"+edgesExpr(y)
  | MinusExpr(x,y) -> edgesExpr(x)+"-"+edgesExpr(y)
  | PowExpr(x,y) -> edgesExpr(x)+"^"+edgesExpr(y)
  | SqrtExpr(x) -> failwith "Not relevant"
  | CubeExpr(x) -> failwith "Not relevant"
  | LogExpr(x) -> failwith "Not relevant"
  | Log10Expr(x) -> failwith "Not relevant"
  | UPlusExpr(x) -> "+"+edgesExpr(x)
  | UMinusExpr(x) -> "-"+edgesExpr(x)
  | Variable(x) -> x.ToString()
  | ArrayValue(x,y) -> x+"["+edgesExpr(y)+"]"

let rec edgesBool (BE) =
 match BE with
  | EqualsExpr(x,y) -> edgesExpr(x)+"="+edgesExpr(y)
  | NotEqualsExpr(x,y) -> edgesExpr(x)+"!="+edgesExpr(y)
  | LargerThanExpr(x,y) -> edgesExpr(x)+">"+edgesExpr(y)
  | LargerThanOrEqualsExpr(x,y) -> edgesExpr(x)+">="+edgesExpr(y)
  | SmallerThanExpr(x,y) -> edgesExpr(x)+"<"+edgesExpr(y)
  | SmallerThanOrEqualsExpr(x,y) -> edgesExpr(x)+"<="+edgesExpr(y)
  | NOTExpr(x) -> "!("+edgesBool(x)+")"
  | BoolLogicOrExpr(x,y) -> edgesBool(x)+"|"+edgesBool(y)
  | BoolLogicAndExpr(x,y) -> edgesBool(x)+"&"+edgesBool(y)
  | LogicOrExpr(x,y) -> edgesBool(x)+"||"+edgesBool(y)
  | LogicAndExpr(x,y) -> edgesBool(x)+"&&"+edgesBool(y)
  | TrueOrFalse(x) -> x

let rec edgesCmd (src:int, sink:int, commands) =
 match commands with
  | AssignVariableCommand(x,y) -> CmdArray <- CmdArray @ [(src, commands, sink)]
                                  ()
  | AssignArrayValue(x,y,z) -> CmdArray <- CmdArray @ [(src, commands, sink)]
                               ()
  | SkipOperation -> CmdArray <- CmdArray @ [(src, commands, sink)]
                     ()
  | ExecuteLoop(x) -> edgesGC(src, src, x)
                      TestArray <- TestArray @ [(src, DONEGUARD(x), sink)]
                      ()
  | ExecuteIf(x) -> edgesGC(src, sink, x)
                    ()//edgesGC(src, sink, x) //failwith "Not Implemented"
  | CommandSequence(x,y) -> nodeCount<-nodeCount+1
                            let edges1 = edgesCmd(src, nodeCount, x)  
                            let edges2 = edgesCmd(nodeCount, sink, y)
                            ()

and edgesGC (src:int, sink:int, GC) =
 match GC with
  | ExecuteCondition(x,y) -> //edgesCmd(nodeCount, sink, y)
                             nodeCount<-nodeCount+1;
                             //let edges1 = edgesBool(src, nodeCount, x)
                             TestArray <- TestArray @ [(src, x, nodeCount)]
                             let edges2 = edgesCmd(nodeCount, sink, y)
                             ()
                             //edges2
  | ExecuteChoice(x,y) ->   let edges1 = edgesGC(src, sink, x)  
                            let edges2 = edgesGC(src, sink, y)
                            ()

and DONEGUARD (GC) =
 match GC with
 | ExecuteCondition(x,y) -> NOTExpr(x)
 | ExecuteChoice(x,y) -> BoolLogicAndExpr(DONEGUARD(x), DONEGUARD(y))
 
//variable, array, bool
//F# type expression
// Set of tuples(start end label)

//Node gen: let mutable count := 0 count <- count+1

let genPG e =
  CmdArray <- []
  TestArray <- []
  edgesCmd (0, -1, e)
  (CmdArray, TestArray)