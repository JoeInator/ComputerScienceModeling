module ProgramGraph
open System
open CalculatorTypesAST

let mutable nodeCount = 0
let mutable CmdArray:List<int * commands *int> = []
let mutable TestArray:List<int * boolExpr * int> = []

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
                    ()
  | CommandSequence(x,y) -> nodeCount<-nodeCount+1
                            let newnode = nodeCount
                            edgesCmd(src, nodeCount, x)  
                            edgesCmd(newnode, sink, y)
                            ()

and edgesGC (src:int, sink:int, GC) =
 match GC with
  | ExecuteCondition(x,y) -> nodeCount<-nodeCount+1;
                             TestArray <- TestArray @ [(src, x, nodeCount)]
                             edgesCmd(nodeCount, sink, y)
                             ()
  | ExecuteChoice(x,y) ->   edgesGC(src, sink, x)  
                            edgesGC(src, sink, y)
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
  nodeCount <- 0
  (CmdArray, TestArray)