
open System
#load "CalculatorTypesAST.fs"
open CalculatorTypesAST
#load "CalculatorParser.fs"
open CalculatorParser
#load "CalculatorLexer.fs"
open CalculatorLexer

let mutable nodeCount = 0
let array:List<int * 'T *int> = []

let rec edgesExpr (expr) =
 match expr with
  | Num(x) -> x.ToString()
  | PlusExpr(x,y) -> x.ToString()+"+"+y.ToString()
  | TimesExpr(x,y) -> x.ToString()+"*"+y.ToString()
  | DivExpr(x,y) -> x.ToString()+"/"+y.ToString()
  | MinusExpr(x,y) -> x.ToString()+"-"+y.ToString()
  | PowExpr(x,y) -> x.ToString()+"^"+y.ToString()
  | SqrtExpr(x) -> failwith "Not relevant"
  | CubeExpr(x) -> failwith "Not relevant"
  | LogExpr(x) -> failwith "Not relevant"
  | Log10Expr(x) -> failwith "Not relevant"
  | UPlusExpr(x) -> "+"+x.ToString()
  | UMinusExpr(x) -> "-"+x.ToString()
  | Variable(x) -> x.ToString()
  | ArrayValue(x,y) -> x+"["+edgesExpr(y)+"]"

let rec edgesBool (BE) =
 match BE with
  | EqualsExpr(x,y) -> edgesExpr(x).ToString()+"="+edgesExpr(y).ToString()
  | NotEqualsExpr(x,y) -> edgesExpr(x).ToString()+"!="+edgesExpr(y).ToString()
  | LargerThanExpr(x,y) -> edgesExpr(x).ToString()+">"+edgesExpr(y).ToString()
  | LargerThanOrEqualsExpr(x,y) -> edgesExpr(x).ToString()+">="+edgesExpr(y).ToString()
  | SmallerThanExpr(x,y) -> edgesExpr(x).ToString()+"<"+edgesExpr(y).ToString()
  | SmallerThanOrEqualsExpr(x,y) -> edgesExpr(x).ToString()+"<="+edgesExpr(y).ToString()
  | NOTExpr(x) -> "!"+edgesBool(x)
  | BoolLogicOrExpr(x,y) -> edgesBool(x)+"|"+edgesBool(y)
  | BoolLogicAndExpr(x,y) -> edgesBool(x)+"&"+edgesBool(y)
  | LogicOrExpr(x,y) -> edgesBool(x)+"||"+edgesBool(y)
  | LogicAndExpr(x,y) -> edgesBool(x)+"&&"+edgesBool(y)
  | TrueOrFalse(x) -> x

let rec edgesCmd (src:int, sink:int, commands) =
 match commands with
  | AssignVariableCommand(x,y) -> [(src, x+":="+edgesExpr(y), sink)]
  | AssignArrayValue(x,y,z) -> [(src, x+"["+edgesExpr(y)+"]:="+edgesExpr(z), sink)]
  | SkipOperation -> [(src, "skip", sink)]
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
                             //let edges1 = edgesBool(src, nodeCount, x)
                             let edges1 = [(src, edgesBool(x), nodeCount)]
                             let edges2 = edgesCmd(nodeCount, sink, y)
                             in List.concat [edges1; edges2]  //TODO: We need to append the edges1 somehow, but it's has a bad type (boolExpr)
                             //edges2
  | ExecuteChoice(x,y) ->   let edges1 = edgesGC(src, sink, x)  
                            let edges2 = edgesGC(src, sink, y)
                            in List.concat [edges1; edges2] 

and DONEGUARD (GC) : string =
 match GC with
 | ExecuteCondition(x,y) -> edgesBool(NOTExpr(x))
 | ExecuteChoice(_) -> failwith "Not Implemented"
 (* | ExecuteChoice(x, y) ->  let invBool1 = DONEGUARD(x)
                           let invBool2 = DONEGUARD(y)
                           edgesBool(BoolLogicAndExpr(invBool1, invBool2)) *)
//variable, array, bool
//F# type expression
// Set of tuples(start end label)

//Node gen: let mutable count := 0 count <- count+1