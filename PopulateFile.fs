module PopulateFile
open System.IO // Name spaces can be opened just as modules
open CalculatorTypesAST

let sw = new StringWriter()
let sb = sw.GetStringBuilder()

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

let rec edgesCmd (commands) =
 match commands with
  | AssignVariableCommand(x,y) -> x+":="+edgesExpr(y)
  | AssignArrayValue(x,y,z) -> x+"["+edgesExpr(y)+"]:="+edgesExpr(z)
  | SkipOperation -> "skip"
  | ExecuteLoop(_) -> failwith "Not Implemented"
  | ExecuteIf(_) -> failwith "Not Implemented"
  | CommandSequence(_) -> failwith "Not Implemented"

let WriteFile2(PG:List<int * commands * int> * List<int * boolExpr * int>) =
 //Open a stringbuilder to construct the file content before pusing it.
 printfn "Writing"
 sb.Clear() |> ignore
 //Sets the first line, that are always the same
 sb.Append("digraph program_graph {rankdir=LR;
 node [shape = circle]; q▷;
 node [shape = doublecircle]; q◀;
 node [shape = circle]\n")|> ignore
 let CMD, BOOLS = PG
 //Iterating through the PG array, and formatting the file
 for edge in CMD do
  let src, content, sink = edge
  //Replace start and finish state identifiers with ▷ and ◀
  let src = match src.ToString() with
            | "-1" -> "◀"
            | "0" -> "▷"
            | _ -> src.ToString()

  let sink = match sink.ToString() with
             | "-1" -> "◀"
             | "0" -> "▷"
             | _ -> sink.ToString()          
  sb.AppendFormat("q{0} -> q{1} [label = \"{2}\"]\n", src, sink, edgesCmd(content)) |> ignore

 for edge in BOOLS do
  let src, content, sink = edge
  //Replace start and finish state identifiers with ▷ and ◀
  let src = match src.ToString() with
            | "-1" -> "◀"
            | "0" -> "▷"
            | _ -> src.ToString()

  let sink = match sink.ToString() with
             | "-1" -> "◀"
             | "0" -> "▷"
             | _ -> sink.ToString()          
  sb.AppendFormat("q{0} -> q{1} [label = \"{2}\"]\n", src, sink, edgesBool(content)) |> ignore
 sb.Append("}")|> ignore

 File.WriteAllText("test.dot", sb.ToString())

let WriteFile(PG:seq<int * string * int>) =
 //Open a stringbuilder to construct the file content before pusing it.
 let sw = new StringWriter()
 let sb = sw.GetStringBuilder()
 //Sets the first line, that are always the same
 sb.Append("digraph program_graph {rankdir=LR;
 node [shape = circle]; q▷;
 node [shape = doublecircle]; q◀;
 node [shape = circle]\n")|> ignore

 //Iterating through the PG array, and formatting the file
 for edge in PG do
  let src, content, sink = edge
  //Replace start and finish state identifiers with ▷ and ◀
  let src = match src.ToString() with
            | "-1" -> "◀"
            | "0" -> "▷"
            | _ -> src.ToString()

  let sink = match sink.ToString() with
             | "-1" -> "◀"
             | "0" -> "▷"
             | _ -> sink.ToString()          
  sb.AppendFormat("q{0} -> q{1} [label = \"{2}\"]\n", src, sink, content) |> ignore
 sb.Append("}")|> ignore

 File.WriteAllText("test.gv", sb.ToString())

let testPG =
    ([ (1, AssignVariableCommand("b", PlusExpr(Variable "b", ArrayValue("A", Variable "a"))), 2)
       (2, AssignVariableCommand("a", MinusExpr(Variable "a", Num 1.0)), 0)
       (3, AssignVariableCommand("b", Num 1000.0), 0) ],
     [ (0, LargerThanExpr(Variable "a", Num 0.0), 1)
       (0, EqualsExpr(Variable "a", Num 0.0), 3)
       (0,
        BoolLogicAndExpr(
         NOTExpr(LargerThanExpr(Variable "a", Num 0.0)),
         NOTExpr(EqualsExpr(Variable "a", Num 0.0))
        ),
        -1) ])
// WriteFile2(testPG)