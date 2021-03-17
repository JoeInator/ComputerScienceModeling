open System.IO // Name spaces can be opened just as modules

let WriteFile(PG:seq<int * string * int>) =
 let sw = new StringWriter()
 let sb = sw.GetStringBuilder() 
 sb.Append("digraph program_graph {rankdir=LR;
 node [shape = circle]; q▷;
 node [shape = doublecircle]; q◀;
 node [shape = circle]\n")|> ignore

 for edge in PG do
  let src, content, sink = edge
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

