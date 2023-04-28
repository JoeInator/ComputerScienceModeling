module Interpreter

open FSharp.Collections
open System
open CalculatorTypesAST

//Dette er min exception  
let gfailwith (exncons : string -> #exn) (msg : string) = exncons msg |> raise
let gfailwithf (exncons : string -> #exn) fmt = 
                    Printf.ksprintf (gfailwith exncons) fmt
let failwithf fmt = gfailwithf (fun msg -> new ArgumentException(msg)) fmt

let mutable dom = [(* ("i", 1.0); ("j", 5.0); ("A[0]", 10.0); ("A[1]", 20.0) *)] |> Map.ofList
let mutable node = 0

// Adding a variable with correct reference to to domain configuration
let addToMap(variable: string, value: float) =
 let alreadythere = dom.TryFind(variable)
 match alreadythere with
 // You aren't allowed add the same variable twice
 | Some value -> failwithf "varible \"%s\" defined twice in initial configuration" variable
 | None -> dom <- dom.Add(variable, value)

// Populating variables with identifyers:string, and values:float
let populateFromConsole (input: string) = 
  let inputs = input.Split([|"; "|], StringSplitOptions.RemoveEmptyEntries)
  for var in inputs do
    let tempvar = var.Trim().Split("=")
    if var.Contains("[") && var.Contains("]") then
      let mutable count = 0
      let i1 = tempvar.[1].IndexOf("[")
      let i2 = tempvar.[1].IndexOf("]")
      let varArr = tempvar.[1].Substring(i1+1, i2-1).Trim().Split(",")
      for index in varArr do
        let floatval = float index
        let vari = tempvar.[0].Substring(0, 1)+"["+count.ToString()+"]"
        addToMap(vari, floatval)
        count <- count+1
      
    else
      let floatval = float tempvar.[1]
      addToMap(tempvar.[0].Trim(), floatval)

/// For looking up a primitive value e.g. i, k, j etc.
let lookupPrimitive(variable: string) =
 let alreadythere = dom.TryFind(variable)
 match alreadythere with
 | Some value -> value
 | None -> failwithf "varible \"%s\" doesn't exist in configuration" variable

/// Update a primitive value e.g. i,j,k,l etc.
let updatePrimitive(variable: string, newValue: float) =
 let TempMap =
     dom 
     |> Map.change // Here's the error!
          variable 
          (fun v -> 
              match v with
              | Some b -> Some newValue
              | None -> None )
 dom <- TempMap

/// Look up a value in the array e.g. A[1], B[65], C[i] etc.
let lookupArray(array: string, index: float) =
 if index < 0.0 then failwithf "Index out of bounds: collection A can't have negative indices"
 let domEntry = array+"["+Convert.ToInt32(index).ToString()+"]"
 let alreadythere = dom.TryFind(domEntry)
 match alreadythere with
 | Some value -> value
 | None -> failwithf "varible \"%s\" doesn't exist in configuration" domEntry

/// Update value in array e.g. A[1], B[65], C[i] etc.
let updateArray(array: string, index: float, newValue: float) =
 if index < 0.0 then failwithf "Index out of bounds: collection A can't have negative indices"
 let domEntry = array+"["+Convert.ToInt32(index).ToString()+"]"
 let TempMap =
     dom 
     |> Map.change // Here's the error!
           domEntry
          (fun v -> 
              match v with
              | Some b -> Some newValue
              | None -> failwithf "varible \"%s\" doesn't exist in configuration" domEntry )
 dom <- TempMap

let rec evalExpr e =
  match e with
  | Num(x) -> x
  | TimesExpr(x,y) -> evalExpr(x) * evalExpr(y)
  | DivExpr(x,y) -> evalExpr(x) / evalExpr(y)
  | PlusExpr(x,y) -> evalExpr(x) + evalExpr(y)
  | MinusExpr(x,y) -> evalExpr(x) - evalExpr(y)
  | PowExpr(x,y) -> evalExpr(x) ** evalExpr(y)
  | SqrtExpr(x) -> sqrt(evalExpr(x))
  | CubeExpr(x) -> evalExpr(x) ** 3.0
  | LogExpr(x) -> log(evalExpr(x))
  | Log10Expr(x) -> log10(evalExpr(x))
  | UPlusExpr(x) -> evalExpr(x)
  | UMinusExpr(x) -> - evalExpr(x)
  | Variable(x) -> lookupPrimitive(x)
  | ArrayValue(x,y) -> lookupArray(x, evalExpr(y))

let rec evalBool e =
  match e with
  | EqualsExpr(x,y) -> evalExpr(x) = evalExpr(y)
  | NotEqualsExpr(x,y) -> evalExpr(x) <> evalExpr(y)
  | LargerThanExpr(x,y) -> evalExpr(x) > evalExpr(y)
  | LargerThanOrEqualsExpr(x,y) -> evalExpr(x) >= evalExpr(y)
  | SmallerThanExpr(x,y) -> evalExpr(x) < evalExpr(y)
  | SmallerThanOrEqualsExpr(x,y) -> evalExpr(x) <= evalExpr(y)
  | NOTExpr(x) -> not(evalBool(x))
  | BoolLogicOrExpr(x,y) -> evalBool(x) || evalBool(y)
  | LogicOrExpr(x,y) -> evalBool(x) || evalBool(y)
  | BoolLogicAndExpr(x,y) -> evalBool(x) && evalBool(y)
  | LogicAndExpr(x,y) -> evalBool(x) && evalBool(y)
  | TrueOrFalse(x) -> match x with
                      | "true" -> true
                      | "false" -> false
                      | _ -> failwithf "How did you do that"

let rec evalCmd c =
  match c with
  | AssignVariableCommand(x,y) -> updatePrimitive(x, evalExpr(y))
  | AssignArrayValue(x,y,z) -> updateArray(x, evalExpr y, evalExpr z)
  | SkipOperation -> ()
  | ExecuteLoop(x) -> evalDoCmd(x)
  | ExecuteIf(x) -> evaliFCmd(x)
  | CommandSequence(x,y) -> evalCmd(x)
                            evalCmd(y)

and evaliFCmd GC =
  match GC with
  | ExecuteCondition(x,y) -> if evalBool(x) then evalCmd(y)
  | ExecuteChoice(x,y) -> evaliFCmd(x) 
                          evaliFCmd(y)
and evalDoCmd GC =
  match GC with
  | ExecuteCondition(x,y) -> if evalBool(x) then evalCmd(y)
                                                 evalDoCmd(GC)
  | ExecuteChoice(x,y) -> evalDoCmd(x)
                          evalDoCmd(y)


let rec search (start, list:List<int * 'T * int>, index) =
  if index >= list.Length then []
  else 
    let item = list.[index]
    let src, content, sink = item
    if src = start then search(start, list, index+1) @ [item]
    elif src < start then search(start, list, index+1)
    else []

let rec iteratebools (PG: List<int * boolExpr * int>, index) =
  if index = PG.Length then failwithf "STUCK AT: %i" (3)
  let src, content, sink = PG.[index]
  match evalBool(content) with
  | true -> PG.[index]
  | false -> iteratebools(PG, index+1)

let rec evalPG(boolList: List<int * boolExpr * int>, cmdList: List<int * commands * int>) = 
  let corrBools = search(node, boolList, 0)
  let corrCmd = search(node, cmdList, 0)
  if corrBools.Length > 0 then let corrBool = iteratebools(boolList, 0) //do stuff
                               let src, content, sink = corrBool
                               node <- sink
  elif corrBools.Length = 0 && corrCmd.Length > 0 then let src, content, sink = corrCmd.[0]
                                                       evalCmd(content)
                                                       node <- sink
  if node = -1 then ()
  else evalPG(boolList, cmdList)

let InterpretPG (PG, initConfig) = 
  dom <- [] |> Map.ofList
  populateFromConsole(initConfig)
  node <- 0
  evalPG(snd(PG), fst(PG))
  printfn "MAP: %A" (dom)
  ()

let InterpretParser (program, initConfig) = 
  dom <- [] |> Map.ofList
  populateFromConsole(initConfig)
  evalCmd(program)
  printfn "MAP: %A" (dom)
// do a>=0 -> b:=b+A[a]; a:=a-1 od
let vars = "a = 5; b = 5; A=[1, 5, 8, 2, 5, 6]"
let testProg = ExecuteLoop(ExecuteCondition(LargerThanOrEqualsExpr (Variable "a", Num 0.0), CommandSequence(AssignVariableCommand ("b", PlusExpr(Variable "b", ArrayValue("A", Variable "a"))), AssignVariableCommand("a", MinusExpr(Variable "a", Num 1.0)))))
let testPG = ([(1,
                AssignVariableCommand
                 ("b", PlusExpr (Variable "b", ArrayValue ("A", Variable "a"))), 2);
                 (2, AssignVariableCommand ("a", MinusExpr (Variable "a", Num 1.0)), 0)],
                [(0, LargerThanExpr (Variable "a", Num 0.0), 1);
                 (0, NOTExpr (LargerThanExpr (Variable "a", Num 0.0)), -1);
                 (6, NOTExpr (LargerThanExpr (Variable "a", Num 0.0)), -1)])
//InterpretParser(prog, vars)
//InterpretPG(testPG, vars)
