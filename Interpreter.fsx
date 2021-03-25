
open FSharp.Collections
open System
#load "CalculatorTypesAST.fs"
open CalculatorTypesAST

//Dette er min exception  
let gfailwith (exncons : string -> #exn) (msg : string) = exncons msg |> raise
let gfailwithf (exncons : string -> #exn) fmt = 
                    Printf.ksprintf (gfailwith exncons) fmt
let failwithf fmt = gfailwithf (fun msg -> new ArgumentException(msg)) fmt

let mutable dom = [(* ("i", 1.0); ("j", 5.0); ("A[0]", 10.0); ("A[1]", 20.0) *)] |> Map.ofList

// Adding a variable with correct reference to to domain configuration
let addToMap(variable: string, value: float) =
 let alreadythere = dom.TryFind(variable)
 match alreadythere with
 /// You aren't allowed add the same variable twice
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
      printfn "%A" (varArr)
      for index in varArr do
        let floatval = float index
        let vari = tempvar.[0].Substring(0, 1)+"["+count.ToString()+"]"
        addToMap(vari, floatval)
        count <- count+1
      
    else
      printfn "%A" (tempvar)
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
 let domEntry = array+"["+Convert.ToInt32(index).ToString()+"]"
 let alreadythere = dom.TryFind(domEntry)
 match alreadythere with
 | Some value -> value
 | None -> failwithf "varible \"%s\" doesn't exist in configuration" domEntry

/// Update value in array e.g. A[1], B[65], C[i] etc.
let updateArray(array: string, index: float, newValue: float) =
 let domEntry = array+"["+Convert.ToInt32(index).ToString()+"]"
 let TempMap =
     dom 
     |> Map.change // Here's the error!
           domEntry
          (fun v -> 
              match v with
              | Some b -> Some newValue
              | None -> None )
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



let execInterpreter (program, initConfig) = 
  dom <- [(* ("i", 1.0); ("j", 5.0); ("A[0]", 10.0); ("A[1]", 20.0) *)] |> Map.ofList
  populateFromConsole(initConfig)
  evalCmd(program)
  printfn "MAP: %A" (dom)
// do a>0 -> a:=a-1; b:=b+1 od
let vars = "a = 5; b = 5; A=[1, 5, 8, 2, 5, 6]"
let prog = ExecuteLoop(ExecuteCondition(LargerThanExpr (Variable "a", Num 0.0), CommandSequence(AssignVariableCommand ("b", PlusExpr(Variable "b", Num 1.0)), AssignVariableCommand("a", MinusExpr(Variable "a", Num 1.0)))))
execInterpreter(prog, vars)
(* 
let input = AssignVariableCommand("i", TimesExpr (Num 5.0, ArrayValue("A", Num 2.0)))
let inputArray = AssignArrayValue ("A", Num 2.0, Num 1.0)
let inputIf = ExecuteIf(ExecuteCondition(SmallerThanExpr (Variable "i", Num 2.0), AssignVariableCommand ("k", Num 0.0)))
let inputIfElse = ExecuteIf(ExecuteChoice(ExecuteCondition(SmallerThanExpr (Variable "i", Num 2.0), AssignVariableCommand ("i", Num 2.0)), ExecuteCondition(LargerThanExpr(Variable "i", Num 2.0), AssignVariableCommand ("i", Num 10.0))))
printfn "MAP: %A" (dom)
addToMap("k", 10.0)
evalCmd(inputArray)
evalCmd(AssignVariableCommand("j", TimesExpr(Variable "i", Num 3.0)))
evalCmd(AssignVariableCommand("k", CubeExpr(Variable "i")))
evalCmd(inputIf)
printfn "MAP: %A" (dom)
populateFromConsole("a = 2; b = 5; B = [1, 5, 8, 2, 5, 6]")
printfn "MAP: %A" (dom)
*)