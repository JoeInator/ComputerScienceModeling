open System


let mutable abstractVariables:Map<string, string> = [(* ("i", +)*)] |> Map.ofList
let mutable abstractArrays:Map<string, string> = [(* ("A", {-,0,+})*)] |> Map.ofList

let extractMemory (dom:Map<string, float>) = 
 
 
 ()