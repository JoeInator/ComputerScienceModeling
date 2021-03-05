// Implementation file for parser generated by fsyacc
module CalculatorParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 2 "CalculatorParser.fsp"

open CalculatorTypesAST

# 10 "CalculatorParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | POW
  | SQRT
  | CUBE
  | LOG
  | LOG10
  | LPAR
  | RPAR
  | EOF
  | BoolLogicOr
  | BoolLogicAnd
  | LogicOr
  | LogicAnd
  | NOT
  | EQUALS
  | NOTEQUALS
  | LargerThan
  | SmallerThan
  | LargerThanOrEquals
  | SmallerThanOrEquals
  | NUM of (float)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_POW
    | TOKEN_SQRT
    | TOKEN_CUBE
    | TOKEN_LOG
    | TOKEN_LOG10
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_EOF
    | TOKEN_BoolLogicOr
    | TOKEN_BoolLogicAnd
    | TOKEN_LogicOr
    | TOKEN_LogicAnd
    | TOKEN_NOT
    | TOKEN_EQUALS
    | TOKEN_NOTEQUALS
    | TOKEN_LargerThan
    | TOKEN_SmallerThan
    | TOKEN_LargerThanOrEquals
    | TOKEN_SmallerThanOrEquals
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_expression0
    | NONTERM_expression1
    | NONTERM_expression2
    | NONTERM_expression3
    | NONTERM_boolExpression0

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | TIMES  -> 0 
  | DIV  -> 1 
  | PLUS  -> 2 
  | MINUS  -> 3 
  | POW  -> 4 
  | SQRT  -> 5 
  | CUBE  -> 6 
  | LOG  -> 7 
  | LOG10  -> 8 
  | LPAR  -> 9 
  | RPAR  -> 10 
  | EOF  -> 11 
  | BoolLogicOr  -> 12 
  | BoolLogicAnd  -> 13 
  | LogicOr  -> 14 
  | LogicAnd  -> 15 
  | NOT  -> 16 
  | EQUALS  -> 17 
  | NOTEQUALS  -> 18 
  | LargerThan  -> 19 
  | SmallerThan  -> 20 
  | LargerThanOrEquals  -> 21 
  | SmallerThanOrEquals  -> 22 
  | NUM _ -> 23 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_TIMES 
  | 1 -> TOKEN_DIV 
  | 2 -> TOKEN_PLUS 
  | 3 -> TOKEN_MINUS 
  | 4 -> TOKEN_POW 
  | 5 -> TOKEN_SQRT 
  | 6 -> TOKEN_CUBE 
  | 7 -> TOKEN_LOG 
  | 8 -> TOKEN_LOG10 
  | 9 -> TOKEN_LPAR 
  | 10 -> TOKEN_RPAR 
  | 11 -> TOKEN_EOF 
  | 12 -> TOKEN_BoolLogicOr 
  | 13 -> TOKEN_BoolLogicAnd 
  | 14 -> TOKEN_LogicOr 
  | 15 -> TOKEN_LogicAnd 
  | 16 -> TOKEN_NOT 
  | 17 -> TOKEN_EQUALS 
  | 18 -> TOKEN_NOTEQUALS 
  | 19 -> TOKEN_LargerThan 
  | 20 -> TOKEN_SmallerThan 
  | 21 -> TOKEN_LargerThanOrEquals 
  | 22 -> TOKEN_SmallerThanOrEquals 
  | 23 -> TOKEN_NUM 
  | 26 -> TOKEN_end_of_input
  | 24 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_expression0 
    | 3 -> NONTERM_expression0 
    | 4 -> NONTERM_expression0 
    | 5 -> NONTERM_expression1 
    | 6 -> NONTERM_expression1 
    | 7 -> NONTERM_expression1 
    | 8 -> NONTERM_expression2 
    | 9 -> NONTERM_expression2 
    | 10 -> NONTERM_expression2 
    | 11 -> NONTERM_expression2 
    | 12 -> NONTERM_expression2 
    | 13 -> NONTERM_expression2 
    | 14 -> NONTERM_expression3 
    | 15 -> NONTERM_expression3 
    | 16 -> NONTERM_expression3 
    | 17 -> NONTERM_expression3 
    | 18 -> NONTERM_boolExpression0 
    | 19 -> NONTERM_boolExpression0 
    | 20 -> NONTERM_boolExpression0 
    | 21 -> NONTERM_boolExpression0 
    | 22 -> NONTERM_boolExpression0 
    | 23 -> NONTERM_boolExpression0 
    | 24 -> NONTERM_boolExpression0 
    | 25 -> NONTERM_boolExpression0 
    | 26 -> NONTERM_boolExpression0 
    | 27 -> NONTERM_boolExpression0 
    | 28 -> NONTERM_boolExpression0 
    | 29 -> NONTERM_boolExpression0 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 26 
let _fsyacc_tagOfErrorTerminal = 24

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | POW  -> "POW" 
  | SQRT  -> "SQRT" 
  | CUBE  -> "CUBE" 
  | LOG  -> "LOG" 
  | LOG10  -> "LOG10" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | EOF  -> "EOF" 
  | BoolLogicOr  -> "BoolLogicOr" 
  | BoolLogicAnd  -> "BoolLogicAnd" 
  | LogicOr  -> "LogicOr" 
  | LogicAnd  -> "LogicAnd" 
  | NOT  -> "NOT" 
  | EQUALS  -> "EQUALS" 
  | NOTEQUALS  -> "NOTEQUALS" 
  | LargerThan  -> "LargerThan" 
  | SmallerThan  -> "SmallerThan" 
  | LargerThanOrEquals  -> "LargerThanOrEquals" 
  | SmallerThanOrEquals  -> "SmallerThanOrEquals" 
  | NUM _ -> "NUM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | POW  -> (null : System.Object) 
  | SQRT  -> (null : System.Object) 
  | CUBE  -> (null : System.Object) 
  | LOG  -> (null : System.Object) 
  | LOG10  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | BoolLogicOr  -> (null : System.Object) 
  | BoolLogicAnd  -> (null : System.Object) 
  | LogicOr  -> (null : System.Object) 
  | LogicAnd  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | EQUALS  -> (null : System.Object) 
  | NOTEQUALS  -> (null : System.Object) 
  | LargerThan  -> (null : System.Object) 
  | SmallerThan  -> (null : System.Object) 
  | LargerThanOrEquals  -> (null : System.Object) 
  | SmallerThanOrEquals  -> (null : System.Object) 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 14us; 65535us; 0us; 6us; 39us; 4us; 40us; 5us; 48us; 6us; 49us; 6us; 50us; 6us; 51us; 6us; 52us; 6us; 54us; 7us; 55us; 8us; 56us; 9us; 57us; 10us; 58us; 11us; 59us; 12us; 16us; 65535us; 0us; 17us; 13us; 14us; 15us; 16us; 39us; 17us; 40us; 17us; 48us; 17us; 49us; 17us; 50us; 17us; 51us; 17us; 52us; 17us; 54us; 17us; 55us; 17us; 56us; 17us; 57us; 17us; 58us; 17us; 59us; 17us; 23us; 65535us; 0us; 22us; 13us; 22us; 15us; 22us; 18us; 19us; 20us; 21us; 24us; 25us; 26us; 27us; 28us; 29us; 30us; 31us; 32us; 33us; 39us; 22us; 40us; 22us; 48us; 22us; 49us; 22us; 50us; 22us; 51us; 22us; 52us; 22us; 54us; 22us; 55us; 22us; 56us; 22us; 57us; 22us; 58us; 22us; 59us; 22us; 25us; 65535us; 0us; 23us; 13us; 23us; 15us; 23us; 18us; 23us; 20us; 23us; 24us; 23us; 26us; 23us; 28us; 23us; 30us; 23us; 32us; 23us; 34us; 35us; 36us; 37us; 39us; 23us; 40us; 23us; 48us; 23us; 49us; 23us; 50us; 23us; 51us; 23us; 52us; 23us; 54us; 23us; 55us; 23us; 56us; 23us; 57us; 23us; 58us; 23us; 59us; 23us; 7us; 65535us; 0us; 2us; 40us; 47us; 48us; 42us; 49us; 43us; 50us; 44us; 51us; 45us; 52us; 46us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 18us; 35us; 59us; 85us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 5us; 1us; 18us; 19us; 20us; 21us; 1us; 1us; 3us; 2us; 3us; 17us; 9us; 2us; 3us; 17us; 24us; 25us; 26us; 27us; 28us; 29us; 8us; 2us; 3us; 24us; 25us; 26us; 27us; 28us; 29us; 3us; 2us; 3us; 24us; 3us; 2us; 3us; 25us; 3us; 2us; 3us; 26us; 3us; 2us; 3us; 27us; 3us; 2us; 3us; 28us; 3us; 2us; 3us; 29us; 1us; 2us; 3us; 2us; 5us; 6us; 1us; 3us; 3us; 3us; 5us; 6us; 3us; 4us; 5us; 6us; 1us; 5us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 7us; 2us; 8us; 13us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 11us; 1us; 11us; 1us; 12us; 1us; 12us; 1us; 14us; 1us; 14us; 1us; 15us; 1us; 15us; 1us; 16us; 1us; 17us; 2us; 17us; 23us; 1us; 17us; 5us; 18us; 18us; 19us; 20us; 21us; 5us; 18us; 19us; 19us; 20us; 21us; 5us; 18us; 19us; 20us; 20us; 21us; 5us; 18us; 19us; 20us; 21us; 21us; 5us; 18us; 19us; 20us; 21us; 22us; 5us; 18us; 19us; 20us; 21us; 23us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 21us; 1us; 22us; 1us; 23us; 1us; 24us; 1us; 25us; 1us; 26us; 1us; 27us; 1us; 28us; 1us; 29us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 10us; 12us; 16us; 26us; 35us; 39us; 43us; 47us; 51us; 55us; 59us; 61us; 65us; 67us; 71us; 75us; 77us; 79us; 81us; 83us; 85us; 88us; 90us; 92us; 94us; 96us; 98us; 100us; 102us; 104us; 106us; 108us; 110us; 112us; 114us; 116us; 118us; 120us; 123us; 125us; 131us; 137us; 143us; 149us; 155us; 161us; 163us; 165us; 167us; 169us; 171us; 173us; 175us; 177us; 179us; 181us; 183us; |]
let _fsyacc_action_rows = 60
let _fsyacc_actionTableElements = [|9us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 40us; 16us; 52us; 23us; 38us; 0us; 49152us; 5us; 32768us; 11us; 3us; 12us; 48us; 13us; 49us; 14us; 50us; 15us; 51us; 0us; 16385us; 3us; 32768us; 2us; 13us; 3us; 15us; 10us; 41us; 9us; 32768us; 2us; 13us; 3us; 15us; 10us; 41us; 17us; 54us; 18us; 55us; 19us; 56us; 20us; 58us; 21us; 57us; 22us; 59us; 8us; 32768us; 2us; 13us; 3us; 15us; 17us; 54us; 18us; 55us; 19us; 56us; 20us; 58us; 21us; 57us; 22us; 59us; 2us; 16408us; 2us; 13us; 3us; 15us; 2us; 16409us; 2us; 13us; 3us; 15us; 2us; 16410us; 2us; 13us; 3us; 15us; 2us; 16411us; 2us; 13us; 3us; 15us; 2us; 16412us; 2us; 13us; 3us; 15us; 2us; 16413us; 2us; 13us; 3us; 15us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 2us; 16386us; 0us; 18us; 1us; 20us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 2us; 16387us; 0us; 18us; 1us; 20us; 2us; 16388us; 0us; 18us; 1us; 20us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 0us; 16389us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 0us; 16390us; 0us; 16391us; 1us; 16397us; 4us; 24us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 0us; 16392us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 0us; 16393us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 0us; 16394us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 0us; 16395us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 0us; 16396us; 4us; 32768us; 2us; 34us; 3us; 36us; 9us; 39us; 23us; 38us; 0us; 16398us; 4us; 32768us; 2us; 34us; 3us; 36us; 9us; 39us; 23us; 38us; 0us; 16399us; 0us; 16400us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 9us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 40us; 16us; 52us; 23us; 38us; 0us; 16401us; 2us; 16402us; 13us; 49us; 15us; 51us; 0us; 16403us; 2us; 16404us; 13us; 49us; 15us; 51us; 0us; 16405us; 0us; 16406us; 5us; 32768us; 10us; 53us; 12us; 48us; 13us; 49us; 14us; 50us; 15us; 51us; 9us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 40us; 16us; 52us; 23us; 38us; 9us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 40us; 16us; 52us; 23us; 38us; 9us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 40us; 16us; 52us; 23us; 38us; 9us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 40us; 16us; 52us; 23us; 38us; 9us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 40us; 16us; 52us; 23us; 38us; 0us; 16407us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; 8us; 32768us; 2us; 34us; 3us; 36us; 5us; 26us; 6us; 28us; 7us; 30us; 8us; 32us; 9us; 39us; 23us; 38us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 10us; 11us; 17us; 18us; 22us; 32us; 41us; 44us; 47us; 50us; 53us; 56us; 59us; 68us; 71us; 80us; 83us; 86us; 95us; 96us; 105us; 106us; 107us; 109us; 118us; 119us; 128us; 129us; 138us; 139us; 148us; 149us; 158us; 159us; 164us; 165us; 170us; 171us; 172us; 181us; 191us; 192us; 195us; 196us; 199us; 200us; 201us; 207us; 217us; 227us; 237us; 247us; 257us; 258us; 267us; 276us; 285us; 294us; 303us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 3us; 3us; 1us; 3us; 3us; 1us; 3us; 2us; 2us; 2us; 2us; 1us; 2us; 2us; 1us; 3us; 3us; 3us; 3us; 3us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 3us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 5us; 5us; 5us; 5us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16389us; 65535us; 16390us; 16391us; 65535us; 65535us; 16392us; 65535us; 16393us; 65535us; 16394us; 65535us; 16395us; 65535us; 16396us; 65535us; 16398us; 65535us; 16399us; 16400us; 65535us; 65535us; 16401us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16407us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |]
let _fsyacc_reductions ()  =    [| 
# 238 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 247 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "CalculatorParser.fsp"
                                                              _1 
                   )
# 41 "CalculatorParser.fsp"
                 : boolExpr));
# 258 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "CalculatorParser.fsp"
                                                           PlusExpr(_1,_3)  
                   )
# 53 "CalculatorParser.fsp"
                 : expr));
# 270 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "CalculatorParser.fsp"
                                                           MinusExpr(_1,_3) 
                   )
# 54 "CalculatorParser.fsp"
                 : expr));
# 282 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "CalculatorParser.fsp"
                                                           _1               
                   )
# 55 "CalculatorParser.fsp"
                 : expr));
# 293 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "CalculatorParser.fsp"
                                                           TimesExpr(_1,_3) 
                   )
# 58 "CalculatorParser.fsp"
                 : expr));
# 305 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "CalculatorParser.fsp"
                                                           DivExpr(_1,_3)   
                   )
# 59 "CalculatorParser.fsp"
                 : expr));
# 317 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "CalculatorParser.fsp"
                                                           _1               
                   )
# 60 "CalculatorParser.fsp"
                 : expr));
# 328 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "CalculatorParser.fsp"
                                                           PowExpr(_1,_3)   
                   )
# 63 "CalculatorParser.fsp"
                 : expr));
# 340 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "CalculatorParser.fsp"
                                                           SqrtExpr(_2)     
                   )
# 64 "CalculatorParser.fsp"
                 : expr));
# 351 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "CalculatorParser.fsp"
                                                           CubeExpr(_2)     
                   )
# 65 "CalculatorParser.fsp"
                 : expr));
# 362 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "CalculatorParser.fsp"
                                                           LogExpr(_2)      
                   )
# 66 "CalculatorParser.fsp"
                 : expr));
# 373 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "CalculatorParser.fsp"
                                                           Log10Expr(_2)    
                   )
# 67 "CalculatorParser.fsp"
                 : expr));
# 384 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "CalculatorParser.fsp"
                                                           _1               
                   )
# 68 "CalculatorParser.fsp"
                 : expr));
# 395 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "CalculatorParser.fsp"
                                                           UPlusExpr(_2)    
                   )
# 71 "CalculatorParser.fsp"
                 : expr));
# 406 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "CalculatorParser.fsp"
                                                           UMinusExpr(_2)   
                   )
# 72 "CalculatorParser.fsp"
                 : expr));
# 417 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : float)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 73 "CalculatorParser.fsp"
                                                           Num(_1)          
                   )
# 73 "CalculatorParser.fsp"
                 : expr));
# 428 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "CalculatorParser.fsp"
                                                           _2               
                   )
# 74 "CalculatorParser.fsp"
                 : expr));
# 439 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "CalculatorParser.fsp"
                                                                           BoolLogicOrExpr(_1,_3)          
                   )
# 77 "CalculatorParser.fsp"
                 : boolExpr));
# 451 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "CalculatorParser.fsp"
                                                                           BoolLogicAndExpr(_1,_3)         
                   )
# 78 "CalculatorParser.fsp"
                 : boolExpr));
# 463 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "CalculatorParser.fsp"
                                                                           LogicOrExpr(_1,_3)              
                   )
# 79 "CalculatorParser.fsp"
                 : boolExpr));
# 475 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "CalculatorParser.fsp"
                                                                           LogicAndExpr(_1,_3)             
                   )
# 80 "CalculatorParser.fsp"
                 : boolExpr));
# 487 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "CalculatorParser.fsp"
                                                                           NOTExpr(_2)                     
                   )
# 81 "CalculatorParser.fsp"
                 : boolExpr));
# 498 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "CalculatorParser.fsp"
                                                                           _2                              
                   )
# 82 "CalculatorParser.fsp"
                 : boolExpr));
# 509 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 84 "CalculatorParser.fsp"
                                                                           EqualsExpr(_1, _3)              
                   )
# 84 "CalculatorParser.fsp"
                 : boolExpr));
# 521 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 85 "CalculatorParser.fsp"
                                                                           NotEqualsExpr(_1,_3)            
                   )
# 85 "CalculatorParser.fsp"
                 : boolExpr));
# 533 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "CalculatorParser.fsp"
                                                                           LargerThanExpr(_1,_3)           
                   )
# 86 "CalculatorParser.fsp"
                 : boolExpr));
# 545 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 87 "CalculatorParser.fsp"
                                                                           LargerThanOrEqualsExpr(_1,_3)   
                   )
# 87 "CalculatorParser.fsp"
                 : boolExpr));
# 557 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 88 "CalculatorParser.fsp"
                                                                           SmallerThanExpr(_1,_3)          
                   )
# 88 "CalculatorParser.fsp"
                 : boolExpr));
# 569 "CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 89 "CalculatorParser.fsp"
                                                                           SmallerThanOrEqualsExpr(_1,_3)  
                   )
# 89 "CalculatorParser.fsp"
                 : boolExpr));
|]
# 582 "CalculatorParser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 27;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : boolExpr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))