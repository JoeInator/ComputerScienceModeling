// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module CalculatorTypesAST

type expr =
  | Num of float
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | PowExpr of (expr * expr)
  | SqrtExpr of (expr)
  | CubeExpr of (expr)
  | LogExpr of (expr)
  | Log10Expr of (expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
  | Variable of (string)
  | ArrayValue of (string * expr)

type boolExpr =
  | EqualsExpr of (expr * expr)
  | NotEqualsExpr of (expr * expr)
  | LargerThanExpr of (expr * expr)
  | LargerThanOrEqualsExpr of (expr * expr)
  | SmallerThanExpr of (expr * expr)
  | SmallerThanOrEqualsExpr of (expr * expr)
  | NOTExpr of (boolExpr)
  | BoolLogicOrExpr of (boolExpr * boolExpr)
  | BoolLogicAndExpr of (boolExpr * boolExpr)
  | LogicOrExpr of (boolExpr * boolExpr)
  | LogicAndExpr of (boolExpr * boolExpr)
  | TrueOrFalse of (string)

type commands =
  | AssignVariableCommand of (string * expr)
  | AssignArrayValue of (string * expr * expr)
  | SkipOperation
  | ExecuteLoop of (guardedCommands)
  | ExecuteIf of (guardedCommands)
  | CommandSequence of (commands * commands)
and guardedCommands = 
  | ExecuteCondition of (boolExpr * commands)
  | ExecuteChoice of (guardedCommands * guardedCommands)