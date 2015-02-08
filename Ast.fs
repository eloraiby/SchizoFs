//
// Schizo F# Referemce Compiler
// Copyright (C) 2014-2015  Wael El Oraiby
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// 
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
module Ast

open System
open Microsoft.FSharp.Text.Lexing

exception SyntaxError of string

type TokenData = {
    FileName    : string
    LineNumber  : int
    Column      : int
    Offset      : int
}
with
    static member New(file: string, lineNumber: int, column: int, offset: int) = {
        FileName    = file
        LineNumber  = lineNumber
        Column      = column
        Offset      = offset
    }

type Node =
    | Bool     of bool          * TokenData
    | SInt64   of int64         * TokenData
    | Real64   of double        * TokenData 
    | String   of string        * TokenData
    | Symbol   of string        * TokenData
    | Operator of string        * TokenData
    | List     of (Node list)   * TokenData * TokenData
    | FFI      of (Node list -> Thunk<Node>)
    | Special  of (Environment -> Node list -> Thunk<Environment * Node>)
    // error can be a symbol and this can get shadowed if someone redefines it
    // | Error of Node
and Environment = Map<string, Node>
and Thunk<'A> =
    | Continue of (unit -> Thunk<'A>)
    | Final    of 'A

type Thunk<'A>
with
    member x.Value   =
        match x with
        | Continue f -> f().Value
        | Final v    -> v
    
    member x.Step   =
        match x with
        | Continue f -> f()
        | _          -> x

type Node
with
    member x.Eval (env: Environment, l: Node list) : Thunk<Environment * Node> =
        failwith "Unimplemented"

let BoolNode     b   (f, ln, col, off)    = Bool     (b,  (TokenData.New(f, ln, col, off)))
let SInt64Node   si  (f, ln, col, off)    = SInt64   (si, (TokenData.New(f, ln, col, off)))
let Real64Node   r   (f, ln, col, off)    = Real64   (r,  (TokenData.New(f, ln, col, off)))
let StringNode   s   (f, ln, col, off)    = String   (s,  (TokenData.New(f, ln, col, off)))
let SymbolNode   sym (f, ln, col, off)    = Symbol   (sym,(TokenData.New(f, ln, col, off)))
let OperatorNode op  (f, ln, col, off)    = Operator (op, (TokenData.New(f, ln, col, off)))
let ListNode     l   td1 td2              = List (l,  td1, td2)

type OpType =
    | UnOp0 
    | UnOp1 
    | UnOp2 
    | UnOp3 
    | UnOp4 
    | UnOp5 
    | UnOp6 
    | UnOp7 
    | UnOp8 
    | UnOp9 
    | BinOp0
    | BinOp1
    | BinOp2
    | BinOp3
    | BinOp4
    | BinOp5
    | BinOp6
    | BinOp7
    | BinOp8
    | BinOp9


type ParserEnv = Map<string, OpType * TokenData>

type ParserState() =
    let stack   = ref []
    let current = ref Map.empty
    
    member x.CurrentEnv : ParserEnv = (!stack).Head
    member x.PushCurrentEnv()       =
        stack := !current :: !stack
    member x.PopEnv()   =
        stack := (!stack).Tail
    member x.AddOperator(s: string, op: OpType, td: TokenData)  =
        current := (!current).Add(s, (op, td))


(*
let isAlpha = function
    | x when x >= 'a' && x <= 'z' -> Some x
    | x when x >= 'A' && x <= 'Z' -> Some x
    | _ -> None
    
let isDecimal = function
    | x when x >= '0' && x <= '9' -> Some x
    | _ -> None

let isHex = function
    | x when x >= '0' && x <= '9' -> Some x
    | x when x >= 'a' && x <= 'f' -> Some x
    | x when x >= 'A' && x <= 'F' -> Some x
    | _ -> None

let isAlphaNumberic = function
    | x when x >= 'a' && x <= 'z' -> Some x
    | x when x >= 'A' && x <= 'Z' -> Some x
    | x when x >= '0' && x <= '9' -> Some x
    | _ -> None

type internal State = {
    FileName    : string
    String      : string
    Offset      : int
    LineNumber  : int    
}

*)