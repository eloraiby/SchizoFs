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

    static member Empty = {
        FileName    = ""
        LineNumber  = 0
        Column      = 0
        Offset      = 0
    }

[<CustomEquality; CustomComparison>]
type Node =
    | Unit     of                 TokenData
    | Bool     of bool          * TokenData
    | SInt64   of int64         * TokenData
    | Real64   of double        * TokenData 
    | String   of string        * TokenData
    | Symbol   of string        * TokenData
    | Tag      of string        * Node * TokenData
    | List     of Node list     * TokenData
    | FFI      of (Node list * TokenData -> Node)
    | Special  of (Environment -> (Node list * TokenData) -> Thunk<Node>)
    | LambdaRawArgs  of LambdaDetail * TokenData
    | LambdaEvalArgs of LambdaDetail * TokenData
    | Except   of Node          * TokenData
    | Env      of Environment
    // error can be a symbol and this can get shadowed if someone redefines it
    // | Error of Node
    override x.Equals(obj) =
        match obj with
        | :? Node as y ->
            match x, y with
            | Node.Unit    _,      Node.Unit    _      -> true
            | Node.Bool   (b0, _), Node.Bool   (b1, _) -> b0 = b1
            | Node.SInt64 (i0, _), Node.SInt64 (i1, _) -> i0 = i1
            | Node.Real64 (r0, _), Node.Real64 (r1, _) -> r0 = r1
            | Node.String (s0, _), Node.String (s1, _) -> s0 = s1
            | Node.Symbol (s0, _), Node.Symbol (s1, _) -> s0 = s1
            | Node.List   (l0, _), Node.List   (l1, _) -> l0 = l1
            | Node.Tag    (t0, n0, _), Node.Tag (t1, n1, _)   -> t0 = t1 && n0 = n1
            | Node.FFI     f0,     Node.FFI     f1     -> failwith (sprintf "Cannot compare functions!")
            | Node.Special f0,     Node.Special f1     -> failwith (sprintf "Cannot compare specials!")
            | Node.LambdaRawArgs (ld0, _), Node.LambdaRawArgs (ld1, _) -> ld0 = ld1
            | Node.LambdaEvalArgs (ld0, _), Node.LambdaEvalArgs (ld1, _) -> ld0 = ld1
            | Node.Except (n0, _), Node.Except (n1, _) -> n0 = n1
            | Node.Env     e0,     Node.Env     e1     -> e0 = e1
            | _ -> false
        | _ -> false
    
    override x.GetHashCode() = hash x

    interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? Node as y -> compare x y
          | _ -> invalidArg "yobj" "cannot compare values of different types" 

and Pin =
    | Pinned
    | Unpinned

and ArgsType =
    | Variadic    
    | NonVariadic

and LambdaDetail = {
    ArgSymbols  : ArgsType * Node list
    Body        : Node list
}                    
and Environment = Map<string, Pin * Node>
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
    member x.TokenData =
        match x with
        | Unit     td      -> td
        | Bool     (_, td) -> td
        | SInt64   (_, td) -> td
        | Real64   (_, td) -> td 
        | String   (_, td) -> td
        | Symbol   (_, td) -> td
        | Tag      (_, _, td) -> td
        | List     (_, td) -> td
        | FFI      _       -> failwith "FFI has no token data"
        | Special  _       -> failwith "Special has no token data"
        | LambdaRawArgs   (_, td) -> td
        | LambdaEvalArgs  (_, td) -> td
        | Except   (_, td) -> td
        | Env      _       -> failwith "Environment has no token data"
        
     
let BoolNode     b     (f, ln, col, off)  = Bool     (b,      (TokenData.New(f, ln, col, off)))
let SInt64Node   si    (f, ln, col, off)  = SInt64   (si,     (TokenData.New(f, ln, col, off)))
let Real64Node   r     (f, ln, col, off)  = Real64   (r,      (TokenData.New(f, ln, col, off)))
let StringNode   s     (f, ln, col, off)  = String   (s,      (TokenData.New(f, ln, col, off)))
let SymbolNode   sym   (f, ln, col, off)  = Symbol   (sym,    (TokenData.New(f, ln, col, off)))
let TagNode      str n (f, ln, col, off)  = Tag      (str, n, (TokenData.New(f, ln, col, off)))
let ListNode     l                        = List      l

//
//type ParserEnv = Map<string, OpType * TokenData>
//
//type ParserState() =
//    let stack   = ref []
//    let current = ref Map.empty
//    
//    member x.CurrentEnv : ParserEnv =
//        !current
//
//    member x.PushCurrentEnv()       =
//        stack := !current :: !stack
//
//    member x.PopEnv()   =
//        current := (!stack).Head
//        stack   := (!stack).Tail
//
//    member x.AddOperator(s: string, op: OpType, td: TokenData)  =
//        current := (!current).Add(s, (op, td))
//
//    member x.FindOperator(s: string) =
//        (!current).TryFind s
//
//

[<AutoOpenAttribute>]
module (*private*) Private =

    let (?=) a b =
        if a = b
        then ()
        else raise (Exception(sprintf "%A is not equal to %A" a b))

    let (?<>) a b =
        if a <> b
        then ()
        else raise (Exception(sprintf "%A is equal to %A" a b))


    let isAlpha = function
        | x when x >= 'a' && x <= 'z' -> true
        | x when x >= 'A' && x <= 'Z' -> true
        | _                           -> false
    
    let isDecimal = function
        | x when x >= '0' && x <= '9' -> true
        | _                           -> false

    let isHex = function
        | x when x >= '0' && x <= '9' -> true
        | x when x >= 'a' && x <= 'f' -> true
        | x when x >= 'A' && x <= 'F' -> true
        | _                           -> false

    let isAlphaNumberic = function
        | x when x >= 'a' && x <= 'z' -> true
        | x when x >= 'A' && x <= 'Z' -> true
        | x when x >= '0' && x <= '9' -> true
        | _                           -> false

    let isSpecial x =
        match x with
        | '.' | '\'' | '@' | '#'
        | '?' | '_'             -> true
        | _                     -> false

    let isOperator x =
        match x with
        | '+' | '-' | '~' | '*'
        | '/' | '%' | '|' | '&'
        | '!' | '^' | ',' | ':'
        | '=' | '<' | '>' | '$' -> true
        | _                     -> false


    type Token =
        | TkTrue
        | TkFalse
        | TkSInt64      of int64
        | TkReal64      of double
        | TkSymbol      of string
        | TkString      of string
        | TkSc
        | TkLeftBrace
        | TkRightBrace
        | TkLeftBracket
        | TkRightBracket
        | TkLeftParen
        | TkRightParen
        | TkNone
        | TkInvalid

    type TokenPass = {
        Length      : int
        Token       : Token
    }

    type State = {
        FileName        : string
        String          : string
        AbsoluteOffset  : int
        LineNumber      : int    
    } with
        member x.LengthToEnd    = x.String.Length - x.AbsoluteOffset
        member x.Item i         = x.String.[x.AbsoluteOffset + i]
        member x.GetSlice(s: int option, e: int option) =
            match s with
            | Some s ->
                match e with
                | Some e -> x.String.[s .. e]
                | None   -> x.String.[s ..]
            | None ->
                match e with
                | Some e -> x.String.[ .. e]
                | None   -> x.String

    let (|?>) (a: TokenPass * State, f: State -> TokenPass * State) : TokenPass * State =
        let state = snd a
        let tka   = fst a
        let ret   = f state
        if tka.Length > (fst ret).Length
        then a
        else ret

    // advance as long as predicate is satisfied
    let rec advanceAsLong (s: State) (offset: int) (pred: char -> bool) =
        if offset < s.LengthToEnd && pred s.[offset]
        then advanceAsLong s (offset + 1) pred
        else offset

    // advance until the predicate is satisfied
    let rec advanceUntil (s: State) (offset: int) (pred: char -> bool) =
        if offset < s.LengthToEnd && not (pred s.[offset])
        then advanceUntil s (offset + 1) pred
        else offset

    /// op = op+
    let readOperator (s: State) =
        let opEnd = advanceAsLong s 0 isOperator
        if opEnd <> 0
        then
            let sym = s.[0 .. opEnd - 1]
            if sym = "//" || sym = "/*" || sym = "*/"
            then { Length = 0
                   Token  = TkNone }
                 , s
            else { Length = opEnd
                   Token  = TkSymbol sym }
                 , s
        else { Length = 0
               Token  = TkNone }
             , s

    /// symbol = (alpha | special) (alpha | special | digit)+
    let readSymbol (s: State) =
        if s.LengthToEnd > 0 && (isAlpha s.[0] || isSpecial s.[0])
        then
             let opEnd = advanceAsLong s 1 (fun x -> isAlpha x || isSpecial x || isDecimal x)
             { Length = opEnd
               Token  = TkSymbol s.String.[0 .. opEnd - 1] }
             , s
        else { Length = 0
               Token = TkNone }
             , s

    // "//"any* -> \r\n
    let readComment0 (s: State) =
        if s.LengthToEnd >= 2 && s.[0] = '/' && s.[1] = '/'
        then
            let opEnd = advanceUntil s 2 (fun x -> match x with | '\r' | '\n' -> true | _ -> false)
            { Length = opEnd
              Token = TkNone }
            , s
        else { Length = 0
               Token  = TkNone }
             , s

    let unitTestFunction (f: State -> TokenPass * State) (l: (string * (Token -> unit) * (int -> unit)) list) =
        l
        |> List.map
            (fun (str, tkF, lF) ->
                let state = { State.AbsoluteOffset = 0; State.FileName = ""; State.LineNumber = 0; State.String = str }
                let tok, state = f state
                tkF tok.Token
                lF tok.Length)
        |> ignore

    let testAll() =
        let operatorTests = [
            "",             ((?=) TkNone),              ((?=) 0)
            "/*/",          ((?=) (TkSymbol "/*/")),    ((?=) 3)
            "//",           ((?=) TkNone),              ((?=) 0)
            "/***/ hello",  ((?=) (TkSymbol "/***/")),  ((?=) 5)
            "/*\n",         ((?=) TkNone),              ((?=) 0)
        ]

        let comment0Tests = [
            "",             ((?=) TkNone),              ((?=) 0)
            "/*/",          ((?=) TkNone),              ((?=) 0)
            "//",           ((?=) TkNone),              ((?=) 2)
            "// hello",     ((?=) TkNone),              ((?=) 8)
            "//\n",         ((?=) TkNone),              ((?=) 2)
        ]

        operatorTests
        |> unitTestFunction readOperator

        comment0Tests
        |> unitTestFunction readComment0

        // 




            





