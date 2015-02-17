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

// Schizo Macro programming language

open System
open Microsoft.FSharp.Text.Lexing

let parse filename schizo =
    let lexbuf = LexBuffer<char>.FromString schizo
    let res = Parser.start (Lexer.read filename schizo) lexbuf
    res

let (|>!) (a: 'A) (b: 'A -> 'B) = b a |> ignore; a

[<EntryPoint>]
let main argv = 
    if argv.Length = 1 then
        if IO.File.Exists argv.[0] then
            try
                let ffi     = FFI.getBuiltIns             |> Map.toArray
                let special = Special.getBuiltIns         |> Map.toArray
                let env     = Seq.concat [|ffi; special|] |> Map.ofSeq
                
                IO.File.ReadAllText argv.[0]
                |> parse argv.[0]
                |>! printfn "%A"
                |> Special.eval env
                |> ignore


                printfn "%A" argv
                0 // return an integer exit code
            with e ->
                printfn "%s" e.Message
                3
        else
            printfn "Error: file %s doesn't exist" argv.[0]
            2
    else
        printfn "Error: usage schizofs filename"
        1
    