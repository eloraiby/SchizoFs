module FFI

open Ast
open Microsoft.FSharp.Collections

module private BuiltIn =
    module Predicate =
        let areEqual (nl: Node list, td: TokenData) : Node =
            match nl with
            | n0 :: n1 :: [] -> Node.Bool ((n0 = n1), td)
            | _              -> failwith (sprintf "function requires 2 elements! @ Line %d, Column %d" td.LineNumber td.Column)
        
        let isSymbol (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.Symbol _ :: [] -> Node.Bool (true, td)
            | _ -> Node.Bool (false, td)

        let isSInt32 (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.SInt64 _ :: [] -> Node.Bool (true, td)
            | _ -> Node.Bool (false, td)

        let isReal64 (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.Real64 _ :: [] -> Node.Bool (true, td)
            | _ -> Node.Bool (false, td)

        let isBool (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.Bool _ :: [] -> Node.Bool (true, td)
            | _ -> Node.Bool (false, td)

        let isString (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.String _ :: [] -> Node.Bool (true, td)
            | _ -> Node.Bool (false, td)

        let isList (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.List _ :: [] -> Node.Bool (true, td)
            | _ -> Node.Bool (false, td)

        let isFFI (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.FFI _ :: [] -> Node.Bool (true, td)
            | _ -> Node.Bool (false, td)

        let isSpecial (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.Special _ :: [] -> Node.Bool (true, td)
            | _ -> Node.Bool (false, td)

    let binSInt32 (f: int64 -> int64 -> 'A) (ap: 'A * TokenData -> Node) (nl: Node list, td: TokenData) : Node =
        match nl with
        | Node.SInt64 (n0, _) :: Node.SInt64(n1, _) :: [] -> ap ((f n0 n1), td)
        | _              -> failwith (sprintf "function requires 2 integer elements! @ Line %d, Column %d" td.LineNumber td.Column)

    let binReal32 (f: float -> float -> 'A) (ap: 'A * TokenData -> Node) (nl: Node list, td: TokenData) : Node =
        match nl with
        | Node.Real64 (r0, _) :: Node.Real64(r1, _) :: [] -> ap ((f r0 r1), td)
        | _              -> failwith (sprintf "function requires 2 real elements! @ Line %d, Column %d" td.LineNumber td.Column)

    let binString (f: string -> string -> 'A) (ap: 'A * TokenData -> Node) (nl: Node list, td: TokenData) : Node =
        match nl with
        | Node.String (s0, _) :: Node.String(s1, _) :: [] -> ap ((f s0 s1), td)
        | _              -> failwith (sprintf "function requires 2 string elements! @ Line %d, Column %d" td.LineNumber td.Column)

    module List =
        let head (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.List (nl, td) :: [] ->
                match nl with
                | n :: _ -> n
                | [] -> failwith (sprintf "head: List @ Line %d, Column %d is empty" td.LineNumber td.Column)
            | x -> failwith (sprintf "head @ Line %d, Column %d : Expecting one list got %A" td.LineNumber td.Column x)

        let tail (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.List (nl, td) :: [] ->
                match nl with
                | _ :: t -> Node.List (t, td)
                | [] -> failwith (sprintf "tail: List @ Line %d, Column %d is empty" td.LineNumber td.Column)
            | x -> failwith (sprintf "tail @ Line %d, Column %d : Expecting one list got %A" td.LineNumber td.Column x)

        let cons (nl: Node list, td: TokenData) : Node =
            match nl with
            | n :: Node.List (nl, td) :: [] -> Node.List (n :: nl, td)
            | x -> failwith (sprintf "<cons node list> @ Line %d, Column %d : Expecting a node and a list got %A" td.LineNumber td.Column x)
    
        let from (nl: Node list, td: TokenData) : Node =
            Node.List (nl, td)

        let rev (nl: Node list, td: TokenData) : Node =
            Node.List (nl |> List.rev, td)
        
    module Symbol =

        let toString (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.Symbol (s, td) :: [] -> Node.String (s, td)
            | x -> failwith (sprintf "<symbol.to_string node> @ Line %d, Column %d : Expecting a symbol got %A" td.LineNumber td.Column x)

        let from (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.String (s, _) :: [] ->
                Node.Symbol (s, td)
            | x -> failwith (sprintf "<symbol.from \"name\"> @ Line %d, Column %d : Expecting a string got %A" td.LineNumber td.Column x)

        let bin (f: string -> string -> 'A) (ap: 'A * TokenData -> Node) (nl: Node list, td: TokenData) : Node =
            match nl with
            | Node.Symbol (s0, _) :: Node.Symbol(s1, _) :: [] -> ap ((f s0 s1), td)
            | _              -> failwith (sprintf "function requires 2 symbol elements! @ Line %d, Column %d" td.LineNumber td.Column)


    module InOut =
        let rec write (nl: Node list, td: TokenData) : Node =
            match nl with
            | h :: t ->
                let rec writeOne n =
                    match n with
                    | Node.Unit   _         -> printf "()"
                    | Node.Bool   (b, _)    -> printf "%b" b
                    | Node.SInt64 (i, _)    -> printf "%d" i
                    | Node.Real64 (r, _)    -> printf "%f" r 
                    | Node.String (s, _)    -> printf "\"%s\"" s  
                    | Node.Symbol (s, _)    -> printf "%s" s 
                    | Node.List   (l, _)    ->
                        match l with
                        | [] -> printf "nil"
                        | l ->
                            l |> List.fold (fun acc n ->
                                                if acc then
                                                    printf " "
                                                    writeOne n
                                                    true
                                                else writeOne n
                                                     true) false
                               |> ignore
                    | Node.FFI     _        -> printf "<ffi>"
                    | Node.Special _        -> printf "<special>"
                    | Node.LambdaRawArgs  _ -> printf "<lambda.rawargs>"
                    | Node.LambdaEvalArgs _ -> printf "<lambda.evalargs>"
                    | Node.Env     env      -> printf "<Environment %A>" env
                    | Node.Tag    (s, n, _) -> printf "<Tag: %s>" s; writeOne n
                    | Node.Except (e, _)    -> printf "<Exception>"; writeOne e
                
                printf "("
                writeOne h
                match t with
                | [] -> ()
                | _  -> printf " "
                        t |> List.map writeOne |> ignore
                printf ")"
            | [] -> printf "nil"
            Node.List ([], td)

open BuiltIn

let getBuiltIns =
    [|
        "pred.equal?",      (Pinned, Node.FFI Predicate.areEqual)
        "pred.symbol?",     (Pinned, Node.FFI Predicate.isSymbol)
        "pred.sint32?",     (Pinned, Node.FFI Predicate.isSInt32)
        "pred.real64?",     (Pinned, Node.FFI Predicate.isReal64)
        "pred.bool?",       (Pinned, Node.FFI Predicate.isBool  )
        "pred.string?",     (Pinned, Node.FFI Predicate.isString)
        "pred.list?",       (Pinned, Node.FFI Predicate.isList  )
        "pred.ffi?",        (Pinned, Node.FFI Predicate.isFFI   )
        "pred.special?",    (Pinned, Node.FFI Predicate.isSpecial)

        "sint64.add",       (Pinned, Node.FFI (binSInt32 (+) Node.SInt64))
        "sint64.sub",       (Pinned, Node.FFI (binSInt32 (-) Node.SInt64))
        "sint64.mul",       (Pinned, Node.FFI (binSInt32 (*) Node.SInt64))
        "sint64.div",       (Pinned, Node.FFI (binSInt32 (/) Node.SInt64))
        "sint64.mod",       (Pinned, Node.FFI (binSInt32 (%) Node.SInt64))

        "real64.add",       (Pinned, Node.FFI (binReal32 (+) Node.Real64))
        "real64.sub",       (Pinned, Node.FFI (binReal32 (-) Node.Real64))
        "real64.mul",       (Pinned, Node.FFI (binReal32 (*) Node.Real64))
        "real64.div",       (Pinned, Node.FFI (binReal32 (/) Node.Real64))
        "real64.mod",       (Pinned, Node.FFI (binReal32 (%) Node.Real64))

        "real64.lt?",       (Pinned, Node.FFI (binSInt32 (<)  Node.Bool))
        "real64.gt?",       (Pinned, Node.FFI (binSInt32 (>)  Node.Bool))
        "real64.leq?",      (Pinned, Node.FFI (binSInt32 (<=) Node.Bool))
        "real64.geq?",      (Pinned, Node.FFI (binSInt32 (>=) Node.Bool))
        "real64.eq?",       (Pinned, Node.FFI (binSInt32 (=)  Node.Bool))
        "real64.noteq?",    (Pinned, Node.FFI (binSInt32 (<>) Node.Bool))

        "sint64.lt?",       (Pinned, Node.FFI (binSInt32 (<)  Node.Bool))
        "sint64.gt?",       (Pinned, Node.FFI (binSInt32 (>)  Node.Bool))
        "sint64.leq?",      (Pinned, Node.FFI (binSInt32 (<=) Node.Bool))
        "sint64.geq?",      (Pinned, Node.FFI (binSInt32 (>=) Node.Bool))
        "sint64.eq?",       (Pinned, Node.FFI (binSInt32 (=)  Node.Bool))
        "sint64.noteq?",    (Pinned, Node.FFI (binSInt32 (<>) Node.Bool))

        "list.head",        (Pinned, Node.FFI List.head)
        "list.tail",        (Pinned, Node.FFI List.tail)
        "list.cons",        (Pinned, Node.FFI List.cons)
        "list.from",        (Pinned, Node.FFI List.from)
        "list.rev",         (Pinned, Node.FFI List.rev )

        "symbol.to_string", (Pinned, Node.FFI Symbol.toString)
        "symbol.from",      (Pinned, Node.FFI Symbol.from)
        "symbol.eq?",       (Pinned, Node.FFI (Symbol.bin (=)  Node.Bool))
        "symbol.noteq?",    (Pinned, Node.FFI (Symbol.bin (<>) Node.Bool))

        "string.eq?",       (Pinned, Node.FFI (binString (=)  Node.Bool))
        "string.noteq?",    (Pinned, Node.FFI (binString (<>) Node.Bool))

        "io.write",         (Pinned, Node.FFI InOut.write)
    |]
    |> Map.ofArray

