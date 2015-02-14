module FFI

open Ast

module private BuiltIn =
    let rec binOpEqual (nl: Node list, td: TokenData) : Node =
        match nl with
        | n0 :: n1 :: [] -> Node.Bool ((n0 = n1), td)
        | _              -> failwith (sprintf "function requires 2 elements! @ Line %d, Column %d" td.LineNumber td.Column)

    let binSInt32 (f: int64 -> int64 -> 'A) (ap: 'A * TokenData -> Node) (nl: Node list, td: TokenData) : Node =
        match nl with
        | Node.SInt64 (n0, _) :: Node.SInt64(n1, _) :: [] -> ap ((f n0 n1), td)
        | _              -> failwith (sprintf "function requires 2 integer elements! @ Line %d, Column %d" td.LineNumber td.Column)

    let binReal32 (f: float -> float -> 'A) (ap: 'A * TokenData -> Node) (nl: Node list, td: TokenData) : Node =
        match nl with
        | Node.Real64 (r0, _) :: Node.Real64(r1, _) :: [] -> ap ((f r0 r1), td)
        | _              -> failwith (sprintf "function requires 2 real elements! @ Line %d, Column %d" td.LineNumber td.Column)


open BuiltIn

let getBuiltIns =
    [|
        "equal?",         Node.FFI binOpEqual

        "sint64.add",     Node.FFI (binSInt32 (+) Node.SInt64)
        "sint64.sub",     Node.FFI (binSInt32 (-) Node.SInt64)
        "sint64.mul",     Node.FFI (binSInt32 (*) Node.SInt64)
        "sint64.div",     Node.FFI (binSInt32 (/) Node.SInt64)
        "sint64.mod",     Node.FFI (binSInt32 (%) Node.SInt64)

        "real64.add",     Node.FFI (binReal32 (+) Node.Real64)
        "real64.sub",     Node.FFI (binReal32 (-) Node.Real64)
        "real64.mul",     Node.FFI (binReal32 (*) Node.Real64)
        "real64.div",     Node.FFI (binReal32 (/) Node.Real64)
        "real64.mod",     Node.FFI (binReal32 (%) Node.Real64)

        "real64.lt?",     Node.FFI (binSInt32 (<)  Node.Bool)
        "real64.gt?",     Node.FFI (binSInt32 (>)  Node.Bool)
        "real64.leq?",    Node.FFI (binSInt32 (<=) Node.Bool)
        "real64.geq?",    Node.FFI (binSInt32 (>=) Node.Bool)
        "real64.eq?",     Node.FFI (binSInt32 (=)  Node.Bool)
        "real64.noteq?",  Node.FFI (binSInt32 (<>) Node.Bool)

        "sint64.lt?",     Node.FFI (binSInt32 (<)  Node.Bool)
        "sint64.gt?",     Node.FFI (binSInt32 (>)  Node.Bool)
        "sint64.leq?",    Node.FFI (binSInt32 (<=) Node.Bool)
        "sint64.geq?",    Node.FFI (binSInt32 (>=) Node.Bool)
        "sint64.eq?",     Node.FFI (binSInt32 (=)  Node.Bool)
        "sint64.noteq?",  Node.FFI (binSInt32 (<>) Node.Bool)
    |]
    |> Map.ofArray

