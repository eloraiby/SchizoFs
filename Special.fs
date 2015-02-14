module Special

open System
open Ast

module private BuiltIn =
    type EvalArg =
        | EVALUATED
        | RAW

    let rec evalOne (env: Environment) (n: Node) : Thunk<Environment * Node> =
        match n with
        | Bool _
        | SInt64 _
        | Real64 _
        | String _
        | FFI _ 
        | Special _         -> Thunk<_>.Final (env, n)
        | Symbol (s, td)    -> Thunk<_>.Final (env, env.[s])
        | Operator (op, td) -> failwith (sprintf "Unexpected operator %s @ line %d, column %d" op td.LineNumber td.Column)
        | List (nl, td)     -> apply env (nl, td)

    and evalList (env: Environment) (nl: Node list) =
        nl
        |> List.map(fun n -> snd (evalOne env n).Value)
     
    and  apply (env: Environment) (nl: Node list, td: TokenData) =
        match nl with
        | []     -> Thunk.Final (env, Node.List (nl, td))
        | h :: t ->
                                   
            match (evalOne env h).Value with
            | env, FFI f     -> Thunk.Final (env, f ((evalList env t), td))
            | env, Special f -> f env (t, td)
            | _ -> failwith "Should never reach this point"

    let rec eval (env: Environment) (nl: Node list, td: TokenData) : Thunk<Environment * Node> =                  
        match nl with
        | x :: []   -> evalOne env x
        | _         -> apply env (nl, td)

    let lowLevelLambda (argSyms: Node list) (body: Node list) (evalArgs: EvalArg) (env: Environment) (nl: Node list, td: TokenData) : Thunk<Environment * Node> =
        let symList = argSyms
                      |> List.map(function
                                  | Node.Symbol (s, td) -> s
                                  | x             -> failwith (sprintf "Expected a symbol, got %A @ line %d, column %d" x x.TokenData.LineNumber x.TokenData.Column))

        match nl with
        | (Node.List (args, tdargs)) :: [] ->
            let env = match evalArgs with
                      | EVALUATED   -> evalList env nl
                      | RAW         -> nl

                      |> List.zip symList
                      |> List.fold(fun (acc: Environment) (k, v) -> acc.Add(k, v)) env
        
            let initalThunk = Thunk.Final (env, Node.List ([], td))

            body
            |> List.fold(fun (acc: Thunk<Environment * Node>) n ->
                            let env, _ = acc.Value
                            evalOne env n) initalThunk
        | x -> failwith (sprintf "lambda expression @ line %d, column %d expected (args) ((def | stmt) ...), got %A" td.LineNumber td.Column x)

    let define (env: Environment) (nl: Node list, td: TokenData) : Thunk<Environment * Node> =
        match nl with
        | Node.Symbol (s, std) :: n :: [] ->
            Thunk.Final (env.Add(s, n), Node.List ([], std))
        | x -> failwith (sprintf "define @ line %d, column %d expected to have a symbol and an expression, got %A" td.LineNumber td.Column x)

    let if_then_else (env: Environment) (nl: Node list, td: TokenData) : Thunk<Environment * Node> =
        match nl with
        | cond :: Node.Symbol ("then", _) :: thenBody :: Node.Symbol ("else", _) :: elseBody :: [] ->
            match (evalOne env cond).Value with
            | _, Node.Bool (true, _)    -> evalOne env thenBody
            | _, Node.Bool (flase, _)   -> evalOne env elseBody
            | x                         -> failwith (sprintf "if expression @ line %d, column %d expects a boolean condition, got %A" td.LineNumber td.Column x)
        | x -> failwith (sprintf "if expression @ line %d, column %d should have the form <if (cond) then (then body) else (else body)>, got %A" td.LineNumber td.Column x)

    let lambdaAndMacro (evalArgs: EvalArg) (env: Environment) (nl: Node list, td: TokenData) : Thunk<Environment * Node> = 
        match nl with
        | Node.List (args, _) :: Node.List (body, _) :: [] ->
            Thunk.Final (env, Node.Special (lowLevelLambda args body evalArgs))
        | x -> failwith (sprintf "lambda expression @ line %d, column %d should have the form <lambda (args...) (body...)>, got %A" td.LineNumber td.Column x)
        
open BuiltIn

let getBuiltIns =
    [|
        "lambda", Node.Special (lambdaAndMacro EvalArg.EVALUATED)
        "macro",  Node.Special (lambdaAndMacro EvalArg.RAW)
        "define", Node.Special define
        "if",     Node.Special if_then_else
        "eval",   Node.Special eval
    |]
    |> Map.ofArray

