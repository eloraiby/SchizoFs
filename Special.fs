module Special

open System
open Ast

module private BuiltIn =


    let rec evalOne (env: Environment) (n: Node) : Thunk<Environment * Node> =
        match n with
        | Unit _
        | Bool _
        | SInt64 _
        | Real64 _
        | String _
        | FFI _ 
        | Special _
        | Lambda _          -> Thunk<_>.Final (env, n)
        | Symbol (s, td)    ->
            match env.TryFind s with
            | Some v -> Thunk<_>.Final (env, snd (evalOne env v).Value) // evaluate lazily (macro language)
            | None   -> failwith (sprintf "Couldn't find binding for symbol %s @ line %d, column %d" s td.LineNumber td.Column)
        | Operator (op, td) -> failwith (sprintf "Unexpected operator %s @ line %d, column %d" op td.LineNumber td.Column)
        | List (nl, td)     -> apply env (nl, td)

    and evalList (env: Environment) (nl: Node list) =
        nl
        |> List.map(fun n -> snd (evalOne env n).Value)
     
    and applyLambda (e: EvalArgs) (variadic: ArgsType, syms: Node list) (body: Node list) (env: Environment) (args: Node list, td: TokenData) =
        let symList =
            syms
            |> List.map(function
                        | Node.Symbol (s, td) -> s
                        | x             -> failwith (sprintf "Expected a symbol, got %A @ line %d, column %d" x x.TokenData.LineNumber x.TokenData.Column))


        let t =
            match args with
            | []                    -> failwith (sprintf "<lambda/macro> expected args, got no arguments @ line %d, column %d" td.LineNumber td.Column)
            | Node.Unit _ :: []     -> []
            | t                     -> t
            
        let t =
            match e with
            | EVAL -> evalList env t
            | RAW  -> t

        let env =
            match variadic with
            | Variadic ->
                let argCount = syms.Length
                let _, args, varargs =
                    t
                    |> List.fold(fun (i, args, varargs) e ->
                        if i < argCount 
                        then i + 1, e :: args, varargs
                        else i    , args     , e :: varargs) (0, [], [])

                let env =
                    args
                    |> List.rev
                    |> List.zip symList
                    |> List.fold(fun (acc: Environment) (k, v) -> acc.Add(k, v)) env

                let varargs = varargs |> List.rev

                env.Add ("...",
                         Node.List (Node.Symbol (match e with
                                                 | EVAL -> "list.from"
                                                 | RAW -> "quote"
                                                 , TokenData.New("", 0, 0, 0)) :: varargs, TokenData.New("", 0, 0, 0)))

            | NonVariadic ->
                t
                |> List.zip symList
                |> List.fold(fun (acc: Environment) (k, v) -> acc.Add(k, v)) env

        let initalThunk = Thunk.Final (env, Node.Unit td)

        body
        |> List.fold(fun (acc: Thunk<Environment * Node>) n ->
                        let env, _ = acc.Value
                        evalOne env n) initalThunk
    and apply (env: Environment) (nl: Node list, td: TokenData) =
        match nl with
        | []     -> Thunk.Final (env, Node.List (nl, td))
        | h :: t ->
                                   
            match (evalOne env h).Value with
            | env, FFI f     -> Thunk.Final (env, f ((evalList env t), td))
            | env, Special f -> f env (t, td)
            | env, Lambda ({ EvalArgs = e; ArgSymbols = syms; Body = body }, td) -> applyLambda e syms body env (t, td)

            | xxxx -> failwith "Should never reach this point"

    let rec eval (env: Environment) (nl: Node list, td: TokenData) : Thunk<Environment * Node> =                  
        match nl with
        | x :: []   -> evalOne env x
        | _         -> apply env (nl, td)

    let define (env: Environment) (nl: Node list, td: TokenData) : Thunk<Environment * Node> =
        match nl with
        | Node.Symbol (s, std) :: n :: [] ->
            Thunk.Final (env.Add(s, n), Node.Unit std)
        | x -> failwith (sprintf "define @ line %d, column %d expected to have a symbol and an expression, got %A" td.LineNumber td.Column x)

    let if_then_else (env: Environment) (nl: Node list, td: TokenData) : Thunk<Environment * Node> =
        match nl with
        | cond :: Node.Symbol ("then", _) :: thenBody :: Node.Symbol ("else", _) :: elseBody :: [] ->
            match (evalOne env cond).Value with
            | _, Node.Bool (true, _)    -> evalOne env thenBody
            | _, Node.Bool (flase, _)   -> evalOne env elseBody
            | x                         -> failwith (sprintf "if expression @ line %d, column %d expects a boolean condition, got %A" td.LineNumber td.Column x)
        | x -> failwith (sprintf "if expression @ line %d, column %d should have the form <if (cond) then (then body) else (else body)>, got %A" td.LineNumber td.Column x)

    let lambdaAndMacro (evalArgs: EvalArgs) (env: Environment) (nl: Node list, td: TokenData) : Thunk<Environment * Node> = 
        match nl with
        | Node.List (args, _) :: Node.List (body, _) :: [] ->

            let revSyms = args |> List.rev

            let argSyms =
                match revSyms.Head with
                | Node.Symbol ("...", _) -> Variadic, revSyms.Tail |> List.rev
                | _                      -> NonVariadic, args

            revSyms.Tail
            |> List.iter (function | Node.Symbol ("...", td) -> failwith "<lambda/macro>: ellipsis \"...\" should always be the ending argument"; | _ -> ())

                     
            Thunk.Final(env, Node.Lambda ({ EvalArgs = evalArgs; ArgSymbols = argSyms; Body = body }, td))
        | Node.Unit  _ :: Node.List (body, _) :: []        -> Thunk.Final(env, Node.Lambda ({ EvalArgs = evalArgs; ArgSymbols = NonVariadic, [];   Body = body }, td))
        | x -> failwith (sprintf "lambda expression @ line %d, column %d should have the form <lambda (args...) (body...)>, got %A" td.LineNumber td.Column x)
        
    let quote (env: Environment) (nl: Node list, td: TokenData) : Thunk<Environment * Node> =
        Thunk.Final(env, Node.List(nl, td))
         
open BuiltIn


let getBuiltIns =
    [|
        "lambda", Node.Special (lambdaAndMacro EvalArgs.EVAL)
        "macro",  Node.Special (lambdaAndMacro EvalArgs.RAW)
        "define", Node.Special define
        "if",     Node.Special if_then_else
        "eval",   Node.Special eval
        "quote",  Node.Special quote
    |]
    |> Map.ofArray

let eval (env: Environment) (n: Node option) : Node =
    match n with
    | Some (Node.List (n, td)) ->
        snd (eval env (n, td)).Value
    | Some n -> n
    | None -> Node.Unit (TokenData.New("", 0, 0, 0))
