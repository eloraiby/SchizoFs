﻿module Special

open System
open Ast

module private BuiltIn =

    let rec evalOne (env: Environment) (n: Node) : Thunk =
        match n with
        | Unit _
        | Bool _
        | SInt64 _
        | Real64 _
        | String _
        | FFI _ 
        | Special _
        | Env _
        | Tag _
        | Except _
        | LambdaRawArgs _
        | LambdaEvalArgs _  -> Thunk.Final n
        | Symbol (s, td)    ->
            match env.TryFind s with
            | Some (_, v) -> Thunk.Final ((evalOne env v).Value) // evaluate lazily (macro language/late binding)
            | None        -> Thunk.Final n
        | List (nl, td)     -> apply env (nl, td)

    and evalList (env: Environment) (nl: Node list) =
        nl
        |> List.map(fun n -> (evalOne env n).Value)
     
    and getSymbolList (nl: Node list) =
        nl
        |> List.map
            (function
             | Node.Symbol (s, td) -> s
             | x             -> failwith (sprintf "Expected a symbol, got %A @ line %d, column %d" x x.TokenData.LineNumber x.TokenData.Column))

    and zipVariadicArgs (env: Environment) (syms: string list) (args: Node list) (func: string) =
        let argCount = syms.Length
        let _, args, varargs =
            args
            |> List.fold(fun (i, args, varargs) e ->
                if i < argCount 
                then i + 1, e :: args, varargs
                else i    , args     , e :: varargs) (0, [], [])

        let env =
            args
            |> List.rev
            |> List.zip syms
            |> List.fold(fun (acc: Environment) (k, v) -> acc.Add(k, (Unpinned, v))) env

        let varargs = varargs |> List.rev

        env.Add ("..." , (Unpinned, Node.List (Node.Symbol (func, TokenData.New("", 0, 0, 0)) :: varargs, TokenData.New("", 0, 0, 0))))

    ///
    /// macros
    ///
    and applyLambdaRawArgs (variadic: ArgsType, syms: Node list) (body: Node list) (env: Environment) (args: Node list, td: TokenData) =
        let origEnv = env
        let symList = getSymbolList syms

        let t =
            match args with
            | []                    -> failwith (sprintf "<lambda/macro> expected args, got no arguments @ line %d, column %d" td.LineNumber td.Column)
            | Node.Unit _ :: []     -> []
            | t                     -> t
            
        let env =
            match variadic with
            | Variadic ->
                zipVariadicArgs env symList args "quote"

            | NonVariadic ->
                t
                |> List.zip symList
                |> List.fold(fun (acc: Environment) (k, v) -> acc.Add(k, (Unpinned, v))) env

        let (newEnv, ret: Thunk) = evalBody env body

        //printfn "****\n\n%A\n" ret.Value

        match ret.Value with
        | Node.List(l, td) ->
            match (eval origEnv (l, td)).Value with
            | Node.Env e -> Thunk.Final (Env e)
            | _ -> failwith "macro requires the last statement to be evaluated to env type"
        // evaluate the quote then evaluate the evaluated version
        //match (evalOne origEnv (eval newEnv ret.Value).Value).Value with
        //| Env env -> Thunk<_>.Final (Env env)
        | _ -> failwith "macro requires the last statement to be evaluated to env type"

    and applyLambdaEvalArgs (variadic: ArgsType, syms: Node list) (body: Node list) (env: Environment) (args: Node list, td: TokenData) =
        let origEnv = env
        let symList = getSymbolList syms

        let t =
            match args with
            | []                    -> failwith (sprintf "<lambda/macro> expected args, got no arguments @ line %d, column %d" td.LineNumber td.Column)
            | Node.Unit _ :: []     -> []
            | t                     -> t
            
        let t = evalList env t

        let env =
            match variadic with
            | Variadic ->
                zipVariadicArgs env symList args "list.from"

            | NonVariadic ->
                t
                |> List.zip symList
                |> List.fold(fun (acc: Environment) (k, v) -> acc.Add(k, (Unpinned, v))) env

        let newEnv, ret = evalBody env body

        ret

    and evalBody env body =
        body
        |> List.fold
            (fun (env: Environment, last: Thunk) n ->
                let env =
                    match last.Value with
                    | Node.Env env -> env
                    | _            -> env
                env, evalOne env n) (env, Thunk.Final(Node.Unit TokenData.Empty))


    and apply (env: Environment) (nl: Node list, td: TokenData) =
        match nl with
        | []     -> Thunk.Final (Node.List (nl, td))
        | h :: t ->
                                   
            match (evalOne env h).Value with
            | FFI f     -> Thunk.Final (f ((evalList env t), td))
            | Special f -> f env (t, td)
            | LambdaRawArgs  ({ ArgSymbols = syms; Body = body }, td) -> applyLambdaRawArgs syms body env (t, td)
            | LambdaEvalArgs ({ ArgSymbols = syms; Body = body }, td) -> applyLambdaEvalArgs syms body env (t, td)
            | Symbol (s, td) -> failwith (sprintf "Couldn't find binding for symbol %s @ line %d, column %d" s td.LineNumber td.Column)
            | xxxx -> failwith "Should never reach this point"

    and eval (env: Environment) (nl: Node list, td: TokenData) : Thunk =                  
        match nl with
        | x :: []   -> evalOne env x
        | _         -> apply env (nl, td)

    let if_then_else (env: Environment) (nl: Node list, td: TokenData) : Thunk =
        match nl with
        | cond :: Node.Symbol ("then", _) :: Node.List(thenBody, _) :: Node.Symbol ("else", _) :: Node.List(elseBody, _) :: [] ->
            match (evalOne env cond).Value with
            | Node.Bool (true, _)    -> evalBody env thenBody |> snd
            | Node.Bool (flase, _)   -> evalBody env elseBody |> snd
            | x                      -> failwith (sprintf "if expression @ line %d, column %d expects a boolean condition, got %A" td.LineNumber td.Column x)
        | x -> failwith (sprintf "if expression @ line %d, column %d should have the form <if (cond) then {then body} else {else body}>, got %A" td.LineNumber td.Column x)

    let lambda (lam: LambdaDetail * TokenData -> Node) (env: Environment) (nl: Node list, td: TokenData) : Thunk = 
        match nl with
        | Node.List (args, _) :: Node.List (body, _) :: [] ->

            let revSyms = args |> List.rev

            let argSyms =
                match revSyms.Head with
                | Node.Symbol ("...", _) -> Variadic, revSyms.Tail |> List.rev
                | _                      -> NonVariadic, args

            revSyms.Tail
            |> List.iter
                (function
                 | Node.Symbol ("...", td) -> failwith "<lambda/macro>: ellipsis \"...\" should always be the ending argument"
                 | _ -> ())

                     
            Thunk.Final(lam ({ ArgSymbols = argSyms; Body = body }, td))
        | Node.Unit  _ :: Node.List (body, _) :: []        -> Thunk.Final(lam ({ ArgSymbols = NonVariadic, [];   Body = body }, td))
        | x -> failwith (sprintf "lambda expression @ line %d, column %d should have the form <lambda (args...) (body...)>, got %A" td.LineNumber td.Column x)
        
    let quote (env: Environment) (nl: Node list, td: TokenData) : Thunk =
        // Note: unquote will splice it in place
        let rec transform (n: Node list) =
            match n with
            | Symbol ("unquote", td) :: t ->
                match t with
                | h :: []  -> (evalOne env h).Value
                | h :: tt  -> (apply env (t, td)).Value
                | []       -> failwith "unquote requires arguments!"
            | h :: t ->
                let tail =
                    t
                    |> List.map (fun x ->
                        match x with
                        | Node.List(nl, td) -> transform nl
                        | _ -> x)
                Node.List(h :: tail, td)
            | [] -> failwith "cannot transform nothing"

        Thunk.Final(transform nl)
         
module Symbol =
        let define (env: Environment) (nl: Node list, td: TokenData) : Thunk =
            match nl with
            | Node.Symbol (s, _) :: n :: [] ->
                match env.TryFind s with
                | Some (p, _) ->
                    match p with
                    | Unpinned  -> Thunk.Final(Node.Env (env.Add(s, (Unpinned, n))))
                    | Pinned    -> failwith (sprintf "symbol %s is pinned, cannot redefine it" s)
                | None -> Thunk.Final(Node.Env (env.Add(s, (Unpinned, n))))
            | x -> failwith (sprintf "define @ line %d, column %d expected to have a symbol and an expression, got %A" td.LineNumber td.Column x)

        let assign (env: Environment) (nl: Node list, td: TokenData) : Thunk =
            match nl with
            | Node.Symbol (s, _) :: n :: [] ->
                let n = BuiltIn.evalOne env n
                match env.TryFind s with
                | Some (p, _) ->
                    match p with
                    | Unpinned  -> Thunk.Final(Node.Env (env.Add(s, (Unpinned, n.Value))))
                    | Pinned    -> failwith (sprintf "symbol %s is pinned, cannot redefine it" s)
                | None -> Thunk.Final(Node.Env (env.Add(s, (Unpinned, n.Value))))
            | x -> failwith (sprintf "define @ line %d, column %d expected to have a symbol and an expression, got %A" td.LineNumber td.Column x)

        let pin (env: Environment) (nl: Node list, td: TokenData) : Thunk =
            match nl with
            | Node.Symbol (s, td) :: [] -> 
                match env.TryFind s with
                | Some (p, v) -> Thunk.Final (Env (env.Add(s, (Pinned, v))))
                | None -> failwith (sprintf "symbol %s not found" s)
            | _                     -> failwith (sprintf "pin requires exactly one symbol argument")

        let unpin (env: Environment) (nl: Node list, td: TokenData) : Thunk =
            match nl with
            | Node.Symbol (s, td) :: [] -> 
                match env.TryFind s with
                | Some (p, v) -> Thunk.Final (Env (env.Add(s, (Unpinned, v))))
                | None -> failwith (sprintf "symbol %s not found" s)
            | _                     -> failwith (sprintf "pin requires exactly one symbol argument")



open BuiltIn


let getBuiltIns =
    [|
        "lambda",           (Pinned, Node.Special (lambda Node.LambdaEvalArgs))
        "macro",            (Pinned, Node.Special (lambda Node.LambdaRawArgs))
        "if",               (Pinned, Node.Special if_then_else)
        "eval",             (Pinned, Node.Special eval)
        "quote",            (Pinned, Node.Special quote)
        "define",           (Pinned, Node.Special Symbol.define)
        "symbol.define",    (Pinned, Node.Special Symbol.define)
        "assign",           (Pinned, Node.Special Symbol.assign)
        "symbol.assign",    (Pinned, Node.Special Symbol.assign)
        "symbol.pin",       (Pinned, Node.Special (Symbol.pin))
        "symbol.unpin",     (Pinned, Node.Special (Symbol.unpin))
    |]
    |> Map.ofArray

let eval (env: Environment) (n: Node option) : Node =
    match n with
    | Some (Node.List (n, td)) -> (eval env (n, td)).Value
    | Some n -> n
    | None -> Node.Unit (TokenData.New("", 0, 0, 0))
