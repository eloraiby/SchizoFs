module Special

open System
open Ast

module private BuiltIn =

    let rec eval (env: Environment, n: Node, td) : Thunk =
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
        | Macro _
        | Function _  -> Thunk.Final n
        | Symbol (s, td)    ->
            match env.TryFind s with
            | Some v -> Thunk.Final ((eval (env, v, td)).Value) // evaluate lazily (macro language/late binding)
            | None   -> Thunk.Final n
        | List (h :: t, td)     ->
            match (eval (env, h, td)).Value with
            | FFI f     -> Thunk.Final (f (evalList td (env, t), td))
            | Special f -> f (env, t, td)
            | Macro    ({ ArgSymbols = syms; Body = body }, td) -> applyMacro    (syms, body, env, t, td)
            | Function ({ ArgSymbols = syms; Body = body }, td) -> applyFunction (syms, body, env, t, td)
            | Symbol (s, td) -> failwith (sprintf "Couldn't find binding for symbol %s @ line %d, column %d" s td.LineNumber td.Column)
            | retVal -> failwith (sprintf "can't apply arguments %A to constant %A @ line %d, column %d" t retVal td.LineNumber td.Column)
        | List ([], td)     -> failwith "Should never reach this point"

    and evalList td (env: Environment, nl: Node list) =
        nl
        |> List.map(fun n -> (eval (env, n, td)).Value)
     
    and getSymbolList (nl: Node list) =
        nl
        |> List.map
            (function
             | Node.Symbol (s, td) -> s
             | x             -> failwith (sprintf "Expected a symbol, got %A @ line %d, column %d" x x.TokenData.LineNumber x.TokenData.Column))

    and applyLambda (argEvalFunc: Environment * Node list -> Node list, symName: string) (syms: Node list, body: Node list, env: Environment, args: Node list, td: TokenData) =
        let origEnv = env
        let symList = getSymbolList syms

        let t =
            match args with
            | []                    -> failwith (sprintf "<lambda/macro> expected args, got no arguments @ line %d, column %d" td.LineNumber td.Column)
            | Node.Unit _ :: []     -> []
            | t                     -> t
            
        let t = argEvalFunc (env, t)

        let env =
            t
            |> List.zip symList
            |> List.fold(fun (acc: Environment) (k, v) -> acc.Add(k, v)) env

        let newEnv, ret = evalBody td (env, body)

        ret
    
    and applyMacro (syms: Node list, body: Node list, env: Environment, args: Node list, td: TokenData) =
        let origEnv = env
        let ret : Thunk = applyLambda ((fun (_, nl) -> nl), "quote") (syms, body, env, args, td)
        let ret = ret.Value
        match ret with
        | Node.List(l, td) ->
            match (eval (origEnv, ret, td)).Value with
            | Node.Env e -> Thunk.Final (Env e)
            | _ -> failwith "macro requires the last statement to be evaluated to env type"

        | _ -> failwith "macro requires the last statement to be evaluated to env type"

    and applyFunction (syms: Node list, body: Node list, env: Environment, args: Node list, td: TokenData) =
        applyLambda (evalList td, "list.from") (syms, body, env, args, td)

    and evalBody td (env, body: Node list) : Environment * Thunk =
        let origEnv = env
        body
        |> List.fold
            (fun (env: Environment, last: Thunk) n ->
                let last = last.Value
                match last with
                | Node.Except (x, _) -> (origEnv, Thunk.Final last)
                | _ ->
                    let env =
                        match last with
                        | Node.Env env -> env
                        | _            -> env
                    env, eval (env, n, td)) (env, Thunk.Final(Node.Unit TokenData.Empty))

    let if_then_else (env: Environment, nl: Node list, td: TokenData) : Thunk =
        match nl with
        | cond :: Node.Symbol ("then", _) :: Node.List(thenBody, _) :: Node.Symbol ("else", _) :: Node.List(elseBody, _) :: [] ->
            match (eval (env, cond, td)).Value with
            | Node.Bool (true, _)    -> evalBody td (env, thenBody) |> snd
            | Node.Bool (flase, _)   -> evalBody td (env, elseBody) |> snd
            | x                      -> failwith (sprintf "if expression @ line %d, column %d expects a boolean condition, got %A" td.LineNumber td.Column x)
        | x -> failwith (sprintf "if expression @ line %d, column %d should have the form <if (cond) then {then body} else {else body}>, got %A" td.LineNumber td.Column x)

    let lambda (lam: LambdaDetail * TokenData -> Node) (env: Environment, nl: Node list, td: TokenData) : Thunk = 
        match nl with
        | Node.List (args, _) :: Node.List (body, _) :: [] -> Thunk.Final(lam ({ ArgSymbols = args; Body = body }, td))
        | Node.Unit  _ :: Node.List (body, _) :: []        -> Thunk.Final(lam ({ ArgSymbols = [];   Body = body }, td))
        | x -> failwith (sprintf "lambda expression @ line %d, column %d should have the form <lambda (args...) (body...)>, got %A" td.LineNumber td.Column x)
        
    let quote (env: Environment, nl: Node list, td: TokenData) : Thunk =
        // Note: unquote will splice it in place
        let rec transform (n: Node list) =
            match n with
            | Symbol ("unquote", td) :: t ->
                match t with
                | h :: tt  -> (eval (env, h, td)).Value
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

    let tryWith (env: Environment, nl: Node list, td: TokenData) : Thunk =
        match nl with
        | Node.List (tryBody, _) :: Symbol("with", td) :: Symbol(sym, _) :: withBody :: [] ->
            let _, ret = (evalBody td (env, tryBody))
           
            let v = ret.Value
            match ret.Value with
            | Node.Except (n, td) -> Thunk.Continue (fun _ -> eval (env.Add(sym, n), withBody, td))
            | _ -> Thunk.Final v
        | _ -> failwith "try ... with ... syntax error"
         
module Symbol =
        // bind a value to a symbol without evaluating
        let define (env: Environment, nl: Node list, td: TokenData) : Thunk =
            match nl with
            | Node.Symbol (s, _) :: n :: [] -> Thunk.Final(Node.Env (env.Add(s, n)))
            | x -> failwith (sprintf "define @ line %d, column %d expected to have a symbol and an expression, got %A" td.LineNumber td.Column x)

        // evaluate before binding the value to the symbol
        let assign (env: Environment, nl: Node list, td: TokenData) : Thunk =
            match nl with
            | Node.Symbol (s, _) :: n :: [] ->
                let n = BuiltIn.eval (env, n, td)
                Thunk.Final(Node.Env (env.Add(s, n.Value)))
            | x -> failwith (sprintf "define @ line %d, column %d expected to have a symbol and an expression, got %A" td.LineNumber td.Column x)

        // forget a symbol
        let forget (env: Environment, nl: Node list, td: TokenData) : Thunk =
            match nl with
            | Node.Symbol (s, td) :: [] -> 
                match env.TryFind s with
                | Some v -> Thunk.Final (Env (env.Remove s))
                | None -> failwith (sprintf "symbol %s not found" s)
            | _                     -> failwith (sprintf "forget requires exactly one symbol argument")

        // add a symbol to an environment
        let add2env (env: Environment, nl: Node list, td: TokenData) : Thunk =
            match nl with
            | Node.Env env :: Node.Symbol (s, td) :: n :: [] -> 
                match env.TryFind s with
                | Some v -> Thunk.Final (Env (env.Remove s))
                | None -> failwith (sprintf "symbol %s not found" s)
            | _                     -> failwith (sprintf "Error: add2env <env> <symbol> <body>")

open BuiltIn

let _eval (env: Environment, nl: Node list, td: TokenData) : Thunk  =
    eval (env, (Node.List (nl, td)), td)

let getBuiltIns =
    [|
        "lambda",           Node.Special (lambda Node.Function)
        "macro",            Node.Special (lambda Node.Macro)
        "if",               Node.Special if_then_else
        "eval",             Node.Special _eval
        "quote",            Node.Special quote
        "define",           Node.Special Symbol.define
        "assign",           Node.Special Symbol.assign
        "forget",           Node.Special Symbol.forget
        "try",              Node.Special tryWith
    |]
    |> Map.ofArray

let eval (env: Environment) (n: Node option) : Node =
    match n with
    | Some (Node.List (nl, td)) -> (evalList td (env, nl)) |> List.rev |> List.head
    | Some n -> n
    | None -> Node.Unit (TokenData.New("", 0, 0, 0))

