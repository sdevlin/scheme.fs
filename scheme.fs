module Scheme

type Obj =
  | Undef
  | Lst of Obj list
  | Sym of string
  | Int of bigint
  | Bool of bool
  | Prim of (Obj list -> Obj)
  | Proc of Obj list * Env * Obj list

and Env =
  {
    mutable bindings : Map<string, Obj>;
    parent : Env option
  }

let rec lookup name env =
  match Map.tryFind name env.bindings with
  | Some obj -> obj
  | None ->
    match env.parent with
    | Some env' -> lookup name env'
    | None -> failwith "binding does not exist"

let define name value env =
  env.bindings <- Map.add name value env.bindings

let rec update name value env =
  if Map.containsKey name env.bindings
  then env.bindings <- Map.add name value env.bindings
  else (match env.parent with
        | Some env' -> update name value env'
        | None -> failwith "binding does not exist")

let rec eval env expr =
  match expr with
  | Int _ | Bool _ | Prim _ | Proc (_, _, _) | Undef -> expr
  | Sym s -> lookup s env
  | Lst [Sym "quote"; expr'] -> expr'
  | Lst [Sym "if"; pred; cons; alt] ->
    match eval env pred with
    | Bool false -> eval env alt
    | _ -> eval env cons
  | Lst ((Sym "begin") :: exprs) ->
    let values = exprs |> List.map (eval env) |> List.rev
    match values with
    | value :: _ -> value
    | [] -> Undef
  | Lst (Sym "lambda" :: Lst parms :: body) -> Proc (parms, env, body)
  | Lst [Sym "define"; Sym name; expr'] ->
    let value = eval env expr'
    (define name value env;
     Undef)
  | Lst [Sym "set!"; Sym name; expr'] ->
    let value = eval env expr'
    (update name value env;
     Undef)
  | Lst exprs ->
    match List.map (eval env) exprs with
    | [] -> failwith "bad application"
    | fn :: args -> apply fn args

and apply fn args =
  match fn with
  | Prim fn' -> fn' args
  | Proc (parms, env, body) ->
    let env' = { bindings = Map.empty; parent = Some env }
    let bind (parm, arg) =
      match parm with
      | Sym name -> define name arg env'
      | _ -> failwith "bad parm"
    List.zip parms args |> List.iter bind;
    body |> List.map (eval env') |> List.rev |> List.head
  | _ -> failwith "bad application"

let rec toString = function
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Int n -> sprintf "%A" n
  | Sym s -> s
  | Lst l -> (l
              |> List.map toString
              |> String.concat " "
              |> sprintf "(%s)")
  | Prim _ | Proc (_, _, _) -> "<#procedure>"
  | Undef -> "<#undefined>"

let print = function
  | Undef -> ()
  | _ as obj -> toString obj |> (printfn "%s")

let add args =
  Int (List.fold (fun acc o ->
                  match o with
                  | Int n -> acc + n
                  | _ -> failwith "not an integer") 0I args)

let mul args =
  Int (List.fold (fun acc o ->
                  match o with
                  | Int n -> acc * n
                  | _ -> failwith "not an integer") 1I args)

let globalEnv =
  {
    bindings = Map.ofList [("+", Prim add); ("*", Prim mul)]
    parent = None
  }

let program =
  [Lst [Sym "define";
        Sym "add";
        Lst [Sym "lambda";
             Lst [Sym "x"; Sym "y"];
             Lst [Sym "+"; Sym "x"; Sym "y"]]];
   Lst [Sym "define";
        Sym "cube";
        Lst [Sym "lambda";
             Lst [Sym "x"];
             Lst [Sym "*"; Sym "x"; Sym "x"; Sym "x"]]];
   Lst [Sym "add"; Int 23I; Int 42I];
   Lst [Sym "cube"; Int 17I]]

let main () =
  List.iter print program
  List.iter ((eval globalEnv) >> print) program

main ();;
