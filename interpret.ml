open Error

type value =
  | Vbase of const (* Les types de base du language *)
  | Vclass of class_expr (* Les classes utilisateurs *)

let get_int = function
  | Vbase (Cint i) -> i
  | _ -> raise Interpretation_error 

let get_bool = function
  | Vbase (Cbool b) -> b
  | _ -> raise Interpretation_error 

let get_string = function
  | Vbase (Cstring s) -> s
  | _ -> raise Interpretation_error 

let unop e = function
  | Unot -> let b = get_bool (interpret_expr e) in Vbase (Cbool not b)
  | Uminus -> let i = get_int (interpret_expr e) in Vbase (Cint -i)

and binop e1 e2 = function
  | Add | Sub | Mult | Div | Mod as op ->
    let i1 = get_int (interpret_expr e1)
    and i2 = get_int (interpret_expr e2)
    and f = function
      | Add -> (+) | Sub -> (-) | Mult -> ( * ) | Div -> (/) | Mod -> (mod)
    in
    f op i1 i2

  | Lt | Le | Gt | Ge as op -> 
      let i1 = interpret_expr e1
      and i2 = interpret_expr e2
      and f = function
        | Lt -> (<)
        | Le -> (<=)
        | Gt -> (>)
        | Ge -> (>=)
      in
      f op i1 i2

  | Eq | Neq as op -> 
      let v1 = interpret_expr e1
      and v2 = interpret_expr e2
      and f = function
        | Eq -> (=)
        | Neq -> (<>)
      in
      f op v1 v2

  | And ->
    let b = get_bool (interpret_expr e1) in
    if not b 
    then Vbase (Cbool false)
    else
      let b = get_bool (interpret_expr e2) in
      if not b
      then Vbase (Cbool false)
      else Vbase (Cbool true)

  | Or ->
    let b = get_bool (interpret_expr e1) in
    if b 
    then Vbase (Cbool true)
    else
      let b = get_bool (interpret_expr e2) in
      if b
      then Vbase (Cbool true)
      else Vbase (Cbool false)

and interpret_expr env = function
  | Const c -> Vconst c
  | Unop (op, e) -> unop e op
  | Binop (op, e1, e2) -> binop e1 e2 op
