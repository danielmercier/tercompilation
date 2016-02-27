open Ast
open Format
open Printf

let string_of_const = function
    | Cbool b ->
        if b
        then "true"
        else "false"
    | Cint i ->
        Int32.to_string i
    | Cstring s ->
        s
    | Cnull ->
        "null"

let rec string_of_class_expr = function
    | CIdent (id, clist) ->
        (match clist with
        | [] -> id
        | h::q ->
            let params = 
                List.fold_left
                    (fun acc elem ->
                        acc ^ ", " ^ string_of_class_expr elem)
                    (string_of_class_expr h)
                    q
            in
            id ^ "<" ^ params ^ ">")

let string_of_type = function
    | Tbool -> "bool"
    | Tint -> "int"
    | Tvoid -> "void"
    | Tident ce -> string_of_class_expr ce

let string_of_unop = function
    | Uminus -> "unary -"
    | Unot -> "unary not"

let string_of_intbinop = function
    | Add -> "binary +" 
    | Sub -> "binary -" 
    | Mult -> "binary *" 
    | Div -> "binary /"
    | Mod -> "binary %"

let string_of_bineq = function
    | Eq -> "equal" 
    | Neq -> "not equal"
    
let string_of_intbincmp = function
    | Lt -> "binary <"
    | Le -> "binary <="
    | Gt -> "binary >"
    | Ge -> "binary >="

let string_of_boolbinop = function
    | And -> "and"
    | Or -> "or"

(* Ce map permet d'avoir le label des noeuds *)
module Env = Map.Make(String)
let myenv = ref (Env.empty)

(* crée un noveau noeud avec le label str *)
let new_node =
    let c = ref 0 in
    fun str -> 
        let () = incr c in
        let node = sprintf "N_%05i" !c in
        myenv := Env.add node str !myenv;
        node

(* crée le lien entre node1 et node2 *)
let mk_link node1 node2 =
    "\t" ^ node1 ^ " -> " ^ node2 ^ ";\n"

(* crée le lien entre node1 et un nouveau noeud avec le label str *)
let mk_link_and_node node1 str =
    let node2 = new_node str in
    (node2, mk_link node1 node2)


(* cnode est le nom du nom sur lequel doivent être ecrite les expressions *)
let rec string_of_expr cnode exp =
    match exp.value with
    | Const c -> 
        let (_, s) = mk_link_and_node cnode (string_of_const c) in
        s

    | Unop (op, nexpr) -> 
        let str = (string_of_unop op) in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr

    | Intbinop (op, nexpr1, nexpr2) ->
        let str = (string_of_intbinop op) in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr1 ^
        string_of_expr node nexpr2

    | Bineq (op, nexpr1, nexpr2) ->
        let str = (string_of_bineq op) in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr1 ^
        string_of_expr node nexpr2

    | Intbincmp (op, nexpr1, nexpr2) ->
        let str = (string_of_intbincmp op) in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr1 ^
        string_of_expr node nexpr2

    | Boolbinop (op, nexpr1, nexpr2) ->
        let str = (string_of_boolbinop op) in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr1 ^
        string_of_expr node nexpr2

    | Instanceof (nexpr, cexpr) ->
        let str = (string_of_class_expr cexpr) in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr

    | Cast (t, nexpr) ->
        let str = "cast in " ^ (string_of_type t) in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr

    | Affect (nexpr1, nexpr2) ->
        let str = "affect" in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr1 ^
        string_of_expr node nexpr2

    | PostIncr nexpr ->
        let str = "postincr" in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr

    | PreIncr nexpr ->
        let str = "preincr" in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr

    | PostDecr nexpr ->
        let str = "postdecr" in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr

    | PreDecr nexpr ->
        let str = "predecr" in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr

    | New (cexpr, lnexpr) ->
        let str = "new" in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        (List.fold_left
            (fun acc elem ->
                acc ^
                string_of_expr node elem)
            ""
            lnexpr)

    | Ident id ->
        let str = id in
        let (_, s) = mk_link_and_node cnode str in
        s

    | This ->
        let str = "this" in
        let (_, s) = mk_link_and_node cnode str in
        s

    | DotAcces (nexpr, id) ->
        let str = ("dotaccess to " ^ id) in
        let (node, s) = mk_link_and_node cnode str in
        s ^
        string_of_expr node nexpr

    | Appel (nexpr, lnexpr) ->
        let str1 = "appel" in
        let str2 = "args" in
        let (node1, s1) = mk_link_and_node cnode str1 in
        let (node2, s2) = mk_link_and_node node1 str2 in
        s1 ^
        string_of_expr node1 nexpr ^
        (List.fold_left
            (fun acc elem ->
                acc ^
                string_of_expr node2 elem)
            ""
            lnexpr)

let rec string_of_instr cnode = function
    | Nothing ->
        let (_, s) = mk_link_and_node cnode "nothing" in
        s

    | Iexpr nexpr ->
        let (node, s) = mk_link_and_node cnode "iexpr" in
        s ^
        string_of_expr node nexpr

    | Declaration (t, id, onexpr) ->
        let (node1, s1) = mk_link_and_node cnode "declaration" in 
        let (node2, s2) = mk_link_and_node node1 (string_of_type t) in
        let (_, sid) = mk_link_and_node node1 id in
        s1 ^
        s2 ^
        sid ^
        (match onexpr with
        | None -> ""
        | Some nexpr ->
            let (node3, s3) = mk_link_and_node node1 "affect" in
            s3 ^
            string_of_expr node3 nexpr)


    | If (nexpr, instr) ->
        let (node, s) = mk_link_and_node cnode "if" in
        s ^
        string_of_expr node nexpr ^
        string_of_instr node instr
        
    | IfElse (nexpr, instr1, instr2) ->
        let (node1, s1) = mk_link_and_node cnode "if" in
        let (node2, s2) = mk_link_and_node node1 "else" in
        s1 ^
        string_of_expr node1 nexpr ^
        string_of_instr node1 instr1 ^
        string_of_instr node2 instr2

    | For (onexpr1, onexpr2, onexpr3, instr) ->
        let f node = function
            | None -> ""
            | Some nexpr ->
               string_of_expr node nexpr
        in
        let (node, s) = mk_link_and_node cnode "for" in
        s ^
        f node onexpr1 ^
        f node onexpr2 ^
        f node onexpr3 ^
        string_of_instr node instr

    | Bloc b ->
        string_of_bloc cnode b

    | Return onexpr ->
        let (node, s) = mk_link_and_node cnode "return" in
        s ^
        (match onexpr with
         | None -> ""
         | Some nexpr ->
            string_of_expr node nexpr)

and string_of_bloc cnode b =
    let (node, s) = mk_link_and_node cnode "bloc" in
    s ^
    List.fold_left
        (fun acc elem ->
            acc ^
            string_of_instr node elem)
        ""
        b

let string_of_args cnode args = 
    let (node, s) = mk_link_and_node cnode "args" in
    s ^
    List.fold_left
        (fun acc (t, id) ->
            let (node1, s1) = mk_link_and_node node (string_of_type t) in
            let (_, s2) = mk_link_and_node node1 id in
            acc ^
            s1 ^
            s2)
        ""
        args

let string_of_decl cnode = function
    | DeclAtt (t, id) ->
        let (node1, s1) = mk_link_and_node cnode "declatt" in
        let (node2, sid) = mk_link_and_node node1 id in
        let (_, s2) = mk_link_and_node node2 (string_of_type t) in
        s1 ^
        sid ^
        s2 

    | DeclConst (id, args, bloc) ->
        let (node, s) = mk_link_and_node cnode "declconst" in
        let (_, sid) = mk_link_and_node node id in
        s ^
        sid ^
        string_of_args node args ^
        string_of_bloc node bloc

    | DeclMeth (t, id, args, bloc) ->
        let (node1, s1) = mk_link_and_node cnode "declmeth" in
        let (node2, sid) = mk_link_and_node node1 id in
        let (_, s2) = mk_link_and_node node2 (string_of_type t) in
        s1 ^
        sid ^
        s2 ^
        string_of_args node1 args ^
        string_of_bloc node1 bloc

    | DeclNativeMeth (t, id, args) ->
        let (node1, s1) = mk_link_and_node cnode "declnativemeth" in
        let (node2, sid) = mk_link_and_node node1 id in
        let (_, s2) = mk_link_and_node node2 (string_of_type t) in
        s1 ^
        sid ^
        s2 ^
        string_of_args node1 args

let string_of_class_params cnode lid =
    List.fold_left  
        (fun acc id ->
            let (_, s) = mk_link_and_node cnode id in
            acc ^
            s)
        ""
        lid

let string_of_class_def cnode cdef =
    let (id, cparams, ocexpr, ldecl) = cdef in
    let (node1, s1) = mk_link_and_node cnode "classdef" in
    let (_, s2) = mk_link_and_node node1 id in
    let (_, s3) = mk_link_and_node node1 (string_of_class_params node1 cparams) in
    let s4 = (match ocexpr with
                | None -> ""
                | Some cexpr ->
                    let (_, s) = mk_link_and_node node1 (string_of_class_expr cexpr) in
                    s) in
    let s5 =
        List.fold_left
            (fun acc decl ->
                acc ^
                string_of_decl cnode decl)
        ""
        ldecl
    in
    s1 ^ s2 ^ s3 ^ s4 ^ s5

let string_of_class_main cnode cmain = 
    let (nident, ident, bloc) = cmain in
    let (node1, s1) = mk_link_and_node cnode "classmain" in
    let (_, s2) = mk_link_and_node node1 nident.value in
    let (_, s3) = mk_link_and_node node1 ident in
    s1 ^ s2 ^ s3 ^
    string_of_bloc node1 bloc

let string_of_prog prog = 
    let (lcdef, cmain) = prog in
    let node = new_node "prog" in
    let s1 = 
        (List.fold_left
            (fun acc cdef ->
                acc ^
                string_of_class_def node cdef)
            ""
            lcdef)
    in
    let s2 = string_of_class_main node cmain in
    let s3 = 
        (Env.fold
            (fun key data acc ->
                acc ^
                (sprintf "\t%s [label=\"%s\"];\n" key data))
            !myenv
            "")
    in
    "Digraph G {\n" ^
    "\tordering = out;\n" ^
    s1 ^
    s2 ^
    s3 ^
    "}\n"

let print_ast file ast =
    let oc = open_out file in
    fprintf oc "%s" (string_of_prog ast);
    close_out oc
