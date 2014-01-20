open Ast

module Env = Map.Make(String)

let ($) f g x = f (g x)

let new_var = let count = ref 0 in function () -> (incr count; "TMP_" ^ string_of_int !count)

exception Variable_undefined of string
exception Function_undefined of string
exception Wrong_number

let find_fun name env = try
  Env.find name env
with Not_found -> raise (Function_undefined name)

let find_var name env = try
  Env.find name env
with Not_found -> raise (Variable_undefined name)

let stdlib = List.fold_left (fun env (name, body) -> Env.add name body env) Env.empty
[]

let rec inline_source funs vars = function
| SNop -> ISeq []
| SFun(f, ans, args, body, next) ->
  inline_source (Env.add f (ans, args, body) funs) vars next
| SAssign(names, App(f, arg_exprs)) -> begin try
    let ans, args, body = find_fun f funs in
    let arg_names = List.map (new_var $ ignore) arg_exprs in
    let new_vars =
      List.fold_left2 (fun env k v -> Env.add k v env)
        (List.fold_left2 (fun env k1 k2 -> Env.add k1 (find_var k2 env) env) vars ans names)
        args arg_names
    in
    ISeq
      (List.fold_left2
        (fun l var expr -> inline_source funs new_vars (SAssign([var], expr)) :: l)
        [] args arg_exprs
      @ List.map (fun k -> IAssign(find_var k new_vars, Int 0, ())) names
      @ [inline_source funs new_vars body])
  with
  | Invalid_argument "List.fold_left2" -> raise Wrong_number
end
| SAssign([name], e) ->
  let before, e_inline  = inline_expr funs vars e in
  ISeq(before @ [IAssign(find_var name vars, e_inline, ())])
| SAssign _ -> raise Wrong_number
| SVar(name, e, s) ->
  let before, e_inline  = inline_expr funs vars e in
  let v = if Env.mem name vars then new_var () else name in
  ISeq(before @ [IAssign(v, e_inline, ()); inline_source funs (Env.add name v vars) s])
| SSeq(s1, s2) -> ISeq [inline_source funs vars s1; inline_source funs vars s2]
| SWhile(b, s) ->
  let before, b_inline = inline_bexpr funs vars b in
  ISeq(before @ [IWhile(b_inline, ISeq(inline_source funs vars s :: before), ())])
| SIte(b, s1, s2) ->
  let before, b_inline = inline_bexpr funs vars b in
  ISeq(before @ [IIte(b_inline, inline_source funs vars s1, inline_source funs vars s2, ())])

and inline_expr funs vars = function
| App(name, arg_exprs) ->
  let ans = new_var () in
  [inline_source funs (Env.add ans ans vars) (SAssign([ans], App(name, arg_exprs)))], Ident ans
| Op(e1, Mul, e2) -> inline_expr funs vars (App("*", [e1; e2]))
| Op(e1, Div, e2) -> inline_expr funs vars (App("/", [e1; e2]))
| Op(e1, Mod, e2) -> inline_expr funs vars (App("%", [e1; e2]))
| Op(e1, op, e2) ->
  let before1, e1_inline = inline_expr funs vars e1
  and before2, e2_inline = inline_expr funs vars e2
  in
  before1 @ before2, Op(e1_inline, op, e2_inline)
| Ident name ->
  [], Ident (find_var name vars)
| Int n -> [], Int n

and inline_bexpr funs vars = function
| Cmp(e1, cmp, e2) ->
  let agent = new_var () in
  let name = match cmp with
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  in
  [inline_source funs (Env.add agent agent vars) (SAssign([agent], App(name, [e1; e2])))], Agent agent
| And(b1, b2) ->
  let before1, b1_inline = inline_bexpr funs vars b1
  and before2, b2_inline = inline_bexpr funs vars b2
  in
  before1 @ before2, And(b1_inline, b2_inline)
| Or(b1, b2) ->
  let before1, b1_inline = inline_bexpr funs vars b1
  and before2, b2_inline = inline_bexpr funs vars b2
  in
  before1 @ before2, Or(b1_inline, b2_inline)
| Not(b) ->
  let before, b_inline = inline_bexpr funs vars b in
  before, Not(b_inline)
| Agent a -> assert false

let rec flatten i =
  let rec aux = function
  | ISeq l -> (List.flatten (List.map aux l))
  | IIte(b, i1, i2, tag) -> [IIte(b, flatten i1, flatten i2, tag)]
  | IWhile(b, i, tag) -> [IWhile(b, flatten i, tag)]
  | IPar l -> [IPar(List.map flatten l)]
  | IAssign(v, e, tag) -> [IAssign(v, e, tag)]
  in
  match aux i with
  | [x] -> x
  | l -> ISeq l

module S = Set.Make(String)

let rec fixpoint (=) f x =
  let y = f x in
  if y = x then y else fixpoint (=) f y

let rec free_expr vars = function
| Ident name -> S.add name vars
| Int _ -> vars
| Op(e1, _, e2) -> free_expr (free_expr vars e1) e2
| App _ -> assert false

let rec free_bexpr vars = function
| Agent name -> S.add name vars
| Or(b1, b2) | And(b1, b2) -> free_bexpr (free_bexpr vars b1) b2
| Not(b) -> free_bexpr vars b
| Cmp _ -> assert false

let rec live vars = function
| IAssign(name, value, _) -> IAssign(name, value, vars), free_expr (S.remove name vars) value
| IIte(b, i1, i2, _) ->
  let tagged_i1, vars_i1 = live vars i1
  and tagged_i2, vars_i2 = live vars i2
  in
  IIte(b, tagged_i1, tagged_i2, vars), free_bexpr (S.union vars_i1 vars_i2) b
| IWhile(b, i, _) ->
  let v = free_bexpr vars b in
  let s = fixpoint S.equal (fun s -> let _, t = live s i in S.union t v) v in
  let tagged, _ = live s i in
  IWhile(b, tagged, vars), s
| ISeq l ->
  let l, v = List.fold_right (fun i (t, v) -> let h, v = live v i in h :: t, v) l ([], vars) in
  ISeq l, v
| IPar l ->
  let l, v = List.fold_left (fun (t, v1) i -> let h, v2 = live vars i in h :: t, S.union v1 v2) ([], vars) l in
  IPar l, v
