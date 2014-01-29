open Ast

let rec canon_expr = function
| Ident id -> (0, [id], [])
| Int n -> (n, [], [])
| Op (a, Add, b) ->
  let ia, pa, na = canon_expr a
  and ib, pb, nb = canon_expr b
  in
  (ia + ib, pa @ pb, na @ nb)
| Op (a, Sub, b) ->
  let ia, pa, na = canon_expr a
  and ib, pb, nb = canon_expr b
  in
  (ia - ib, pa @ nb, na @ pb)
| Op _ 
| App _ -> assert false

(* put an expression in DNF *)
let rec canon_bexpr = function
| Not (Not a) -> canon_bexpr a
| Not (Or (a, b)) -> canon_bexpr (And (Not a, Not b))
| Not (And (a, b)) -> canon_bexpr (Or (Not a, Not b))
| Or (a, b) -> canon_bexpr a @ canon_bexpr b
| And (a, b) ->
  let ca = canon_bexpr a
  and cb = canon_bexpr b
  in
  List.flatten (List.map (fun l -> List.map ((@) l) cb) ca)
| Agent _ | Not (Agent _) as b -> [[b]]
| Cmp _
| Not (Cmp _) -> assert false

let rec gen = function
| IAssign(name, expr, alive) ->
  let n, pos, neg = canon_expr expr in
  ()
