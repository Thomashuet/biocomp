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

(* set of reactions with conditions *)
module RC = Set.Make(struct
  type t = bexpr list list * reaction * bexpr list list
  let compare = compare
end)

let rec make_reactions set = function
| IAssign(name, expr, alive) ->
  let n, pos, neg = canon_expr expr in
  ()

(*
 * Given an index and a list, returns a pair of lists, splitted at the given
 * index.
 *)
let splitl n l =
let rec aux k = function
  | (xs,[]) -> (xs,[])
  | (xs,(y :: ys)) ->
      if k == 0 then (xs, (y :: ys)) else aux (k-1) ((y::xs), ys)
in let (l1,l2) = aux n ([],l) in (List.rev l1, l2)

(*
 * Returns a new, unused reactant.
 *)
let new_prod = let count = ref 0 in function () -> (incr count;"__" ^ string_of_int !count)

(*
 * Given a set of reactions (with pre- and post-conditions) and a reaction,
 * split the reaction into multiple reactions so that no reaction has more
 * than two reactants, e.g. the following reaction :
 *
 * R1 + R2 + R3 -> P
 *
 * will become:

 * R1 + R2 -> P'
 * P' + R3 -> P
 *
 * The returned set is the fusion of the given set and the new reactions
 * that were introduced.
 *)
let rec split_reaction set (pre, x, post) =
  let l   = List.length x.reactants in
  let set = RC.remove (pre, x, post) set in
  if l == 3 then
    let (r1, r2) = splitl 2 x.reactants in
    let p = new_prod () in
    let rr1 = (pre, { reactants = r1 ; products = [p] }, post) in
    let rr2 = (pre, { reactants = (p :: r2) ; products = x.products }, post) in
    RC.add rr1 (RC.add rr2 set)
  else if l > 2 then
    let (r1,r2) = splitl (l/2) x.reactants in
    let p1 = new_prod () in
    let p2 = new_prod () in
    let rr1 = (pre, { reactants = r1 ; products = [p1] }, post) in
    let rr2 = (pre, { reactants = r2 ; products = [p2] }, post) in
    let rr3 = (pre, { reactants = [p1 ; p2] ; products = x.products }, post) in
    let set = RC.add rr3 set in
    split_reaction (split_reaction set rr2) rr1
  else RC.add (pre, x, post) set
