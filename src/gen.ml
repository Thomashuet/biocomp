open Ast

let swap f x y = f y x

(* canonicalize expression into [agents, multiplicity] *)
let rec canon_expr =
  let rec merge (+) a b = match a,b with
  | [], l | l, [] -> l
  | (a, na) :: ta, (b, nb) :: tb when a = b -> (a, na + nb) :: merge (+) ta tb
  | h1 :: t1, h2 :: _ when h1 < h2 -> h1 :: merge (+) t1 b
  | _, (h, nh) :: t -> (h, 0 + nh) :: merge (+) a t
  in
  function
  | Ident id -> [id, 1]
  | Int n -> ["__unit", n]
  | Op (a, Add, b) ->
    merge (+) (canon_expr a) (canon_expr b)
  | Op (a, Sub, b) ->
    merge (-) (canon_expr a) (canon_expr b)
  | Op _
  | App _ -> assert false

let rec and_dnf a b = List.flatten (List.map (fun l -> List.map ((@) l) b) a)

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
  and_dnf ca cb
| Agent _ | Not (Agent _) as b -> [[b]]
| Cmp _
| Not (Cmp _) -> assert false

(* set of reactions with conditions *)
module RC = Set.Make(struct
  type t = bexpr list list * reaction * bexpr list list
  let compare = compare
end)

let rec copy set pre agent =
  let fresh = Opt.new_var () and tmp = Opt.new_var () in
  RC.add
    (and_dnf pre [[Not(Agent fresh)]],
     {reactants = [agent]; products = [tmp; fresh]},
     [[Not(Agent agent)]])
  (RC.add
    ([[Not(Agent agent)]],
     {reactants = [tmp]; products = [agent]},
     [[Not(Agent tmp)]])
    set), [[Not(Agent tmp); Agent fresh]], fresh

let rec list_make n x = if n = 0 then [] else x :: list_make (n-1) x

let rec make_reactions set pre = function
| ISeq l ->
  List.fold_left (fun (s, p) -> make_reactions s p) (set, pre) l
| IAssign(name, expr, alive) ->
  let agents = canon_expr expr in
  let self = try List.assoc name agents with Not_found -> 0 in
  let add (s, p) (agent, mul) =
    let s, pre, agent = 
      if Opt.S.mem agent alive then
        copy s p agent
      else s, pre, agent
    in
    if mul > 0 then
      RC.add (pre, {reactants = [agent]; products = list_make mul name}, [[Not(Agent agent)]]) s,
      and_dnf p [[Not(Agent agent)]]
    else
      RC.add (pre, {reactants = agent :: list_make (-mul) name; products = []}, [[Not(Agent agent)]]) s,
      and_dnf p [[Not(Agent agent)]]
  in
  if self = 1 then
    let agents = List.filter (fun (x, _) -> x <> name) agents in
    List.fold_left add (set, pre) agents
  else
    (* TODO: nullify agent *)
    List.fold_left add (set, pre) agents
| IComp(a, b, _) -> 
  (* a and b were created locally and won't be used again without being reassigned *)
  RC.add (pre, {reactants = [a; b]; products = []}, [[Not(Agent a)]; [Not(Agent b)]]) set,
  [[Not(Agent a)]; [Not(Agent b)]]
| IIte(cond, a, b, _) ->
  let set, posta = make_reactions set (and_dnf pre (canon_bexpr cond)) a in
  let set, postb = make_reactions set (and_dnf pre (canon_bexpr (Not cond)))  b in
  set, and_dnf (canon_bexpr cond) posta @ and_dnf (canon_bexpr (Not cond)) postb
| IWhile(cond, body, _) ->
  (* TODO: implement while *)
  set, pre

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
let new_prod = Opt.new_var

(*
 * To simplify the problem we will consider that we need to introduce
 * a buffer reaction whenever a component appears in a positive form
 * in either pre- or post-condition and in a negative form in the
 * other condition, or vice versa.
*)
module BC = Set.Make(String)

let positive_agents l =
let rec aux set = function
| []      -> set
| x :: xs -> match x with
  | Agent(s)      -> aux (BC.add s set) xs
  | Not(Agent(s)) -> aux set xs
  | _ -> assert false
in List.fold_left BC.union BC.empty (List.map (aux BC.empty) l)

let negative_agents l =
let rec aux set = function
| []      -> set
| x :: xs -> match x with
  | Not(Agent(s)) -> aux (BC.add s set) xs
  | Agent(s)      -> aux set xs
  | _ -> assert false
in List.fold_left BC.union BC.empty (List.map (aux BC.empty) l)

let sat (pre, post) =
let pos_pre  = positive_agents pre in
let neg_pre  = negative_agents pre in
let pos_post = positive_agents post in
let neg_post = negative_agents post in
BC.is_empty (BC.union (BC.inter pos_pre neg_post) (BC.inter neg_pre pos_post))

let contradiction (pre, x, post) = not (sat (pre, post))

let rec negate_reactants = function
| [x]     -> Not(Agent(x))
| x :: xs -> And(Not(Agent(x)), negate_reactants xs)

(*
 * Given a set of reactions and a reaction, add the reaction to the set,
 * possibly modifying it to introduce a buffer reactant.
 *)
let make_buffer set (pre, x, post) =
if contradiction (pre, x, post) then
  let p    = new_prod () in
  let r1   = (pre, { reactants = x.reactants ; products = [p] }, []) in
  let pre2 = canon_bexpr (negate_reactants x.reactants) in
  let r2   = (pre2, { reactants = [p] ; products = x.products }, post) in
  RC.add r2 (RC.add r1 set)
else
  RC.add (pre, x, post) set

let make_all_buffers set =
RC.fold (swap make_buffer) set RC.empty

(* Put precondition into the reaction *)

module R = Set.Make(struct
  type t = reaction
  let compare = compare
end)

let remove_conditions set (pre, x, _) =
  let rec agent_of_bexpr = function
  | Agent a -> a
  | Not(Agent a) -> "__not" ^ a
  | _ -> assert false
  in
  let condition set conj =
    let add (r, p) y =
      if List.mem y r then r, p
      else (y :: r), (y :: p)
    in
    let r, p = List.fold_left add (x.reactants, x.products) (List.map agent_of_bexpr conj) in
    R.add {reactants = r; products = p} set
  in
  List.fold_left condition set pre

let remove_all_conditions set =
  RC.fold (swap remove_conditions) set R.empty

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
let rec split_reaction set x =
  let l = List.length x.reactants in
  if l == 3 then
    let (r1, r2) = splitl 2 x.reactants in
    let p = new_prod () in
    let rr1 = { reactants = r1 ; products = [p] } in
    let rr2 = { reactants = (p :: r2) ; products = x.products } in
    R.add rr1 (R.add rr2 set)
  else if l > 2 then
    let (r1,r2) = splitl (l/2) x.reactants in
    let p1 = new_prod () in
    let p2 = new_prod () in
    let rr1 = { reactants = r1 ; products = [p1] } in
    let rr2 = { reactants = r2 ; products = [p2] } in
    let rr3 = { reactants = [p1 ; p2] ; products = x.products } in
    let set = R.add rr3 set in
    split_reaction (split_reaction set rr2) rr1
  else R.add x set

let split_all_reactions set =
  R.fold (swap split_reaction) set R.empty

let print_reaction r = begin
  let rec print = function
  | [] -> ()
  | [s] -> print_string (s^" ")
  | h :: t -> print_string (h^" + "); print t
  in
  print r.reactants;
  print_string "-> ";
  print r.products;
  print_newline ()
end

let print_all_reactions =
  R.iter print_reaction
