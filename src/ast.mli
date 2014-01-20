type operator =
| Add
| Sub
| Mul
| Div
| Mod

type expr =
| Ident of string
| Int of int
| Op of expr * operator * expr
| App of string * expr list

type comparator =
| Eq
| Neq
| Lt
| Lte
| Gt
| Gte

type bexpr =
| Agent of string
| Cmp of expr * comparator * expr
| Or of bexpr * bexpr
| And of bexpr * bexpr
| Not of bexpr

type source =
| SFun of string * string list * string list * source * source
| SAssign of string list * expr
| SIte of bexpr * source * source
| SWhile of bexpr * source
| SSeq of source * source
| SVar of string * expr * source
| SNop

type inlined =
| IAssign of string * expr
| IIte of bexpr * inlined * inlined
| IWhile of bexpr * inlined
| ISeq of inlined list
| IPar of inlined list

type reaction = { reactants : (int * string) list; products : (int * string) list; is_fast : bool }
