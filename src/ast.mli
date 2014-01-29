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
| SFun of string * string list * source * source
| SAssign of string list * expr
| SIte of bexpr * source * source
| SWhile of bexpr * source
| SSeq of source * source
| SVar of string * expr * source
| SReturn of expr list
| SNop

type 'a inlined =
| IAssign of string * expr * 'a
| IComp of string * string * expr * expr * 'a
| IIte of bexpr * 'a inlined * 'a inlined * 'a
| IWhile of bexpr * 'a inlined * 'a
| ISeq of 'a inlined list
| IPar of 'a inlined list
