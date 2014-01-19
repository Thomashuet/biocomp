%{
  open Ast
%}

%token DO ELSE FUN IF THEN VAR WHILE
%token AFFECT COMMA EOF SEMICOLON LP RP LB RB
%token NOT AND OR
%token <bool> BOOL
%token <int> INT
%token <string> IDENT
%token ADD SUB MUL DIV MOD
%token EQ NEQ LT LTE GT GTE


%right SEMICOLON IF WHILE VAR LB LP IDENT
%right DO THEN ELSE
%left OR
%left AND
%left NOT
%left ADD SUB
%left MUL DIV MOD
%nonassoc FUN

%start <Ast.source> source

%%

parenthesis(e):
| LP e = e RP { e }
| LB e = e RB { e }

source: s = stmt EOF { s }

stmt:
| s = parenthesis(stmt) { s }
| FUN ans = separated_nonempty_list(COMMA, IDENT) EQ  name = IDENT args = parenthesis(separated_list(COMMA, IDENT))
  body = parenthesis(stmt)
  s = stmt
  { SFun(name, ans, args, body, s) } %prec FUN
| names = separated_nonempty_list(COMMA, IDENT) AFFECT value = expr SEMICOLON { SAssign (names, value) }
| IF cond = bexpr THEN s1 = stmt ELSE s2 = stmt { SIte (cond, s1, s2) }
| IF cond = bexpr THEN s1 = stmt { SIte (cond, s1, SNop) }
| VAR name = IDENT AFFECT value = expr SEMICOLON next = stmt { SVar(name, value, next) }
| VAR name = IDENT AFFECT value = expr SEMICOLON { SVar(name, value, SNop) }
| WHILE cond = bexpr DO s = stmt { SWhile (cond, s) }
| s1 = stmt s2 = stmt { SSeq (s1, s2) } %prec SEMICOLON

comparator:
| EQ { Eq }
| NEQ { Neq }
| LT { Lt }
| LTE { Lte }
| GT { Gt }
| GTE { Gte }

bexpr:
| b = parenthesis(bexpr) { b }
| NOT b = bexpr { Not b }
| b = BOOL { Bool b }
| e1 = expr cmp = comparator e2 = expr { Cmp (e1, cmp, e2) }
| b1 = bexpr OR b2 = bexpr { Or (b1, b2) }
| b1 = bexpr AND b2 = bexpr { And (b1, b2) }

%inline operator:
| ADD { Add }
| SUB { Sub }
| MUL { Mul }
| DIV { Div }
| MOD { Mod }

expr:
| e = parenthesis(expr) { e }
| id = IDENT { Ident id }
| n = INT { Int n }
| e1 = expr op = operator e2 = expr { Op (e1, op, e2) }
| f = IDENT args = parenthesis(separated_list(COMMA, expr)) { App(f, args) }
