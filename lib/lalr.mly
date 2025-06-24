
%token <(string * Location.location)> TokNumber
%token <(string * Location.location)> TokIdent
%token <(string * Location.location)> TokString
%token <(string * Location.location)> TokNull
%token <(string * Location.location)> TokAdd
%token <(string * Location.location)> TokSub
%token <(string * Location.location)> TokMul
%token <(string * Location.location)> TokDiv
%token <(string * Location.location)> TokMod
%token <(string * Location.location)> TokLt
%token <(string * Location.location)> TokLe
%token <(string * Location.location)> TokGt
%token <(string * Location.location)> TokGe
%token <(string * Location.location)> TokAssign
%token <(string * Location.location)> TokAddAssign
%token <(string * Location.location)> TokSubAssign
%token <(string * Location.location)> TokMulAssign
%token <(string * Location.location)> TokDivAssign
%token <(string * Location.location)> TokEq
%token <(string * Location.location)> TokNeq
%token <(string * Location.location)> TokNot
%token <(string * Location.location)> TokAnd
%token <(string * Location.location)> TokOr
%token <(string * Location.location)> TokYield
%token <(string * Location.location)> TokResume
%token <(string * Location.location)> TokCreate
%token <(string * Location.location)> TokNew
%token <(string * Location.location)> TokLp
%token <(string * Location.location)> TokRp
%token <(string * Location.location)> TokLb
%token <(string * Location.location)> TokRb
%token <(string * Location.location)> TokLs
%token <(string * Location.location)> TokRs
%token <(string * Location.location)> TokSemi
%token <(string * Location.location)> TokColon
%token <(string * Location.location)> TokComa
%token <(string * Location.location)> TokDot
%token <(string * Location.location)> TokReturn
%token <(string * Location.location)> TokBreak
%token <(string * Location.location)> TokContinue
%token <(string * Location.location)> TokIf
%token <(string * Location.location)> TokElse
%token <(string * Location.location)> TokFor
%token <(string * Location.location)> TokWhile
%token <(string * Location.location)> TokLet
%token <(string * Location.location)> TokFn
%token <(string * Location.location)> TokCo
%token <(string * Location.location)> TokStruct
%token <(string * Location.location)> TokType
%token <(string * Location.location)> TokEnd

%start <root> root

%{
    open Ast

    let to_op (tok:token) : Tokenizer.token = match tok with 
    | TokNumber (str,loc) -> {kind = TokNumber; str; loc;}
    | TokIdent (str,loc) -> {kind = TokIdent; str; loc;}
    | TokString (str,loc) -> {kind = TokString; str; loc;}
    | TokNull (str,loc) -> {kind = TokNull; str; loc;}
    | TokAdd (str,loc) -> {kind = TokAdd; str; loc;}
    | TokSub (str,loc) -> {kind = TokSub; str; loc;}
    | TokMul (str,loc) -> {kind = TokMul; str; loc;}
    | TokDiv (str,loc) -> {kind = TokDiv; str; loc;}
    | TokMod (str,loc) -> {kind = TokMod; str; loc;}
    | TokLt (str,loc) -> {kind = TokLt; str; loc;}
    | TokLe (str,loc) -> {kind = TokLe; str; loc;}
    | TokGt (str,loc) -> {kind = TokGt; str; loc;}
    | TokGe (str,loc) -> {kind = TokGe; str; loc;}
    | TokAssign (str,loc) -> {kind = TokAssign; str; loc;}
    | TokAddAssign (str,loc) -> {kind = TokAddAssign; str; loc;}
    | TokSubAssign (str,loc) -> {kind = TokSubAssign; str; loc;}
    | TokMulAssign (str,loc) -> {kind = TokMulAssign; str; loc;}
    | TokDivAssign (str,loc) -> {kind = TokDivAssign; str; loc;}
    | TokEq (str,loc) -> {kind = TokEq; str; loc;}
    | TokNeq (str,loc) -> {kind = TokNeq; str; loc;}
    | TokNot (str,loc) -> {kind = TokNot; str; loc;}
    | TokAnd (str,loc) -> {kind = TokAnd; str; loc;}
    | TokOr (str,loc) -> {kind = TokOr; str; loc;}
    | TokYield (str,loc) -> {kind = TokYield; str; loc;}
    | TokResume (str,loc) -> {kind = TokResume; str; loc;}
    | TokCreate (str,loc) -> {kind = TokCreate; str; loc;}
    | TokNew (str,loc) -> {kind = TokNew; str; loc;}
    | TokLp (str,loc) -> {kind = TokLp; str; loc;}
    | TokRp (str,loc) -> {kind = TokRp; str; loc;}
    | TokLb (str,loc) -> {kind = TokLb; str; loc;}
    | TokRb (str,loc) -> {kind = TokRb; str; loc;}
    | TokLs (str,loc) -> {kind = TokLs; str; loc;}
    | TokRs (str,loc) -> {kind = TokRs; str; loc;}
    | TokSemi (str,loc) -> {kind = TokSemi; str; loc;}
    | TokColon (str,loc) -> {kind = TokColon; str; loc;}
    | TokComa (str,loc) -> {kind = TokComa; str; loc;}
    | TokDot (str,loc) -> {kind = TokDot; str; loc;}
    | TokReturn (str,loc) -> {kind = TokReturn; str; loc;}
    | TokBreak (str,loc) -> {kind = TokBreak; str; loc;}
    | TokContinue (str,loc) -> {kind = TokContinue; str; loc;}
    | TokIf (str,loc) -> {kind = TokIf; str; loc;}
    | TokElse (str,loc) -> {kind = TokElse; str; loc;}
    | TokFor (str,loc) -> {kind = TokFor; str; loc;}
    | TokWhile (str,loc) -> {kind = TokWhile; str; loc;}
    | TokLet (str,loc) -> {kind = TokLet; str; loc;}
    | TokFn (str,loc) -> {kind = TokFn; str; loc;}
    | TokCo (str,loc) -> {kind = TokCo; str; loc;}
    | TokStruct (str,loc) -> {kind = TokStruct; str; loc;}
    | TokType (str,loc) -> {kind = TokType; str; loc;}
    | TokEnd (str,loc) -> {kind = TokEnd; str; loc;}

let global_idx = ref 1000

let next_idx () = 
    global_idx := !global_idx + 1;
    !global_idx


%}

%%

%public arglist(X):
    | {[]}
    | x=X; TokComa; xs=arglist(X) {x::xs}
    | x=X {[x]}

root: 
    | stmts = list(top_stmt); fin = TokEnd { 
    let n = ({node_idx=next_idx();stmts; loc = snd fin; }:root) in
    global_idx := 1000;
    n }

top_stmt:
    | fn_decl { (FnDecl $1:top_stmt) }
    | struct_decl { (StructDecl $1:top_stmt) }
    | co_decl { (CoDecl $1:top_stmt) }
    | let_stmt { (LetStmt $1:top_stmt) }
    | alias_stmt { (AliasStmt $1:top_stmt) }

fn_decl:
    | start=TokFn; name=TokIdent; TokLp; args=arglist(argument); TokRp; ret_type=typ; body=fn_body {
    let (n:fn_decl) = {node_idx=next_idx();parent=ref Invalid; name=fst name; args; ret=ret_type; body; loc = snd start} in 
    n } 

fn_body:
    | block { (Block $1 : stmt) }

argument:
    | name=TokIdent; TokColon; arg_type=typ {
    let n = {node_idx=next_idx();parent=ref Invalid;name=fst name; typ=arg_type;  loc=snd name; } in
    n }

struct_decl:
    | TokStruct; name=TokIdent; TokLb; decls=list(decl); TokRb { 
    let n = {node_idx=next_idx();parent=ref Invalid;name=fst name; decls;  loc = snd name} in
    n }

decl:
    | let_stmt { (LetStmt $1:decl) } 
    | fn_decl { (FnDecl $1:decl) }
    | struct_decl { (StructDecl $1:decl) }
    | co_decl { (CoDecl $1:decl) }
    | field { (Field $1:decl) }

field:
    | field_with_val {$1}
    | field_no_val {$1}

field_with_val: 
    | var_name=TokIdent; TokColon; field_type=typ; TokAssign; value=expr; TokComa {
    let n = {node_idx=next_idx();parent=ref Invalid;name=fst var_name; typ=field_type; value = Some value; loc = snd var_name } in 
    n }

field_no_val: 
    | var_name=TokIdent; TokColon; field_type=typ; TokComa {
    let n = {node_idx=next_idx();parent=ref Invalid;name=fst var_name; typ=field_type; value=None; loc = snd var_name } in
    n }

co_decl:
    | start=TokCo; name=TokIdent; TokLp; args=arglist(argument); TokRp; yield_type=typ; body=fn_body {
    let n = {node_idx=next_idx();parent=ref Invalid;name=fst name; args; yield=yield_type; body; loc = snd start;  } in   
    n }

let_stmt:
    | TokLet; var_name=TokIdent; TokColon; var_type=typ; TokAssign; value=expr; TokSemi {
let (n:let_stmt) = {node_idx=next_idx();parent=ref Invalid;name = fst var_name; typ = var_type; value; loc = snd var_name; } in 
    n }

alias_stmt: 
    | TokType; type_name=TokIdent; TokAssign; other_type=typ; TokSemi {
    let (n:alias_stmt) = {node_idx=next_idx();parent=ref Invalid;name=fst type_name; typ=other_type; loc = snd type_name; } in
    n }
   

typ:
    | parent_type { $1 }
    | fn_type { (FnType $1:typ) }
    | co_type { (CoType $1:typ) }
    | co_obj_type { (CoObjType $1:typ) }

parent_type:
    | named_type { (NamedType $1:typ) }
    | array_type { (ArrayType $1:typ) }
    | dot_type { (DotType $1:typ) }

named_type:
    | name=TokIdent {
    let n = ({node_idx=next_idx();parent=ref Invalid;name=fst name; loc = snd name;}:named_type) in 
    n }

array_type:
    | start=TokLs; elem=typ; TokRs {
    let n = {node_idx=next_idx();parent=ref Invalid;elem; loc = snd start;} in
    n }

fn_type:
    | start=TokFn; TokLp; args=arglist(typ); TokRp; ret=typ {
    let n = {node_idx=next_idx();parent=ref Invalid;args; ret; loc = snd start;} in 
    n }


co_type: 
    | start=TokCo; TokLp; args=arglist(typ); TokRp; yield=typ {
    let n = {node_idx=next_idx();parent=ref Invalid;args; yield; loc = snd start; } in
    n }

co_obj_type:
    | start=TokCo; yield=typ {
    let n = {node_idx=next_idx();parent=ref Invalid;yield; loc = snd start;  } in  
    n }


dot_type:
    | parent=parent_type; TokDot; child=TokIdent {
    let n = {node_idx=next_idx();parent=ref Invalid;namespace=parent; name=fst child; loc = snd child;} in
    n }


body_stmt:
    | block { (Block $1 : stmt) }
    | let_stmt { (LetStmt $1 : stmt) }
    | continue_stmt { (ContinueStmt $1 : stmt) }
    | break_stmt { (BreakStmt $1 : stmt) }
    | return_stmt { (ReturnStmt $1 : stmt) }
    | expr_stmt { (Expr $1 : stmt) }
    | alias_stmt { (AliasStmt $1 : stmt) }
    | yield_stmt { (YieldStmt $1 : stmt) }

stmt:
    | body_stmt {$1}
    | for_loop { (ForLoop $1:stmt) }
    | while_loop { (WhileLoop $1:stmt) }
    | if_stmt { (IfStmt $1 : stmt) }
    | fn_decl { (FnDecl $1:stmt) }
    | co_decl { (CoDecl $1:stmt) }
    | struct_decl { (StructDecl $1:stmt) }
    | if_resume_stmt { (IfResumeStmt $1:stmt) }

block:
    | start=TokLb; stmts=list(stmt); TokRb {
    let n = {node_idx=next_idx();parent=ref Invalid;stmts; loc = snd start;} in
    n }


for_loop:
    | TokFor; TokLp; iter_var=TokIdent; TokColon; iterator=expr; TokRp; body=body_stmt; {
    let n = {node_idx=next_idx();parent=ref Invalid;var=fst iter_var; iterator; body; loc = snd iter_var;} in
    n }

while_loop:
    | start = TokWhile; TokLp; condition=expr; TokRp; body=body_stmt; {
    let n = {node_idx=next_idx();parent=ref Invalid;condition; body; loc = snd start;} in
    n }

continue_stmt:
    | start=TokContinue; TokSemi; {
    let n = ({node_idx=next_idx();parent=ref Invalid;loc = snd start;}:continue_stmt) in   
    n }


break_stmt:
    | start=TokBreak; TokSemi; {
    let n = ({node_idx=next_idx();parent=ref Invalid;loc = snd start;}:break_stmt) in
    n }

if_stmt:
    | if_stmt_no_else {$1}
    | if_stmt_with_else {$1}

if_stmt_no_else:
    | start=TokIf; TokLp; condition=expr; TokRp; if_true=body_stmt;  {
    let n = {node_idx=next_idx();parent=ref Invalid;condition; if_true; if_false=None; loc = snd start;} in
    n }

if_stmt_with_else:
    | start=TokIf; TokLp; condition=expr; TokRp; if_true=body_stmt; TokElse; if_false=stmt; {
    let n = {node_idx=next_idx();parent=ref Invalid;condition; if_true; if_false = Some if_false; loc = snd start;} in
    n }

if_resume_stmt:
    | if_resume_stmt_base TokElse stmt {
    let n = {$1 with if_bad= Some $3} in
    n }
    | if_resume_stmt_base {$1}

if_resume_stmt_base:
    | if_resume_stmt_with_var {$1}
    | if_resume_stmt_void {$1}

if_resume_stmt_with_var:
    | start=TokIf; TokResume; TokLp; var=TokIdent; TokColon; coroutine=expr; TokRp; if_ok=body_stmt; {
    let n = {node_idx=next_idx();parent=ref Invalid; var=Some (fst var); coroutine; if_ok; if_bad=None; loc = snd start;} in
    n }

if_resume_stmt_void:
    | start=TokIf; TokResume; TokLp; coroutine=expr; TokRp; if_ok=body_stmt; {
    let n = {node_idx=next_idx();parent=ref Invalid; var=None; coroutine; if_ok; if_bad=None; loc = snd start;} in
    n }

yield_stmt:
    | start=TokYield; value=option(expr); TokSemi; {
    let n = ({node_idx=next_idx();parent=ref Invalid;value; loc = snd start;}:yield_stmt) in
    n }

return_stmt:
    | start=TokReturn; value = option(expr); TokSemi; {
    let n = {node_idx=next_idx();parent=ref Invalid;value; loc = snd start;} in
    n }

expr_stmt:
    | expr=expr; TokSemi; {expr}

binop0: 
    | TokAnd { ( TokAnd $1, snd $1)}
    | TokOr { ( TokOr $1, snd $1)}

binop1: 
    | TokEq { ( TokEq $1, snd $1)}
    | TokNeq { ( TokNeq $1, snd $1)}
binop2: 
    | TokLt { ( TokLt $1, snd $1)}
    | TokGt { ( TokGt $1, snd $1)}
    | TokLe { ( TokLe $1, snd $1)}
    | TokGe { ( TokGe $1, snd $1)}
binop3:
    | TokAdd { ( TokAdd $1, snd $1)}
    | TokSub { ( TokSub $1, snd $1)}
binop4:
    | TokMul { ( TokMul $1, snd $1)}
    | TokDiv { ( TokDiv $1, snd $1)}
    | TokMod { ( TokMod $1, snd $1)}

assign_op:
    | TokAddAssign { (TokAdd $1, $1) }
    | TokSubAssign { (TokSub $1, $1) }
    | TokMulAssign { (TokMul $1, $1) }
    | TokDivAssign { (TokDiv $1, $1) }

expr:
    | lhs=expr; op=TokAssign; rhs=callable_expr {
    let n = {node_idx=next_idx();parent=ref Invalid;lhs; rhs; op=to_op (TokAssign op); loc = snd op; } in
    BinExpr n }
    | lhs=expr; op=assign_op; rhs=callable_expr {
    let n = {node_idx=next_idx();parent=ref Invalid;lhs; rhs; op=to_op (fst op); loc = snd (snd op); } in
    let n = {node_idx=next_idx();parent=ref Invalid;lhs; rhs=(BinExpr n); op=to_op (TokAssign (snd op)); loc = snd (snd op); } in
    BinExpr n }
    | expr0 {$1}

expr0:
    | lhs=expr0; op=binop0; rhs=expr1; {
    let n = {node_idx=next_idx();parent=ref Invalid;lhs; rhs; op=to_op (fst op); loc = snd op; } in
    BinExpr n }
    | expr1 {$1}

expr1:
    | lhs=expr1; op=binop1; rhs=expr2; {
    let n = {node_idx=next_idx();parent=ref Invalid;lhs; rhs; op=to_op (fst op); loc = snd op; } in
    BinExpr n }
    | expr2 {$1}

expr2:
    | lhs=expr2; op=binop2; rhs=expr3; {
    let n = {node_idx=next_idx();parent=ref Invalid;lhs; rhs; op=to_op (fst op); loc = snd op; } in
    BinExpr n }
    | expr3 {$1}

expr3:
    | lhs=expr3; op=binop3; rhs=expr4; {
    let n = {node_idx=next_idx();parent=ref Invalid;lhs; rhs; op=to_op (fst op); loc = snd op; } in
    BinExpr n }
    | expr4 {$1}

expr4:
    | lhs=expr4; op=binop4; rhs=expr5; {
    let n = {node_idx=next_idx();parent=ref Invalid;lhs; rhs; op=to_op (fst op); loc = snd op; } in
    BinExpr n }
    | expr5 {$1}

expr5:
    | unary_expr { (UnaryExpr $1:expr) }
    | callable_expr { $1 }

callable_expr:
    | resume_expr { (ResumeExpr $1:expr) }
    | create_expr { (CreateExpr $1:expr) }
    | num_expr { (NumExpr $1:expr) }
    | string_expr { (StringExpr $1:expr) }
    | null_literal { (NullLiteral $1:expr) }
    | array_literal { (ArrayLiteral $1:expr) }
    | new_expr { (NewExpr $1:expr) }
    | call_expr { (CallExpr $1:expr) }
    | index_expr { (IndexExpr $1:expr) }
    | dot_expr { (DotExpr $1:expr) }
    | var_expr { (VarExpr $1:expr) }
    | TokLp; sub=expr; TokRp { sub }

unop:
    | TokSub {(TokSub $1, snd $1)}
    | TokNot {(TokNot $1, snd $1)}

unary_expr:
    | op=unop; sub_expr=expr5; {
    let n = {node_idx=next_idx();parent=ref Invalid;op=to_op (fst op); sub_expr; loc = snd op; } in
    n }

call_expr:
    | fn=callable_expr; loc=TokLp; args=arglist(expr); TokRp; {
    let n = {node_idx=next_idx();parent=ref Invalid;fn; args; loc = snd loc;} in
    n }

index_expr:
    | arr=callable_expr; loc=TokLs; ids=arglist(expr); TokRs; {
    let n = {node_idx=next_idx();parent=ref Invalid;arr;ids; loc=snd loc;} in
    n }

dot_expr:
    | obj=callable_expr; TokDot; field=TokIdent; {
    let n = {node_idx=next_idx();parent=ref Invalid;obj;field=fst field; loc = snd field; } in
    n }

var_expr:
    | name=TokIdent; {
    let n = {node_idx=next_idx();parent=ref Invalid;name=fst name; loc = snd name;} in
    n }

num_expr:
    | num=TokNumber; {
    let n = {node_idx=next_idx();parent=ref Invalid;num=int_of_string (fst num); loc = snd num;} in
    n }

string_expr:
    | str=TokString; {
    let n = {node_idx=next_idx();parent=ref Invalid;str=fst str; loc = snd str;} in
    n }

array_literal:
    | start=TokLs; elems=arglist(expr); TokRs; {
    let n = {node_idx=next_idx();parent=ref Invalid;elems; loc = snd start;} in
    n }

null_literal:
    | null=TokNull; {
    let n = ({node_idx=next_idx();parent=ref Invalid;loc = snd null;}:null_literal) in
    n }

new_expr:
    | start=TokNew; typ=typ; TokLb; fields=arglist(field_literal); TokRb; {
    let n = {node_idx=next_idx();parent=ref Invalid;typ; fields; loc = snd start;} in
    n }
    

field_literal:
    | name=TokIdent; TokColon; value=expr; {
    let n = ({node_idx=next_idx();parent=ref Invalid;name=fst name; value; loc = snd name;}:field_literal) in
    n }

resume_expr:
    | start=TokResume; TokLp; coroutine=expr; TokRp; {
    let n = ({node_idx=next_idx();parent=ref Invalid;coroutine; loc = snd start; }:resume_expr) in
    n }

create_expr:
    | start=TokCreate; TokLp; args=arglist(expr); TokRp; {
    let n = {node_idx=next_idx();parent=ref Invalid;coroutine=List.hd args; args=List.tl args; loc = snd start; } in
    n }
