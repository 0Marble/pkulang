
%token <(string * Location.location)> TokNumber
%token <(string * Location.location)> TokIdent
%token <(string * Location.location)> TokString
%token <(string * Location.location)> TokNull
%token <(string * Location.location)> TokAdd
%token <(string * Location.location)> TokSub
%token <(string * Location.location)> TokMul
%token <(string * Location.location)> TokDiv
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

    let global_index = ref 100
    
    let global_nodes_store = Hashtbl.create 64

    let reset_parser_global_state () = 
        global_index := 100;
        Hashtbl.reset global_nodes_store;
        ()

    let next_idx () =
      global_index := !global_index + 1;
      !global_index - 1

    let to_op (tok:token) : Tokenizer.token = match tok with 
    | TokNumber (str,loc) -> {kind = TokNumber; str; loc;}
    | TokIdent (str,loc) -> {kind = TokIdent; str; loc;}
    | TokString (str,loc) -> {kind = TokString; str; loc;}
    | TokNull (str,loc) -> {kind = TokNull; str; loc;}
    | TokAdd (str,loc) -> {kind = TokAdd; str; loc;}
    | TokSub (str,loc) -> {kind = TokSub; str; loc;}
    | TokMul (str,loc) -> {kind = TokMul; str; loc;}
    | TokDiv (str,loc) -> {kind = TokDiv; str; loc;}
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

%}

%%

%public arglist(X):
    | {[]}
    | x=X; TokComa; xs=arglist(X) {x::xs}
    | x=X {[x]}

root: 
    | stmts = list(top_stmt); fin = TokEnd { 
    let table_copy = Hashtbl.copy global_nodes_store in
    let n = ({stmts; all_nodes = table_copy; node_idx = next_idx (); loc = snd fin; }:root) in
    Hashtbl.add n.all_nodes n.node_idx (Root n);
    reset_parser_global_state ();
    n }

top_stmt:
    | fn_decl { (FnDecl $1:top_stmt) }
    | struct_decl { (StructDecl $1:top_stmt) }
    | co_decl { (CoDecl $1:top_stmt) }
    | let_stmt { (LetStmt $1:top_stmt) }
    | alias_stmt { (AliasStmt $1:top_stmt) }

fn_decl:
    | start=TokFn; name=TokIdent; TokLp; args=arglist(argument); TokRp; ret_type=typ; body=fn_body {
    let n = {name=fst name; args; ret_type; body; node_idx = next_idx (); loc = snd start} in 
    Hashtbl.add global_nodes_store n.node_idx (FnDecl n);
    n } 

fn_body:
    | block { (Block $1 : stmt) }

argument:
    | name=TokIdent; TokColon; arg_type=typ {
    let n = {name=fst name; arg_type; node_idx = next_idx (); loc=snd name; } in
    Hashtbl.add global_nodes_store n.node_idx (Argument n);
    n }

struct_decl:
    | TokStruct; name=TokIdent; TokLb; decls=list(decl); TokRb { 
    let n = {name=fst name; decls; node_idx = next_idx (); loc = snd name} in
    Hashtbl.add global_nodes_store n.node_idx (StructDecl n);
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
    let n = {var_name=fst var_name; field_type; value = Some value; node_idx = next_idx (); loc = snd var_name } in 
    Hashtbl.add global_nodes_store n.node_idx (Field n);
    n }

field_no_val: 
    | var_name=TokIdent; TokColon; field_type=typ; TokComa {
    let n = {var_name=fst var_name; field_type; value=None; node_idx = next_idx (); loc = snd var_name } in
    Hashtbl.add global_nodes_store n.node_idx (Field n);
    n }

co_decl:
    | start=TokCo; name=TokIdent; TokLp; args=arglist(argument); TokRp; yield_type=typ; body=fn_body {
    let n = {name=fst name; args; yield_type; body; node_idx = next_idx (); loc = snd start;  } in   
    Hashtbl.add global_nodes_store n.node_idx (CoDecl n);
    n }

let_stmt:
    | TokLet; var_name=TokIdent; TokColon; var_type=typ; TokAssign; value=expr; TokSemi {
    let n = {var_name=fst var_name; var_type; value; node_idx = next_idx (); loc = snd var_name; } in 
    Hashtbl.add global_nodes_store n.node_idx (LetStmt n);
    n }

alias_stmt: 
    | TokType; type_name=TokIdent; TokAssign; other_type=typ; TokSemi {
    let n = {type_name=fst type_name; other_type; node_idx = next_idx (); loc = snd type_name; } in
    Hashtbl.add global_nodes_store n.node_idx (AliasStmt n);
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
    let n = ({name=fst name; node_idx = next_idx (); loc = snd name;}:named_type) in 
    Hashtbl.add global_nodes_store n.node_idx (NamedType n);
    n }

array_type:
    | start=TokLs; elem=typ; TokRs {
    let n = {elem; node_idx = next_idx (); loc = snd start;} in
    Hashtbl.add global_nodes_store n.node_idx (ArrayType n);
    n }

fn_type:
    | start=TokFn; TokLp; args=arglist(typ); TokRp; ret=typ {
    let n = {args; ret; node_idx = next_idx (); loc = snd start;} in 
    Hashtbl.add global_nodes_store n.node_idx (FnType n);
    n }


co_type: 
    | start=TokCo; TokLp; args=arglist(typ); TokRp; yield=typ {
    let n = {args; yield; node_idx = next_idx (); loc = snd start; } in
    Hashtbl.add global_nodes_store n.node_idx (CoType n);
    n }

co_obj_type:
    | start=TokCo; yield=typ {
    let n = {yield; node_idx = next_idx (); loc = snd start;  } in  
    Hashtbl.add global_nodes_store n.node_idx (CoObjType n);
    n }


dot_type:
    | parent=parent_type; TokDot; child=TokIdent {
    let n = {parent; child=fst child; node_idx = next_idx (); loc = snd child;} in
    Hashtbl.add global_nodes_store n.node_idx (DotType n);
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
    let n = {stmts; node_idx = next_idx (); loc = snd start;} in
    Hashtbl.add global_nodes_store n.node_idx (Block n);
    n }


for_loop:
    | TokFor; TokLp; iter_var=TokIdent; TokColon; iterator=expr; TokRp; body=body_stmt; {
    let n = {iter_var=fst iter_var; iterator; body; node_idx = next_idx (); loc = snd iter_var;} in
    Hashtbl.add global_nodes_store n.node_idx (ForLoop n);
    n }

while_loop:
    | start = TokWhile; TokLp; condition=expr; TokRp; body=body_stmt; {
    let n = {condition; body; node_idx = next_idx (); loc = snd start;} in
    Hashtbl.add global_nodes_store n.node_idx (WhileLoop n);
    n }

continue_stmt:
    | start=TokContinue; TokSemi; {
    let n = ({node_idx = next_idx (); loc = snd start;}:continue_stmt) in   
    Hashtbl.add global_nodes_store n.node_idx (ContinueStmt n);
    n }


break_stmt:
    | start=TokBreak; TokSemi; {
    let n = ({node_idx = next_idx (); loc = snd start;}:break_stmt) in
    Hashtbl.add global_nodes_store n.node_idx (BreakStmt n);
    n }

if_stmt:
    | if_stmt_no_else {$1}
    | if_stmt_with_else {$1}

if_stmt_no_else:
    | start=TokIf; TokLp; condition=expr; TokRp; if_true=body_stmt;  {
    let n = {condition; if_true; if_false=None; node_idx = next_idx (); loc = snd start;} in
    Hashtbl.add global_nodes_store n.node_idx (IfStmt n);
    n }

if_stmt_with_else:
    | start=TokIf; TokLp; condition=expr; TokRp; if_true=body_stmt; TokElse; if_false=stmt; {
    let n = {condition; if_true; if_false = Some if_false; node_idx = next_idx (); loc = snd start;} in
    Hashtbl.add global_nodes_store n.node_idx (IfStmt n);
    n }

if_resume_stmt:
    | if_resume_stmt_base TokElse stmt {
    let n = {$1 with if_bad= Some $3} in
    Hashtbl.replace global_nodes_store n.node_idx (IfResumeStmt n);
    n }
    | if_resume_stmt_base {$1}

if_resume_stmt_base:
    | if_resume_stmt_with_var {$1}
    | if_resume_stmt_void {$1}

if_resume_stmt_with_var:
    | start=TokIf; TokResume; TokLp; var=TokIdent; TokColon; coroutine=expr; TokRp; if_ok=body_stmt; {
    let n = { var=Some (fst var); coroutine; if_ok; if_bad=None; node_idx = next_idx (); loc = snd start;} in
    Hashtbl.add global_nodes_store n.node_idx (IfResumeStmt n);
    n }

if_resume_stmt_void:
    | start=TokIf; TokResume; TokLp; coroutine=expr; TokRp; if_ok=body_stmt; {
    let n = { var=None; coroutine; if_ok; if_bad=None; node_idx = next_idx (); loc = snd start;} in
    Hashtbl.add global_nodes_store n.node_idx (IfResumeStmt n);
    n }

yield_stmt:
    | start=TokYield; value=option(expr); TokSemi; {
    let n = ({value; node_idx = next_idx (); loc = snd start;}:yield_stmt) in
    Hashtbl.add global_nodes_store n.node_idx (YieldStmt n);
    n }

return_stmt:
    | start=TokReturn; value = option(expr); TokSemi; {
    let n = {value; node_idx = next_idx (); loc = snd start;} in
    Hashtbl.add global_nodes_store n.node_idx (ReturnStmt n);
    n }

expr_stmt:
    | expr=expr; TokSemi; {expr}

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

assign_op:
    | TokAddAssign { (TokAdd $1, $1) }
    | TokSubAssign { (TokSub $1, $1) }
    | TokMulAssign { (TokMul $1, $1) }
    | TokDivAssign { (TokDiv $1, $1) }

expr:
    | lhs=expr; op=TokAssign; rhs=callable_expr {
    let n = {lhs; rhs; op=to_op (TokAssign op); node_idx=next_idx (); loc = snd op; } in
    Hashtbl.add global_nodes_store n.node_idx (BinExpr n);
    BinExpr n }
    | lhs=expr; op=assign_op; rhs=callable_expr {
    let n = {lhs; rhs; op=to_op (fst op); node_idx=next_idx (); loc = snd (snd op); } in
    Hashtbl.add global_nodes_store n.node_idx (BinExpr n);
    let n = {lhs; rhs=(BinExpr n); op=to_op (TokAssign (snd op)); node_idx=next_idx (); loc = snd (snd op); } in
    Hashtbl.add global_nodes_store n.node_idx (BinExpr n);
    BinExpr n }
    | expr1 {$1}

expr1:
    | lhs=expr1; op=binop1; rhs=expr2; {
    let n = {lhs; rhs; op=to_op (fst op); node_idx=next_idx (); loc = snd op; } in
    Hashtbl.add global_nodes_store n.node_idx (BinExpr n);
    BinExpr n }
    | expr2 {$1}

expr2:
    | lhs=expr2; op=binop2; rhs=expr3; {
    let n = {lhs; rhs; op=to_op (fst op); node_idx=next_idx (); loc = snd op; } in
    Hashtbl.add global_nodes_store n.node_idx (BinExpr n);
    BinExpr n }
    | expr3 {$1}

expr3:
    | lhs=expr3; op=binop3; rhs=expr4; {
    let n = {lhs; rhs; op=to_op (fst op); node_idx=next_idx (); loc = snd op; } in
    Hashtbl.add global_nodes_store n.node_idx (BinExpr n);
    BinExpr n }
    | expr4 {$1}

expr4:
    | lhs=expr4; op=binop4; rhs=expr5; {
    let n = {lhs; rhs; op=to_op (fst op); node_idx=next_idx (); loc = snd op; } in
    Hashtbl.add global_nodes_store n.node_idx (BinExpr n);
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
    let n = {op=to_op (fst op); sub_expr; node_idx = next_idx (); loc = snd op; } in
    Hashtbl.add global_nodes_store n.node_idx (UnaryExpr n);
    n }

call_expr:
    | fn=callable_expr; loc=TokLp; params=arglist(expr); TokRp; {
    let n = {fn; params; node_idx = next_idx (); loc = snd loc;} in
    Hashtbl.add global_nodes_store n.node_idx (CallExpr n);
    n }

index_expr:
    | arr=callable_expr; loc=TokLs; idx=arglist(expr); TokRs; {
    let n = {arr;idx; node_idx=next_idx (); loc=snd loc;} in
    Hashtbl.add global_nodes_store n.node_idx (IndexExpr n);
    n }

dot_expr:
    | obj=callable_expr; TokDot; field=TokIdent; {
    let n = {obj;field=fst field; node_idx = next_idx (); loc = snd field; } in
    Hashtbl.add global_nodes_store n.node_idx (DotExpr n);
    n }

var_expr:
    | name=TokIdent; {
    let n = {name=fst name; node_idx = next_idx (); loc = snd name;} in
    Hashtbl.add global_nodes_store n.node_idx (VarExpr n);
    n }

num_expr:
    | num=TokNumber; {
    let n = {num=int_of_string (fst num); node_idx = next_idx (); loc = snd num;} in
    Hashtbl.add global_nodes_store n.node_idx (NumExpr n);
    n }

string_expr:
    | str=TokString; {
    let n = {str=fst str; node_idx = next_idx (); loc = snd str;} in
    Hashtbl.add global_nodes_store n.node_idx (StringExpr n);
    n }

array_literal:
    | start=TokLs; elems=arglist(expr); TokRs; {
    let n = {elems; node_idx=next_idx (); loc = snd start;} in
    Hashtbl.add global_nodes_store n.node_idx (ArrayLiteral n);
    n }

null_literal:
    | null=TokNull; {
    let n = ({node_idx=next_idx(); loc = snd null;}:null_literal) in
    Hashtbl.add global_nodes_store n.node_idx (NullLiteral n);
    n }

new_expr:
    | start=TokNew; typ=typ; TokLb; fields=arglist(field_literal); TokRb; {
    let n = {typ; fields; node_idx = next_idx (); loc = snd start;} in
    Hashtbl.add global_nodes_store n.node_idx (NewExpr n);
    n }
    

field_literal:
    | name=TokIdent; TokColon; value=expr; {
    let n = ({name=fst name; value; node_idx = next_idx (); loc = snd name;}:field_literal) in
    Hashtbl.add global_nodes_store n.node_idx (FieldLiteral n);
    n }

resume_expr:
    | start=TokResume; TokLp; coroutine=expr; TokRp; {
    let n = ({coroutine; node_idx=next_idx (); loc = snd start; }:resume_expr) in
    Hashtbl.add global_nodes_store n.node_idx (ResumeExpr n);
    n }

create_expr:
    | start=TokCreate; TokLp; args=arglist(expr); TokRp; {
    let n = {coroutine=List.hd args; params=List.tl args; node_idx = next_idx (); loc = snd start; } in
    Hashtbl.add global_nodes_store n.node_idx (CreateExpr n);
    n }
