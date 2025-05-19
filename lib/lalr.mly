
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

root: 
    | stmts = list(top_stmt); fin = TokEnd { ({stmts; node_idx = next_idx (); loc = snd fin; }:root) }

top_stmt:
    | fn_decl { (FnDecl $1:top_stmt) }
    | struct_decl { (StructDecl $1:top_stmt) }
    | co_decl { (CoDecl $1:top_stmt) }
    | let_stmt { (LetStmt $1:top_stmt) }
    | alias_stmt { (AliasStmt $1:top_stmt) }

fn_decl:
    | start=TokFn; name=TokIdent; TokLp; args=separated_list(TokComa, argument); TokRp; ret_type=typ; body=fn_body { {name=fst name; args; ret_type; body; node_idx = next_idx (); loc = snd start} } 

fn_body:
    | block { (Block $1 : stmt) }

argument:
    | name=TokIdent; TokColon; arg_type=typ { {name=fst name; arg_type; node_idx = next_idx (); loc=snd name; } }

struct_decl:
    | TokStruct; name=TokIdent; TokLb; decls=list(decl); TokRb { {name=fst name; decls; node_idx = next_idx (); loc = snd name} }

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
    | var_name=TokIdent; TokColon; field_type=typ; TokAssign; value=expr; TokComa { {var_name=fst var_name; field_type; value = Some value; node_idx = next_idx (); loc = snd var_name } }

field_no_val: 
    | var_name=TokIdent; TokColon; field_type=typ; TokComa { {var_name=fst var_name; field_type; value=None; node_idx = next_idx (); loc = snd var_name } }

co_decl:
    | start=TokCo; name=TokIdent; TokLp; args=separated_list(TokComa, argument); TokRp; yield_type=typ; body=fn_body { {name=fst name; args; yield_type; body; node_idx = next_idx (); loc = snd start;  } } 

let_stmt:
    | TokLet; var_name=TokIdent; TokColon; var_type=typ; TokAssign; value=expr; TokSemi { {var_name=fst var_name; var_type; value; node_idx = next_idx (); loc = snd var_name; } }

alias_stmt: 
    | TokType; type_name=TokIdent; TokAssign; other_type=typ; TokSemi { {type_name=fst type_name; other_type; node_idx = next_idx (); loc = snd type_name; } }

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
    | name=TokIdent { {name=fst name; node_idx = next_idx (); loc = snd name;} }

array_type:
    | start=TokLs; elem=typ; TokRs { {elem; node_idx = next_idx (); loc = snd start;} }

fn_type:
    | start=TokFn; TokLp; args=separated_list(TokComa, typ); TokRp; ret=typ { {args; ret; node_idx = next_idx (); loc = snd start;} }

co_type: 
    | start=TokCo; TokLp; args=separated_list(TokComa, typ); TokRp; yield=typ { {args; yield; node_idx = next_idx (); loc = snd start; } }

co_obj_type:
    | start=TokCo; yield=typ {{yield; node_idx = next_idx (); loc = snd start;  }}

dot_type:
    | parent=parent_type; TokDot; child=TokIdent {{parent; child=fst child; node_idx = next_idx (); loc = snd child;}}


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
    | start=TokLb; stmts=list(stmt); TokRb {{stmts; node_idx = next_idx (); loc = snd start;}}

for_loop:
    | TokFor; TokLp; iter_var=TokIdent; TokColon; iterator=expr; TokRp; body=body_stmt; {{iter_var=fst iter_var; iterator; body; node_idx = next_idx (); loc = snd iter_var;}}

while_loop:
    | start = TokWhile; TokLp; condition=expr; TokRp; body=body_stmt; {{condition; body; node_idx = next_idx (); loc = snd start;}}

continue_stmt:
    | start=TokContinue; TokSemi; {({node_idx = next_idx (); loc = snd start;}:continue_stmt)}

break_stmt:
    | start=TokBreak; TokSemi; {({node_idx = next_idx (); loc = snd start;}:break_stmt)}

if_stmt:
    | if_stmt_no_else {$1}
    | if_stmt_with_else {$1}

if_stmt_no_else:
    | start=TokIf; TokLp; condition=expr; TokRp; if_true=body_stmt;  {{condition; if_true; if_false=None; node_idx = next_idx (); loc = snd start;}}

if_stmt_with_else:
    | start=TokIf; TokLp; condition=expr; TokRp; if_true=body_stmt; TokElse; if_false=stmt; {{condition; if_true; if_false = Some if_false; node_idx = next_idx (); loc = snd start;}}

if_resume_stmt:
    | if_resume_stmt_base TokElse stmt {{$1 with if_bad= Some $3}}
    | if_resume_stmt_base {$1}

if_resume_stmt_base:
    | if_resume_stmt_with_var {$1}
    | if_resume_stmt_void {$1}

if_resume_stmt_with_var:
    | start=TokIf; TokResume; TokLp; var=TokIdent; TokColon; coroutine=expr; TokRp; if_ok=body_stmt; {{ var=Some (fst var); coroutine; if_ok; if_bad=None; node_idx = next_idx (); loc = snd start;}}

if_resume_stmt_void:
    | start=TokIf; TokResume; TokLp; coroutine=expr; TokRp; if_ok=body_stmt; {{ var=None; coroutine; if_ok; if_bad=None; node_idx = next_idx (); loc = snd start;}}

return_stmt:
    | start=TokReturn; value = option(expr); TokSemi; {{value; node_idx = next_idx (); loc = snd start;}}

expr_stmt:
    | expr=expr; TokSemi; {expr}

binop0: 
    | TokAssign { ( TokAssign $1, snd $1)}
    | TokAddAssign { ( TokAddAssign $1, snd $1)}
    | TokSubAssign { ( TokSubAssign $1, snd $1)}
    | TokMulAssign { ( TokMulAssign $1, snd $1)}
    | TokDivAssign { ( TokDivAssign $1, snd $1)}
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

expr:
    | lhs=expr; op=binop0; rhs=callable_expr { BinExpr {lhs; rhs; op=to_op (fst op); node_idx=next_idx (); loc = snd op; } }
    | expr1 {$1}

expr1:
    | lhs=expr1; op=binop1; rhs=expr2; { BinExpr {lhs; rhs; op=to_op (fst op); node_idx=next_idx (); loc = snd op; } }
    | expr2 {$1}

expr2:
    | lhs=expr2; op=binop2; rhs=expr3; { BinExpr {lhs; rhs; op=to_op (fst op); node_idx=next_idx (); loc = snd op; } }
    | expr3 {$1}

expr3:
    | lhs=expr3; op=binop3; rhs=expr4; { BinExpr {lhs; rhs; op=to_op (fst op); node_idx=next_idx (); loc = snd op; } }
    | expr4 {$1}

expr4:
    | lhs=expr4; op=binop4; rhs=expr5; { BinExpr {lhs; rhs; op=to_op (fst op); node_idx=next_idx (); loc = snd op; } }
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
    | op=unop; sub_expr=expr5; {{op=to_op (fst op); sub_expr; node_idx = next_idx (); loc = snd op; }}

call_expr:
    | fn=callable_expr; loc=TokLp; params=separated_list(TokComa, expr); TokRp; {{fn; params; node_idx = next_idx (); loc = snd loc;}}

index_expr:
    | arr=callable_expr; loc=TokLs; idx=separated_list(TokComa, expr); TokRs; {{arr;idx; node_idx=next_idx (); loc=snd loc;}}

dot_expr:
    | obj=callable_expr; TokDot; field=TokIdent; {{obj;field=fst field; node_idx = next_idx (); loc = snd field; }}

var_expr:
    | name=TokIdent; {{name=fst name; node_idx = next_idx (); loc = snd name;}}

num_expr:
    | num=TokNumber; {{num=int_of_string (fst num); node_idx = next_idx (); loc = snd num;}} 

string_expr:
    | str=TokString; {{str=fst str; node_idx = next_idx (); loc = snd str;}}

array_literal:
    | start=TokLs; elems=separated_list(TokComa, expr); TokRs; {{elems; node_idx=next_idx (); loc = snd start;}}

null_literal:
    | null=TokNull; {({node_idx=next_idx(); loc = snd null;}:null_literal)}

new_expr:
    | start=TokNew; typ=typ; TokLb; fields=separated_list(TokComa, field_literal); TokRb; {{typ; fields; node_idx = next_idx (); loc = snd start;}}

field_literal:
    | name=TokIdent; TokColon; value=expr; {({name=fst name; value; node_idx = next_idx (); loc = snd name;}:field_literal)}

yield_stmt:
    | start=TokYield; value=option(expr); TokSemi; {({value; node_idx = next_idx (); loc = snd start;}:yield_stmt)}

resume_expr:
    | start=TokResume; TokLp; coroutine=expr; TokRp; {({coroutine; node_idx=next_idx (); loc = snd start; }:resume_expr)}

create_expr:
    | start=TokCreate; TokLp; args=separated_nonempty_list(TokComa, expr); TokRp; {{coroutine=List.hd args; params=List.tl args; node_idx = next_idx (); loc = snd start; }}
