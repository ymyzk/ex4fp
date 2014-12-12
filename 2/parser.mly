%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT LAND LOR EQ RARROW
%token IF THEN ELSE TRUE FALSE LET IN FUN REC

%token <int> INTV
%token <Syntax.id> ID

%right LAND LOR

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LET ID EQ Expr SEMISEMI { Decl ($2, $4) }
  | LET REC ID EQ FUN ID RARROW Expr SEMISEMI { RecDecl ($3, $6, $8) }

Expr :
    IfExpr { $1 }
  | FunExpr { $1 }
  | LetExpr { $1 }
  | LetRecExpr { $1 }
  | LORExpr { $1 }

LORExpr :
    LANDExpr LOR LORExpr { BinOp (Lor, $1, $3) }
  | LANDExpr { $1 }

LANDExpr :
    LTExpr LAND LANDExpr { BinOp (Land, $1, $3) }
  | LTExpr { $1 }

LTExpr :
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr :
    MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | AppExpr { $1 }

AppExpr :
    AppExpr AExpr { AppExp ($1, $2) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

LetExpr :
    LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }

FunExpr :
    FUN ID RARROW Expr { FunExp ($2, $4) }

/*
    FUN ID FunSubExpr { FunExp ($2, $3) }

FunSubExpr :
    RARROW Expr { $2 }
  | ID FunSubExpr { FunExp ($1, $2) }
*/

LetRecExpr :
  LET REC ID EQ FUN ID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }
