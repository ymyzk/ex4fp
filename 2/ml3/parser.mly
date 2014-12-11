%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT LAND LOR EQ RARROW
%token IF THEN ELSE TRUE FALSE LET IN FUN

%token <int> INTV
%token <Syntax.id> ID

%right LAND LOR

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LET ID EQ Expr SEMISEMI { Decl ($2, $4) }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | FunExpr { $1 }
  | LORExpr { $1 }

LORExpr :
    LANDExpr LOR LANDExpr { BinOp (Lor, $1, $3) }
  | LANDExpr { $1 }

LANDExpr :
    LTExpr LAND LTExpr { BinOp (Land, $1, $3) }
  | LTExpr { $1 }

LTExpr :
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr :
    MExpr MULT AExpr { BinOp (Mult, $1, $3) }
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
    FUN ID FunSubExpr { FunExp ($2, $3) }

FunSubExpr :
    RARROW Expr { $2 }
  | ID FunSubExpr { FunExp ($1, $2) }
