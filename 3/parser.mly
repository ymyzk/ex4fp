%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT DIV LT GT LAND LOR EQ RARROW
%token IF THEN ELSE TRUE FALSE LET IN FUN REC

%token <int> INTV
%token <Syntax.id> ID

%right LAND LOR

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LET ID LetDeclSubExpr { Decl ($2, $3) }
  | LET REC ID EQ FUN ID RARROW Expr SEMISEMI { RecDecl ($3, $6, $8) }

LetDeclSubExpr :
    EQ Expr SEMISEMI { $2 }
  | ID LetDeclSubExpr { FunExp ($1, $2) }

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
    CmpExpr LAND LANDExpr { BinOp (Land, $1, $3) }
  | CmpExpr { $1 }

CmpExpr :
    CmpExpr EQ PExpr { BinOp (Eq, $1, $3) }
  | CmpExpr LT PExpr { BinOp (Lt, $1, $3) }
  | CmpExpr GT PExpr { BinOp (Gt, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MDExpr { BinOp (Plus, $1, $3) }
  | MDExpr { $1 }

MDExpr :
    MDExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | MDExpr DIV AppExpr { BinOp (Div, $1, $3) }
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
    LET ID LetSubExpr IN Expr { LetExp ($2, $3, $5) }

LetSubExpr :
    EQ Expr { $2 }
  | ID LetSubExpr { FunExp ($1, $2) }

FunExpr :
    FUN ID FunSubExpr { FunExp ($2, $3) }

FunSubExpr :
    RARROW Expr { $2 }
  | ID FunSubExpr { FunExp ($1, $2) }

LetRecExpr :
  LET REC ID EQ FUN ID RARROW Expr IN Expr { LetRecExp ($3, $6, $8, $10) }
