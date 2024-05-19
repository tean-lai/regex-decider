%{
    open Ast
%}

%token ZERO
%token ONE
%token <char> CHAR
%token PLUS
%token TIMES
%token STAR
%token LPAREN
%token RPAREN
%token EOF


%left PLUS
%left TIMES
%right STAR

%start <Ast.exp> prog

%%

prog:
  | exp EOF { $1 }

exp:
  | ZERO { Zero }
  | ONE { One }
  | CHAR { Char $1 }
  | exp PLUS exp { Sum [$1; $3] }
  | exp TIMES exp { Prod [$1; $3] }
  | LPAREN exp RPAREN { $2 }
  | exp STAR { Star $1 }
