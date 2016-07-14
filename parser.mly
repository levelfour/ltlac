%{
open Syntax
%}

%token TOP
%token BOTTOM
%token NOT
%token OR
%token AND
%token NEXT
%token FUTURE
%token GLOBALLY
%token UNTIL
%token RELEASE
%token <string> IDENT
%token LPAREN
%token RPAREN
%token EOF

%left OR
%left NOT
%left AND
%left UNTIL RELEASE
%left NEXT FUTURE GLOBALLY

%type <Syntax.t> fml
%start fml

%%

fml:
| LPAREN fml RPAREN { $2 }
| TOP               { Bool(true) }
| BOTTOM            { Bool(false) }
| IDENT             { AP($1) }
| NOT fml           { Not($2) }
| fml OR fml        { Or($1, $3) }
| fml AND fml       { And($1, $3) }
| NEXT fml          { Next($2) }
| FUTURE fml        { Future($2) }
| GLOBALLY fml      { Globally($2) }
| fml UNTIL fml     { Until($1, $3) }
| fml RELEASE fml   { Release($1, $3) }
| error             { raise Parsing.Parse_error }
