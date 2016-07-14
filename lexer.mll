{
open Parser
exception Syntax_error
}

rule token = parse
| [' ' '\t'] { token lexbuf }
| '('   { LPAREN }
| ')'   { RPAREN }
| '~'   { NOT }
| "/\\" { AND }
| "\\/" { OR }
| 'X'   { NEXT }
| 'F'   { FUTURE }
| 'G'   { GLOBALLY }
| 'U'   { UNTIL }
| 'R'   { RELEASE }
| ['a'-'z']+ as lxm { IDENT(lxm) }
| '\n'  { EOF }
| eof   { EOF }
| _     { raise Syntax_error }
