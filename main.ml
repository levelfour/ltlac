open Syntax

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let t = Parser.fml Lexer.token lexbuf in
      Printf.printf "%s\n" (to_s t);
      flush_all ()
    done
  with Lexer.Syntax_error ->
    Printf.fprintf stderr "syntax error!\n";
    exit 0
