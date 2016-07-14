open Syntax

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let t = Parser.fml Lexer.token lexbuf in
      let t' = Normal.f t in
      Printf.printf "%s => %s\n" (to_s t) (to_s t');
      flush_all ()
    done
  with Lexer.Syntax_error ->
    Printf.fprintf stderr "syntax error!\n";
    exit 0
