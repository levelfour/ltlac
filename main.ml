open Syntax

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let a = AltBuchi.f (Normal.f (Parser.fml Lexer.token lexbuf)) in
    let a' = Mh84.f a in
    Dot.from_altBuchi a "abuchi.dot";
    Dot.from_Buchi a' "buchi.dot"
  with Lexer.Syntax_error ->
    Printf.fprintf stderr "syntax error!\n";
    exit 0
