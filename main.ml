open Syntax

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let a = AltBuchi.f (Normal.f (Parser.fml Lexer.token lexbuf)) in
    Dot.from_altBuchi a "abuchi.dot";
    Printf.fprintf stderr "[output] alternating Buchi automaton\n";
    let a' = Mh84.f a in
    Dot.from_Buchi a' "buchi.dot";
    Printf.fprintf stderr "[output] non-deterministic Buchi automaton\n"
  with Lexer.Syntax_error ->
    Printf.fprintf stderr "syntax error!\n";
    exit 0
