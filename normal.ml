open Syntax

(* 
 * 正規化(/\, F, G, Rを除去する)
 *
 * LTLFml e ::= top | bottom | p in AP | ~e | e \/ e | Xe | eUe
 * 
 *)

let rec f = function
  | Not(e) -> Not(f e)
  | Or(e1, e2) -> Or(f e1, f e2)
  | And(e1, e2) -> Not(Or(Not(f e1), Not(f e2)))
  | Next(e) -> Next(f e)
  | Future(e) -> Until(Bool(true), f e)
  | Globally(e) -> Not(Until(Bool(true), Not(f e)))
  | Until(e1, e2) -> Until(f e1, f e2)
  | Release(e1, e2) -> Not(Until(Not(f e1), Not(f e2)))
  | e -> e
