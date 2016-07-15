open Syntax

(* 
 * 正規化(/\, F, G, Rを除去する)
 *
 * LTLFml e ::= top | bottom | p in AP | ~e | e \/ e | Xe | eUe
 * 
 *)

let rec normalize = function
  | Not(e) -> Not(normalize e)
  | Or(e1, e2) -> Or(normalize e1, normalize e2)
  | And(e1, e2) -> Not(Or(Not(normalize e1), Not(normalize e2)))
  | Next(e) -> Next(normalize e)
  | Future(e) -> Until(Bool(true), normalize e)
  | Globally(e) -> Not(Until(Bool(true), Not(normalize e)))
  | Until(e1, e2) -> Until(normalize e1, normalize e2)
  | Release(e1, e2) -> Not(Until(Not(normalize e1), Not(normalize e2)))
  | e -> e

let rec elim_triv = function
  | Not(e) -> Not(elim_triv e)
  | Or(Bool(b), e) | Or(e, Bool(b)) -> if b then Bool(true) else e
  | Or(e1, e2) -> Or(elim_triv e1, elim_triv e2)
  | Next(e) -> Next(elim_triv e)
  | Until(e1, e2) -> Until(elim_triv e1, elim_triv e2)
  | e -> e

let rec elim_dneg = function
  | Not(Bool(false)) -> Bool(true)
  | Bool(false) -> Not(Bool(true))
  | Not(Not(e)) -> elim_dneg e
  | Not(e) -> Not(elim_dneg e)
  | Or(e1, e2) -> Or(elim_dneg e1, elim_dneg e2)
  | Next(e) -> Next(elim_dneg e)
  | Until(e1, e2) -> Until(elim_dneg e1, elim_dneg e2)
  | e -> e

let rec f e = 
  elim_dneg @@ elim_triv @@ normalize e
