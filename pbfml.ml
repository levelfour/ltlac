(* Positive Boolean formulas *)

open Ltl

exception Illegal_pattern
exception Not_found

type 'a t =
  | PBVar of 'a
  | PBBool of bool
  | PBNot of 'a t
  | PBOr of 'a t * 'a t
  | PBAnd of 'a t * 'a t

let rec to_s = function
  | PBVar(p) -> Syntax.to_s p
  | PBBool(true) -> "true"
  | PBBool(false) -> "false"
  | PBNot(e) -> Printf.sprintf "not %s" (to_s e)
  | PBOr(e1, e2) -> Printf.sprintf "(%s or %s)" (to_s e1) (to_s e2)
  | PBAnd(e1, e2) -> Printf.sprintf "(%s and %s)" (to_s e1) (to_s e2)

let rec elim_triv = function
  | PBNot(e) -> PBNot(elim_triv e)
  | PBOr(PBBool(b), e) | PBOr(e, PBBool(b)) -> if b then PBBool(true) else e
  | PBOr(e1, e2) -> PBOr(elim_triv e1, elim_triv e2)
  | PBAnd(PBBool(b), e) | PBAnd(e, PBBool(b)) -> if b then e else PBBool(false)
  | PBAnd(e1, e2) -> PBAnd(elim_triv e1, elim_triv e2)
  | e -> e

let rec elim_dneg = function
  | PBNot(PBBool(b)) -> PBBool(not b)
  | PBNot(PBNot(e)) -> elim_dneg e
  | PBNot(e) -> PBNot(elim_dneg e)
  | PBOr(e1, e2) -> PBOr(elim_dneg e1, elim_dneg e2)
  | PBAnd(e1, e2) -> PBAnd(elim_dneg e1, elim_dneg e2)
  | e -> e

let rec normalize e = 
  elim_dneg @@ elim_triv e

let inter xs =
  List.fold_left (fun u v -> PBAnd(u, v)) (PBBool(true)) xs

let satisfy tset fset e =
  let rec subst = function
    | PBVar(p) ->
        if LTLSet.mem p tset then PBBool(true)
        else if LTLSet.mem p fset then PBBool(false)
        else if p = Syntax.Bool(true) then PBBool(true)
        else if p = Syntax.Bool(false) then PBBool(false)
        else raise Not_found
    | PBNot(e) -> PBNot(subst e)
    | PBOr(e1, e2) -> PBOr(subst e1, subst e2)
    | PBAnd(e1, e2) -> PBAnd(subst e1, subst e2)
    | e -> e in
  let rec reduce = function
    | PBBool(_) as e -> e
    | PBNot(PBBool(b)) -> PBBool(not b)
    | PBNot(e) -> (
      match reduce e with
      | PBBool(b) -> PBBool(not b)
      | _ -> raise Illegal_pattern)
    | PBOr(e1, e2) -> (
      match reduce e1, reduce e2 with
      | PBBool(b1), PBBool(b2) -> PBBool(b1 || b2)
      | _ -> raise Illegal_pattern)
    | PBAnd(e1, e2) -> (
      match reduce e1, reduce e2 with
      | PBBool(b1), PBBool(b2) -> PBBool(b1 && b2)
      | _ -> raise Illegal_pattern)
    | e -> raise Illegal_pattern in
  match reduce @@ subst e with
  | PBBool(b) -> b
  | _ -> raise Illegal_pattern
