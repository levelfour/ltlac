open Syntax
open Pbfml
open Ltl

type ('state, 'alphabet) t = {
  states     : 'state list;
  alphabets  : 'alphabet list;
  initial    : 'state;
  final      : 'state list;
  transition : ('state * 'alphabet * 'state Pbfml.t) list;
}

let rec aps set = function
  | AP(p) -> APSet.add p set
  | Bool(_) -> set
  | Not(e) | Next(e) -> aps set e
  | Or(e1, e2) | Until(e1, e2) -> 
      let s1 = aps APSet.empty e1 in
      let s2 = aps APSet.empty e2 in
      APSet.union set (APSet.union s1 s2)
  | _ -> raise Illegal_formula

let rec subformulas set = function
  | AP(p) as e -> LTLSet.add e (LTLSet.add (Not(e)) set)
  | Bool(_) -> set
  | Not(e) ->
      let set' = LTLSet.add e (LTLSet.add (Not(e)) set) in
      subformulas set' e
  | Next(e') as e ->
      let set' = LTLSet.add e (LTLSet.add (Not(e)) set) in
      subformulas set' e'
  | Or(e1, e2) | Until(e1, e2) as e ->
      let set' = LTLSet.add e (LTLSet.add (Not(e)) set) in
      let s1 = subformulas LTLSet.empty e1 in
      let s2 = subformulas LTLSet.empty e2 in
      LTLSet.union set' (LTLSet.union s1 s2)
  | _ -> raise Illegal_formula

let dump_states s =
  List.iter (fun e -> Printf.printf "%s," (to_s e)) s

let f e =
  (* 状態集合 *)
  let states = LTLSet.elements @@ subformulas LTLSet.empty e in
  (* アルファベット集合 *)
  let alphabets = powerset @@ APSet.elements (aps APSet.empty e) in
  (* 始状態 *)
  let s0 = e in
  (* 終状態集合 *)
  let sf = List.filter (function Not(Until(_,_)) -> true | _ -> false) states in
  (* 遷移関係 *)
  let rec rho s a =
    match s with
    | Bool(_) as b -> PBVar(b)
    | AP(p) -> PBBool(List.mem p a)
    | Or(s1, s2) -> PBOr(rho s1 a, rho s2 a)
    | Not(s') -> PBNot(rho s' a)
    | Next(s') -> PBVar(s')
    | Until(s1, s2) -> PBOr(rho s2 a, PBAnd(rho s1 a, PBVar(s)))
    | _ -> raise Illegal_formula in
  let r =
    List.map (fun (s, a) ->
      (s, a, Pbfml.normalize @@ rho s a))
    (perm_list states alphabets)
  in
  (* オートマトンの構成 *)
  {
    states = states;
    alphabets = alphabets;
    initial = s0;
    final = sf;
    transition = r
  }
