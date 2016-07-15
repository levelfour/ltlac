open Ltl

type ('state, 'alphabet) t = {
  initial    : 'state list;
  final      : 'state list;
  transition : ('state * 'alphabet * 'state list) list;
}

let f a =
  let states = a.AltBuchi.states in
  let ss = powerset states in
  let alphabets = a.AltBuchi.alphabets in
  (* 始状態 *)
  let s0 = a.AltBuchi.initial in
  let s0' = [([s0], [])] in
  (* 終状態 *)
  let sf = LTLSet.of_list a.AltBuchi.final in
  let sf' = List.map (fun s -> ([], s)) ss in
  (* 遷移規則([MH84]に基づく変換アルゴリズム) *)
  let r = a.AltBuchi.transition in
  let rho s a =
    let (_, _, s') = List.hd @@
      List.filter (fun (x, y, _) -> x = s && y = a) r
  in s' in
  let r' = List.map (fun ((u, v), a) ->
    let after =
      if List.length u <> 0 then
        List.map (fun (x, y) ->
          let x = LTLSet.of_list x in
          let y = LTLSet.of_list y in
          (LTLSet.elements @@ LTLSet.diff x sf,
           LTLSet.elements @@ LTLSet.union y @@ LTLSet.inter x sf))
        (List.filter (fun (x, y) ->
          let x = LTLSet.of_list x in
          let x' = LTLSet.diff (LTLSet.of_list states) x in
          let y = LTLSet.of_list y in
          let y' = LTLSet.diff (LTLSet.of_list states) y in
          let pbfml1 = Pbfml.inter (List.map (fun t -> rho t a) u) in
          let pbfml2 = Pbfml.inter (List.map (fun t -> rho t a) v) in
          (Pbfml.satisfy x x' pbfml1) && (Pbfml.satisfy y y' pbfml2)
        ) (perm_list ss ss))
      else
        List.map (fun y ->
          let y = LTLSet.of_list y in
          (LTLSet.elements @@ LTLSet.diff y sf,
           LTLSet.elements @@ LTLSet.inter y sf))
        (List.filter (fun y ->
          let y = LTLSet.of_list y in
          let y' = LTLSet.diff (LTLSet.of_list states) y in
          let pbfml = Pbfml.inter (List.map (fun t -> rho t a) v) in
          Pbfml.satisfy y y' pbfml
        ) ss)
    in ((u, v), a, after)
  ) (perm_list (perm_list ss ss) alphabets) in
  { initial = s0'; final = sf'; transition = r' }
