let from_altBuchi (autom: ('state, 'alphabet) AltBuchi.t) filename =
  let oc = open_out filename in
  Printf.fprintf oc
  "digraph {
  rankdir = LR;
  _nil [style = \"invis\"];\n";
  (* 終状態 *)
  Printf.fprintf oc
  "  node [shape = doublecircle]; %s%s\n"
  (String.concat " " (List.map (fun s ->
    Printf.sprintf "\"%s\"" @@ String.escaped (Syntax.to_s s)
   ) autom.AltBuchi.final))
  (if List.length autom.AltBuchi.final > 0 then ";" else "");
  (* 始状態 *)
  Printf.fprintf oc "  node [shape = circle];\n";
  Printf.fprintf oc
  "  _nil -> \"%s\";\n"
  (String.escaped (Syntax.to_s autom.AltBuchi.initial));
  (* 遷移規則 *)
  List.iter (fun (s, a, s') ->
    Printf.fprintf oc
    "  \"%s\" -> \"%s\" [ label = \"{%s}\" ];\n"
    (String.escaped (Syntax.to_s s)) (String.escaped (Pbfml.to_s s'))
    (String.concat "," a)
  ) autom.AltBuchi.transition;
  Printf.fprintf oc "}";
  close_out oc

let ltlset_to_s xs =
  Printf.sprintf "{%s}"
  (String.concat "," (List.map (fun x -> Syntax.to_s x) xs))

let from_Buchi (autom: ('state, 'alphabet) Mh84.t) filename =
  let oc = open_out filename in
  Printf.fprintf oc
  "digraph {
  rankdir = LR;
  _nil [style = \"invis\"];\n";
  (* 終状態 *)
  Printf.fprintf oc
  "  node [shape = doublecircle]; %s%s\n"
  (String.concat " " (List.map (fun (u,v) ->
    Printf.sprintf "\"(%s,%s)\""
    (ltlset_to_s u) (ltlset_to_s v)) autom.Mh84.final))
  (if List.length autom.Mh84.final > 0 then ";" else "");
  (* 始状態 *)
  Printf.fprintf oc "  node [shape = circle];\n";
  (List.iter (fun (u,v) ->
    Printf.fprintf oc "  _nil -> \"(%s,%s)\";\n"
    (ltlset_to_s u) (ltlset_to_s v)) autom.Mh84.initial);
  (* 遷移規則 *)
  List.iter (fun ((u0,v0), a, uvs) ->
    List.iter (fun (u1,v1) ->
      Printf.fprintf oc
      "  \"(%s,%s)\" -> \"(%s,%s)\" [ label = \"{%s}\" ];\n"
      (String.escaped (ltlset_to_s u0)) (String.escaped (ltlset_to_s v0))
      (String.escaped (ltlset_to_s u1)) (String.escaped (ltlset_to_s v1))
      (String.concat "," a)
    ) uvs
  ) autom.Mh84.transition;
 
  Printf.fprintf oc "}";
  close_out oc
