exception Illegal_formula

module APSet = Set.Make(String)
module LTLSet = Set.Make(
  struct
    type t = Syntax.t
    let compare = compare
  end
)

let perm_list xs ys =
  let perm_list' x ys = List.map (fun y -> (x, y)) ys in
  List.flatten @@ List.map (fun x -> perm_list' x ys) xs

let rec powerset = function
  | [] -> [[]]
  | h::t -> List.fold_left (fun xs t -> (h::t)::t::xs) [] (powerset t)
