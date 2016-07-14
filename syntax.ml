type t =
  | Bool of bool
  | AP of string
  | Not of t
  | Or of t * t
  | And of t * t
  | Next of t
  | Future of t
  | Globally of t
  | Until of t * t
  | Release of t * t

let rec to_s = function
  | Bool(true) -> "T"
  | Bool(false) -> "!"
  | AP(p) -> p
  | Not(e') -> Printf.sprintf "(~%s)" (to_s e')
  | Or(e1, e2) -> Printf.sprintf "(%s\\/%s)" (to_s e1) (to_s e2)
  | And(e1, e2) -> Printf.sprintf "(%s/\\%s)" (to_s e1) (to_s e2)
  | Next(e') -> Printf.sprintf "(X%s)" (to_s e')
  | Future(e') -> Printf.sprintf "(F%s)" (to_s e')
  | Globally(e') -> Printf.sprintf "(G%s)" (to_s e')
  | Until(e1, e2) -> Printf.sprintf "(%sU%s)" (to_s e1) (to_s e2)
  | Release(e1, e2) -> Printf.sprintf "(%sR%s)" (to_s e1) (to_s e2)
