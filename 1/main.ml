let rec fact n = if n = 0 then 1
                 else n * fact (n - 1)
;;

(* print_endline "hello";; *)

let dist (x1, y1) (x2, y2) =
  let norm (a : float) (b : float) = Float.sqrt (a *. a +. b *. b) in
  norm (x1 -. x2) (y1 -. y2)

;;

(* print_float (dist (0., 1.) (5., 13.));; *)

(* List.hd, List.tl, ::, l = [], ... *)

let rec length xs =
  if xs = [] then 0
  else 1 + length (List.tl xs)
;;

let rec length' (xs : 'a list) : int =
  if xs = [] then 0
  else 1 + length (List.tl xs)
;;

let rec length'' (xs : int list) : int =
  if xs = [] then 0
  else 1 + length (List.tl xs)
;;
