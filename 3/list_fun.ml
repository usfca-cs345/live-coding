(* Helper to mark the rest of the functions as pending *)
let todo () = failwith "todo" ;;

(* List.hd, List.tl, ::, l = [] *)

let rec length xs =
  if xs = [] then 0
  else 1 + length (List.tl xs)
;;

let rec length' (xs : 'a list) : int =
  if xs = [] then 0
  else 1 + length' (List.tl xs)
;;

(* Checks if a list is empty.
 *)
let is_empty xs = match xs with
  | [] -> true
  | _ -> false

(* Checks if a list is empty. Prints the first element if the list is not empty.

   print_first []
   -> prints "empty"
   print_first ["foo"; "bar"]
   -> prints "not empty. the first element is foo"

   You print a string s using print_endline s.
 *)
let rec print_first xs = print_endline (match xs with
  | [] -> "empty"
  | first :: _ -> "not empty. the first element is " ^ first)

(* with pattern matching *)

let rec length'' xs = match xs with
  | [] -> 0
  | first :: rest -> 1 + length'' rest

(* Adds all numbers in the list

   sum [1; 3; 5; 7] = 16
 *)
let rec sum nums = match nums with
  | [] -> 0
  | head :: tail -> head + sum tail

(* Takes the square of each number in the list

   square_all [1; 3; 5; 7] = [1; 9; 25; 49]
 *)
let rec square_all nums = match nums with
  | [] -> []
  | h :: t -> (h * h) :: square_all t

(* Converts each element to a string

   stringify [1; 2; 3] = ["1"; "2"; "3"]
 *)
let rec stringify nums = match nums with
  | [] -> []
  | h :: t -> string_of_int h :: stringify t

(* Converts a list to a string, ready to be pretty printed.

   stringify [1; 2; 3] = "[1; 2; 3]"
 *)
let rec string_of_list nums =
  let rec string_of_strings strs = match strs with
    | [] -> ""
    | first :: [] -> first
    | first :: rest -> first ^ "; " ^ string_of_strings rest
  and add_brackets s = "[" ^ s ^ "]"
  in 
  nums |> stringify |> string_of_strings |> add_brackets


(* Returns only the odd numbers in a list.

   only_odd [] = []
   only_odd [1; 2; 3; 4; 6; 7] = [1; 3; 7]

 *)
let rec only_odd nums = match nums with
  | [] -> []
  | x :: xs when x mod 2 = 1 -> x :: only_odd xs
  | _ :: xs -> only_odd xs

(* Tail recursion.
 *)
let only_odd' nums =
  let rec add_remaining_odds remaining odd_so_far = match remaining with
    | [] -> odd_so_far
    | x :: xs when x mod 2 = 1 -> add_remaining_odds xs (x :: odd_so_far)
    | _ :: xs -> add_remaining_odds xs odd_so_far
  in add_remaining_odds nums [] |> List.rev
;;

(* Higher-order functions *)

(* Applies f to each element of xs.

   Example:
   map (fun x -> x + 4) [1; 2; 3] = [5; 6; 7]
 *)
let rec map f xs = match xs with
  | [] -> []
  | head :: tail -> f head :: map f tail

(* Tail-recursively *)
let map' f xs =
  let rec helper remaining applied_values = match remaining with
    | [] -> applied_values
    | head :: tail -> helper tail (f head :: applied_values)
  in helper xs [] |> List.rev

(* now, we can re-define these (and use point-free style): *)
let rec square_all' nums = map' (fun x -> x * x) nums
let rec square_all'' = map' (fun x -> x * x)
let rec stringify' nums = List.map string_of_int nums

(* Keeps elements for which pred returns true.

   Example:

   filter (fun x -> x mod 2 == 1) [1; 2; 3] = [1; 3]

   filter (fun student -> student.major = "CS") roster
 *)
let rec filter pred xs = match xs with
  | [] -> []
  | x :: xs when pred x -> x :: filter pred xs
  | _ :: xs -> filter pred xs

(* Now, we can re-define only_odd: *)
let rec only_odd'' xs = filter (fun y -> y mod 2 = 1) xs

(* "Folds" given list using the given binary operation.

   fold f start [x1; x2; ...; xn] = (f (... (f (f start x1) x2) ...) xn).

   Examples:
   fold (+) 0 [1; 2; 3; 4] = ((((0 + 1) + 2) + 3) + 4) = 10
   fold (^) "" ["a"; "b"; "c"] = (("" ^ "a") ^ "b") ^ "c" = "abc"
   fold (fun len x -> len + 1) 0 [1; 2; 3; 4] = (((0 + 1) + 1) + 1) + 1 = 4

   fold f a [] = a
   fold f a [b] = (f a b)
   fold f a [b; c] = (f (f a b) c)
 *)
let rec fold
    (f : 'acc -> 'elem -> 'acc) (start : 'acc) (xs : 'elem list)
    : 'acc
  = match xs with
  | [] -> start
  | head :: tail -> fold f (f start head) tail

(* Now, we can re-define length, sum, and string_of_list *)
let rec length''' = fold (fun length_so_far x -> length_so_far + 1) 0
let rec sum' = fold (+) 0
;;
(*
   sum' [1;4] = fold (+) 0 [1;4] = ((0 + 1) + 4) = 5
 *)
(* [1; 2; 3] -> ["1"; "2"; "3"] -> "1; 2; 3; " -> "[1; 2; 3; ]" *)
let string_of_list' nums = (
  (* [3; 2; 1] -> "1" ^ "; " ^ ("2" ^ "; " ^ ("3" ^ "; " ^ "")) *)
  let string_of_strings strs = (
    fold
      (fun stuff_so_far curr_elem -> curr_elem ^ "; " ^ stuff_so_far)
      ""
      strs
  )
  and add_brackets s = "[" ^ s ^ "]"
  in 
  (
    nums |> stringify |> List.rev |> string_of_strings |> add_brackets
  )
)

(* (1 + 2) + 3 = 1 + (2 + 3) *)

(* Fold is powerful enough to implement map and filter

   map f [] = []
   map f h :: t = f h :: map f t

   (on a reversed list)

   [x3; x2; x1] -> f x1 :: (f x2 :: (f x3 :: []))

   [] x3 ??? f x3 :: []
   ... x1 ??? f x1 :: ...
*)
let map'' f xs =
  fold (fun applied_values head -> f head :: applied_values) [] (List.rev xs)
let filter' f xs = todo () ;;

(* Optionals *)

(* Finds the largest element in the list.

   Q: What should this function return if the list is empty?

   max [x1; ... ; xn] = max2(x1, max2(x2, ... max2(x(n-1), xn)))
 *)
let rec max (xs : int list) : int option =
  let max2 a b = if a > b then a else b
  in
  match xs with
  | [] -> None
  | h :: t -> (
      match max t with
      | None -> Some h
      | Some max_rest -> Some (max2 h max_rest)
    )

(* Optionals:

   'a option = (Some 'a) | None

   'a list = [] | ('a :: ('a list))

   Option.get: unsafely get the value
 *)

(* Extracts the integer inside and optional, or returns zero if there is no
   int. *)
let or_zero n = match n with
  | Some value -> value
  | None -> 0
