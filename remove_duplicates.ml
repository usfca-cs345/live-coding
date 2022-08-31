(* Exercise: remove duplicates.

   The goal of this exercise is to show how to solve a classic programming
   interview problem involving accumulating data using fold. Some sections of
   this file are going to be marked with a line of asterisks like so:

   *****************************************************************************

   When you see asterisks like these, pause and ponder about the answer. Try
   finding your own solution and write it down. Then, you can compare it to how
   I solve the problem.

   Here is the problem description:

   Given a list, remove consecutive duplicates. For example, given a list

   [a; a; b; b; b; c; a; d; d; c]

   Return

   [a; b; c; a; d; c]

   So, we keep duplicates if there is an intervening element, and remove them
   otherwise. We are going to implement this as a function named
   remove_duplicates. First, lets look at a few examples to help us think about
   this problem:

   remove_duplicates [] = []
   remove_duplicates [a] = [a]
   remove_duplicates [a; b] = [a; b]
   remove_duplicates [a; a] = [a]
   remove_duplicates [a; b; a] = [a; b; a]
   remove_duplicates [a; b; b; a] = [a; b; a]

   This problem is a good example for fold because it satisfies some interesting
   properties:

   - The decision for each element depends on the surrounding elements, so it
   cannot be solved only by filter, so trying fold is a justifiable decision.

   - The decision for each element needs to be made separately by inspecting
   that element (unlike every_nth in the homework), so searching for a binary
   operation that "folds" the current element to the previous solution makes
   sense.

   - Once we handle the tail of the list, we can use the solution for the tail
   and the current head to solve the problem.

   Before carrying on, think about how you would solve this problem:

   1. Using a loop
   2. Using recursion two ways (finding only one of the two ways is going to be
   enough):
      a. By building up values from the head of the list, where we use the
      solution of, e.g., [a; a; b] to solve [a; a; b; b].
      a. By building up values from the tail of the list, where we use the
      solution of, e.g., [a; b; b] to solve [a; a; b; b].

   *****************************************************************************

   I will approach the solution from the right (solving the tail first).

   If we already know that remove_duplicates [a; b; b] = [a; b], and the current
   element is x, then we can compare x against the solution for the tail ([a;
   b]):

   If x = head (remove_duplicates [a; b; b]) = a, then we do not insert x. So we
   return the previous solution (we return [a; b]).
   
   Otherwise (if x <> head (remove_duplicates [a; b; b])), we prepend x to the
   previous solution and return x :: [a; b] = [x; a; b].

   Side note: Here, <> is the inequality operator in OCaml (OCaml has two
              notions of equality: a == b and a != b check physical equality
              (whether the two are the same objects in memory, like == in Java
              and "is" in Python), a = b and a <> b check logical equality
              (whether the two objects have the same value, like the "equals"
              method in Java and == in Python).

   Now, let's implement our solution recursively:
*)

let rec remove_duplicates xs = match xs with
  | [] -> []
  | first :: rest ->
    (* first compute the result for rest, then check if first = head(result) *)
    match remove_duplicates rest with
    | [] -> [first] (* rest is empty *)
    | head :: tail when first = head -> head :: tail (* first is a duplicate *)
    | result -> first :: result (* first is not a duplicate *)

(* The definition above works in two steps for an empty list:

   1. It computes remove_duplicates rest. Call this "result_of_rest".

   2. It attaches the data about first to that result. Try to tease out what
   this second step does before carrying on.

   *****************************************************************************

   The second step checks whether "first" is not a duplicate element (by
   comparing it against the head of "result_of_rest"), and prepends it . Let's
   call this step add_nonduplicate, this is the operation we want to derive for
   fold.

   So, remove_duplicates [x1; x2; ...; x(n-1); xn] computes the following
   expression:

   add_nonduplicate x1 (add_nonduplicate x2 (... (add_nonduplicate xn [])))

   Note that we have to start evaluating this expression from the right end. So,
   this is a job for fold_right! The expression above is equivalent to the call
   to fold_right below:

   List.fold_right add_nonduplicate [] [x1; x2; ...; xn]
   = add_nonduplicate x1 (add_nonduplicate x2 (... (add_nonduplicate xn [])))
 
   Now, we are ready to define add_nonduplicate as a function of its own. Try to
   define this function based on the description before it.
*)

(** Prepends x to the list xs if x is not a duplicate of the first element of
    xs.

   *****************************************************************************
*)
let add_nonduplicate x xs = match xs with
  | head :: tail when head = x -> xs (* the case when x is a duplicate of
                                        head *)
  | _ -> x :: xs (* for this case we know that x is not a duplicate of the head
                    of xs (we already checked it in the previous case) *)

(* Now, we are ready to define remove_duplicates again using fold:

   - our binary operation is add_nonduplicate
   - the start value (the return value for the base case) is []
*)
let remove_duplicates_using_fold xs = List.fold_right add_nonduplicate xs []

(* The definition of remove_duplicates_using_fold above is perfectly
   functional. However, it has a fatal flaw: it would overflow the stack for a
   large list because fold_right is not tail-recursive.

   If we know we are not going to use it for huge lists, we don't have to care
   about this problem. But, we will solve this issue. There are two potential
   solutions we can have:

   1. Define remove_duplicates using tail recursion, which is cumbersome.
   2. Convert the solution above to use fold_left.

   Try to convert the solution above to use fold_right yourself. The method I
   use below uses some algebra to solve this problem in the general case. You
   may find a less involved solution for this specific problem.

   *****************************************************************************

   It turns out that there is a pretty neat trick to convert between fold_left
   and fold_right using list reversal.  The trick we are going to use is going
   to involve solving a solution to a functional equation:

   Suppose we have a binary function f (such as add_nonduplicate), a starting
   value z, and a list [x1; ...; xn]. We want to find a function g such that

   (f x1 (f x2 (... (f xn z)))) = (g (... (g (g z xn) x(n-1)) x1) z)

   The left-hand side is what "fold_right f [x1; ...; xn] z" computes.
   The right-hand side is what "fold_left g z [xn; ...; x1]" computes.

   Once we find g, we can convert fold_right into fold_left + reverse.

   For the sake of visualization, the left-hand side (the call to fold_right)
   computes the following:

     f
    / \
   x1  f
      / \
     x2 ...
         \
          f
         / \
        xn  z

   And, the right-hand side (the call to fold_left _of the reversed list_)
   computes the following:

             g
            / \
           g  x1
          / \
        ... x2
        /
       g
      / \
     g  x(n-1)
    / \
   z  xn

   If we find a function g such that

   g z xn = f xn z
   g ... x(n-1) = f x(n-1) ...
   ...
   g ... x2 = f x2 ...
   g ... x1 = f x1 ...

   Then, we are done. Notice that the only thing g does is flipping the order of
   arguments. So, we can define g as follows:

   let g a b = f b a

   or,

   let g = fun a b -> f b a

   When we plug this definition of g to the call to fold_left, we get an
   implementation using fold_left:

   fold_right f [x1; ...; xn] z
   = fold_left g z [xn; ...; x1]
   = fold_left (fun a b -> f b a) z [xn; ...; x1]
   = fold_left (fun a b -> f b a) z (List.rev [x1; ...; xn])

   Now, we can specialize this to remove_duplicates (by substituting
   add_nonduplicate for f, and [] for z):
*)

let remove_duplicates_using_fold_left xs =
  List.fold_left (fun a b -> add_nonduplicate b a) [] (List.rev xs)

(* We are technically done, but we can make this code a bit more idiomatic. The
   operation of flipping the order of arguments of a binary function (that is,
   converting f to (fun a b -> f b a)) is a really common one. So, there is
   already a function in the standard library for it:

   Fun.flip f = fun a b -> f b a

   So, we can use Fun.flip to have the final version of remove_duplicates:

*)

let remove_duplicates' xs =
  List.fold_left (Fun.flip add_nonduplicate) [] (List.rev xs)

(* Let's finish up with running some tests to see that our function works. We
   will improvize something simple rather than using Dune and Alcotest.

   These are the examples from the beginning, with 1 for a, 2 for b, 3 for c,
   and 4 for d.
*)

(* We will need our string_of_list function from the lectures, let's bring it
   in *)
let rec string_of_list nums =
  let rec stringify nums = match nums with
    | [] -> []
    | h :: t -> string_of_int h :: stringify t
  and string_of_strings strs = match strs with
    | [] -> ""
    | first :: [] -> first
    | first :: rest -> first ^ "; " ^ string_of_strings rest
  and add_brackets s = "[" ^ s ^ "]"
  in 
  nums |> stringify |> string_of_strings |> add_brackets

(** A test case consisting of an input list and an output list *)
type test_case = {
  input: int list;
  output: int list;
}

(** Our test data is a list of test cases we will run *)
let test_data = [
    { input = []; output = [] };
    { input = [1]; output = [1] };
    { input = [1; 2]; output = [1; 2] };
    { input = [1; 1]; output = [1] };
    { input = [1; 2; 1]; output = [1; 2; 1] };
    { input = [1; 2; 2; 1]; output = [1; 2; 1] };
    { input = [1; 1; 2; 2; 2; 3; 1; 4; 4; 3]; output = [1; 2; 3; 1; 4; 3] };
  ]

(** A function that runs a function on a test case. It prints if a test
    fails. It returns "()" (a unit value).
*)
let run f case =
  let actual_output = f case.input in
  if actual_output <> case.output
  then print_endline ("The test failed on input " ^ string_of_list case.input)

(** A function that runs the given function on the whole test suite. *)
let run_on_all_tests f name =
 (* We can have semicolons to have a sequence of statements. We use this only
    for printing/debugging in this class. *)
  print_endline ("Running " ^ name ^ " on all tests.");
  (* List.iter is like List.map but it takes a function that returns unit, and
     doesn't build a list (it applies the function to each element only for the
     side effect). *)
  List.iter (run f) test_data
;;

(* Now, let's test all versions of remove_duplicates: *)

run_on_all_tests remove_duplicates "remove_duplicates (recursive version)";;
run_on_all_tests remove_duplicates_using_fold "remove_duplicates (using fold_right)";;
run_on_all_tests remove_duplicates_using_fold_left "remove_duplicates (using fold_left + rev)";;
run_on_all_tests remove_duplicates' "remove_duplicates (using fold_left + rev + flip)";;
