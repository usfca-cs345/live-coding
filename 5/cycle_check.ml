let todo _ = failwith "todo"

(* An implementation of depth-first search to check if a graph is cyclic.

   The goal of this example is to show:

   1. How to represent graphs using adjacency maps in a functional setting.

   2. How to use OCaml's Map module, and to expose the module system.

   We will represent a graph using its adjacency map. The adjacency map is a map
   (as in the data structure, not the higher-order function) from each node to
   the set of nodes it points to (its successors).

   So, using mathematical notation, what we have is:

   adjacency_map : node -> node set

   In Java, we can realize this using Map and Set abstract classes:

   Map<Node, Set<Node>> adjacency_map;

   If we want to create a concrete map or set, we would choose between a HashMap
   vs TreeMap. Map in OCaml standard library is a TreeMap (persistent hash-map
   like data structures are a very recent invention so they are not in the
   standard library, other languages like Scala have those).

   If we want to create a TreeMap<Node>, we need to make sure that we can compare
   Node objects. How do we solve this problem in Java?

   OCaml solves this problem of "making sure that the input type (node) has an
   associated function (compare)" using modules. So, we are going to take a huge
   detour before getting back to this problem.
*)

(* A module in OCaml is just a bundle of code, like packages in Java. But it has
   a signature, and an implementation. The signature allows us to hide the
   implementation type while exposing an interface (i.e., "encapsulation" from
   CS 112).

   The syntax of declaring a module is:

   module <name> : <signature> = <implementation>

   So, the module's signature is the Interface in Java, and the implementation
   is the bundle of code that implements it.

   Let's define a module to represent an abstract data type for 2D vectors
*)

module Vector2D : sig
  type t (* This is the abstract type we declare, the users of the module don't
            know what t is *)

  (* These are the associated values, they are like let declarations but without
     a right-hand side. Think of them as methods in the interface *)

  (** The zero vector *)
  val zero : t
  (** Addition *)
  val add : t -> t -> t
  (** Multiply with a scalar *)
  val scale : float -> t -> t
  (** Build a vector from Cartesian coordinates *)
  val from_coord : float -> float -> t
  (** Convert a vector to a coordinate pair *)
  val to_coord : t -> (float * float)
  (** Build a vector from polar coordinates *)
  val from_polar : float -> float -> t
end = struct
  (* This part is the implementation of the vector module *)

  (* We will represent a vector as a pair of floats, but the users don't know it *)
  type t = float * float

  (* Implementation of our functions *)
  let zero = (0.0, 0.0)

  let add (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
  let scale c (x, y) = (c *. x, c *. y)

  (* A vector is just a pair of coordinates so these are pretty straightforward *)
  let from_coord x y = (x, y)
  let to_coord pair = pair

  (* we can define a helper function to use in our actual functions *)
  let rotate phi (x, y) =
    let sin = Float.sin phi
    and cos = Float.cos phi
    in (x *. cos -. y *. sin, x *. sin +. y *. cos)

  let from_polar z phi = scale z (rotate phi (1.0, 0.0))
end

(* Now, we can create and manipulate vectors *)

let onex : Vector2D.t = Vector2D.from_coord 1.0 0.0
let oney = Vector2D.from_polar 1.0 (Float.pi /. 2.)

(* Notice that the type Vector2D is opaque. We achieved encapsulation.

   We could also separate the interface and the implementation. Here is an
   interface for a cyclic group (think of integers mod N):
*)

module type CyclicGroup = sig
  type elem (* element type *)

  (** group operation *)
  val op : elem -> elem -> elem

  (** identity element *)
  val id : elem
    
  (** a generator for the cycle *)
  val gen : elem
end

(* Bits form a cycle group with xor:

   0 is identitiy: 0 xor x = x

   1 is the generator, repeatedly applying it generates all elements.

   Let's implement the Group interface for bits.

*)

module Bit = struct
  type elem = Zero | One

  (* Truth table for xor *)
  let op a b = match (a, b) with
    | (Zero, x) -> x
    | (x, Zero) -> x
    | _ -> Zero

  let ( && ) a b = if (a, b) = (One, One) then One else Zero
      
  let id = Zero
  let gen = One
end

(* We can also define digits with addition modulo 10 as a group:

   0 is the identity

   1 is a generator: 1 + 1 = 2, 1 + 1 + 1 = 3, ..., 1 + 1 + ... + 1 = 10 = 0 mod 10

*)


module Digit = struct
  type elem = int

  (* Addition mod 10 *)
  let op a b = (a + b) mod 10

  let id = 0
  let gen = 1
end

(* Now, we can define a generic function that derives the order of the group.

   It will apply the generator until it reaches the identity. How do we know
   which group we are talking about? We can pass the whole module as a
   parameter!
*)

let order (module G : CyclicGroup) =
  let rec compute_order order_so_far curr_elem =
    if curr_elem = G.id
    then order_so_far
    else compute_order (order_so_far + 1) (G.op G.gen curr_elem) 
  in
  compute_order 1 G.gen

(* So, modules let us define generic functions that expect an interface.

   Why did we go through all this trouble?

*)

(* OCaml's Map module is *parametric*, it takes another module M as a
   parameter. The module M provides the implementation for the comparison
   function for the keys of the map.

   For example, if we define the following module, we can use it to build a map
   that sorts its keys according to lexicographic ordering of int pairs.

*)

module IntPair = struct
  type t = int * int

  (* The comparison function we will pass to the map.

     It implements the lexicographic ordering:

     (x0, x1) < (y0, y1) if x0 < x1 or (x0 = x1 and y0 < y1)

     And, it uses the compare function for integers (it is just like the
     comparator method compare() used in Java.)

     compare returns an integer that is

     - negative if left-hand side is < right-hand side
     - zero if left-hand side is = right-hand side
     - positive if left-hand side is > right-hand side
  *)
  let compare (x0, x1) (y0, y1) = match compare x0 y0 with
    | 0 -> compare x1 y1
    | result -> result (* x0 <> y0, use the first comparison's result. *)

end

(* Now, we can create a map module with int pairs as keys.  Map module has a
   "module function" (called functor in OCaml) named Make for this purpose.  We
   won't go into OCaml's functors, the textbook has a chapter if you are
   interested in how they are useful for larger-scale software engineering.

   These "functors" have to be called with parentheses.
*)

module IntPairMap = Map.Make(IntPair)

(* IntPairMap.t is a generic map whose keys are int pairs (but the values can be
   any type).

   Let's create a map from these int pairs to Vector2D instances (recall that
   generic argument is first):
*)

let empty_map : Vector2D.t IntPairMap.t = IntPairMap.empty ;;

(* We can insert an element to this map using Map.add:

   (we will use IntPairMap.add. In general, when I say Map.foo, translate it to
   <MyMapModule>.foo in your head) *)

let my_first_pair = (3, 4)
let my_first_vector = Vector2D.from_coord 3.0 4.0
let singleton_map = IntPairMap.add my_first_pair my_first_vector empty_map

(* We can find an element in our map using find_opt:

   NOTE: You can look up the function signatures in the API docs. I will link
   them on Canvas.
*)

let three_four = IntPairMap.find_opt (3, 4) singleton_map
let three_five = IntPairMap.find_opt (3, 5) singleton_map

(* Now, let's get back to our original problem. We will use strings as nodes.
*)

type node = string

(* OCaml provides built-in modules that implement comparison, etc. So, creating
   a set of nodes (strings) is easy, we can just grab the String module: *)

module NodeSet = Set.Make(String)

(* Now, we can create a module for maps whose keys are nodes: *)

module NodeMap = Map.Make(String)

(* Finally, we can have a type alias for the adjacency maps, so that we don't
   have to write "NodeSet.t NodeMap.t" every time. I will use the name graph to
   communicate that this map type represents graphs.

*)

(** Mathematically, this is the types of adjacency maps: node -> node set *)
type graph = NodeSet.t NodeMap.t

(* Now, we can implement the cycle checking algorithm:

   1. Find the roots of the graph (nodes that do not have anything pointing to
   them).

   2. If the graph is nonempty and has no roots, then it is cyclic. Halt.

   3. Starting from this root set, perform a depth-first search maintaining the
   following:

      - The nodes in the current recursion stack (the path we took to reach this
      node). If we see the same node again, then there is a cycle. Halt.

*)

(* find the roots of the graph:

   1. Get all nodes that have a node that points to it. These are definitely not
   roots.
   2. Get all keys of the map that are not in the set in step 1.
*)
let find_roots (g : graph) : NodeSet.t =
  let non_roots =
    NodeMap.fold
      (fun _ neighbors nodes -> NodeSet.union neighbors nodes)
      g
      NodeSet.empty
  and all_nodes =
    NodeMap.fold
      (fun node _ node_set -> NodeSet.add node node_set) g NodeSet.empty
  in
  NodeSet.diff all_nodes non_roots

(* depth-first search for a cycle in a connected graph (there are no islands).

   1. Check if the current node is seen, if so stop.
   2. Add the current node to the seen node set.
   3. Visit all neighbors with this node set. If no cycle is found, return false.

*)

let rec is_there_cycle_from (g : graph) (current : node) (path_so_far : NodeSet.t) : bool =
  if NodeSet.mem current path_so_far then true (* found a cycle *)
  else
    let neighbors = NodeMap.find current g
    and new_path = NodeSet.add current path_so_far
    in
    NodeSet.exists (fun n -> is_there_cycle_from g n new_path) neighbors

(* the actual function that checks cycles using is_there_cycle_from *)

let is_cyclic (g : graph) : bool =
  let roots : NodeSet.t = find_roots g in
  if NodeSet.is_empty roots && not (NodeMap.is_empty g) then true
  else
    NodeSet.exists (fun root -> is_there_cycle_from g root NodeSet.empty) roots
    
(* some test graphs *)

let build_graph (pairs : (node * node list) list) =
  List.fold_left
    (fun g (node, neighbor_list) ->
       NodeMap.add node (NodeSet.of_list neighbor_list) g)
    NodeMap.empty
    pairs

let empty = build_graph []

let singleton = build_graph ["a", []]
let singleton_cyclic = build_graph ["a", ["a"]]
let tree = build_graph [
    "a", ["b"; "c"];
    "b", ["d"];
    "c", [];
    "d", [];
  ]
let dag = build_graph [
    "a", ["b"; "c"];
    "b", ["d"];
    "c", ["d"];
    "d", [];
  ]
let cycle_no_root = build_graph [
    "a", ["b"; "c"];
    "b", ["d"];
    "c", [];
    "d", ["a"];
  ]

let cycle_with_root = build_graph [
    "a", ["b"; "c"];
    "b", ["d"];
    "c", [];
    "d", ["b"];
  ]

let long_cycle_with_root = build_graph [
    "e", ["f"; "g"];
    "f", ["g"];
    "g", ["a"];
    "a", ["b"; "c"];
    "b", ["d"];
    "c", [];
    "d", ["a"];
  ]

let acyclic_graphs = [empty; singleton; tree; dag]
let cyclic_graphs = [cycle_no_root; cycle_with_root; long_cycle_with_root]
