let todo _ = failwith "todo"

(* Records *)

type command = {
    program : string;
    arguments : string list;
  }
;;

let ls_detail = { program = "ls"; arguments = ["-l"; "-h"] }

(** Convert a command to the string

    'program' 'argument1' 'argument2' ...

    Try using fold_left
 *)
let string_of_command cmd =
  let quote s = "'" ^ s ^ "'" in
  List.fold_left (fun cmd_line arg -> cmd_line ^ " " ^ quote arg)
    (quote cmd.program)
    cmd.arguments

type machine = {
    name : string;
    address : string;
    user : string;
}

let stargate = {
  name = "stargate";
  address = "stargate.cs.usfca.edu";
  user = "memre";
}

(* This is a type alias, so that we don't have to write float * float every time. *)
type vector2d = float * float

(* This is a generic type that holds a pair *)
type 'a with_dir = 'a * string

(* java: class WithDir<A> { A first; String second; } *)

(* Variants *)

type weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

let lecture_days = [Tuesday; Thursday] ;;

(** Returns the next day of the week. *)
let next_day day = match day with
  | Monday   -> Tuesday
  | Tuesday  -> Wednesday
  | Wednesday-> Thursday
  | Thursday -> Friday
  | Friday   -> Saturday
  | Saturday -> Sunday
  | Sunday   -> Monday

type gatewayed_machine = { gateway : machine; target : machine }

type location = Local
              | Remote of machine
(* We could add more cases like:
   | RemoteWithGateway of gatewayed_machine *)

(* Prefix for a location.

   For local locations, it is empty.
   For remote locations, it is <user>@<address>:

   Notice that we can pattern match on records.
 *)
let location_prefix loc = match loc with
  | Local -> ""
  | Remote { user; address } -> user ^ "@" ^ address ^ ":" 

type file = {
    name : string;
    location : location with_dir;
}

(** Returns the file path as a string.

    That is <location prefix><directory>/<file>

    NOTE: Records have a single pattern to match, so we can match agaisnt them
    in a let binding.

*)
let file_path { name = name; location = (location, dir) } =
    (location_prefix location) ^ dir ^ "/" ^ name

(* These two functions need type signatures to resolve the ambiguity on whether
   f (or m) is a file or a machine. *)

(** Returns the name of the given file *)
let file_name (f : file) = f.name

(** Returns the name of the given machine *)
let machine_name (m : machine) = m.name

(** High-level operations *)
type operation =
    Fetch of string * file (* fetching a url and saving it to a file *)
  | Open of file (* opening a file *)
;;

(** Generates the command to perform the given operation.

    This is like a compiler, it converts a high-level representation (an
    abstract operation like fetching a file) to another representation to enable
    running it (a shell command).

    The specific commands we call are not as important.
*)
let gen_command operation = match operation with
  (* we can match nested patterns, here we are extracting out whether a file has
     a remote location to cover the error cases.  *)
  | Open { name = _; location = (Remote _, _) } ->
    failwith "opening remote locations is not supported"
  | Fetch (url, { name = _; location = (Remote _, _) }) ->
    failwith "cannot fetch to a remote file"
  | Open file -> {
      program = "open"; (* on Linux, use xdg-open *)
      arguments = [file_path file]
    }
  | Fetch (url, file) -> {
      program = "curl"; (* on Ubuntu, use wget or make sure you have curl installed *)
      arguments = [url; "-o"; file_path file]
    }

(** The commands that fetch and display XKCD 1312 *)
let xkcd1312 filename =
  let file = { name = filename; location = (Local, "."); }
  and url = "https://imgs.xkcd.com/comics/haskell.png"
  in
  List.map gen_command [
      Fetch (url, file);
      Open file
  ]

(** Runs the given command, and returns whether it was successful *)
let run command : bool = Sys.command (string_of_command command) = 0

(** Runs the given set of commands until failure.

    That is, if a command fails, we will stop running the rest.

    Try expressing this with fold.

    - what data do I need to keep track of?
    - how do I combine it with the result of running the current command.

    See the lecture video for how the arguments to fold_left are derived.
*)
let run' commands =
  List.fold_left
    (fun success command -> if success then run command else success)
    true
    commands
