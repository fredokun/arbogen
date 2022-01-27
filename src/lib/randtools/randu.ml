module State = struct
  type t = int

  let to_bytes x = Marshal.to_bytes x []

  let from_bytes buf = Marshal.from_bytes buf 0
end

let name = "randu"

let state = ref 3

let max_mod = 2 lsl 31

let max_mod_f = float_of_int max_mod

let init n = state := n

let self_init () =
  Random.self_init ();
  state := Random.int (2 lsl 20)

let get_state () = !state

let set_state s = state := s

let int n =
  let r = 65539 * !state mod max_mod in
  state := r;
  r mod n

(* FIXME: this is not uniform! *)

let float f =
  let n = int max_mod in
  state := n;
  f *. (float_of_int n /. max_mod_f)

(* XXX. Precision? *)
