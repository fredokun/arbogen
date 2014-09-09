module type Sig =
sig
  val name : string
  val init : int -> unit
  val self_init : unit -> unit
  val int : int -> int
  val float : float -> float
  val get_state : unit -> Random.State.t
  val set_state : Random.State.t -> unit
end

module OcamlRandom : Sig =
struct
  let name = "ocaml"
  let init = Random.init
  let self_init = Random.self_init
  let int = Random.int
  let float = Random.float
  let get_state = Random.get_state
  let set_state = Random.set_state
end

module Randu : Sig =
struct
  let name = "randu"
  let state = ref 3
  let max_mod = 2 lsl 31
  let max_mod_f = float_of_int max_mod

  type random_state_t = { st : int array; mutable idx : int }

  let init n = state := n

  let self_init () = Random.self_init (); state := Random.int (2 lsl 20)

  let int n =
    let r = ((65539 * !state ) mod max_mod) in
    state := r;
    r mod n

  let float f = 
    let n = (int max_mod) in
    state := n;
    f *. ((float_of_int n) /. max_mod_f)

  let get_state () = Obj.magic { st = Array.make 55 0; idx = !state }

  let set_state s = 
    let { st = _ ; idx = n} = Obj.magic s in
    state := n
end
  
