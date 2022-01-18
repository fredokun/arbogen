module State = struct
  type t = Random.State.t

  let to_bytes x = Marshal.to_bytes x []

  let from_bytes buf = Marshal.from_bytes buf 0
end

let name = "ocaml"

let init = Random.init

let self_init = Random.self_init

let int = Random.int

let float = Random.float

let get_state = Random.get_state

let set_state = Random.set_state
