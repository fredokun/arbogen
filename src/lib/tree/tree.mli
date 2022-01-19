(** Generic trees *)

(** The type of trees *)
type 'a t = Node of 'a * 'a t list

val annotate : 'a t -> ('a * string) t
(** Annotate a tree with unique identifiers. *)

(** {2 Output functions} *)

val output_arb :
     show_type:bool
  -> show_id:bool
  -> indent:bool
  -> out_channel
  -> (string * string) t
  -> unit
(** Print a tree to an out channel in arb format *)

val output_dot :
     show_type:bool
  -> show_id:bool
  -> indent:bool
  -> out_channel
  -> (string * string) t
  -> unit
(** Print a tree to an out channel in dot format *)

val output_xml :
     show_type:bool
  -> show_id:bool
  -> indent:bool
  -> out_channel
  -> (string * string) t
  -> unit
(** Print a tree to an out channel in xml format *)