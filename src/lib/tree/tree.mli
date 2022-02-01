(** Generic trees *)

(** The type of trees *)
type 'a t = Label of 'a * 'a t list | Tuple of 'a t list

val annotate : 'a t -> ('a * string) t
(** Annotate a tree with unique identifiers. *)

(** {2 Iterators} *)

val fold : label:('a -> 'b list -> 'b) -> tuple:('b list -> 'b) -> 'a t -> 'b

(** A tail-recursive fold on trees. *)

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
