(** Generic trees *)

(** The type of trees *)
type 'a t = Node of 'a * ('a t list)


(** Annotate a tree with unique identifiers. *)
val annotate: 'a t -> ('a * string) t


(** {2 Output functions} *)

(** Print a tree to an out channel in arb format *)
val output_arb:
  show_type:bool ->
  show_id:bool ->
  indent:bool ->
  out_channel ->
  (string * string) t
  -> unit

(** Print a tree to an out channel in dot format *)
val output_dot:
  show_type:bool ->
  show_id:bool ->
  indent:bool ->
  out_channel ->
  (string * string) t
  -> unit

(** Print a tree to an out channel in xml format *)
val output_xml:
  show_type:bool ->
  show_id:bool ->
  indent:bool ->
  out_channel ->
  (string * string) t
  -> unit
