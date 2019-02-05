open Ast_mapper

(** Same as [Ast_mapper.register] *)
val register : string -> (string list -> mapper) -> unit

(** Entry point *)
val run : unit -> unit
