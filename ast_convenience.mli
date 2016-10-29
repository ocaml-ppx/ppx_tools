(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(** {1 Convenience functions to help build and deconstruct AST fragments.} *)

open Asttypes
open Ast_helper
open Parsetree

(** {2 Compatibility modules} *)

module Label : sig
  type t = Asttypes.arg_label

  type desc = Asttypes.arg_label =
      Nolabel
    | Labelled of string
    | Optional of string

  val explode : t -> desc

  val nolabel : t
  val labelled : string -> t
  val optional : string -> t

end

(** {2 Provides a unified abstraction over differences in Parsetree.constant and Asttypes.constant 
 * types defined in ocaml 4.03 and 4.02 respectively}*)
module Constant : sig 
  type t = Parsetree.constant =
     Pconst_integer of string * char option 
   | Pconst_char of char 
   | Pconst_string of string * string option 
   | Pconst_float of string * char option 
 
  (** Convert Asttypes.constant to Constant.t *) 
  val of_constant : Parsetree.constant -> t

  (** Convert Constant.t to Asttypes.constant *)
  val to_constant : t -> Parsetree.constant

end

(** Abstracts the addition of Ppat_open in OCaml 4.04. *)
module Pattern : sig
  type 'a loc = 'a Asttypes.loc
  type constant = Constant.t

  type t =
      Ppat_any
    | Ppat_var of string loc
    | Ppat_alias of pattern * string loc
    | Ppat_constant of constant
    | Ppat_interval of constant * constant
    | Ppat_tuple of pattern list
    | Ppat_construct of Longident.t loc * pattern option
    | Ppat_variant of label * pattern option
    | Ppat_record of (Longident.t loc * pattern) list * closed_flag
    | Ppat_array of pattern list
    | Ppat_or of pattern * pattern
    | Ppat_constraint of pattern * core_type
    | Ppat_type of Longident.t loc
    | Ppat_lazy of pattern
    | Ppat_unpack of string loc
    | Ppat_exception of pattern
    | Ppat_extension of extension
    | Ppat_open of Longident.t loc * pattern

  val of_pattern_desc : Parsetree.pattern_desc -> t

  (** Converts [Pattern.t] to [Parsetree.pattern_desc]. If the given value is
      constructed with [Ppat_open] and the OCaml version is less than [4.04],
      raises [Failure]. *)
  val to_pattern_desc : t -> Parsetree.pattern_desc

  (** [true] if and only if [Ppat_open] is available (OCaml >= [4.04]). *)
  val have_ppat_open : bool

  (** On OCaml >= [4.04], forwards to [Ast_helper.Pat.open_]. Otherwise, raises
      [Failure]. *)
  val open_ : ?loc:Ast_helper.loc -> ?attrs:attrs -> lid -> pattern -> pattern
end

(** {2 Misc} *)

val lid: ?loc:loc -> string -> lid

(** {2 Expressions} *)

val evar: ?loc:loc -> ?attrs:attrs -> string -> expression
val let_in: ?loc:loc -> ?attrs:attrs -> ?recursive:bool -> value_binding list -> expression -> expression

val constr: ?loc:loc -> ?attrs:attrs -> string -> expression list -> expression
val record: ?loc:loc -> ?attrs:attrs -> ?over:expression -> (string * expression) list -> expression
val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression

val nil: ?loc:loc -> ?attrs:attrs -> unit -> expression
val cons: ?loc:loc -> ?attrs:attrs -> expression -> expression -> expression
val list: ?loc:loc -> ?attrs:attrs -> expression list -> expression

val unit: ?loc:loc -> ?attrs:attrs -> unit -> expression

val func: ?loc:loc -> ?attrs:attrs -> (pattern * expression) list -> expression
val lam: ?loc:loc -> ?attrs:attrs -> ?label:Label.t -> ?default:expression -> pattern -> expression -> expression
val app: ?loc:loc -> ?attrs:attrs -> expression -> expression list -> expression

val str: ?loc:loc -> ?attrs:attrs -> string -> expression
val int: ?loc:loc -> ?attrs:attrs -> int -> expression
val int32: ?loc:loc -> ?attrs:attrs -> int32 -> expression
val int64: ?loc:loc -> ?attrs:attrs -> int64 -> expression
val char: ?loc:loc -> ?attrs:attrs -> char -> expression
val float: ?loc:loc -> ?attrs:attrs -> float -> expression

val sequence: ?loc:loc -> ?attrs:attrs -> expression list -> expression
(** Return [()] if the list is empty. Tail rec. *)

(** {2 Patterns} *)

val pvar: ?loc:loc -> ?attrs:attrs -> string -> pattern
val pconstr: ?loc:loc -> ?attrs:attrs -> string -> pattern list -> pattern
val precord: ?loc:loc -> ?attrs:attrs -> ?closed:closed_flag -> (string * pattern) list -> pattern
val ptuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern

val pnil: ?loc:loc -> ?attrs:attrs -> unit -> pattern
val pcons: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
val plist: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern

val pstr: ?loc:loc -> ?attrs:attrs -> string -> pattern
val pint: ?loc:loc -> ?attrs:attrs -> int -> pattern
val pchar: ?loc:loc -> ?attrs:attrs -> char -> pattern
val pfloat: ?loc:loc -> ?attrs:attrs -> float -> pattern

val punit: ?loc:loc -> ?attrs:attrs -> unit -> pattern


(** {2 Types} *)

val tconstr: ?loc:loc -> ?attrs:attrs -> string -> core_type list -> core_type

(** {2 AST deconstruction} *)

val get_str: expression -> string option
val get_str_with_quotation_delimiter: expression -> (string * string option) option
val get_lid: expression -> string option

val has_attr: string -> attributes -> bool
val find_attr: string -> attributes -> payload option
val find_attr_expr: string -> attributes -> expression option
