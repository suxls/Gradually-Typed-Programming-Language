open! Core

type t =
  | Name of string
  | ErrVar of string

val to_string : t -> string
val of_string : string -> t
val module_of_string : string -> t
val contain_err_name : t -> bool
val equal : t -> t -> bool
