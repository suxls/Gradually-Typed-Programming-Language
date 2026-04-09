open! Core

(** {2 CESK Machine Types} *)

module rec Value : sig
  type t =
    | Object of Object.t
    | Proxy of Proxy.t
    | Number of float
end

and Object : sig
  type t = Name.t * (string, Value.t) Hashtbl.t * Wellformed_system.Method.t list
end

and Proxy : sig
  type t = Object.t * Wellformed_system.Shape.t
end

module Control : sig
  type t =
    | Search
    | Expression of Wellformed_system.Expr.t
    | Number of float
    | Object of Object.t
    | Proxy of Proxy.t
    | RuntimeErr of exn
end

module Environment : sig
  type t = (string, string) Hashtbl.t
end

module Store : sig
  type t = (string, Value.t) Hashtbl.t
end

module Closure : sig
  type t =
    | Closure of
        Wellformed_system.Def.t list
        * Wellformed_system.Stmt.t list
        * Wellformed_system.Expr.t option
        * Environment.t
    | ReturnType of Wellformed_system.Type.t
end

module Cesk : sig
  type t =
    { c : Control.t
    ; e : Environment.t
    ; s : Store.t
    ; k : Closure.t list
    }
end

(** Execute a linked program and return the final value. *)
val run : Linker.WellformedTypedProgram.t -> (Value.t, string) Result.t
