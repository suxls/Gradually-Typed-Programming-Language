module Hash_set = Core.Hash_set

(** {2 Well-formed AST Types}

    Mirror types of [System] but without error variants, representing
    a validated program that has passed all static checks. *)

module rec Stmt : sig
  type t =
    | AssignVar of Name.t * Expr.t
    | If0 of Expr.t * Block.t * Block.t
    | While0 of Expr.t * Block.t
    | AssignField of Name.t * Name.t * Expr.t
end

and Block : sig
  type t =
    | Stmt of Stmt.t
    | Block of Def.t list * Stmt.t list
end

and Expr : sig
  type t =
    | Var of Name.t
    | Gnum of float
    | Plus of Name.t * Name.t
    | Divide of Name.t * Name.t
    | Equal of Name.t * Name.t
    | Instance of Name.t * Name.t list
    | FieldAccess of Name.t * Name.t
    | MethodCall of Name.t * Name.t * Name.t list
    | IsA of Name.t * Name.t
end

and Def : sig
  type t = Name.t * Expr.t
end

module Method : sig
  type t = Name.t * Name.t list * Def.t list * Stmt.t list * Expr.t
end

module VarSet : sig
  type t = string Hash_set.t
end

module Import : sig
  type t = Name.t
end

module rec Type : sig
  type t =
    | Number
    | Shape of Shape.t
end

and Shape : sig
  type t = FieldType.t list * MethodType.t list
end

and FieldType : sig
  type t = Name.t * Type.t
end

and MethodType : sig
  type t = Name.t * Type.t list * Type.t
end

module MixedImport : sig
  type t =
    | Import of Name.t
    | TypedImport of Name.t * Shape.t
end

module ClassShape : sig
  type t =
    | ModuleClassShape
    | TModuleClassShape of Shape.t
end

module Class : sig
  type t = Name.t * Name.t list * Method.t list * ClassShape.t
end

module MixedModule : sig
  type t =
    | MixedModule of Name.t * MixedImport.t list * Class.t * Shape.t
    | Module of Name.t * Import.t list * Class.t
end

module WellformedMixedSystem : sig
  type t = MixedModule.t list * MixedImport.t list * Def.t list * Stmt.t list * Expr.t
end

module PostSoundLinkerSystem : sig
  type t = MixedModule.t list * Import.t list * Def.t list * Stmt.t list * Expr.t
end

(** Convert a validated [System.MixedSystem.t] into a well-formed system
    (stripping all error variants). *)
val wellformed_typed_system_from_mixed_system
  :  System.MixedSystem.t
  -> (WellformedMixedSystem.t, string) result

(** Perform sound linking: synthesize typed module copies for [timport]s
    so that each typed import gets a consistently-shaped module. *)
val sound_link_system : WellformedMixedSystem.t -> PostSoundLinkerSystem.t

val print_sound_linker_result : PostSoundLinkerSystem.t -> string
