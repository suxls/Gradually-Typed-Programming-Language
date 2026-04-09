(** The final linked program: a flat list of classes, definitions,
    statements, and a final expression. *)
module WellformedTypedProgram : sig
  type t =
    Wellformed_system.Class.t list
    * Wellformed_system.Def.t list
    * Wellformed_system.Stmt.t list
    * Wellformed_system.Expr.t
end

(** Resolve module-qualified class names across the system,
    producing a flat linked program ready for execution. *)
val link_system : Wellformed_system.PostSoundLinkerSystem.t -> WellformedTypedProgram.t
