(** Type-checks a mixed system, returning the system with type errors
    annotated as [ErrExpr], [ErrStmt], etc. *)
val typecheck_mixed_system : System.MixedSystem.t -> System.MixedSystem.t
