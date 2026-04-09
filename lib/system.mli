module Hash_set = Base.Hash_set
module Sexp = Core.Sexp

module Error : sig
  type t =
    | ParseError of string
    | DuplicateClassNameError of string
    | DuplicateClassVariableError of string
    | BadImportError of string
    | UndeclaredVariableError of string
    | DuplicateModuleNameError of string
    | TypeError of string
end

(** {2 AST Node Types} *)

(** Statement ::= (Variable = Expression)
               | (if0 Expression Block Block)
               | (while0 Expression Block)
               | (Variable --> FieldName = Expression) *)
module rec Stmt : sig
  type t =
    | AssignVar of Name.t * Expr.t
    | If0 of Expr.t * Block.t * Block.t
    | While0 of Expr.t * Block.t
    | AssignField of Name.t * Name.t * Expr.t
    | ErrStmt of Error.t
end

(** Block ::= Statement
           | (block Declaration* Statement+) *)
and Block : sig
  type t =
    | Stmt of Stmt.t
    | Block of Def.t list * Stmt.t list
    | ErrBlock of Error.t
end

(** Expression ::= GoodNumber | Variable
                | (Variable + Variable) | (Variable / Variable) | (Variable == Variable)
                | (new ClassName (Variable* ))
                | (Variable --> FieldName) | (Variable --> MethodName (Variable* ))
                | (Variable isa ClassName) *)
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
    | ErrExpr of Error.t
end

(** Declaration ::= (def Variable Expression) *)
and Def : sig
  type t =
    | Def of Name.t * Expr.t
    | ErrDef of Error.t
end

(** Method ::= (method MethodName (Parameter* ) Declaration* Statement* Expression) *)
module Method : sig
  type t =
    | Method of Name.t * Name.t list * Def.t list * Stmt.t list * Expr.t
    | ErrMethod of Error.t
end

(** {2 Type System} *)

(** Type ::= Number | Shape *)
module rec Type : sig
  type t =
    | Number
    | Shape of Shape.t
    | ErrType of Error.t
end

(** Shape ::= ((FieldType* ) (MethodType* )) *)
and Shape : sig
  type t =
    | Shape of FieldType.t list * MethodType.t list
    | ErrShape of Error.t
end

(** FieldType ::= (FieldName Type) *)
and FieldType : sig
  type t =
    | FieldType of Name.t * Type.t
    | ErrFieldType of Error.t
end

(** MethodType ::= (MethodName (Type* ) Type) *)
and MethodType : sig
  type t =
    | MethodType of Name.t * Type.t list * Type.t
    | ErrMethodType of Error.t
end

module ClassShape : sig
  type t =
    | ModuleClassShape
    | TModuleClassShape of Shape.t
end

(** Class ::= (class ClassName (FieldName* ) Method* ) *)
module Class : sig
  type t =
    | Class of Name.t * Name.t list * Method.t list * ClassShape.t
    | ErrClass of Error.t
end

module VarSet : sig
  type t = string Hash_set.t list * string Hash_set.t * string Hash_set.t
end

(** Program ::= (Class* Declaration* Statement* Expression) *)
module Program : sig
  type t =
    | Program of Def.t list * Stmt.t list * Expr.t
    | ErrProgram of Error.t
end

(** {2 Module System} *)

(** Import ::= (import ModuleName) *)
module Import : sig
  type t =
    | Import of Name.t
    | ErrImport of Error.t
end

(** MixedImport ::= (import ModuleName) | (timport ModuleName Shape) *)
module MixedImport : sig
  type t =
    | Import of Name.t
    | TypedImport of Name.t * Shape.t
    | ErrImport of Error.t
end

(** MixedModule ::= (tmodule ModuleName MixedImport* Class Shape)
                 | (module ModuleName Import* Class) *)
module MixedModule : sig
  type t =
    | MixedModule of Name.t * MixedImport.t list * Class.t * Shape.t
    | Module of Name.t * Import.t list * Class.t
    | ErrModule of Error.t
end

(** MixedSystem ::= (MixedModule* MixedImport* Declaration* Statement* Expression) *)
module MixedSystem : sig
  type t =
    | MixedSystem of
        MixedModule.t list * MixedImport.t list * Def.t list * Stmt.t list * Expr.t
    | ErrMixedSystem of Error.t
end

(** {2 Pipeline Functions} *)

val type_equal : Type.t -> Type.t -> bool
val parse : Sexp.t -> MixedSystem.t
val validate_system_duplicates : MixedSystem.t -> (MixedSystem.t, Error.t) result
val validate_bad_import_errors : MixedSystem.t -> MixedSystem.t
val validate_undeclared_vars : MixedSystem.t -> MixedSystem.t
val ensure_no_system_errors : MixedSystem.t -> (MixedSystem.t, string) result
val get_error_message : Error.t -> string
val find_module_from_import : Name.t -> MixedModule.t list -> MixedModule.t
val get_classname_from_module : MixedModule.t -> string
