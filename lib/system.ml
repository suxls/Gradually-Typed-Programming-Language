module Hash_set = Base.Hash_set
module Hashtbl = Base.Hashtbl
module List = Core.List
module Sexp = Core.Sexp
module String = Core.String
module Result = Core.Result

(* ------------------------------------------------------------------ *)
(* TYPE DEFINITIONS                                                    *)
(* ------------------------------------------------------------------ *)

module Error : sig
  type t =
    | ParseError of string
    | DuplicateClassNameError of string
    | DuplicateClassVariableError of string
    | BadImportError of string
    | UndeclaredVariableError of string
    | DuplicateModuleNameError of string
    | TypeError of string
end = struct
  type t =
    | ParseError of string
    | DuplicateClassNameError of string
    | DuplicateClassVariableError of string
    | BadImportError of string
    | UndeclaredVariableError of string
    | DuplicateModuleNameError of string
    | TypeError of string
end

module rec Stmt : sig
  type t =
    | AssignVar of Name.t * Expr.t
    | If0 of Expr.t * Block.t * Block.t
    | While0 of Expr.t * Block.t
    | AssignField of Name.t * Name.t * Expr.t
    | ErrStmt of Error.t
end = struct
  type t =
    | AssignVar of Name.t * Expr.t
    | If0 of Expr.t * Block.t * Block.t
    | While0 of Expr.t * Block.t
    | AssignField of Name.t * Name.t * Expr.t
    | ErrStmt of Error.t
end

and Block : sig
  type t =
    | Stmt of Stmt.t
    | Block of Def.t list * Stmt.t list
    | ErrBlock of Error.t
end = struct
  type t =
    | Stmt of Stmt.t
    | Block of Def.t list * Stmt.t list
    | ErrBlock of Error.t
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
    | ErrExpr of Error.t
end = struct
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

and Def : sig
  type t =
    | Def of Name.t * Expr.t
    | ErrDef of Error.t
end = struct
  type t =
    | Def of Name.t * Expr.t
    | ErrDef of Error.t
end

module Method : sig
  type t =
    | Method of Name.t * Name.t list * Def.t list * Stmt.t list * Expr.t
    | ErrMethod of Error.t
end = struct
  type t =
    | Method of Name.t * Name.t list * Def.t list * Stmt.t list * Expr.t
    | ErrMethod of Error.t
end

module rec Type : sig
  type t =
    | Number
    | Shape of Shape.t
    | ErrType of Error.t
end = struct
  type t =
    | Number
    | Shape of Shape.t
    | ErrType of Error.t
end

and Shape : sig
  type t =
    | Shape of FieldType.t list * MethodType.t list
    | ErrShape of Error.t
end = struct
  type t =
    | Shape of FieldType.t list * MethodType.t list
    | ErrShape of Error.t
end

and FieldType : sig
  type t =
    | FieldType of Name.t * Type.t
    | ErrFieldType of Error.t
end = struct
  type t =
    | FieldType of Name.t * Type.t
    | ErrFieldType of Error.t
end

and MethodType : sig
  type t =
    | MethodType of Name.t * Type.t list * Type.t
    | ErrMethodType of Error.t
end = struct
  type t =
    | MethodType of Name.t * Type.t list * Type.t
    | ErrMethodType of Error.t
end

module ClassShape : sig
  type t =
    | ModuleClassShape
    | TModuleClassShape of Shape.t
end = struct
  type t =
    | ModuleClassShape
    | TModuleClassShape of Shape.t
end

module Class : sig
  type t =
    | Class of Name.t * Name.t list * Method.t list * ClassShape.t
    | ErrClass of Error.t
end = struct
  type t =
    | Class of Name.t * Name.t list * Method.t list * ClassShape.t
    | ErrClass of Error.t
end

(* VarSet: (scoped variable sets, module names, class names) *)
module VarSet : sig
  type t = string Hash_set.t list * string Hash_set.t * string Hash_set.t
end = struct
  type t = string Hash_set.t list * string Hash_set.t * string Hash_set.t
end

module Program : sig
  type t =
    | Program of Def.t list * Stmt.t list * Expr.t
    | ErrProgram of Error.t
end = struct
  type t =
    | Program of Def.t list * Stmt.t list * Expr.t
    | ErrProgram of Error.t
end

module Import : sig
  type t =
    | Import of Name.t
    | ErrImport of Error.t
end = struct
  type t =
    | Import of Name.t
    | ErrImport of Error.t
end

module MixedImport : sig
  type t =
    | Import of Name.t
    | TypedImport of Name.t * Shape.t
    | ErrImport of Error.t
end = struct
  type t =
    | Import of Name.t
    | TypedImport of Name.t * Shape.t
    | ErrImport of Error.t
end

module MixedModule : sig
  type t =
    | MixedModule of Name.t * MixedImport.t list * Class.t * Shape.t
    | Module of Name.t * Import.t list * Class.t
    | ErrModule of Error.t
end = struct
  type t =
    | MixedModule of Name.t * MixedImport.t list * Class.t * Shape.t
    | Module of Name.t * Import.t list * Class.t
    | ErrModule of Error.t
end

module MixedSystem : sig
  type t =
    | MixedSystem of
        MixedModule.t list * MixedImport.t list * Def.t list * Stmt.t list * Expr.t
    | ErrMixedSystem of Error.t
end = struct
  type t =
    | MixedSystem of
        MixedModule.t list * MixedImport.t list * Def.t list * Stmt.t list * Expr.t
    | ErrMixedSystem of Error.t
end

(* ------------------------------------------------------------------ *)
(* VARIABLE SET HELPERS                                                *)
(* ------------------------------------------------------------------ *)

let var_is_missing (var_set : VarSet.t) (v : string) : bool =
  match var_set with
  | variable_set, _, _ ->
    not (List.exists variable_set ~f:(fun set -> Hash_set.mem set v))
;;

let class_name_is_missing (var_set : VarSet.t) (class_name : string) : bool =
  match var_set with
  | _, _, class_names_set -> not (Hash_set.mem class_names_set class_name)
;;

let module_name_is_missing (var_set : VarSet.t) (module_name : string) : bool =
  match var_set with
  | _, module_names_set, _ -> not (Hash_set.mem module_names_set module_name)
;;

let initialize_var_set () : VarSet.t =
  let variable_set = Hash_set.create (module String) in
  let module_names_set = Hash_set.create (module String) in
  let class_names_set = Hash_set.create (module String) in
  [ variable_set ], module_names_set, class_names_set
;;

let add_new_scope (var_set : VarSet.t) : VarSet.t =
  match var_set with
  | variable_set, module_name_set, class_name_set ->
    (match variable_set with
     | [] ->
       raise (Exceptions.UnexpectedError "unexpected error: hashset failed to initialize")
     | _ ->
       Hash_set.create (module String) :: variable_set, module_name_set, class_name_set)
;;

let add_var_to_current_scope (var_set : VarSet.t) (v : string) : VarSet.t =
  match var_set with
  | variable_set, module_name_set, class_name_set ->
    (match variable_set with
     | [] -> raise (Exceptions.UnexpectedError "unexpected error: no existing scope")
     | hd :: tl ->
       let new_head = Hash_set.create (module String) in
       Hash_set.iter hd ~f:(fun s -> Hash_set.add new_head s);
       Hash_set.add new_head v;
       new_head :: tl, module_name_set, class_name_set)
;;

let clear_class_name_set (var_set : VarSet.t) : VarSet.t =
  match var_set with
  | variable_set, module_name_set, class_name_set ->
    Hash_set.clear class_name_set;
    variable_set, module_name_set, class_name_set
;;

let add_class_to_class_name_set (var_set : VarSet.t) (c : string list) : VarSet.t =
  List.fold c ~init:var_set ~f:(fun var_set c ->
    match var_set with
    | variable_set, module_name_set, class_name_set ->
      Hash_set.add class_name_set c;
      variable_set, module_name_set, class_name_set)
;;

let add_module_to_module_name_set (var_set : VarSet.t) (m : string) : VarSet.t =
  match var_set with
  | variable_set, module_name_set, class_name_set ->
    Hash_set.add module_name_set m;
    variable_set, module_name_set, class_name_set
;;

let get_error_message (error : Error.t) : string =
  match error with
  | Error.DuplicateClassNameError _ -> "\"duplicate class name\""
  | Error.DuplicateClassVariableError _ ->
    "\"duplicate method, field, or parameter name\""
  | Error.BadImportError _ -> "\"bad import\""
  | Error.UndeclaredVariableError _ -> "\"undeclared variable error\""
  | Error.ParseError _ -> "\"parser error\""
  | Error.DuplicateModuleNameError _ -> "\"duplicate module name\""
  | Error.TypeError _ -> "\"type error\""
;;

(* ------------------------------------------------------------------ *)
(* PARSING (S-expression -> AST)                                       *)
(* ------------------------------------------------------------------ *)

let unpack_program (program : Program.t) : Def.t list * Stmt.t list * Expr.t =
  match program with
  | Program (def_list, stmt_list, expr) -> def_list, stmt_list, expr
  | ErrProgram _ -> [], [], ErrExpr (ParseError "parser error")
;;

let is_list_of_sexp_all_atoms (sexps : Sexp.t list) : bool =
  List.for_all sexps ~f:(function
    | Sexp.Atom _ -> true
    | _ -> false)
;;

let split_typed_module_sexps_from_rest (typed_module_and_rest_sexp : Sexp.t list)
  : Sexp.t list * Sexp.t list
  =
  List.split_while typed_module_and_rest_sexp ~f:(function
    | Sexp.List (Sexp.Atom "tmodule" :: Sexp.Atom _ :: _) -> true
    | Sexp.List (Sexp.Atom "module" :: Sexp.Atom _ :: _) -> true
    | _ -> false)
;;

let split_import_sexps_from_rest (import_and_rest_sexp : Sexp.t list)
  : Sexp.t list * Sexp.t list
  =
  List.split_while import_and_rest_sexp ~f:(function
    | Sexp.List (Sexp.Atom "import" :: Sexp.Atom _ :: _) -> true
    | Sexp.List (Sexp.Atom "timport" :: Sexp.Atom _ :: _) -> true
    | _ -> false)
;;

let split_def_sexps_from_rest (defs_and_rest_sexp : Sexp.t list)
  : Sexp.t list * Sexp.t list
  =
  List.split_while defs_and_rest_sexp ~f:(function
    | Sexp.List [ Sexp.Atom "def"; Sexp.Atom _; _ ] -> true
    | _ -> false)
;;

let mixed_import_from_import (import : Import.t) : MixedImport.t =
  match import with
  | Import module_ -> MixedImport.Import module_
  | ErrImport e -> MixedImport.ErrImport e
;;

let mixed_import_to_import (import : MixedImport.t) : Import.t =
  match import with
  | Import name | TypedImport (name, _) -> Import.Import name
  | ErrImport e -> Import.ErrImport e
;;

let expr_of_atom_str (s : string) : Expr.t =
  match float_of_string_opt s with
  | Some num -> Gnum num
  | None -> Var (Name.of_string s)
;;

let wellformed_block_of_defs_and_stmts (defs : Def.t list) (stmts : Stmt.t list) : Block.t
  =
  if List.is_empty stmts
  then ErrBlock (Error.ParseError "Block requires at least one statement")
  else Block (defs, stmts)
;;

let sexp_to_name (s : Sexp.t) : Name.t =
  match s with
  | Sexp.Atom field -> Name.of_string field
  | _ -> ErrVar "Found something that isn't an Atom"
;;

let sexp_to_expr (s : Sexp.t) : Expr.t =
  match s with
  | Sexp.Atom str -> expr_of_atom_str str
  | Sexp.List [ Sexp.Atom v1; Sexp.Atom "+"; Sexp.Atom v2 ] ->
    Plus (Name.of_string v1, Name.of_string v2)
  | Sexp.List [ Sexp.Atom v1; Sexp.Atom "/"; Sexp.Atom v2 ] ->
    Divide (Name.of_string v1, Name.of_string v2)
  | Sexp.List [ Sexp.Atom v1; Sexp.Atom "=="; Sexp.Atom v2 ] ->
    Equal (Name.of_string v1, Name.of_string v2)
  | Sexp.List [ Sexp.Atom "new"; Sexp.Atom className; Sexp.List variable_list ] ->
    Instance (Name.of_string className, List.map variable_list ~f:sexp_to_name)
  | Sexp.List [ Sexp.Atom v1; Sexp.Atom "-->"; Sexp.Atom fieldName ] ->
    FieldAccess (Name.of_string v1, Name.of_string fieldName)
  | Sexp.List
      [ Sexp.Atom v1; Sexp.Atom "-->"; Sexp.Atom methodName; Sexp.List variable_list ] ->
    MethodCall
      ( Name.of_string v1
      , Name.of_string methodName
      , List.map variable_list ~f:sexp_to_name )
  | Sexp.List [ Sexp.Atom v1; Sexp.Atom "isa"; Sexp.Atom className ] ->
    IsA (Name.of_string v1, Name.of_string className)
  | _ -> ErrExpr (ParseError "error expression ")
;;

let sexp_to_def (sexp : Sexp.t) : Def.t =
  match sexp with
  | Sexp.List [ Sexp.Atom "def"; Sexp.Atom v; e ] -> Def (Name.of_string v, sexp_to_expr e)
  | _ -> ErrDef (Error.ParseError "error declaration")
;;

let rec sexp_to_stmt (s : Sexp.t) : Stmt.t =
  match s with
  | Sexp.List [ Sexp.Atom v; Sexp.Atom "="; e ] ->
    AssignVar (Name.of_string v, sexp_to_expr e)
  | Sexp.List [ Sexp.Atom "if0"; e; b1; b2 ] ->
    If0 (sexp_to_expr e, sexp_to_block b1, sexp_to_block b2)
  | Sexp.List [ Sexp.Atom "while0"; e; b ] -> While0 (sexp_to_expr e, sexp_to_block b)
  | Sexp.List [ Sexp.Atom v; Sexp.Atom "-->"; Sexp.Atom fieldName; Sexp.Atom "="; e ] ->
    AssignField (Name.of_string v, Name.of_string fieldName, sexp_to_expr e)
  | _ -> ErrStmt (ParseError "error statement")

and sexp_to_block (s : Sexp.t) : Block.t =
  match s with
  | Sexp.List (Sexp.Atom "block" :: rest) ->
    let defs_sexp, rest_sexp = split_def_sexps_from_rest rest in
    let defs = List.map defs_sexp ~f:sexp_to_def in
    let stmts = List.map rest_sexp ~f:sexp_to_stmt in
    wellformed_block_of_defs_and_stmts defs stmts
  | stmt -> Stmt (sexp_to_stmt stmt)
;;

let rec sexp_to_method (s : Sexp.t) : Method.t =
  match s with
  | Sexp.List
      (Sexp.Atom "method"
      :: Sexp.Atom methodName
      :: Sexp.List parameter_list
      :: defs_stmts_and_expr) ->
    let program = sexp_to_program (Sexp.List defs_stmts_and_expr) in
    let defs, stmts, expr = unpack_program program in
    let parameters = List.map parameter_list ~f:sexp_to_name in
    Method (Name.of_string methodName, parameters, defs, stmts, expr)
  | _ -> ErrMethod (ParseError "error method")

and sexp_to_class (s : Sexp.t) (shape : ClassShape.t) : Class.t =
  match s with
  | Sexp.List [ Sexp.Atom "class"; Sexp.Atom className; Sexp.List field_list ] ->
    let field_list_names = List.map field_list ~f:sexp_to_name in
    Class (Name.of_string className, field_list_names, [], shape)
  | Sexp.List
      (Sexp.Atom "class" :: Sexp.Atom className :: Sexp.List maybe_field_list :: rest) ->
    if is_list_of_sexp_all_atoms maybe_field_list
    then (
      let fields_list_names = List.map maybe_field_list ~f:sexp_to_name in
      let method_list_names = List.map rest ~f:sexp_to_method in
      Class (Name.of_string className, fields_list_names, method_list_names, shape))
    else (
      let method_list_names = List.map (maybe_field_list @ rest) ~f:sexp_to_method in
      Class (Name.of_string className, [], method_list_names, shape))
  | _ -> ErrClass (ParseError "error class")

and sexp_to_program (s : Sexp.t) : Program.t =
  match s with
  | Sexp.List items when List.length items >= 1 ->
    let n = List.length items - 1 in
    let all_sexps, expr_sexp = List.split_n items n in
    let def_sexps, stmt_sexps = split_def_sexps_from_rest all_sexps in
    let defs = List.map def_sexps ~f:sexp_to_def in
    let stmts = List.map stmt_sexps ~f:sexp_to_stmt in
    let expr = sexp_to_expr (List.hd_exn expr_sexp) in
    Program (defs, stmts, expr)
  | _ -> ErrProgram (ParseError "error program")
;;

let sexp_to_import (s : Sexp.t) : Import.t =
  match s with
  | Sexp.List [ Sexp.Atom "import"; Sexp.Atom moduleName ] ->
    Import (Name.of_string moduleName)
  | _ -> ErrImport (ParseError "error import")
;;

let rec sexp_to_type (s : Sexp.t) : Type.t =
  match s with
  | Sexp.Atom "Number" -> Number
  | Sexp.List [ Sexp.List _; Sexp.List _ ] -> Shape (sexp_to_shape s)
  | _ -> ErrType (ParseError "error type")

and sexp_to_shape (s : Sexp.t) : Shape.t =
  match s with
  | Sexp.List [ Sexp.List field_types; Sexp.List method_types ] ->
    let parsed_field_types = List.map field_types ~f:sexp_to_field_type in
    let parsed_method_types = List.map method_types ~f:sexp_to_method_type in
    Shape (parsed_field_types, parsed_method_types)
  | _ -> ErrShape (ParseError "error shape")

and sexp_to_field_type (s : Sexp.t) : FieldType.t =
  match s with
  | Sexp.List [ Sexp.Atom name; field_type ] ->
    FieldType (Name.of_string name, sexp_to_type field_type)
  | _ -> ErrFieldType (ParseError "error field type")

and sexp_to_method_type (s : Sexp.t) : MethodType.t =
  match s with
  | Sexp.List [ Sexp.Atom name; Sexp.List param_types; return_type ] ->
    let parsed_param_types = List.map param_types ~f:sexp_to_type in
    let parsed_return_type = sexp_to_type return_type in
    MethodType (Name.of_string name, parsed_param_types, parsed_return_type)
  | _ -> ErrMethodType (ParseError "error method type")
;;

let sexp_to_mixed_import (s : Sexp.t) : MixedImport.t =
  match s with
  | Sexp.List [ Sexp.Atom "timport"; Sexp.Atom moduleName; shape ] ->
    TypedImport (Name.of_string moduleName, sexp_to_shape shape)
  | _ -> mixed_import_from_import (sexp_to_import s)
;;

let sexp_to_mixed_module (s : Sexp.t) : MixedModule.t =
  match s with
  | Sexp.List (Sexp.Atom "module" :: Sexp.Atom moduleName :: rest_sexps) ->
    let n = List.length rest_sexps - 1 in
    let import_sexps, class_sexp = List.split_n rest_sexps n in
    let parsed_name = Name.module_of_string moduleName in
    let parsed_imports = List.map import_sexps ~f:sexp_to_import in
    let parsed_class = sexp_to_class (List.hd_exn class_sexp) ModuleClassShape in
    Module (parsed_name, parsed_imports, parsed_class)
  | Sexp.List [ Sexp.Atom "tmodule"; Sexp.Atom _; Sexp.List _ ] ->
    ErrModule (ParseError "error module")
  | Sexp.List (Sexp.Atom "tmodule" :: Sexp.Atom moduleName :: rest_sexps) ->
    let n = List.length rest_sexps - 2 in
    let import_sexps, class_and_shape_sexp = List.split_n rest_sexps n in
    let parsed_name = Name.module_of_string moduleName in
    let parsed_imports = List.map import_sexps ~f:sexp_to_mixed_import in
    let parsed_shape = sexp_to_shape (List.hd_exn (List.tl_exn class_and_shape_sexp)) in
    let parsed_class =
      sexp_to_class (List.hd_exn class_and_shape_sexp) (TModuleClassShape parsed_shape)
    in
    MixedModule (parsed_name, parsed_imports, parsed_class, parsed_shape)
  | _ -> ErrModule (ParseError "error module")
;;

let sexp_to_system (s : Sexp.t) : MixedSystem.t =
  match s with
  | Sexp.List items when List.length items >= 1 ->
    let n = List.length items - 1 in
    let all_sexps, expr_sexp = List.split_n items n in
    let mixed_module_sexps, rest_sexps = split_typed_module_sexps_from_rest all_sexps in
    let mixed_modules = List.map mixed_module_sexps ~f:sexp_to_mixed_module in
    let import_sexps, rest_sexps = split_import_sexps_from_rest rest_sexps in
    let imports = List.map import_sexps ~f:sexp_to_mixed_import in
    let def_sexps, stmt_sexps = split_def_sexps_from_rest rest_sexps in
    let defs = List.map def_sexps ~f:sexp_to_def in
    let stmts = List.map stmt_sexps ~f:sexp_to_stmt in
    let expr = sexp_to_expr (List.hd_exn expr_sexp) in
    MixedSystem (mixed_modules, imports, defs, stmts, expr)
  | _ -> ErrMixedSystem (ParseError "error system")
;;

let parse (s : Sexp.t) : MixedSystem.t =
  match s with
  | Sexp.List _ -> sexp_to_system s
  | _ ->
    ErrMixedSystem
      (ParseError "Invalid system structure, expression must be wrapped in parentheses.")
;;

(* ------------------------------------------------------------------ *)
(* ERROR DETECTION HELPERS                                             *)
(* ------------------------------------------------------------------ *)

let contain_err_expr (expr : Expr.t) : bool =
  match expr with
  | ErrExpr _ -> true
  | Var v -> Name.contain_err_name v
  | Gnum _ -> false
  | Plus (e1, e2) -> Name.contain_err_name e1 || Name.contain_err_name e2
  | Divide (e1, e2) -> Name.contain_err_name e1 || Name.contain_err_name e2
  | Equal (e1, e2) -> Name.contain_err_name e1 || Name.contain_err_name e2
  | Instance (className, variable_list) ->
    Name.contain_err_name className
    || List.exists variable_list ~f:(fun v -> Name.contain_err_name v)
  | FieldAccess (v1, v2) -> Name.contain_err_name v1 || Name.contain_err_name v2
  | MethodCall (v1, methodName, variable_list) ->
    Name.contain_err_name v1
    || Name.contain_err_name methodName
    || List.exists variable_list ~f:(fun v -> Name.contain_err_name v)
  | IsA (v1, v2) -> Name.contain_err_name v1 || Name.contain_err_name v2
;;

let contain_err_def (defs : Def.t list) : bool =
  List.exists defs ~f:(fun def ->
    match def with
    | Def (var, expr) -> Name.contain_err_name var || contain_err_expr expr
    | ErrDef _ -> true)
;;

let rec contain_err_block (block : Block.t) : bool =
  match block with
  | ErrBlock _ -> true
  | Stmt s -> contain_err_stmt [ s ]
  | Block (defs, stmts) -> contain_err_def defs || contain_err_stmt stmts

and contain_err_stmt (stmts : Stmt.t list) : bool =
  List.exists stmts ~f:(fun stmt ->
    match stmt with
    | ErrStmt _ -> true
    | AssignVar (v, e) -> Name.contain_err_name v || contain_err_expr e
    | If0 (e, b1, b2) ->
      contain_err_expr e || contain_err_block b1 || contain_err_block b2
    | While0 (e, b) -> contain_err_expr e || contain_err_block b
    | AssignField (v, fieldName, e) ->
      Name.contain_err_name v || Name.contain_err_name fieldName || contain_err_expr e)
;;

let contain_err_field (fields : Name.t list) : bool =
  List.exists fields ~f:(fun a_field -> Name.contain_err_name a_field)
;;

let contain_err_parameter (parameters : Name.t list) : bool =
  List.exists parameters ~f:(fun a_parameter -> Name.contain_err_name a_parameter)
;;

let contain_err_method (methods : Method.t list) : bool =
  List.exists methods ~f:(fun a_method ->
    match a_method with
    | Method (name, parameters, defs, stmts, expr) ->
      Name.contain_err_name name
      || contain_err_parameter parameters
      || contain_err_def defs
      || contain_err_stmt stmts
      || contain_err_expr expr
    | ErrMethod _ -> true)
;;

let rec contain_err_type (type_ : Type.t) : bool =
  match type_ with
  | Number -> false
  | Shape shape -> contain_err_shape shape
  | ErrType _ -> true

and contain_err_field_type (field_type : FieldType.t) : bool =
  match field_type with
  | FieldType (name, type_) -> Name.contain_err_name name || contain_err_type type_
  | ErrFieldType _ -> true

and contain_err_method_type (method_type : MethodType.t) : bool =
  match method_type with
  | MethodType (name, param_types, return_type) ->
    Name.contain_err_name name
    || List.exists param_types ~f:contain_err_type
    || contain_err_type return_type
  | ErrMethodType _ -> true

and contain_err_shape (shape : Shape.t) : bool =
  match shape with
  | Shape (field_types, method_types) ->
    List.exists field_types ~f:contain_err_field_type
    || List.exists method_types ~f:contain_err_method_type
  | ErrShape _ -> true

and contain_err_class_shape (class_shape : ClassShape.t) : bool =
  match class_shape with
  | ModuleClassShape -> false
  | TModuleClassShape shape -> contain_err_shape shape
;;

let contain_err_class (classes : Class.t list) : bool =
  List.exists classes ~f:(fun a_class ->
    match a_class with
    | Class (name, fields, methods, shape) ->
      Name.contain_err_name name
      || contain_err_field fields
      || contain_err_method methods
      || contain_err_class_shape shape
    | ErrClass _ -> true)
;;

let contain_err_import (imports : Import.t list) : bool =
  List.exists imports ~f:(fun a_import ->
    match a_import with
    | Import name -> Name.contain_err_name name
    | ErrImport _ -> true)
;;

let contain_err_mixed_import (imports : MixedImport.t list) : bool =
  List.exists imports ~f:(fun import_ ->
    match import_ with
    | TypedImport (name, shape) -> Name.contain_err_name name || contain_err_shape shape
    | Import name -> Name.contain_err_name name
    | ErrImport _ -> true)
;;

let contain_err_mixed_module (modules : MixedModule.t list) : bool =
  List.exists modules ~f:(fun a_module ->
    match a_module with
    | Module (name, imports, class_) ->
      Name.contain_err_name name
      || contain_err_import imports
      || contain_err_class [ class_ ]
    | MixedModule (name, imports, class_info, shape) ->
      Name.contain_err_name name
      || contain_err_mixed_import imports
      || contain_err_class [ class_info ]
      || contain_err_shape shape
    | ErrModule _ -> true)
;;

(* ------------------------------------------------------------------ *)
(* SYSTEM ERROR CHECK                                                  *)
(* ------------------------------------------------------------------ *)

let ensure_no_system_errors (system : MixedSystem.t) : (MixedSystem.t, string) result =
  match system with
  | MixedSystem (mixed_modules, imports, defs, stmts, expr) ->
    let has_module_err = contain_err_mixed_module mixed_modules in
    let has_import_err = contain_err_mixed_import imports in
    let has_def_err = contain_err_def defs in
    let has_stmt_err = contain_err_stmt stmts in
    let has_expr_err = contain_err_expr expr in
    if has_module_err || has_import_err || has_def_err || has_stmt_err || has_expr_err
    then Error "System has error"
    else Ok (MixedSystem (mixed_modules, imports, defs, stmts, expr))
  | ErrMixedSystem _ -> Error "System is in an unexpected state"
;;

(* ------------------------------------------------------------------ *)
(* DUPLICATE VALIDATION                                                *)
(* ------------------------------------------------------------------ *)

let check_for_duplicates
      (get_name : 'a -> Name.t)
      (on_error : string -> Error.t)
      (items : 'a list)
  : (unit, Error.t) result
  =
  let seen_names = String.Hash_set.create () in
  let find_result =
    List.find_map items ~f:(fun item ->
      let name_str = Name.to_string (get_name item) in
      if Hash_set.mem seen_names name_str
      then Some (Error (on_error name_str))
      else (
        Hash_set.add seen_names name_str;
        None))
  in
  match find_result with
  | Some err -> err
  | None -> Ok ()
;;

let validate_method_duplicates (meth : Method.t) : (unit, Error.t) result =
  match meth with
  | Method (_, params, _, _, _) ->
    check_for_duplicates
      (fun name -> name)
      (fun _ ->
         Error.DuplicateClassVariableError
           "\"duplicate method, field, or parameter name\"")
      params
  | ErrMethod err -> Error err
;;

let list_iter_result xs ~f =
  let open Result.Let_syntax in
  let rec loop = function
    | [] -> Ok ()
    | x :: xs ->
      let%bind () = f x in
      loop xs
  in
  loop xs
;;

let validate_class_duplicates (cls : Class.t) : (unit, Error.t) result =
  match cls with
  | Class (_, fields, methods, _) ->
    let open Result.Let_syntax in
    let%bind () =
      check_for_duplicates
        (fun name -> name)
        (fun _ ->
           Error.DuplicateClassVariableError
             "\"duplicate method, field, or parameter name\"")
        fields
    in
    let%bind () =
      check_for_duplicates
        (function
          | Method.Method (name, _, _, _, _) -> name
          | Method.ErrMethod _ -> Name.ErrVar "")
        (fun _ ->
           Error.DuplicateClassVariableError
             "\"duplicate method, field, or parameter name\"")
        methods
    in
    list_iter_result methods ~f:validate_method_duplicates
  | ErrClass err -> Error err
;;

let get_field_name_from_field_type (field_type : FieldType.t) : Name.t =
  match field_type with
  | FieldType (name, _) -> name
  | ErrFieldType _ -> Name.ErrVar "Found an err field type post parsing"
;;

let get_method_name_from_method_type (method_type : MethodType.t) : Name.t =
  match method_type with
  | MethodType (name, _, _) -> name
  | ErrMethodType _ -> Name.ErrVar "Found an err method type post parsing"
;;

let validate_shape_duplicates (shape : Shape.t) : (unit, Error.t) result =
  match shape with
  | Shape (field_types, method_types) ->
    let open Result.Let_syntax in
    let%bind () =
      check_for_duplicates
        (fun name -> name)
        (fun _ ->
           Error.DuplicateClassVariableError
             "\"duplicate method, field, or parameter name\"")
        (List.map field_types ~f:get_field_name_from_field_type)
    in
    let%bind () =
      check_for_duplicates
        (fun name -> name)
        (fun _ ->
           Error.DuplicateClassVariableError
             "\"duplicate method, field, or parameter name\"")
        (List.map method_types ~f:get_method_name_from_method_type)
    in
    Ok ()
  | ErrShape err -> Error err
;;

let validate_module_duplicates (typed_module : MixedModule.t) : (unit, Error.t) result =
  match typed_module with
  | MixedModule.MixedModule (_, _, class_, shape) ->
    let open Result.Let_syntax in
    let%bind () = validate_class_duplicates class_ in
    let%bind () = validate_shape_duplicates shape in
    Ok ()
  | MixedModule.Module (_, _, class_) -> validate_class_duplicates class_
  | MixedModule.ErrModule err -> Error err
;;

let validate_system_duplicates (mixed_system : MixedSystem.t)
  : (MixedSystem.t, Error.t) result
  =
  match mixed_system with
  | MixedSystem (modules, _, _, _, _) ->
    let open Result.Let_syntax in
    let%bind () =
      check_for_duplicates
        (function
          | MixedModule.MixedModule (name, _, _, _) -> name
          | MixedModule.Module (name, _, _) -> name
          | MixedModule.ErrModule _ -> Name.ErrVar "Found an err module post parsing")
        (fun name_str -> Error.DuplicateModuleNameError name_str)
        modules
    in
    let%bind () = list_iter_result modules ~f:validate_module_duplicates in
    Ok mixed_system
  | ErrMixedSystem err -> Error err
;;

(* ------------------------------------------------------------------ *)
(* IMPORT VALIDATION                                                   *)
(* ------------------------------------------------------------------ *)

let rec shape_equal (shape1 : Shape.t) (shape2 : Shape.t) : bool =
  match shape1, shape2 with
  | Shape.Shape (fields1, methods1), Shape.Shape (fields2, methods2) ->
    field_types_equal fields1 fields2 && method_types_equal methods1 methods2
  | Shape.ErrShape _, Shape.ErrShape _ -> true
  | _ -> false

and field_types_equal (fields1 : FieldType.t list) (fields2 : FieldType.t list) : bool =
  List.length fields1 = List.length fields2
  && List.for_all2_exn fields1 fields2 ~f:field_type_equal

and field_type_equal (field1 : FieldType.t) (field2 : FieldType.t) : bool =
  match field1, field2 with
  | FieldType.FieldType (name1, type1), FieldType.FieldType (name2, type2) ->
    Name.equal name1 name2 && type_equal type1 type2
  | FieldType.ErrFieldType _, FieldType.ErrFieldType _ -> true
  | _ -> false

and method_types_equal (methods1 : MethodType.t list) (methods2 : MethodType.t list)
  : bool
  =
  List.length methods1 = List.length methods2
  && List.for_all2_exn methods1 methods2 ~f:method_type_equal

and method_type_equal (method1 : MethodType.t) (method2 : MethodType.t) : bool =
  match method1, method2 with
  | ( MethodType.MethodType (name1, params1, return1)
    , MethodType.MethodType (name2, params2, return2) ) ->
    Name.equal name1 name2
    && List.length params1 = List.length params2
    && List.for_all2_exn params1 params2 ~f:type_equal
    && type_equal return1 return2
  | MethodType.ErrMethodType _, MethodType.ErrMethodType _ -> true
  | _ -> false

and type_equal (type1 : Type.t) (type2 : Type.t) : bool =
  match type1, type2 with
  | Type.Number, Type.Number -> true
  | Type.Shape shape1, Type.Shape shape2 -> shape_equal shape1 shape2
  | Type.ErrType _, Type.ErrType _ -> true
  | Type.Number, _ -> false
  | Type.Shape _, _ -> false
  | Type.ErrType _, _ -> false
;;

let initialize_module_sets () : string Hash_set.t * string Hash_set.t =
  let typed_modules = Hash_set.create (module String) in
  let untyped_modules = Hash_set.create (module String) in
  typed_modules, untyped_modules
;;

let validate_mixed_import_in_mixed_module
      (import : MixedImport.t)
      (typed_mods : string Hash_set.t)
      (untyped_mods : string Hash_set.t)
      (imported_modules_with_shape : (string, Shape.t) Hashtbl.t)
  : MixedImport.t * (string, Shape.t) Hashtbl.t
  =
  match import with
  | TypedImport (name, shape) ->
    if
      Hashtbl.mem imported_modules_with_shape (Name.to_string name)
      && not
           (shape_equal
              shape
              (Hashtbl.find_exn imported_modules_with_shape (Name.to_string name)))
    then
      ( ErrImport (Error.BadImportError "tried to import the same module twice")
      , imported_modules_with_shape )
    else if Hash_set.mem typed_mods (Name.to_string name)
    then
      ( ErrImport (Error.BadImportError "tried to timport in a tmodule")
      , imported_modules_with_shape )
    else (
      Hashtbl.set imported_modules_with_shape ~key:(Name.to_string name) ~data:shape;
      import, imported_modules_with_shape)
  | Import name ->
    if Hash_set.mem untyped_mods (Name.to_string name)
    then
      ( ErrImport
          (Error.BadImportError
             "tried to import an untyped module using import instead of timport")
      , imported_modules_with_shape )
    else import, imported_modules_with_shape
  | ErrImport _ -> import, imported_modules_with_shape
;;

let validate_mixed_imports
      (imports : MixedImport.t list)
      (typed_mods : string Hash_set.t)
      (untyped_mods : string Hash_set.t)
  : MixedImport.t list
  =
  let init_shaped_imports = Hashtbl.create (module String) in
  let potentially_valid_imports, _ =
    List.fold
      imports
      ~init:([], init_shaped_imports)
      ~f:(fun (imports_acc, imports_so_far) import ->
        let potentially_valid_import, new_imports_so_far =
          validate_mixed_import_in_mixed_module
            import
            typed_mods
            untyped_mods
            imports_so_far
        in
        imports_acc @ [ potentially_valid_import ], new_imports_so_far)
  in
  potentially_valid_imports
;;

let validate_imports_in_module
      (module_ : MixedModule.t)
      (typed_mods : string Hash_set.t)
      (untyped_mods : string Hash_set.t)
  : MixedModule.t * string Hash_set.t * string Hash_set.t
  =
  match module_ with
  | MixedModule (name, mixed_imports, class_, shape) ->
    let potentially_valid_imports =
      validate_mixed_imports mixed_imports typed_mods untyped_mods
    in
    let module_with_potentially_valid_imports =
      MixedModule.MixedModule (name, potentially_valid_imports, class_, shape)
    in
    Hash_set.add typed_mods (Name.to_string name);
    module_with_potentially_valid_imports, typed_mods, untyped_mods
  | Module (name, _, _) ->
    Hash_set.add untyped_mods (Name.to_string name);
    module_, typed_mods, untyped_mods
  | ErrModule _ ->
    raise (Exceptions.UnexpectedError "Found an error module while validating imports")
;;

let validate_bad_import_errors (system : MixedSystem.t) : MixedSystem.t =
  match system with
  | MixedSystem (modules, imports, defs, stmts, expr) ->
    let init_typed_modules, init_untyped_modules = initialize_module_sets () in
    let potentially_valid_modules, typed_modules, untyped_modules =
      List.fold
        modules
        ~init:([], init_typed_modules, init_untyped_modules)
        ~f:(fun (modules_acc, typed_modules_acc, untyped_modules_acc) module_ ->
          let potentially_valid_mod, new_typed_modules, new_untyped_modules =
            validate_imports_in_module module_ typed_modules_acc untyped_modules_acc
          in
          modules_acc @ [ potentially_valid_mod ], new_typed_modules, new_untyped_modules)
    in
    let potentially_valid_imports =
      validate_mixed_imports imports typed_modules untyped_modules
    in
    MixedSystem (potentially_valid_modules, potentially_valid_imports, defs, stmts, expr)
  | ErrMixedSystem _ ->
    raise (Exceptions.UnexpectedError "Tried to validate the imports of an err system")
;;

(* ------------------------------------------------------------------ *)
(* UNDECLARED VARIABLE VALIDATION                                      *)
(* ------------------------------------------------------------------ *)

let get_name_from_class (class_ : Class.t) : string =
  match class_ with
  | Class (name, _, _, _) -> Name.to_string name
  | ErrClass _ -> ""
;;

let get_classname_from_module (typed_module : MixedModule.t) : string =
  match typed_module with
  | MixedModule (_, _, class_, _) | Module (_, _, class_) -> get_name_from_class class_
  | ErrModule _ -> ""
;;

let find_module_from_import (module_name : Name.t) (modules : MixedModule.t list)
  : MixedModule.t
  =
  List.find_exn modules ~f:(fun module_ ->
    match module_ with
    | MixedModule (name, _, _, _) | Module (name, _, _) -> Name.equal name module_name
    | ErrModule _ -> false)
;;

let get_list_of_classnames_from_imports
      (imports : Import.t list)
      (modules : MixedModule.t list)
  : string list
  =
  List.map imports ~f:(fun import_ ->
    match import_ with
    | Import name -> get_classname_from_module (find_module_from_import name modules)
    | ErrImport _ -> "")
;;

let get_list_of_class_names_from_modules (typed_modules : MixedModule.t list)
  : string list
  =
  List.map typed_modules ~f:(fun module_ ->
    match module_ with
    | MixedModule (_, _, class_, _) | Module (_, _, class_) -> get_name_from_class class_
    | ErrModule _ ->
      raise
        (Exceptions.UnexpectedError "Did not find a wellformed module while validating"))
;;

let populate_classname_set
      (var_set : VarSet.t)
      (source :
        [ `From_imports of Import.t list * MixedModule.t list
        | `From_typed_imports of MixedImport.t list * MixedModule.t list
        | `From_modules of MixedModule.t list
        ])
  : VarSet.t
  =
  let classnames =
    match source with
    | `From_imports (imports, modules) ->
      get_list_of_classnames_from_imports imports modules
    | `From_typed_imports (mixed_imports, modules) ->
      let imports = List.map mixed_imports ~f:mixed_import_to_import in
      get_list_of_classnames_from_imports imports modules
    | `From_modules modules -> get_list_of_class_names_from_modules modules
  in
  let var_set = clear_class_name_set var_set in
  add_class_to_class_name_set var_set classnames
;;

let validate_expr (e : Expr.t) (var_set : VarSet.t) : Expr.t =
  match e with
  | Var v ->
    if var_is_missing var_set (Name.to_string v)
    then ErrExpr (Error.UndeclaredVariableError "Variable is undeclared in Expression")
    else Var v
  | Gnum _ -> e
  | Plus (v1, v2) ->
    if
      var_is_missing var_set (Name.to_string v1)
      || var_is_missing var_set (Name.to_string v2)
    then
      ErrExpr (Error.UndeclaredVariableError "Variable is undeclared in Plus Expression")
    else Plus (v1, v2)
  | Divide (v1, v2) ->
    if
      var_is_missing var_set (Name.to_string v1)
      || var_is_missing var_set (Name.to_string v2)
    then
      ErrExpr
        (Error.UndeclaredVariableError "Variable is undeclared in Divide Expression")
    else Divide (v1, v2)
  | Equal (v1, v2) ->
    if
      var_is_missing var_set (Name.to_string v1)
      || var_is_missing var_set (Name.to_string v2)
    then
      ErrExpr
        (Error.UndeclaredVariableError "Variable is undeclared in Equals Expression")
    else Equal (v1, v2)
  | Instance (class_name, fields_list) ->
    let class_missing = class_name_is_missing var_set (Name.to_string class_name) in
    let field_missing =
      List.exists fields_list ~f:(fun field ->
        var_is_missing var_set (Name.to_string field))
    in
    if class_missing || field_missing
    then
      ErrExpr
        (Error.UndeclaredVariableError "Variable is undeclared in Instance Expression")
    else Instance (class_name, fields_list)
  | FieldAccess (v1, v2) ->
    if var_is_missing var_set (Name.to_string v1)
    then
      ErrExpr
        (Error.UndeclaredVariableError "Variable is undeclared in FieldAccess Expression")
    else FieldAccess (v1, v2)
  | MethodCall (var, method_name, param_list) ->
    if
      var_is_missing var_set (Name.to_string var)
      || List.exists param_list ~f:(fun param ->
        var_is_missing var_set (Name.to_string param))
    then
      ErrExpr
        (Error.UndeclaredVariableError "Variable is undeclared in MethodCall Expression")
    else MethodCall (var, method_name, param_list)
  | IsA (var, class_name) ->
    let has_var_error = var_is_missing var_set (Name.to_string var) in
    let has_class_error = class_name_is_missing var_set (Name.to_string class_name) in
    if has_var_error || has_class_error
    then
      ErrExpr (Error.UndeclaredVariableError "Variable is undeclared in IsA Expression")
    else IsA (var, class_name)
  | ErrExpr _ ->
    raise
      (Exceptions.UnexpectedError
         "Did not match to a wellformed expression while validating")
;;

let validate_def (def : Def.t) (var_set : VarSet.t) : Def.t * VarSet.t =
  match def with
  | Def (var, e) ->
    let validated_expr = validate_expr e var_set in
    Def (var, validated_expr), add_var_to_current_scope var_set (Name.to_string var)
  | ErrDef _ ->
    raise
      (Exceptions.UnexpectedError "Did not find a wellformed declaration while validating")
;;

let rec validate_stmt (s : Stmt.t) (var_set : VarSet.t) : Stmt.t =
  match s with
  | AssignVar (var, e) ->
    if var_is_missing var_set (Name.to_string var)
    then
      ErrStmt
        (Error.UndeclaredVariableError "Variable is undeclared in AssignVar statement")
    else AssignVar (var, validate_expr e var_set)
  | If0 (e, b1, b2) ->
    If0
      ( validate_expr e var_set
      , validate_block b1 (add_new_scope var_set)
      , validate_block b2 (add_new_scope var_set) )
  | While0 (e, b) ->
    While0 (validate_expr e var_set, validate_block b (add_new_scope var_set))
  | AssignField (var, field_name, expr) ->
    if var_is_missing var_set (Name.to_string var)
    then
      ErrStmt
        (Error.UndeclaredVariableError "Variable is undeclared in AssignField statement")
    else AssignField (var, field_name, validate_expr expr var_set)
  | ErrStmt _ ->
    raise
      (Exceptions.UnexpectedError "Did not find a wellformed statement while validating")

and validate_block (b : Block.t) (var_set : VarSet.t) : Block.t =
  match b with
  | Stmt s -> Stmt (validate_stmt s var_set)
  | Block (defs, stmts) ->
    let potential_valid_defs, block_var_set_list =
      List.fold defs ~init:([], var_set) ~f:(fun (defs_acc, var_set_list) cur_def ->
        let validated_def, new_var_set_list = validate_def cur_def var_set_list in
        defs_acc @ [ validated_def ], new_var_set_list)
    in
    let potential_valid_stmts =
      List.map stmts ~f:(fun cur_stmt -> validate_stmt cur_stmt block_var_set_list)
    in
    Block (potential_valid_defs, potential_valid_stmts)
  | ErrBlock _ ->
    raise (Exceptions.UnexpectedError "Did not find a wellformed block while validating")
;;

let validate_method (method_ : Method.t) (var_set : VarSet.t) : Method.t =
  match method_ with
  | Method (name, parameters, defs, stmts, expr) ->
    let var_set =
      List.fold parameters ~init:var_set ~f:(fun var_set parameter ->
        add_var_to_current_scope var_set (Name.to_string parameter))
    in
    let var_set_with_this = add_var_to_current_scope var_set "this" in
    let potential_valid_defs, method_var_set_list =
      List.fold
        defs
        ~init:([], var_set_with_this)
        ~f:(fun (defs_acc, var_set_list) cur_def ->
          let validated_def, new_var_set_list = validate_def cur_def var_set_list in
          defs_acc @ [ validated_def ], new_var_set_list)
    in
    let potential_valid_stmts =
      List.map stmts ~f:(fun cur_stmt -> validate_stmt cur_stmt method_var_set_list)
    in
    Method
      ( name
      , parameters
      , potential_valid_defs
      , potential_valid_stmts
      , validate_expr expr method_var_set_list )
  | _ ->
    raise (Exceptions.UnexpectedError "Did not find a wellformed method while validating")
;;

let validate_class (class_ : Class.t) (var_set : VarSet.t) : Class.t =
  match class_ with
  | Class (name, fields, methods, shape) ->
    let var_set = add_class_to_class_name_set var_set [ Name.to_string name ] in
    let potential_valid_methods =
      List.map methods ~f:(fun method_ -> validate_method method_ var_set)
    in
    Class (name, fields, potential_valid_methods, shape)
  | _ ->
    raise (Exceptions.UnexpectedError "Did not find a wellformed class while validating")
;;

let validate_import (import : Import.t) (var_set : VarSet.t) : Import.t =
  match import with
  | Import name ->
    if module_name_is_missing var_set (Name.to_string name)
    then ErrImport (Error.UndeclaredVariableError "Variable is undeclared in Import")
    else import
  | ErrImport _ ->
    raise (Exceptions.UnexpectedError "Did not find a wellformed import while validating")
;;

let validate_mixed_import_from_import
      (import : Import.t)
      (var_set : VarSet.t)
      (opt_shape : Shape.t option)
  : MixedImport.t
  =
  let potentially_valid_import = validate_import import var_set in
  match potentially_valid_import, opt_shape with
  | Import name, Some shape -> MixedImport.TypedImport (name, shape)
  | Import name, None -> MixedImport.Import name
  | ErrImport e, _ -> MixedImport.ErrImport e
;;

let validate_mixed_import (import : MixedImport.t) (var_set : VarSet.t) : MixedImport.t =
  match import with
  | TypedImport (name, shape) ->
    validate_mixed_import_from_import (Import.Import name) var_set (Some shape)
  | Import name -> validate_mixed_import_from_import (Import.Import name) var_set None
  | ErrImport _ ->
    raise (Exceptions.UnexpectedError "Did not find a wellformed import while validating")
;;

let validate_mixed_module
      (mixed_module : MixedModule.t)
      (var_set : VarSet.t)
      (modules : MixedModule.t list)
  : MixedModule.t * VarSet.t
  =
  match mixed_module with
  | MixedModule (name, imports, class_, shape) ->
    let validated_imports =
      List.map imports ~f:(fun import_ -> validate_mixed_import import_ var_set)
    in
    let var_set =
      populate_classname_set var_set (`From_typed_imports (validated_imports, modules))
    in
    let var_set = add_module_to_module_name_set var_set (Name.to_string name) in
    let validated_class = validate_class class_ var_set in
    MixedModule (name, validated_imports, validated_class, shape), var_set
  | Module (name, imports, class_) ->
    let validated_imports =
      List.map imports ~f:(fun import_ -> validate_import import_ var_set)
    in
    let var_set =
      populate_classname_set var_set (`From_imports (validated_imports, modules))
    in
    let var_set = add_module_to_module_name_set var_set (Name.to_string name) in
    let validated_class = validate_class class_ var_set in
    Module (name, validated_imports, validated_class), var_set
  | _ ->
    raise (Exceptions.UnexpectedError "Did not find a wellformed module while validating")
;;

let validate_undeclared_vars (system : MixedSystem.t) : MixedSystem.t =
  match system with
  | MixedSystem (modules, imports, defs, stmts, expr) ->
    let var_set = initialize_var_set () in
    let var_set = populate_classname_set var_set (`From_modules modules) in
    let potential_valid_modules, var_set =
      List.fold
        modules
        ~init:([], var_set)
        ~f:(fun (modules_acc, var_set_list) curr_module ->
          let validated_module, new_var_set_list =
            validate_mixed_module curr_module var_set_list modules
          in
          modules_acc @ [ validated_module ], new_var_set_list)
    in
    let potential_valid_imports =
      List.map imports ~f:(fun cur_import -> validate_mixed_import cur_import var_set)
    in
    let var_set =
      populate_classname_set
        var_set
        (`From_typed_imports (potential_valid_imports, potential_valid_modules))
    in
    let potential_valid_defs, global_var_set =
      List.fold defs ~init:([], var_set) ~f:(fun (defs_acc, var_set_list) cur_def ->
        let validated_def, new_var_set_list = validate_def cur_def var_set_list in
        defs_acc @ [ validated_def ], new_var_set_list)
    in
    let potential_valid_stmts =
      List.map stmts ~f:(fun cur_stmt -> validate_stmt cur_stmt global_var_set)
    in
    let potential_valid_expr = validate_expr expr global_var_set in
    MixedSystem
      ( potential_valid_modules
      , potential_valid_imports
      , potential_valid_defs
      , potential_valid_stmts
      , potential_valid_expr )
  | _ -> ErrMixedSystem (ParseError "parser error")
;;
