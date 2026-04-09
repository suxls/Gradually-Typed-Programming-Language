module String = Core.String
module Hash_set = Core.Hash_set
module Result = Core.Result
module List = Core.List

(* ------------------------------------------------------------------ *)
(* WELL-FORMED TYPE DEFINITIONS                                        *)
(* ------------------------------------------------------------------ *)

module rec Stmt : sig
  type t =
    | AssignVar of Name.t * Expr.t
    | If0 of Expr.t * Block.t * Block.t
    | While0 of Expr.t * Block.t
    | AssignField of Name.t * Name.t * Expr.t
end = struct
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
end = struct
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
end

and Def : sig
  type t = Name.t * Expr.t
end = struct
  type t = Name.t * Expr.t
end

module Method : sig
  type t = Name.t * Name.t list * Def.t list * Stmt.t list * Expr.t
end = struct
  type t = Name.t * Name.t list * Def.t list * Stmt.t list * Expr.t
end

module VarSet : sig
  type t = string Hash_set.t
end = struct
  type t = string Hash_set.t
end

module Import : sig
  type t = Name.t
end = struct
  type t = Name.t
end

module rec Type : sig
  type t =
    | Number
    | Shape of Shape.t
end = struct
  type t =
    | Number
    | Shape of Shape.t
end

and Shape : sig
  type t = FieldType.t list * MethodType.t list
end = struct
  type t = FieldType.t list * MethodType.t list
end

and FieldType : sig
  type t = Name.t * Type.t
end = struct
  type t = Name.t * Type.t
end

and MethodType : sig
  type t = Name.t * Type.t list * Type.t
end = struct
  type t = Name.t * Type.t list * Type.t
end

module MixedImport : sig
  type t =
    | Import of Name.t
    | TypedImport of Name.t * Shape.t
end = struct
  type t =
    | Import of Name.t
    | TypedImport of Name.t * Shape.t
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
  type t = Name.t * Name.t list * Method.t list * ClassShape.t
end = struct
  type t = Name.t * Name.t list * Method.t list * ClassShape.t
end

module MixedModule : sig
  type t =
    | MixedModule of Name.t * MixedImport.t list * Class.t * Shape.t
    | Module of Name.t * Import.t list * Class.t
end = struct
  type t =
    | MixedModule of Name.t * MixedImport.t list * Class.t * Shape.t
    | Module of Name.t * Import.t list * Class.t
end

module WellformedMixedSystem : sig
  type t = MixedModule.t list * MixedImport.t list * Def.t list * Stmt.t list * Expr.t
end = struct
  type t = MixedModule.t list * MixedImport.t list * Def.t list * Stmt.t list * Expr.t
end

module PostSoundLinkerSystem : sig
  type t = MixedModule.t list * Import.t list * Def.t list * Stmt.t list * Expr.t
end = struct
  type t = MixedModule.t list * Import.t list * Def.t list * Stmt.t list * Expr.t
end

(* ------------------------------------------------------------------ *)
(* CONVERSION: System AST -> Well-formed AST                          *)
(* ------------------------------------------------------------------ *)

let wellformed_expr_from_expr (ast_expr : System.Expr.t) : (Expr.t, string) result =
  match ast_expr with
  | System.Expr.ErrExpr _ -> Error "Found an err Expr"
  | System.Expr.Var v -> Ok (Expr.Var v)
  | System.Expr.Gnum n -> Ok (Expr.Gnum n)
  | System.Expr.Plus (v1, v2) -> Ok (Expr.Plus (v1, v2))
  | System.Expr.Divide (v1, v2) -> Ok (Expr.Divide (v1, v2))
  | System.Expr.Equal (v1, v2) -> Ok (Expr.Equal (v1, v2))
  | System.Expr.Instance (className, fields) -> Ok (Expr.Instance (className, fields))
  | System.Expr.FieldAccess (var, field) -> Ok (Expr.FieldAccess (var, field))
  | System.Expr.MethodCall (var, methodName, params) ->
    Ok (Expr.MethodCall (var, methodName, params))
  | System.Expr.IsA (var, className) -> Ok (Expr.IsA (var, className))
;;

let wellformed_def_from_def (ast_def : System.Def.t) : (Def.t, string) result =
  let open Result.Let_syntax in
  match ast_def with
  | ErrDef _ -> Error "Found an err def"
  | Def (var, expr) ->
    let%bind wellformed_expr = wellformed_expr_from_expr expr in
    Ok (var, wellformed_expr)
;;

let rec wellformed_stmt_from_stmt (ast_stmt : System.Stmt.t) : (Stmt.t, string) result =
  let open Result.Let_syntax in
  match ast_stmt with
  | System.Stmt.ErrStmt _ -> Error "Found an err Stmt"
  | System.Stmt.AssignVar (v, expr) ->
    let%bind wellformed_expr = wellformed_expr_from_expr expr in
    Ok (Stmt.AssignVar (v, wellformed_expr))
  | System.Stmt.If0 (expr, b1, b2) ->
    let%bind wellformed_expr = wellformed_expr_from_expr expr in
    let%bind wellformed_b1 = wellformed_block_from_block b1 in
    let%bind wellformed_b2 = wellformed_block_from_block b2 in
    Ok (Stmt.If0 (wellformed_expr, wellformed_b1, wellformed_b2))
  | System.Stmt.While0 (expr, block) ->
    let%bind wellformed_expr = wellformed_expr_from_expr expr in
    let%bind wellformed_block = wellformed_block_from_block block in
    Ok (Stmt.While0 (wellformed_expr, wellformed_block))
  | System.Stmt.AssignField (var, field, expr) ->
    let%bind wellformed_expr = wellformed_expr_from_expr expr in
    Ok (Stmt.AssignField (var, field, wellformed_expr))

and wellformed_block_from_block (ast_block : System.Block.t) : (Block.t, string) result =
  let open Result.Let_syntax in
  match ast_block with
  | System.Block.ErrBlock _ -> Error "Found an err block"
  | System.Block.Stmt stmt ->
    let%bind wellformed_stmt = wellformed_stmt_from_stmt stmt in
    Ok (Block.Stmt wellformed_stmt)
  | System.Block.Block (defs, stmts) ->
    let%bind wellformed_defs = Result.all (List.map defs ~f:wellformed_def_from_def) in
    let%bind wellformed_stmts =
      Result.all (List.map stmts ~f:wellformed_stmt_from_stmt)
    in
    Ok (Block.Block (wellformed_defs, wellformed_stmts))
;;

let wellformed_import_from_import (ast_import : System.Import.t)
  : (Import.t, string) result
  =
  match ast_import with
  | System.Import.ErrImport _ -> Error "Found an err import"
  | System.Import.Import name -> Ok name
;;

let rec wellformed_type_from_type (ast_type : System.Type.t) : (Type.t, string) result =
  match ast_type with
  | System.Type.ErrType _ -> Error "Found an err type"
  | System.Type.Number -> Ok Type.Number
  | System.Type.Shape shape ->
    let open Result.Let_syntax in
    let%bind wellformed_shape = wellformed_shape_from_shape shape in
    Ok (Type.Shape wellformed_shape)

and wellformed_shape_from_shape (ast_shape : System.Shape.t) : (Shape.t, string) result =
  match ast_shape with
  | System.Shape.ErrShape _ -> Error "Found an err shape"
  | System.Shape.Shape (field_types, method_types) ->
    let open Result.Let_syntax in
    let%bind wellformed_field_types =
      Result.all (List.map field_types ~f:wellformed_field_type_from_field_type)
    in
    let%bind wellformed_method_types =
      Result.all (List.map method_types ~f:wellformed_method_type_from_method_type)
    in
    Ok (wellformed_field_types, wellformed_method_types)

and wellformed_field_type_from_field_type (ast_field_type : System.FieldType.t)
  : (FieldType.t, string) result
  =
  match ast_field_type with
  | System.FieldType.ErrFieldType _ -> Error "Found an err field type"
  | System.FieldType.FieldType (name, type_) ->
    let open Result.Let_syntax in
    let%bind wellformed_type = wellformed_type_from_type type_ in
    Ok (name, wellformed_type)

and wellformed_method_type_from_method_type (ast_method_type : System.MethodType.t)
  : (MethodType.t, string) result
  =
  match ast_method_type with
  | System.MethodType.ErrMethodType _ -> Error "Found an err method type"
  | System.MethodType.MethodType (name, param_types, return_type) ->
    let open Result.Let_syntax in
    let%bind wellformed_param_types =
      Result.all (List.map param_types ~f:wellformed_type_from_type)
    in
    let%bind wellformed_return_type = wellformed_type_from_type return_type in
    Ok (name, wellformed_param_types, wellformed_return_type)

and wellformed_class_shape_from_class_shape (ast_class_shape : System.ClassShape.t)
  : (ClassShape.t, string) result
  =
  match ast_class_shape with
  | System.ClassShape.ModuleClassShape -> Ok ClassShape.ModuleClassShape
  | System.ClassShape.TModuleClassShape shape ->
    let open Result.Let_syntax in
    let%bind wellformed_shape = wellformed_shape_from_shape shape in
    Ok (ClassShape.TModuleClassShape wellformed_shape)
;;

let rec wellformed_method_from_method (ast_method : System.Method.t)
  : (Method.t, string) result
  =
  let open Result.Let_syntax in
  match ast_method with
  | System.Method.ErrMethod _ -> Error "Found an err method"
  | System.Method.Method (name, params, defs, stmts, expr) ->
    let%bind wellformed_defs = Result.all (List.map defs ~f:wellformed_def_from_def) in
    let%bind wellformed_stmts =
      Result.all (List.map stmts ~f:wellformed_stmt_from_stmt)
    in
    let%bind wellformed_expr = wellformed_expr_from_expr expr in
    Ok (name, params, wellformed_defs, wellformed_stmts, wellformed_expr)

and wellformed_class_from_class (ast_class : System.Class.t) : (Class.t, string) result =
  let open Result.Let_syntax in
  match ast_class with
  | System.Class.ErrClass _ -> Error "Found an err class"
  | System.Class.Class (name, fields, methods, shape) ->
    let%bind wellformed_methods =
      Result.all (List.map methods ~f:wellformed_method_from_method)
    in
    let%bind wellformed_class_shape = wellformed_class_shape_from_class_shape shape in
    Ok (name, fields, wellformed_methods, wellformed_class_shape)
;;

let wellformed_import_from_mixed_import (system_mixed_import : System.MixedImport.t)
  : (MixedImport.t, string) result
  =
  match system_mixed_import with
  | System.MixedImport.ErrImport _ -> Error "Found an err import"
  | System.MixedImport.Import name -> Ok (MixedImport.Import name)
  | System.MixedImport.TypedImport (name, shape) ->
    let open Result.Let_syntax in
    let%bind wellformed_shape = wellformed_shape_from_shape shape in
    Ok (MixedImport.TypedImport (name, wellformed_shape))
;;

let wellformed_module_from_mixed_module (system_module : System.MixedModule.t)
  : (MixedModule.t, string) result
  =
  let open Result.Let_syntax in
  match system_module with
  | System.MixedModule.ErrModule _ -> Error "Found an err module"
  | System.MixedModule.MixedModule (name, imports, class_, shape) ->
    let%bind wellformed_imports =
      Result.all (List.map imports ~f:wellformed_import_from_mixed_import)
    in
    let%bind wellformed_class = wellformed_class_from_class class_ in
    let%bind wellformed_shape = wellformed_shape_from_shape shape in
    Ok
      (MixedModule.MixedModule
         (name, wellformed_imports, wellformed_class, wellformed_shape))
  | System.MixedModule.Module (name, imports, class_) ->
    let%bind wellformed_imports =
      Result.all (List.map imports ~f:wellformed_import_from_import)
    in
    let%bind wellformed_class = wellformed_class_from_class class_ in
    Ok (MixedModule.Module (name, wellformed_imports, wellformed_class))
;;

let wellformed_typed_system_from_mixed_system (system : System.MixedSystem.t)
  : (WellformedMixedSystem.t, string) result
  =
  let open Result.Let_syntax in
  match system with
  | System.MixedSystem.ErrMixedSystem _ -> Error "Found an err system"
  | System.MixedSystem.MixedSystem (modules, imports, defs, stmts, expr) ->
    let%bind wellformed_modules =
      Result.all (List.map modules ~f:wellformed_module_from_mixed_module)
    in
    let%bind wellformed_imports =
      Result.all (List.map imports ~f:wellformed_import_from_mixed_import)
    in
    let%bind wellformed_defs = Result.all (List.map defs ~f:wellformed_def_from_def) in
    let%bind wellformed_stmts =
      Result.all (List.map stmts ~f:wellformed_stmt_from_stmt)
    in
    let%bind wellformed_expr = wellformed_expr_from_expr expr in
    Ok
      ( wellformed_modules
      , wellformed_imports
      , wellformed_defs
      , wellformed_stmts
      , wellformed_expr )
;;

(* ------------------------------------------------------------------ *)
(* SOUND LINKER                                                        *)
(* ------------------------------------------------------------------ *)

let find_module_by_name (name : Name.t) (modules : MixedModule.t list)
  : MixedModule.t option
  =
  List.find modules ~f:(fun m ->
    match m with
    | MixedModule.MixedModule (n, _, _, _) -> Name.equal n name
    | MixedModule.Module (n, _, _) -> Name.equal n name)
;;

let get_class_from_module (module_ : MixedModule.t) : Class.t =
  match module_ with
  | MixedModule.MixedModule (_, _, class_, _) -> class_
  | MixedModule.Module (_, _, class_) -> class_
;;

let get_imports_from_module (module_ : MixedModule.t) : MixedImport.t list =
  match module_ with
  | MixedModule.MixedModule (_, imports, _, _) -> imports
  | MixedModule.Module (_, imports, _) ->
    List.map imports ~f:(fun i -> MixedImport.Import i)
;;

let create_tmodule_from_module
      (original_module : MixedModule.t)
      (new_name : Name.t)
      (shape : Shape.t)
  : MixedModule.t
  =
  let class_ = get_class_from_module original_module in
  let imports = get_imports_from_module original_module in
  MixedModule.MixedModule (new_name, imports, class_, shape)
;;

let process_single_import
      (mixed_import : MixedImport.t)
      (seen_modules : String.t Hash_set.t)
      (all_modules : MixedModule.t list)
      (suffix : string)
  : Import.t * MixedModule.t option
  =
  match mixed_import with
  | MixedImport.Import name -> name, None
  | MixedImport.TypedImport (name, shape) ->
    let string_name = Name.to_string name in
    let new_import_name = Name.of_string (string_name ^ suffix) in
    let tmodule_opt =
      if Hash_set.mem seen_modules string_name
      then None
      else (
        Hash_set.add seen_modules string_name;
        match find_module_by_name name all_modules with
        | Some original_module ->
          Some
            (create_tmodule_from_module
               original_module
               (Name.of_string (string_name ^ suffix))
               shape)
        | None -> None)
    in
    new_import_name, tmodule_opt
;;

let process_imports_with_deduplication
      (imports : MixedImport.t list)
      (all_modules : MixedModule.t list)
      (suffix : string)
  : Import.t list * MixedModule.t list
  =
  let seen_modules = Hash_set.create (module String) in
  let results =
    List.map imports ~f:(fun imp ->
      process_single_import imp seen_modules all_modules suffix)
  in
  let new_imports = List.map results ~f:fst in
  let new_tmodules = List.filter_map results ~f:snd in
  new_imports, new_tmodules
;;

let update_body_imports_and_synthesize_tmodules
      (imports : MixedImport.t list)
      (modules : MixedModule.t list)
  : Import.t list * MixedModule.t list
  =
  process_imports_with_deduplication imports modules ".into.Body"
;;

let process_module_and_synthesize
      (module_ : MixedModule.t)
      (all_modules : MixedModule.t list)
  : MixedModule.t * MixedModule.t list
  =
  match module_ with
  | MixedModule.Module (_, _, _) -> module_, []
  | MixedModule.MixedModule (name, mixed_imports, class_, shape) ->
    let string_module_name = Name.to_string name in
    let suffix = ".into." ^ string_module_name in
    let _new_imports, new_tmodules =
      process_imports_with_deduplication mixed_imports all_modules suffix
    in
    let new_module = MixedModule.MixedModule (name, mixed_imports, class_, shape) in
    new_module, new_tmodules
;;

let synthesis ((modules, imports, defs, stmts, expr) : WellformedMixedSystem.t)
  : PostSoundLinkerSystem.t
  =
  let process_results =
    List.map modules ~f:(fun m -> process_module_and_synthesize m modules)
  in
  let processed_modules = List.map process_results ~f:fst in
  let module_tmodules = List.concat (List.map process_results ~f:snd) in
  let new_imports, body_tmodules =
    update_body_imports_and_synthesize_tmodules imports modules
  in
  let all_modules = processed_modules @ module_tmodules @ body_tmodules in
  all_modules, new_imports, defs, stmts, expr
;;

let sound_link_system (system : WellformedMixedSystem.t) : PostSoundLinkerSystem.t =
  synthesis system
;;

let module_to_string (module_ : MixedModule.t) : string =
  match module_ with
  | MixedModule.MixedModule (name, _, _, _) -> Name.to_string name
  | MixedModule.Module (name, _, _) -> Name.to_string name
;;

let print_sound_linker_result ((modules, _, _, _, _) : PostSoundLinkerSystem.t) : string =
  let module_strings = List.map modules ~f:(fun m -> "\"" ^ module_to_string m ^ "\"") in
  "[" ^ String.concat ~sep:", " module_strings ^ "]"
;;
