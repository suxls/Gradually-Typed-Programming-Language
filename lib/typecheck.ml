module String = Core.String
module Hashtbl = Core.Hashtbl
module List = Core.List

(* ------------------------------------------------------------------ *)
(* SHAPE TABLE (s_classes) CONSTRUCTION                                *)
(* ------------------------------------------------------------------ *)

let get_shape_from_module_or_import
      (module_ : System.MixedModule.t)
      (import : System.MixedImport.t)
  : System.Shape.t
  =
  match module_, import with
  | System.MixedModule.MixedModule (_, _, _, shape), _ -> shape
  | System.MixedModule.Module _, System.MixedImport.TypedImport (_, shape) -> shape
  | System.MixedModule.ErrModule _, _ ->
    System.Shape.ErrShape (TypeError "Did not find a wellformed shape while validating")
  | _, System.MixedImport.ErrImport _ ->
    System.Shape.ErrShape (TypeError "Did not find a wellformed import while validating")
  | System.MixedModule.Module _, _ ->
    System.Shape.ErrShape (TypeError "Module did not come with a shape in the import")
;;

let get_module_from_import
      (import_ : System.MixedImport.t)
      (modules : System.MixedModule.t list)
  : System.MixedModule.t
  =
  match import_ with
  | System.MixedImport.TypedImport (name, _) ->
    System.find_module_from_import name modules
  | System.MixedImport.Import name -> System.find_module_from_import name modules
  | System.MixedImport.ErrImport _ -> raise (Failure "Not implemented")
;;

let build_s_classes
      (modules : System.MixedModule.t list)
      (imports : System.MixedImport.t list)
  : (string, System.Shape.t) Hashtbl.t
  =
  let s_classes = Hashtbl.create (module String) in
  List.fold imports ~init:s_classes ~f:(fun acc import_ ->
    let module_ = get_module_from_import import_ modules in
    let shape = get_shape_from_module_or_import module_ import_ in
    let classname = System.get_classname_from_module module_ in
    Hashtbl.set acc ~key:classname ~data:shape;
    acc)
;;

(* ------------------------------------------------------------------ *)
(* TYPE ENVIRONMENT (t_var) CONSTRUCTION                               *)
(* ------------------------------------------------------------------ *)

let build_t_var () : (string, System.Type.t) Hashtbl.t = Hashtbl.create (module String)

let add_var_to_tvar
      (t_var : (string, System.Type.t) Hashtbl.t)
      (var : Name.t)
      (type_ : System.Type.t)
  : (string, System.Type.t) Hashtbl.t
  =
  Hashtbl.set t_var ~key:(Name.to_string var) ~data:type_;
  t_var
;;

let new_tvar_from_tvar (t_var : (string, System.Type.t) Hashtbl.t)
  : (string, System.Type.t) Hashtbl.t
  =
  Hashtbl.copy t_var
;;

(* ------------------------------------------------------------------ *)
(* LOOKUP FUNCTIONS                                                    *)
(* ------------------------------------------------------------------ *)

let lookup_var_from_t_var (t_var : (string, System.Type.t) Hashtbl.t) (var : Name.t)
  : System.Type.t
  =
  if Hashtbl.mem t_var (Name.to_string var)
  then Hashtbl.find_exn t_var (Name.to_string var)
  else System.Type.ErrType (System.Error.TypeError "Variable not found in t_var")
;;

let lookup_field_type_from_shape (shape : System.Shape.t) (fieldname : Name.t)
  : System.FieldType.t
  =
  match shape with
  | System.Shape.Shape (fields, _) ->
    (match
       List.find fields ~f:(fun field ->
         match field with
         | System.FieldType.FieldType (name, _) -> Name.equal fieldname name
         | System.FieldType.ErrFieldType _ -> false)
     with
     | Some field_type -> field_type
     | None ->
       System.FieldType.ErrFieldType
         (System.Error.TypeError
            ("Field '" ^ Name.to_string fieldname ^ "' not found in shape")))
  | System.Shape.ErrShape _ ->
    System.FieldType.ErrFieldType
      (System.Error.TypeError "Found an error shape while checking for a field type")
;;

let lookup_method_type_from_shape (shape : System.Shape.t) (methodname : Name.t)
  : System.MethodType.t
  =
  match shape with
  | System.Shape.Shape (_, methods) ->
    (match
       List.find methods ~f:(fun method_ ->
         match method_ with
         | System.MethodType.MethodType (name, _, _) -> Name.equal methodname name
         | System.MethodType.ErrMethodType _ -> false)
     with
     | Some method_type -> method_type
     | None ->
       System.MethodType.ErrMethodType
         (System.Error.TypeError
            ("Method '" ^ Name.to_string methodname ^ "' not found in shape")))
  | System.Shape.ErrShape _ ->
    System.MethodType.ErrMethodType
      (System.Error.TypeError "Found an error shape while checking for a method type")
;;

let lookup_shape_from_s_classes
      (s_classes : (string, System.Shape.t) Hashtbl.t)
      (classname : Name.t)
  : System.Shape.t
  =
  if Hashtbl.mem s_classes (Name.to_string classname)
  then Hashtbl.find_exn s_classes (Name.to_string classname)
  else System.Shape.ErrShape (System.Error.TypeError "Class not found in s_classes")
;;

let get_field_types_from_shape (shape : System.Shape.t) : System.Type.t list =
  match shape with
  | System.Shape.Shape (fields, _) ->
    List.map fields ~f:(fun field ->
      match field with
      | System.FieldType.FieldType (_, type_) -> type_
      | System.FieldType.ErrFieldType _ ->
        System.Type.ErrType
          (System.Error.TypeError "Found an error field type while checking for its type"))
  | System.Shape.ErrShape _ -> []
;;

let get_type_of_field_type (field_type : System.FieldType.t) : System.Type.t =
  match field_type with
  | System.FieldType.FieldType (_, type_) -> type_
  | System.FieldType.ErrFieldType _ ->
    System.Type.ErrType
      (System.Error.TypeError "Found an error field type while checking for its type")
;;

let get_type_of_method_type (method_type : System.MethodType.t) : System.Type.t =
  match method_type with
  | System.MethodType.MethodType (_, _, type_) -> type_
  | System.MethodType.ErrMethodType _ ->
    System.Type.ErrType
      (System.Error.TypeError "Found an error method type while checking for its type")
;;

let get_param_types_from_method_type (method_type : System.MethodType.t)
  : System.Type.t list
  =
  match method_type with
  | System.MethodType.MethodType (_, param_types, _) -> param_types
  | System.MethodType.ErrMethodType _ -> []
;;

(* ------------------------------------------------------------------ *)
(* EXPRESSION TYPE INFERENCE                                           *)
(* ------------------------------------------------------------------ *)

let get_type_of_expr
      (expr : System.Expr.t)
      (s_classes : (string, System.Shape.t) Hashtbl.t)
      (t_var : (string, System.Type.t) Hashtbl.t)
  : System.Type.t
  =
  match expr with
  | Var v -> Hashtbl.find_exn t_var (Name.to_string v)
  | Plus (_, _) | Divide (_, _) | Equal (_, _) | IsA (_, _) | Gnum _ -> System.Type.Number
  | Instance (classname, _) ->
    let shape = lookup_shape_from_s_classes s_classes classname in
    System.Type.Shape shape
  | FieldAccess (var, fieldname) ->
    let var_shape = lookup_var_from_t_var t_var var in
    (match var_shape with
     | System.Type.Shape shape ->
       let field_type = lookup_field_type_from_shape shape fieldname in
       get_type_of_field_type field_type
     | _ ->
       System.Type.ErrType
         (System.Error.TypeError "Found an error type while checking for a field type"))
  | MethodCall (var, methodname, _) ->
    let var_shape = lookup_var_from_t_var t_var var in
    (match var_shape with
     | System.Type.Shape shape ->
       let method_type = lookup_method_type_from_shape shape methodname in
       get_type_of_method_type method_type
     | _ ->
       System.Type.ErrType
         (System.Error.TypeError "Found an error type while checking for a method type"))
  | ErrExpr _ ->
    System.Type.ErrType
      (System.Error.TypeError "Found an error expr while checking for its type")
;;

let check_type_lists_equal (list1 : System.Type.t list) (list2 : System.Type.t list)
  : bool
  =
  List.length list1 = List.length list2
  && List.for_all2_exn list1 list2 ~f:System.type_equal
;;

(* ------------------------------------------------------------------ *)
(* TYPE CHECKING                                                       *)
(* ------------------------------------------------------------------ *)

let typecheck_expr
      (expr : System.Expr.t)
      (s_classes : (string, System.Shape.t) Hashtbl.t)
      (t_var : (string, System.Type.t) Hashtbl.t)
  : System.Expr.t
  =
  match expr with
  | Var _ | Gnum _ | Equal (_, _) -> expr
  | IsA (_, _) | ErrExpr _ -> expr
  | Plus (a, b) | Divide (a, b) ->
    let a_type = lookup_var_from_t_var t_var a in
    let b_type = lookup_var_from_t_var t_var b in
    if a_type = System.Type.Number && b_type = System.Type.Number
    then expr
    else ErrExpr (System.Error.TypeError "Types of plus operands do not match")
  | Instance (classname, fields) ->
    let shape = lookup_shape_from_s_classes s_classes classname in
    let field_types_from_fields =
      List.map fields ~f:(fun field -> lookup_var_from_t_var t_var field)
    in
    let field_types_from_shape = get_field_types_from_shape shape in
    if not (check_type_lists_equal field_types_from_fields field_types_from_shape)
    then ErrExpr (System.Error.TypeError "Field number mismatch")
    else expr
  | FieldAccess (var, fieldname) ->
    let var_type = lookup_var_from_t_var t_var var in
    (match var_type with
     | System.Type.Shape shape ->
       let field_type = lookup_field_type_from_shape shape fieldname in
       (match field_type with
        | System.FieldType.ErrFieldType _ ->
          ErrExpr
            (System.Error.TypeError
               ("Field '" ^ Name.to_string fieldname ^ "' not found in shape"))
        | _ -> expr)
     | _ ->
       ErrExpr
         (System.Error.TypeError "Found an error type while checking for a field type"))
  | MethodCall (var, methodname, params) ->
    let var_type = lookup_var_from_t_var t_var var in
    (match var_type with
     | System.Type.Number ->
       ErrExpr
         (System.Error.TypeError
            "Cannot call method on Number type - methods only exist on Shape types")
     | System.Type.Shape shape ->
       let method_type = lookup_method_type_from_shape shape methodname in
       (match method_type with
        | System.MethodType.ErrMethodType _ ->
          ErrExpr
            (System.Error.TypeError
               ("Method '" ^ Name.to_string methodname ^ "' not found in shape"))
        | _ ->
          let actual_param_types =
            List.map params ~f:(fun param -> lookup_var_from_t_var t_var param)
          in
          let expected_param_types = get_param_types_from_method_type method_type in
          if check_type_lists_equal actual_param_types expected_param_types
          then expr
          else
            ErrExpr
              (System.Error.TypeError
                 ("Parameter type mismatch for method '" ^ Name.to_string methodname ^ "'")))
     | System.Type.ErrType _ ->
       ErrExpr
         (System.Error.TypeError "Found an error type while checking for a method type"))
;;

let typecheck_final_expr
      (expr : System.Expr.t)
      (s_classes : (string, System.Shape.t) Hashtbl.t)
      (t_var : (string, System.Type.t) Hashtbl.t)
  : System.Expr.t
  =
  let type_checked_expr = typecheck_expr expr s_classes t_var in
  match type_checked_expr with
  | ErrExpr _ -> type_checked_expr
  | _ ->
    let type_of_expr = get_type_of_expr type_checked_expr s_classes t_var in
    if type_of_expr = System.Type.Number
    then type_checked_expr
    else ErrExpr (System.Error.TypeError "Expression is not a number")
;;

let typecheck_def
      (def : System.Def.t)
      (s_classes : (string, System.Shape.t) Hashtbl.t)
      (t_var : (string, System.Type.t) Hashtbl.t)
  : System.Def.t * (string, System.Type.t) Hashtbl.t
  =
  match def with
  | Def (name, expr) ->
    let expr_type = get_type_of_expr expr s_classes t_var in
    let t_var = add_var_to_tvar t_var name expr_type in
    Def (name, typecheck_expr expr s_classes t_var), t_var
  | ErrDef _ -> def, t_var
;;

let rec typecheck_block
          (block : System.Block.t)
          (s_classes : (string, System.Shape.t) Hashtbl.t)
          (t_var : (string, System.Type.t) Hashtbl.t)
  : System.Block.t
  =
  match block with
  | Block (defs, stmts) ->
    let new_tvar = new_tvar_from_tvar t_var in
    let potential_type_safe_defs, finished_tvar =
      List.fold defs ~init:([], new_tvar) ~f:(fun (defs_acc, var_set_list) cur_def ->
        let type_checked_def, new_tvar = typecheck_def cur_def s_classes var_set_list in
        defs_acc @ [ type_checked_def ], new_tvar)
    in
    let potential_type_safe_stmts =
      List.map stmts ~f:(fun stmt -> typecheck_stmt stmt s_classes finished_tvar)
    in
    Block (potential_type_safe_defs, potential_type_safe_stmts)
  | Stmt stmt -> Stmt (typecheck_stmt stmt s_classes t_var)
  | ErrBlock _ -> block

and typecheck_stmt
      (stmt : System.Stmt.t)
      (s_classes : (string, System.Shape.t) Hashtbl.t)
      (t_var : (string, System.Type.t) Hashtbl.t)
  : System.Stmt.t
  =
  match stmt with
  | AssignVar (name, expr) ->
    let type_of_name = lookup_var_from_t_var t_var name in
    let type_of_expr = get_type_of_expr expr s_classes t_var in
    if System.type_equal type_of_name type_of_expr
    then AssignVar (name, expr)
    else ErrStmt (System.Error.TypeError "Types of assign var do not match")
  | If0 (expr, b1, b2) ->
    If0 (expr, typecheck_block b1 s_classes t_var, typecheck_block b2 s_classes t_var)
  | While0 (expr, block) -> While0 (expr, typecheck_block block s_classes t_var)
  | AssignField (name, field, expr) ->
    let type_of_name = lookup_var_from_t_var t_var name in
    (match type_of_name with
     | System.Type.Number -> ErrStmt (System.Error.TypeError "Type of name is a number")
     | System.Type.Shape shape ->
       let field_type = lookup_field_type_from_shape shape field in
       let type_of_field_type = get_type_of_field_type field_type in
       let type_of_expr = get_type_of_expr expr s_classes t_var in
       if System.type_equal type_of_expr type_of_field_type
       then AssignField (name, field, expr)
       else ErrStmt (System.Error.TypeError "Types of assign field do not match")
     | _ -> ErrStmt (System.Error.TypeError "Type of name is not a number or shape"))
  | ErrStmt _ -> stmt
;;

let get_fieldnames_of_fieldtypes_from_shape (shape : System.Shape.t) : Name.t list =
  match shape with
  | System.Shape.Shape (fields, _) ->
    List.map fields ~f:(fun field ->
      match field with
      | System.FieldType.FieldType (name, _) -> name
      | System.FieldType.ErrFieldType _ ->
        Name.ErrVar "Found an error field type post parsing")
  | System.Shape.ErrShape _ -> [ Name.ErrVar "Found an error shape post parsing" ]
;;

let get_methodnames_of_methodtypes_from_shape (shape : System.Shape.t) : Name.t list =
  match shape with
  | System.Shape.Shape (_, methods) ->
    List.map methods ~f:(fun method_ ->
      match method_ with
      | System.MethodType.MethodType (name, _, _) -> name
      | System.MethodType.ErrMethodType _ ->
        Name.ErrVar "Found an error method type post parsing")
  | System.Shape.ErrShape _ -> [ Name.ErrVar "Found an error shape post parsing" ]
;;

let typecheck_all_fields (fields : Name.t list) (shape : System.Shape.t) : Name.t list =
  let fields_from_shape = get_fieldnames_of_fieldtypes_from_shape shape in
  if List.length fields <> List.length fields_from_shape
  then [ Name.ErrVar "Field number mismatch" ]
  else (
    let mismatches =
      List.fold2_exn fields fields_from_shape ~init:[] ~f:(fun acc field shape_field ->
        if Name.equal field shape_field
        then acc
        else
          Name.ErrVar
            ("Field mismatch: expected '"
             ^ Name.to_string shape_field
             ^ "' but got '"
             ^ Name.to_string field
             ^ "'")
          :: acc)
    in
    if List.is_empty mismatches then fields else [ Name.ErrVar "Field number mismatch" ])
;;

let get_method_names_from_methods (methods : System.Method.t list) : Name.t list =
  List.map methods ~f:(fun method_ ->
    match method_ with
    | System.Method.Method (name, _, _, _, _) -> name
    | System.Method.ErrMethod _ -> Name.ErrVar "Found an error method post parsing")
;;

let typecheck_all_methods_methodtypes
      (methods : System.Method.t list)
      (shape : System.Shape.t)
  : System.Method.t list
  =
  let method_names_from_shape = get_methodnames_of_methodtypes_from_shape shape in
  let method_names_from_methods = get_method_names_from_methods methods in
  if List.length method_names_from_methods <> List.length method_names_from_shape
  then [ System.Method.ErrMethod (System.Error.TypeError "Method number mismatch") ]
  else (
    let mismatches =
      List.fold2_exn
        method_names_from_methods
        method_names_from_shape
        ~init:[]
        ~f:(fun acc method_name methodname_from_shape ->
          if Name.equal method_name methodname_from_shape
          then acc
          else
            Name.ErrVar
              ("Method mismatch: expected '"
               ^ Name.to_string methodname_from_shape
               ^ "' but got '"
               ^ Name.to_string method_name
               ^ "'")
            :: acc)
    in
    if List.is_empty mismatches
    then methods
    else
      [ System.Method.ErrMethod (System.Error.TypeError "Method name mismatch") ])
;;

let populate_tvar_with_method_fields
      (t_var : (string, System.Type.t) Hashtbl.t)
      (params : Name.t list)
      (method_name : Name.t)
      (shape : System.Shape.t)
  : (string, System.Type.t) Hashtbl.t
  =
  let method_type = lookup_method_type_from_shape shape method_name in
  match method_type with
  | System.MethodType.MethodType (_, param_types, _) ->
    if List.length params <> List.length param_types
    then t_var
    else
      List.fold2_exn params param_types ~init:t_var ~f:(fun acc param param_type ->
        add_var_to_tvar acc param param_type)
  | System.MethodType.ErrMethodType _ -> t_var
;;

let typecheck_method_body
      (method_ : System.Method.t)
      (s_classes : (string, System.Shape.t) Hashtbl.t)
      (shape : System.Shape.t)
  : System.Method.t
  =
  match method_ with
  | System.Method.Method (name, params, defs, stmts, expr) ->
    let tvar_init = build_t_var () in
    let tvar_with_this =
      add_var_to_tvar tvar_init (Name.Name "this") (System.Type.Shape shape)
    in
    let tvar_with_params =
      populate_tvar_with_method_fields tvar_with_this params name shape
    in
    let potential_type_safe_defs, finished_tvar =
      List.fold
        defs
        ~init:([], tvar_with_params)
        ~f:(fun (defs_acc, var_set_list) cur_def ->
          let type_checked_def, new_tvar = typecheck_def cur_def s_classes var_set_list in
          defs_acc @ [ type_checked_def ], new_tvar)
    in
    let potential_type_safe_stmts =
      List.map stmts ~f:(fun stmt -> typecheck_stmt stmt s_classes finished_tvar)
    in
    let potential_type_safe_expr = typecheck_expr expr s_classes finished_tvar in
    System.Method.Method
      ( name
      , params
      , potential_type_safe_defs
      , potential_type_safe_stmts
      , potential_type_safe_expr )
  | System.Method.ErrMethod _ -> method_
;;

let typecheck_class
      (class_ : System.Class.t)
      (s_classes : (string, System.Shape.t) Hashtbl.t)
      (shape : System.Shape.t)
  : System.Class.t
  =
  match class_ with
  | Class (name, fields, methods, s) ->
    let potential_type_safe_fields = typecheck_all_fields fields shape in
    let potential_type_safe_methods_by_type =
      typecheck_all_methods_methodtypes methods shape
    in
    let potential_type_safe_methods_by_body =
      List.map potential_type_safe_methods_by_type ~f:(fun method_ ->
        typecheck_method_body method_ s_classes shape)
    in
    Class (name, potential_type_safe_fields, potential_type_safe_methods_by_body, s)
  | ErrClass _ -> class_
;;

let typecheck_mixed_module
      (cur_module : System.MixedModule.t)
      (modules_so_far : System.MixedModule.t list)
  : System.MixedModule.t * System.MixedModule.t list
  =
  match cur_module with
  | Module _ -> cur_module, cur_module :: modules_so_far
  | MixedModule (name, imports, class_, shape) ->
    let s_classes = build_s_classes modules_so_far imports in
    let potential_type_safe_class = typecheck_class class_ s_classes shape in
    let potential_type_safe_module =
      System.MixedModule.MixedModule (name, imports, potential_type_safe_class, shape)
    in
    potential_type_safe_module, potential_type_safe_module :: modules_so_far
  | ErrModule _ -> cur_module, cur_module :: modules_so_far
;;

let typecheck_mixed_system (system : System.MixedSystem.t) : System.MixedSystem.t =
  match system with
  | System.MixedSystem.MixedSystem (modules, imports, defs, stmts, expr) ->
    let potential_type_safe_modules, _ =
      List.fold modules ~init:([], []) ~f:(fun (modules_acc, modules_so_far) cur_module ->
        let type_checked_module, new_modules_so_far =
          typecheck_mixed_module cur_module modules_so_far
        in
        modules_acc @ [ type_checked_module ], new_modules_so_far)
    in
    let s_classes = build_s_classes modules imports in
    let tvar = build_t_var () in
    let potential_type_safe_defs, finished_tvar =
      List.fold defs ~init:([], tvar) ~f:(fun (defs_acc, tvar_acc) cur_def ->
        let type_checked_def, new_tvar = typecheck_def cur_def s_classes tvar_acc in
        defs_acc @ [ type_checked_def ], new_tvar)
    in
    let potential_type_safe_stmts =
      List.map ~f:(fun stmt -> typecheck_stmt stmt s_classes finished_tvar) stmts
    in
    let potential_type_safe_expr = typecheck_final_expr expr s_classes finished_tvar in
    System.MixedSystem.MixedSystem
      ( potential_type_safe_modules
      , imports
      , potential_type_safe_defs
      , potential_type_safe_stmts
      , potential_type_safe_expr )
  | System.MixedSystem.ErrMixedSystem err -> System.MixedSystem.ErrMixedSystem err
;;
