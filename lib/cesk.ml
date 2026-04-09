module String = Core.String
module Hashtbl = Core.Hashtbl
module List = Core.List
module Set = Core.Set
module Int = Core.Int
module Float = Core.Float
module Result = Core.Result
module Option = Core.Option

(* ------------------------------------------------------------------ *)
(* TYPE DEFINITIONS                                                    *)
(* ------------------------------------------------------------------ *)

module rec Value : sig
  type t =
    | Object of Object.t
    | Proxy of Proxy.t
    | Number of float
end = struct
  type t =
    | Object of Object.t
    | Proxy of Proxy.t
    | Number of float
end

and Object : sig
  type t = Name.t * (string, Value.t) Hashtbl.t * Wellformed_system.Method.t list
end = struct
  type t = Name.t * (string, Value.t) Hashtbl.t * Wellformed_system.Method.t list
end

and Proxy : sig
  type t = Object.t * Wellformed_system.Shape.t
end = struct
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
end = struct
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
end = struct
  type t = (string, string) Hashtbl.t
end

module Store : sig
  type t = (string, Value.t) Hashtbl.t
end = struct
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
end = struct
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
end = struct
  type t =
    { c : Control.t
    ; e : Environment.t
    ; s : Store.t
    ; k : Closure.t list
    }
end

(* ------------------------------------------------------------------ *)
(* ENVIRONMENT AND STORE                                               *)
(* ------------------------------------------------------------------ *)

let create_env_copy (prev_env : (string, string) Hashtbl.t) : (string, string) Hashtbl.t =
  Hashtbl.copy prev_env
;;

let create_empty_env () : (string, string) Hashtbl.t = Hashtbl.create (module String)

let add_to_env (env : Environment.t) (var : Name.t) (storelength : int) : Environment.t =
  let var_str = Name.to_string var in
  let new_location = var_str ^ string_of_int storelength in
  Hashtbl.set env ~key:var_str ~data:new_location;
  env
;;

let update_store (store : Store.t) (var : Name.t) (env : Environment.t) (num : Value.t)
  : Store.t
  =
  Hashtbl.set store ~key:(Hashtbl.find_exn env (Name.to_string var)) ~data:num;
  store
;;

let lookup_var (env : Environment.t) (store : Store.t) (var : Name.t) : Value.t =
  let location = Hashtbl.find_exn env (Name.to_string var) in
  Hashtbl.find_exn store location
;;

(* ------------------------------------------------------------------ *)
(* OBJECT / PROXY OPERATIONS                                          *)
(* ------------------------------------------------------------------ *)

let class_by_name (name : Name.t) (classes : Wellformed_system.Class.t list)
  : Wellformed_system.Class.t
  =
  List.find_exn classes ~f:(fun (className, _, _, _) -> Name.equal name className)
;;

let sufficient_fields (fields : Name.t list) (template_fields : Name.t list) : bool =
  Int.equal (List.length fields) (List.length template_fields)
;;

let is_field_in_class ((_, tbl, _) : Object.t) (field : Name.t) : bool =
  Hashtbl.mem tbl (Name.to_string field)
;;

let is_method_in_class ((_, _, methods) : Object.t) (methodName : Name.t)
  : Wellformed_system.Method.t option
  =
  List.find methods ~f:(fun (name, _, _, _, _) -> Name.equal name methodName)
;;

let init_fields
      (template : Name.t list)
      (fields : Name.t list)
      (env : Environment.t)
      (store : Store.t)
  : (string, Value.t) Hashtbl.t
  =
  let tbl = Hashtbl.create (module String) in
  List.iter2_exn template fields ~f:(fun template_field field_name ->
    Hashtbl.set
      tbl
      ~key:(Name.to_string template_field)
      ~data:(lookup_var env store field_name));
  tbl
;;

(* ------------------------------------------------------------------ *)
(* SHAPE CONFORMANCE CHECKING                                         *)
(* ------------------------------------------------------------------ *)

let rec shape_equal
          ((fields1, methods1) : Wellformed_system.Shape.t)
          ((fields2, methods2) : Wellformed_system.Shape.t)
  : bool
  =
  field_types_equal fields1 fields2 && method_types_equal methods1 methods2

and field_types_equal
      (fields1 : Wellformed_system.FieldType.t list)
      (fields2 : Wellformed_system.FieldType.t list)
  : bool
  =
  List.length fields1 = List.length fields2
  && List.for_all2_exn fields1 fields2 ~f:field_type_equal

and field_type_equal
      ((name1, type1) : Wellformed_system.FieldType.t)
      ((name2, type2) : Wellformed_system.FieldType.t)
  : bool
  =
  Name.equal name1 name2 && type_equal type1 type2

and method_types_equal
      (methods1 : Wellformed_system.MethodType.t list)
      (methods2 : Wellformed_system.MethodType.t list)
  : bool
  =
  List.length methods1 = List.length methods2
  && List.for_all2_exn methods1 methods2 ~f:method_type_equal

and method_type_equal
      ((name1, params1, return1) : Wellformed_system.MethodType.t)
      ((name2, params2, return2) : Wellformed_system.MethodType.t)
  : bool
  =
  Name.equal name1 name2
  && List.length params1 = List.length params2
  && List.for_all2_exn params1 params2 ~f:type_equal
  && type_equal return1 return2

and type_equal (type1 : Wellformed_system.Type.t) (type2 : Wellformed_system.Type.t)
  : bool
  =
  match type1, type2 with
  | Wellformed_system.Type.Number, Wellformed_system.Type.Number -> true
  | Wellformed_system.Type.Shape shape1, Wellformed_system.Type.Shape shape2 ->
    shape_equal shape1 shape2
  | _, _ -> false
;;

let shape_from_name_in_field_type
      (shape_fields : Wellformed_system.FieldType.t list)
      (name : Name.t)
  : Wellformed_system.Type.t option
  =
  List.find_map shape_fields ~f:(fun (field_name, type_) ->
    if String.equal (Name.to_string field_name) (Name.to_string name)
    then Some type_
    else None)
;;

let shape_from_name_in_method_types
      (shape_methods : Wellformed_system.MethodType.t list)
      (name : Name.t)
  : (Wellformed_system.Type.t list * Wellformed_system.Type.t) option
  =
  List.find_map shape_methods ~f:(fun (method_name, param_types, return_type) ->
    if String.equal (Name.to_string method_name) (Name.to_string name)
    then Some (param_types, return_type)
    else None)
;;

let rec methods_conform
          (object_methods : Wellformed_system.Method.t list)
          (shape_methods : Wellformed_system.MethodType.t list)
  : bool
  =
  List.for_all2_exn object_methods shape_methods ~f:method_conform

and method_conform
      (object_method : Wellformed_system.Method.t)
      (shape_method : Wellformed_system.MethodType.t)
  : bool
  =
  let method_name, o_params, _, _, _ = object_method in
  let shape_method_name, s_param_types, _s_return_type = shape_method in
  Name.equal method_name shape_method_name
  && List.length o_params = List.length s_param_types
;;

let rec object_conforms_to_shape
          ((_, fields, methods) : Object.t)
          ((field_types, methods_types) : Wellformed_system.Shape.t)
  : bool
  =
  fields_conform fields field_types && methods_conform methods methods_types

and value_conforms_to_type
      (given_value : Value.t)
      (promised_field_type : Wellformed_system.Type.t option)
  : bool
  =
  match given_value, promised_field_type with
  | _, None -> false
  | Object o, Some (Shape shape) -> object_conforms_to_shape o shape
  | Proxy (_, proxy_shape), Some (Shape promised_shape) ->
    shape_equal proxy_shape promised_shape
  | Number _, Some Number -> true
  | Object (_, _, _), Some Number -> false
  | Proxy (_, _), Some Number -> false
  | Number _, Some (Shape _) -> false

and fields_conform
      (object_fields : (string, Value.t) Hashtbl.t)
      (shape_fields : Wellformed_system.FieldType.t list)
  : bool
  =
  Hashtbl.for_alli object_fields ~f:(fun ~key:name ~data:given_value ->
    let promised_field_type =
      shape_from_name_in_field_type shape_fields (Name.of_string name)
    in
    value_conforms_to_type given_value promised_field_type)
;;

let rec method_params_conform
          (o_params : Name.t list)
          (s_param_types : Wellformed_system.Type.t list)
          (env : Environment.t)
          (store : Store.t)
  : bool
  =
  List.for_all2_exn o_params s_param_types ~f:(fun o_param type_ ->
    method_param_conform o_param type_ env store)

and method_param_conform
      (method_param : Name.t)
      (shape_type : Wellformed_system.Type.t)
      (env : Environment.t)
      (store : Store.t)
  : bool
  =
  let lookup_val = lookup_var env store method_param in
  let some_type = Option.some shape_type in
  value_conforms_to_type lookup_val some_type
;;

let control_from_conform_fields
      (o : Object.t)
      (shape_fields : Wellformed_system.FieldType.t list)
      (shape_methods : Wellformed_system.MethodType.t list)
  : Control.t
  =
  let _, fields, _ = o in
  if fields_conform fields shape_fields
  then Control.Proxy (o, (shape_fields, shape_methods))
  else
    Control.RuntimeErr
      (Exceptions.RuntimeError "Fields do not conform to class shape")
;;

let control_from_class_values_and_template
      (className : Name.t)
      (given_fields : Name.t list)
      (template_fields : Name.t list)
      (template_methods : Wellformed_system.Method.t list)
      (template_shape : Wellformed_system.ClassShape.t)
      (env : Environment.t)
      (store : Store.t)
  : Control.t
  =
  match template_shape with
  | Wellformed_system.ClassShape.ModuleClassShape ->
    if not (sufficient_fields given_fields template_fields)
    then
      Control.RuntimeErr
        (Exceptions.RuntimeError "Insufficient number of fields for this object")
    else (
      let obj =
        className, init_fields template_fields given_fields env store, template_methods
      in
      Control.Object obj)
  | Wellformed_system.ClassShape.TModuleClassShape (shape_fields, shape_methods) ->
    if not (sufficient_fields given_fields template_fields)
    then
      Control.RuntimeErr
        (Exceptions.RuntimeError "Insufficient number of fields for this object")
    else (
      let obj =
        className, init_fields template_fields given_fields env store, template_methods
      in
      control_from_conform_fields obj shape_fields shape_methods)
;;

(* ------------------------------------------------------------------ *)
(* VALUE OPERATIONS                                                    *)
(* ------------------------------------------------------------------ *)

let float_equal_with_eps f1 f2 epsilon = Float.abs (f1 -. f2) <= epsilon

let is_truthy (value : Value.t) : bool =
  match value with
  | Number n -> float_equal_with_eps 0.0 n 0.001
  | Object _ -> false
  | Proxy _ -> false
;;

let control_from_value (value : Value.t) : Control.t =
  match value with
  | Number n -> Control.Number n
  | Object o -> Control.Object o
  | Proxy p -> Control.Proxy p
;;

let lookup_field_from_object ((_, tbl, _) : Object.t) (field : Name.t) =
  Hashtbl.find_exn tbl (Name.to_string field)
;;

let can_method_be_called (o : Object.t) (methodName : Name.t) (params : Name.t list)
  : bool
  =
  match is_method_in_class o methodName with
  | None -> false
  | Some (_, param_names, _, _, _) ->
    Int.equal (List.length params) (List.length param_names)
;;

type visited_obj_pair = (Object.t * Object.t) Set.Poly.t

let rec value_equal_acc (val1 : Value.t) (val2 : Value.t) (visited : visited_obj_pair)
  : bool
  =
  match val1, val2 with
  | Number n1, Number n2 -> float_equal_with_eps n1 n2 0.001
  | Object o1, Object o2 -> object_equal_acc o1 o2 visited
  | Proxy (o1, _), Proxy (o2, _) -> object_equal_acc o1 o2 visited
  | Proxy (o1, _), Object o2 -> object_equal_acc o1 o2 visited
  | Object o1, Proxy (o2, _) -> object_equal_acc o1 o2 visited
  | _ -> false

and object_equal_acc (obj1 : Object.t) (obj2 : Object.t) (visited : visited_obj_pair)
  : bool
  =
  let name1, tbl1, _ = obj1 in
  let name2, tbl2, _ = obj2 in
  if Set.mem visited (obj1, obj2)
  then true
  else (
    let new_visited = Set.add visited (obj1, obj2) in
    if not (Name.equal name1 name2)
    then false
    else if Hashtbl.length tbl1 <> Hashtbl.length tbl2
    then false
    else (
      let all_equal = ref true in
      Hashtbl.iteri tbl1 ~f:(fun ~key ~data ->
        match Hashtbl.find tbl2 key with
        | None -> all_equal := false
        | Some v2 -> if not (value_equal_acc data v2 new_visited) then all_equal := false);
      !all_equal))
;;

let value_equal (val1 : Value.t) (val2 : Value.t) : bool =
  value_equal_acc val1 val2 Set.Poly.empty
;;

let value_same_type
      (val1 : Value.t)
      (val2 : Value.t)
      (_env : Environment.t)
      (_store : Store.t)
  : bool
  =
  match val1, val2 with
  | Number _, Number _ -> true
  | Object (name1, _, _), Object (name2, _, _) ->
    String.equal (Name.to_string name1) (Name.to_string name2)
  | Proxy ((name1, _, _), _), Proxy ((name2, _, _), _) ->
    String.equal (Name.to_string name1) (Name.to_string name2)
  | Object (o_name, _, _), Proxy ((p_name, _, _), _) ->
    String.equal (Name.to_string o_name) (Name.to_string p_name)
  | Proxy ((p_name, _, _), _), Object (o_name, _, _) ->
    String.equal (Name.to_string o_name) (Name.to_string p_name)
  | _ -> false
;;

(* ------------------------------------------------------------------ *)
(* CLOSURE OPERATIONS                                                  *)
(* ------------------------------------------------------------------ *)

let remove_next_def (closures : Closure.t list) : Closure.t list =
  match List.hd_exn closures with
  | Closure (defs_list, stmts_list, expr_opt, env) ->
    Closure (List.tl_exn defs_list, stmts_list, expr_opt, env) :: List.tl_exn closures
  | ReturnType _ ->
    raise (Exceptions.UnexpectedError "Expected a closure, found a return type")
;;

let remove_next_stmt (closures : Closure.t list) : Closure.t list =
  match List.hd_exn closures with
  | Closure (defs_list, stmts_list, expr_opt, env) ->
    Closure (defs_list, List.tl_exn stmts_list, expr_opt, env) :: List.tl_exn closures
  | ReturnType _ ->
    raise (Exceptions.UnexpectedError "Expected a closure, found a return type")
;;

let add_block_to_closures
      (block : Wellformed_system.Block.t)
      (new_env : Environment.t)
      (closures : Closure.t list)
  : Closure.t list
  =
  match List.hd_exn closures with
  | ReturnType _ ->
    raise (Exceptions.UnexpectedError "Cannot add block to ReturnType closure")
  | Closure (def_list, stmt_list, expr_opt, prev_env) ->
    (match block with
     | Stmt stmt ->
       Closure (def_list, stmt :: stmt_list, expr_opt, prev_env) :: List.tl_exn closures
     | Block (def_list, stmt_list) ->
       Closure (def_list, stmt_list, None, new_env) :: closures)
;;

(* ------------------------------------------------------------------ *)
(* TRANSITION HELPERS                                                  *)
(* ------------------------------------------------------------------ *)

let control_from_def ((_, expr) : Wellformed_system.Def.t) : Control.t =
  Control.Expression expr
;;

let control_from_stmt (stmt : Wellformed_system.Stmt.t) : Control.t =
  match stmt with
  | AssignVar (_, expr) -> Control.Expression expr
  | If0 (expr, _, _) -> Control.Expression expr
  | While0 (expr, _) -> Control.Expression expr
  | AssignField (_, _, expr) -> Control.Expression expr
;;

let control_from_field_access_of_value (value : Value.t) (field : Name.t) : Control.t =
  match value with
  | Number _ ->
    Control.RuntimeErr
      (Exceptions.RuntimeError "Tried to access a field of a number")
  | Object o ->
    if not (is_field_in_class o field)
    then
      Control.RuntimeErr
        (Exceptions.RuntimeError "Field does not exist on this object")
    else control_from_value (lookup_field_from_object o field)
  | Proxy (o, (shape_fields, _)) ->
    if not (is_field_in_class o field)
    then
      Control.RuntimeErr
        (Exceptions.RuntimeError "Field does not exist on this object")
    else (
      let field_value = lookup_field_from_object o field in
      let promised_field_type = shape_from_name_in_field_type shape_fields field in
      if value_conforms_to_type field_value promised_field_type
      then control_from_value field_value
      else
        Control.RuntimeErr
          (Exceptions.RuntimeError "Field does not conform to proxy shape"))
;;

let rec control_from_value_is_a (value : Value.t) (className : Name.t) : Control.t =
  match value with
  | Number _ -> Control.Number 1.0
  | Object (name, _, _) ->
    if Name.equal name className then Control.Number 0.0 else Control.Number 1.0
  | Proxy (o, _) -> control_from_value_is_a (Value.Object o) className
;;

let create_env_with_params_and_this (params : Name.t list) (storelength : int)
  : Environment.t
  =
  let new_env = create_empty_env () in
  let env_with_this = add_to_env new_env (Name.of_string "this") storelength in
  List.fold params ~init:env_with_this ~f:(fun acc_env param_name ->
    add_to_env acc_env param_name storelength)
;;

let store_new_env_vars
      (store : Store.t)
      (param_names : Name.t list)
      (param_values : Value.t list)
      (env : Environment.t)
  : Store.t
  =
  List.fold2_exn
    param_names
    param_values
    ~init:store
    ~f:(fun acc_store param_name param_val ->
      update_store acc_store param_name env param_val)
;;

let control_after_reassign_field (o : Object.t) (field : Name.t) (new_val : Value.t)
  : Control.t
  =
  if not (is_field_in_class o field)
  then
    Control.RuntimeErr
      (Exceptions.RuntimeError "Tried to assign to a nonexistent field")
  else (
    let _, tbl, _ = o in
    Hashtbl.set tbl ~key:(Name.to_string field) ~data:new_val;
    Control.Search)
;;

let control_after_proxy_reassign_field
      (o : Object.t)
      (field_shape : Wellformed_system.FieldType.t list)
      (field : Name.t)
      (new_val : Value.t)
  : Control.t
  =
  if not (is_field_in_class o field)
  then
    Control.RuntimeErr
      (Exceptions.RuntimeError "Tried to assign to a nonexistent field")
  else (
    let field_type = shape_from_name_in_field_type field_shape field in
    if value_conforms_to_type new_val field_type
    then (
      let _, tbl, _ = o in
      Hashtbl.set tbl ~key:(Name.to_string field) ~data:new_val;
      Control.Search)
    else
      Control.RuntimeErr
        (Exceptions.RuntimeError "Field does not conform to proxy shape"))
;;

(* ------------------------------------------------------------------ *)
(* METHOD INVOCATION                                                   *)
(* ------------------------------------------------------------------ *)

let add_method_to_cesk
      (o : Object.t)
      (methodName : Name.t)
      (params : Name.t list)
      (cesk : Cesk.t)
  : Cesk.t
  =
  let method_def =
    match is_method_in_class o methodName with
    | None -> raise (Exceptions.UnexpectedError "Method not found in object")
    | Some method_def -> method_def
  in
  let _, param_names, defs, stmts, return_expr = method_def in
  let s1 = Hashtbl.copy cesk.s in
  let e1 = create_env_with_params_and_this param_names (Hashtbl.length s1) in
  let param_vals =
    List.map params ~f:(fun param_names -> lookup_var cesk.e cesk.s param_names)
  in
  let s1 = store_new_env_vars s1 param_names param_vals e1 in
  let e1 = add_to_env e1 (Name.of_string "this") (Hashtbl.length s1) in
  let s1 = update_store s1 (Name.of_string "this") e1 (Value.Object o) in
  let return_frame = Closure.Closure (defs, stmts, Some return_expr, e1) in
  { c = Control.Search; e = e1; s = s1; k = return_frame :: cesk.k }
;;

let add_return_to_cesk (type_ : Wellformed_system.Type.t) (cesk : Cesk.t) : Cesk.t =
  { cesk with k = Closure.ReturnType type_ :: cesk.k }
;;

let add_proxy_method_to_cesk
      (o : Object.t)
      (methodName : Name.t)
      (params : Name.t list)
      (method_shapes : Wellformed_system.MethodType.t list)
      (cesk : Cesk.t)
  : Cesk.t
  =
  let param_types, return_type =
    match shape_from_name_in_method_types method_shapes methodName with
    | None ->
      raise (Exceptions.UnexpectedError "Method not found in proxy shape")
    | Some (params, return) -> params, return
  in
  if method_params_conform params param_types cesk.e cesk.s
  then add_method_to_cesk o methodName params (add_return_to_cesk return_type cesk)
  else
    { cesk with
      c =
        Control.RuntimeErr
          (Exceptions.RuntimeError "Method params do not conform to proxy shape")
    }
;;

(* ------------------------------------------------------------------ *)
(* EXPRESSION EVALUATION                                               *)
(* ------------------------------------------------------------------ *)

let evaluate_expr
      (expr : Wellformed_system.Expr.t)
      (templates : Wellformed_system.Class.t list)
      (cesk : Cesk.t)
  : Cesk.t
  =
  match expr with
  | Var var ->
    let lookup = lookup_var cesk.e cesk.s var in
    { cesk with c = control_from_value lookup }
  | Gnum n -> { cesk with c = Control.Number n }
  | Plus (v1, v2) ->
    let v1_val = lookup_var cesk.e cesk.s v1 in
    let v2_val = lookup_var cesk.e cesk.s v2 in
    let new_control =
      match v1_val, v2_val with
      | Number n1, Number n2 -> Control.Number (n1 +. n2)
      | _, _ ->
        Control.RuntimeErr (Exceptions.RuntimeError "Cannot add non-numeric values")
    in
    { cesk with c = new_control }
  | Divide (v1, v2) ->
    let v1_val = lookup_var cesk.e cesk.s v1 in
    let v2_val = lookup_var cesk.e cesk.s v2 in
    let new_control =
      match v1_val, v2_val with
      | Number n1, Number n2 ->
        if float_equal_with_eps 0.0 n2 0.001
        then Control.RuntimeErr (Exceptions.RuntimeError "Division by zero")
        else Control.Number (n1 /. n2)
      | _, _ ->
        Control.RuntimeErr (Exceptions.RuntimeError "Cannot divide non-numeric values")
    in
    { cesk with c = new_control }
  | Equal (v1, v2) ->
    let v1_val = lookup_var cesk.e cesk.s v1 in
    let v2_val = lookup_var cesk.e cesk.s v2 in
    let new_control =
      if value_equal v1_val v2_val then Control.Number 0.0 else Control.Number 1.0
    in
    { cesk with c = new_control }
  | Instance (className, fields) ->
    let template = class_by_name className templates in
    let _, template_fields, template_methods, template_shape = template in
    { cesk with
      c =
        control_from_class_values_and_template
          className
          fields
          template_fields
          template_methods
          template_shape
          cesk.e
          cesk.s
    }
  | FieldAccess (var, field) ->
    let lookup = lookup_var cesk.e cesk.s var in
    { cesk with c = control_from_field_access_of_value lookup field }
  | MethodCall (var, methodName, params) ->
    let lookup = lookup_var cesk.e cesk.s var in
    (match lookup with
     | Number _ ->
       { cesk with
         c =
           Control.RuntimeErr
             (Exceptions.RuntimeError "Cannot call method on a number")
       }
     | Object o ->
       if not (can_method_be_called o methodName params)
       then
         { cesk with
           c =
             Control.RuntimeErr
               (Exceptions.RuntimeError
                  "Method does not exist or has wrong parameter count")
         }
       else add_method_to_cesk o methodName params cesk
     | Proxy (o, (_, method_shape)) ->
       if not (can_method_be_called o methodName params)
       then
         { cesk with
           c =
             Control.RuntimeErr
               (Exceptions.RuntimeError
                  "Method does not exist or has wrong parameter count")
         }
       else add_proxy_method_to_cesk o methodName params method_shape cesk)
  | IsA (var, className) ->
    let lookup = lookup_var cesk.e cesk.s var in
    { cesk with c = control_from_value_is_a lookup className }
;;

(* ------------------------------------------------------------------ *)
(* DEFINITION AND STATEMENT EVALUATION                                 *)
(* ------------------------------------------------------------------ *)

let evaluate_def_with_value
      (value : Value.t)
      ((var, _) : Wellformed_system.Def.t)
      (cesk : Cesk.t)
  : Cesk.t
  =
  let new_env = add_to_env cesk.e var (Hashtbl.length cesk.s) in
  { c = Control.Search
  ; e = new_env
  ; s = update_store cesk.s var new_env value
  ; k = remove_next_def cesk.k
  }
;;

let evaluate_stmt_with_value
      (value : Value.t)
      (stmt : Wellformed_system.Stmt.t)
      (cesk : Cesk.t)
  : Cesk.t
  =
  match stmt with
  | If0 (_, b1, b2) ->
    let new_env = create_env_copy cesk.e in
    let block_to_add = if is_truthy value then b1 else b2 in
    { cesk with
      c = Control.Search
    ; e = new_env
    ; k = add_block_to_closures block_to_add new_env (remove_next_stmt cesk.k)
    }
  | While0 (_, b) ->
    let new_env = create_env_copy cesk.e in
    if is_truthy value
    then
      { cesk with
        c = Control.Search
      ; e = new_env
      ; k = add_block_to_closures b new_env cesk.k
      }
    else { cesk with c = Control.Search; k = remove_next_stmt cesk.k }
  | AssignVar (var, _) ->
    let cur_var_value = lookup_var cesk.e cesk.s var in
    if value_same_type cur_var_value value cesk.e cesk.s
    then
      { cesk with
        c = Control.Search
      ; s = update_store cesk.s var cesk.e value
      ; k = remove_next_stmt cesk.k
      }
    else
      { cesk with
        c =
          Control.RuntimeErr
            (Exceptions.RuntimeError "Cannot reassign variable to a different type")
      }
  | AssignField (var, field, _) ->
    let lookup = lookup_var cesk.e cesk.s var in
    (match lookup with
     | Number _ ->
       { cesk with
         c =
           Control.RuntimeErr
             (Exceptions.RuntimeError "Cannot assign field on a number")
       }
     | Object o ->
       let new_control = control_after_reassign_field o field value in
       { cesk with c = new_control; k = remove_next_stmt cesk.k }
     | Proxy (o, (field_shape, _)) ->
       let new_control = control_after_proxy_reassign_field o field_shape field value in
       { cesk with c = new_control; k = remove_next_stmt cesk.k })
;;

let control_after_checking_conformity (val_ : Value.t) (type_ : Wellformed_system.Type.t)
  : Control.t
  =
  if value_conforms_to_type val_ (Option.some type_)
  then control_from_value val_
  else
    Control.RuntimeErr
      (Exceptions.RuntimeError
         "Method return value does not conform to promised return type")
;;

let prev_env_from_next_closure (rest : Closure.t list) : Environment.t =
  let next_closure = List.hd_exn rest in
  match next_closure with
  | ReturnType _ ->
    raise (Exceptions.UnexpectedError "Expected closure env, found return type")
  | Closure (_, _, _, prev_env) -> prev_env
;;

(* ------------------------------------------------------------------ *)
(* CESK MACHINE                                                        *)
(* ------------------------------------------------------------------ *)

let load (wellformed_ast : Linker.WellformedTypedProgram.t)
  : Cesk.t * Wellformed_system.Class.t list
  =
  let c = Control.Search in
  let e = Hashtbl.create (module String) in
  let s = Hashtbl.create (module String) in
  let templates, def_list, stmt_list, expr = wellformed_ast in
  let k : Closure.t list =
    [ Closure.Closure (def_list, stmt_list, Option.some expr, e) ]
  in
  { c; e; s; k }, templates
;;

let transition (cesk : Cesk.t) (templates : Wellformed_system.Class.t list) : Cesk.t =
  match cesk.c, cesk.k with
  | Search, Closure (def :: _, _, _, _) :: _ -> { cesk with c = control_from_def def }
  | Search, Closure ([], stmt :: _, _, _) :: _ -> { cesk with c = control_from_stmt stmt }
  | Search, Closure ([], [], Some expr, _) :: _ ->
    { cesk with c = Control.Expression expr }
  | Search, Closure ([], [], None, _) :: rest -> { cesk with k = rest }
  | Expression expr, _ -> evaluate_expr expr templates cesk
  | Number n, Closure (def :: _, _, _, _) :: _ ->
    evaluate_def_with_value (Value.Number n) def cesk
  | Object o, Closure (def :: _, _, _, _) :: _ ->
    evaluate_def_with_value (Value.Object o) def cesk
  | Proxy p, Closure (def :: _, _, _, _) :: _ ->
    evaluate_def_with_value (Value.Proxy p) def cesk
  | Number n, Closure ([], stmt :: _, _, _) :: _ ->
    evaluate_stmt_with_value (Value.Number n) stmt cesk
  | Object o, Closure ([], stmt :: _, _, _) :: _ ->
    evaluate_stmt_with_value (Value.Object o) stmt cesk
  | Proxy p, Closure ([], stmt :: _, _, _) :: _ ->
    evaluate_stmt_with_value (Value.Proxy p) stmt cesk
  | Number n, ReturnType type_ :: rest ->
    { cesk with
      c = control_after_checking_conformity (Value.Number n) type_
    ; e = prev_env_from_next_closure rest
    ; k = rest
    }
  | Object o, ReturnType type_ :: rest ->
    { cesk with
      c = control_after_checking_conformity (Value.Object o) type_
    ; e = prev_env_from_next_closure rest
    ; k = rest
    }
  | Proxy p, ReturnType type_ :: rest ->
    { cesk with
      c = control_after_checking_conformity (Value.Proxy p) type_
    ; e = prev_env_from_next_closure rest
    ; k = rest
    }
  | (Number _ | Object _ | Proxy _), Closure ([], [], Some _, _) :: rest ->
    (match List.hd_exn rest with
     | Closure (_, _, _, prev_env) -> { cesk with e = prev_env; k = rest }
     | ReturnType _ -> { cesk with k = rest })
  | _, _ ->
    raise (Exceptions.UnexpectedError "Unreachable CESK state")
;;

let unload (cesk : Cesk.t) : (Value.t, string) Result.t =
  match cesk.c with
  | Number n -> Ok (Value.Number n)
  | Object o -> Ok (Value.Object o)
  | Proxy p -> Ok (Value.Proxy p)
  | RuntimeErr _ -> Error "Run time error found"
  | _ -> Error "Unexpected terminal state"
;;

let keep_transitioning (cesk : Cesk.t) : bool =
  match cesk.c, cesk.k with
  | (Number _ | Object _), [ Closure (def_list, stmt_list, _, _) ] ->
    not (List.is_empty def_list && List.is_empty stmt_list)
  | RuntimeErr _, _ -> false
  | _, _ -> true
;;

let run (wellformed_ast : Linker.WellformedTypedProgram.t) : (Value.t, string) Result.t =
  let initial_state, templates = load wellformed_ast in
  let rec loop state =
    if keep_transitioning state
    then loop (transition state templates)
    else unload state
  in
  loop initial_state
;;
