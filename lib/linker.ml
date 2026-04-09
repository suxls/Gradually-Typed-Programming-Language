module String = Core.String
module Hashtbl = Core.Hashtbl
module List = Core.List

module WellformedTypedProgram : sig
  type t =
    Wellformed_system.Class.t list
    * Wellformed_system.Def.t list
    * Wellformed_system.Stmt.t list
    * Wellformed_system.Expr.t
end = struct
  type t =
    Wellformed_system.Class.t list
    * Wellformed_system.Def.t list
    * Wellformed_system.Stmt.t list
    * Wellformed_system.Expr.t
end

let lookup_name (tbl : (string, string) Hashtbl.t) (name : Name.t) : Name.t =
  Name (Hashtbl.find_exn tbl (Name.to_string name))
;;

let link_expr
      (expr : Wellformed_system.Expr.t)
      (cname_to_mname : (string, string) Hashtbl.t)
  : Wellformed_system.Expr.t
  =
  match expr with
  | Var _ | Gnum _ | Plus _ | Divide _ | Equal _
  | FieldAccess (_, _)
  | MethodCall (_, _, _) -> expr
  | Instance (className, fields) -> Instance (lookup_name cname_to_mname className, fields)
  | IsA (v, className) -> IsA (v, lookup_name cname_to_mname className)
;;

let link_def
      ((def_name, expr) : Wellformed_system.Def.t)
      (cname_to_mname : (string, string) Hashtbl.t)
  : Wellformed_system.Def.t
  =
  def_name, link_expr expr cname_to_mname
;;

let rec link_stmt
          (stmt : Wellformed_system.Stmt.t)
          (cname_to_mname : (string, string) Hashtbl.t)
  : Wellformed_system.Stmt.t
  =
  match stmt with
  | AssignVar (v, expr) -> AssignVar (v, link_expr expr cname_to_mname)
  | If0 (expr, b1, b2) ->
    If0
      ( link_expr expr cname_to_mname
      , link_block b1 cname_to_mname
      , link_block b2 cname_to_mname )
  | While0 (expr, block) ->
    While0 (link_expr expr cname_to_mname, link_block block cname_to_mname)
  | AssignField (v, field, expr) -> AssignField (v, field, link_expr expr cname_to_mname)

and link_block
      (block : Wellformed_system.Block.t)
      (cname_to_mname : (string, string) Hashtbl.t)
  : Wellformed_system.Block.t
  =
  match block with
  | Stmt stmt -> Stmt (link_stmt stmt cname_to_mname)
  | Block (defs, stmts) ->
    let linked_defs = List.map defs ~f:(fun def -> link_def def cname_to_mname) in
    let linked_stmts = List.map stmts ~f:(fun stmt -> link_stmt stmt cname_to_mname) in
    Block (linked_defs, linked_stmts)
;;

let link_method
      ((name, params, defs, stmts, expr) : Wellformed_system.Method.t)
      (cname_to_mname : (string, string) Hashtbl.t)
  : Wellformed_system.Method.t
  =
  let linked_defs = List.map defs ~f:(fun def -> link_def def cname_to_mname) in
  let linked_stmts = List.map stmts ~f:(fun stmt -> link_stmt stmt cname_to_mname) in
  let linked_expr = link_expr expr cname_to_mname in
  name, params, linked_defs, linked_stmts, linked_expr
;;

let link_class
      ((name, fields, methods, classShape) : Wellformed_system.Class.t)
      (cname_to_mname : (string, string) Hashtbl.t)
  : Wellformed_system.Class.t
  =
  let new_class_name = lookup_name cname_to_mname name in
  let linked_methods = List.map methods ~f:(fun m -> link_method m cname_to_mname) in
  new_class_name, fields, linked_methods, classShape
;;

let create_linked_names_from_imports
      (imports : Wellformed_system.Import.t list)
      (all_links : (string, string) Hashtbl.t)
  : (string, string) Hashtbl.t
  =
  let cname_to_mname = Hashtbl.create (module String) in
  List.fold imports ~init:cname_to_mname ~f:(fun acc name ->
    let className = Hashtbl.find_exn all_links (Name.to_string name) in
    let module_class_name = Name.to_string name ^ "." ^ className in
    Hashtbl.set acc ~key:className ~data:module_class_name;
    acc)
;;

let module_to_class
      (mixedModule : Wellformed_system.MixedModule.t)
      (prev_links : (string, string) Hashtbl.t)
  : Wellformed_system.Class.t * (string, string) Hashtbl.t
  =
  match mixedModule with
  | MixedModule (moduleName, mixedImports, c, _) ->
    let imports =
      List.filter_map mixedImports ~f:(fun mi ->
        match mi with
        | Wellformed_system.MixedImport.Import name -> Some name
        | Wellformed_system.MixedImport.TypedImport (name, _) -> Some name)
    in
    let className, _, _, _ = c in
    Hashtbl.set
      prev_links
      ~key:(Name.to_string moduleName)
      ~data:(Name.to_string className);
    let cname_to_mname = create_linked_names_from_imports imports prev_links in
    Hashtbl.set
      cname_to_mname
      ~key:(Name.to_string className)
      ~data:(Name.to_string moduleName ^ "." ^ Name.to_string className);
    link_class c cname_to_mname, prev_links
  | Module (moduleName, imports, c) ->
    let className, _, _, _ = c in
    Hashtbl.set
      prev_links
      ~key:(Name.to_string moduleName)
      ~data:(Name.to_string className);
    let cname_to_mname = create_linked_names_from_imports imports prev_links in
    Hashtbl.set
      cname_to_mname
      ~key:(Name.to_string className)
      ~data:(Name.to_string moduleName ^ "." ^ Name.to_string className);
    link_class c cname_to_mname, prev_links
;;

let link_system
      ((modules, imports, defs, stmts, expr) : Wellformed_system.PostSoundLinkerSystem.t)
  : WellformedTypedProgram.t
  =
  let all_links = Hashtbl.create (module String) in
  let classes, all_module_to_classes =
    List.fold modules ~init:([], all_links) ~f:(fun (classes_acc, cur_links) cur_module ->
      let linked_class, new_link = module_to_class cur_module cur_links in
      linked_class :: classes_acc, new_link)
  in
  let program_links = create_linked_names_from_imports imports all_module_to_classes in
  let linked_defs = List.map defs ~f:(fun def -> link_def def program_links) in
  let linked_stmts = List.map stmts ~f:(fun stmt -> link_stmt stmt program_links) in
  let linked_expr = link_expr expr program_links in
  classes, linked_defs, linked_stmts, linked_expr
;;
