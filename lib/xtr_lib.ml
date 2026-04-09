open! Core

let format_float f =
  let s = Float.to_string f in
  if String.is_suffix ~suffix:"." s then s ^ "0" else s
;;

(* ------------------------------------------------------------------ *)
(* PIPELINE STAGES                                                     *)
(* ------------------------------------------------------------------ *)

let read_and_parse_input () =
  let input = In_channel.input_all In_channel.stdin in
  let sexp = Core.Sexp.of_string input in
  System.parse sexp
;;

let check_parser_errors potential_illformed_system =
  match System.ensure_no_system_errors potential_illformed_system with
  | Ok system -> system
  | Error _ ->
    print_string "\"parser error\"";
    exit 1
;;

let check_duplicate_error_ast (wellformed_structure_system : System.MixedSystem.t) =
  match System.validate_system_duplicates wellformed_structure_system with
  | Ok system -> system
  | Error e ->
    print_endline (System.get_error_message e);
    exit 1
;;

let check_valid_imports_error_system (wellformed_structure_system : System.MixedSystem.t) =
  let potential_invalid_imports =
    System.validate_bad_import_errors wellformed_structure_system
  in
  match System.ensure_no_system_errors potential_invalid_imports with
  | Ok system -> system
  | Error _ ->
    print_endline "\"bad import\"";
    exit 1
;;

let validate_and_check_errors (wellformed_structure_system : System.MixedSystem.t)
  : System.MixedSystem.t
  =
  let potential_invalid_system =
    System.validate_undeclared_vars wellformed_structure_system
  in
  match System.ensure_no_system_errors potential_invalid_system with
  | Ok system -> system
  | Error _ ->
    print_endline "\"undeclared variable error\"";
    exit 1
;;

let typecheck_and_check_errors (wellformed_structure_ast : System.MixedSystem.t)
  : System.MixedSystem.t
  =
  let potential_invalid_ast = Typecheck.typecheck_mixed_system wellformed_structure_ast in
  match System.ensure_no_system_errors potential_invalid_ast with
  | Ok system -> system
  | Error _ ->
    print_endline "\"type error\"";
    exit 1
;;

let convert_to_wellformed_mixed_system (validated_system : System.MixedSystem.t)
  : Wellformed_system.WellformedMixedSystem.t
  =
  match Wellformed_system.wellformed_typed_system_from_mixed_system validated_system with
  | Ok system -> system
  | Error _ ->
    print_endline "\"unexpected internal error during wellformed conversion\"";
    exit 1
;;

let sound_linker (wellformed_system : Wellformed_system.WellformedMixedSystem.t)
  : Wellformed_system.PostSoundLinkerSystem.t
  =
  Wellformed_system.sound_link_system wellformed_system
;;

let system_to_program_linker
      (sound_linked_system : Wellformed_system.PostSoundLinkerSystem.t)
  : Linker.WellformedTypedProgram.t
  =
  Linker.link_system sound_linked_system
;;

let execute_and_format_result wellformed_ast =
  let result_of_program = Cesk.run wellformed_ast in
  match result_of_program with
  | Ok (Cesk.Value.Number n) -> print_string (format_float n)
  | Ok (Cesk.Value.Object _) -> print_string "\"object\""
  | Ok (Cesk.Value.Proxy _) -> print_string "\"proxy\""
  | Error _ -> print_string "\"run-time error\""
;;

(* ------------------------------------------------------------------ *)
(* MAIN                                                                *)
(* ------------------------------------------------------------------ *)

let main () =
  let potential_illformed_system = read_and_parse_input () in
  let wellformed_structure_system = check_parser_errors potential_illformed_system in
  let no_duplicate_error_system = check_duplicate_error_ast wellformed_structure_system in
  let validated_imports_system =
    check_valid_imports_error_system no_duplicate_error_system
  in
  let validated_undeclared_vars_system =
    validate_and_check_errors validated_imports_system
  in
  let typechecked_system = typecheck_and_check_errors validated_undeclared_vars_system in
  let wellformed_validated_system =
    convert_to_wellformed_mixed_system typechecked_system
  in
  let wellformed_post_sound_linker_system = sound_linker wellformed_validated_system in
  let wellformed_typed_program =
    system_to_program_linker wellformed_post_sound_linker_system
  in
  execute_and_format_result wellformed_typed_program
;;
