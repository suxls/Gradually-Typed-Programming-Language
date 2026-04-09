open! Core

(* ------------------------------------------------------------------ *)
(* Test helpers                                                        *)
(* ------------------------------------------------------------------ *)

let parse_system input =
  let sexp = Sexp.of_string input in
  System.parse sexp
;;

let run_full_pipeline input =
  let system = parse_system input in
  match System.ensure_no_system_errors system with
  | Error _ -> `Parser_error
  | Ok system ->
    (match System.validate_system_duplicates system with
     | Error e -> `Duplicate_error (System.get_error_message e)
     | Ok system ->
       let with_imports = System.validate_bad_import_errors system in
       (match System.ensure_no_system_errors with_imports with
        | Error _ -> `Bad_import
        | Ok system ->
          let with_vars = System.validate_undeclared_vars system in
          (match System.ensure_no_system_errors with_vars with
           | Error _ -> `Undeclared_var
           | Ok system ->
             let typechecked = Typecheck.typecheck_mixed_system system in
             (match System.ensure_no_system_errors typechecked with
              | Error _ -> `Type_error
              | Ok system ->
                (match
                   Wellformed_system.wellformed_typed_system_from_mixed_system system
                 with
                 | Error _ -> `Wellformed_error
                 | Ok wf_system ->
                   let post_sound = Wellformed_system.sound_link_system wf_system in
                   let program = Linker.link_system post_sound in
                   let result = Cesk.run program in
                   (match result with
                    | Ok (Cesk.Value.Number n) -> `Number n
                    | Ok (Cesk.Value.Object _) -> `Object
                    | Ok (Cesk.Value.Proxy _) -> `Proxy
                    | Error _ -> `Runtime_error))))))
;;

(* ------------------------------------------------------------------ *)
(* Parsing tests                                                       *)
(* ------------------------------------------------------------------ *)

let test_parse_simple_number () =
  let result = run_full_pipeline "(1.0)" in
  Alcotest.(check bool) "simple number" true
    (match result with `Number n -> Float.( <= ) (Float.abs (n -. 1.0)) 0.001 | _ -> false)
;;

let test_parse_addition () =
  let result = run_full_pipeline "((def x 2.0) (def y 3.0) (x + y))" in
  Alcotest.(check bool) "addition" true
    (match result with `Number n -> Float.( <= ) (Float.abs (n -. 5.0)) 0.001 | _ -> false)
;;

let test_parse_division () =
  let result = run_full_pipeline "((def x 10.0) (def y 2.0) (x / y))" in
  Alcotest.(check bool) "division" true
    (match result with `Number n -> Float.( <= ) (Float.abs (n -. 5.0)) 0.001 | _ -> false)
;;

let test_division_by_zero () =
  let result = run_full_pipeline "((def x 10.0) (def y 0.0) (x / y))" in
  Alcotest.(check bool) "division by zero" true
    (match result with `Runtime_error -> true | _ -> false)
;;

let test_parse_equality_true () =
  let result = run_full_pipeline "((def x 5.0) (def y 5.0) (x == y))" in
  Alcotest.(check bool) "equality true" true
    (match result with `Number n -> Float.( <= ) (Float.abs n) 0.001 | _ -> false)
;;

let test_parse_equality_false () =
  let result = run_full_pipeline "((def x 5.0) (def y 6.0) (x == y))" in
  Alcotest.(check bool) "equality false" true
    (match result with `Number n -> Float.( <= ) (Float.abs (n -. 1.0)) 0.001 | _ -> false)
;;

(* ------------------------------------------------------------------ *)
(* Validation tests                                                    *)
(* ------------------------------------------------------------------ *)

let test_undeclared_variable () =
  let result = run_full_pipeline "(x)" in
  Alcotest.(check bool) "undeclared variable" true
    (match result with `Undeclared_var -> true | _ -> false)
;;

let test_duplicate_module_name () =
  let input =
    {|((tmodule Foo
          (class foo (x)
                 (method woo (nOne nTwo) (nOne + nTwo)))
          (((x Number)) ((woo (Number Number) Number))))
       (module Foo (import Foo) (class foo (x y)))
       4.0)|}
  in
  let result = run_full_pipeline input in
  Alcotest.(check bool) "duplicate module name" true
    (match result with `Duplicate_error _ -> true | _ -> false)
;;

(* ------------------------------------------------------------------ *)
(* Module system tests                                                 *)
(* ------------------------------------------------------------------ *)

let test_simple_module () =
  let input =
    {|((module Mone (class cone () (method m (x) x)))
       (tmodule Mtwo
        (timport Mone (() ((m (Number) Number))))
        (class ctwo () (method m (x) x))
        (() ((m ((() ((m (Number) Number)))) (() ((m (Number) Number)))))))
       (timport Mone (() ((m (Number) Number))))
       (import Mtwo)
       (def mone (new cone ()))
       (def mtwo (new ctwo ()))
       (def monetoo (mtwo --> m (mone)))
       (def two 2.0)
       (monetoo --> m (two)))|}
  in
  let result = run_full_pipeline input in
  Alcotest.(check bool) "cross-module method call" true
    (match result with `Number n -> Float.( <= ) (Float.abs (n -. 2.0)) 0.001 | _ -> false)
;;

let test_typed_module_cross_module () =
  let input =
    {|((tmodule M (class D () (method m () 666.0)) (() ((m () Number))))
       (tmodule N
        (import M)
        (class C () (method id () 1.0) (method m () (new D ())))
        (() ((id () Number) (m () (() ((m () Number)))))))
       (import M)
       (import N)
       (def c (new C ()))
       (def d (c --> m ()))
       (def e (d --> m ()))
       (def f (c --> id ()))
       (e + f))|}
  in
  let result = run_full_pipeline input in
  Alcotest.(check bool) "typed module chain" true
    (match result with `Number n -> Float.( <= ) (Float.abs (n -. 667.0)) 0.001 | _ -> false)
;;

(* ------------------------------------------------------------------ *)
(* Control flow tests                                                  *)
(* ------------------------------------------------------------------ *)

let test_if0_true_branch () =
  let result = run_full_pipeline "((def x 0.0) (def y 1.0) (def r 0.0) (if0 x (r = y) (r = x)) r)" in
  Alcotest.(check bool) "if0 true branch" true
    (match result with `Number n -> Float.( <= ) (Float.abs (n -. 1.0)) 0.001 | _ -> false)
;;

let test_if0_false_branch () =
  let result = run_full_pipeline "((def x 1.0) (def y 99.0) (def r 0.0) (if0 x (r = y) (r = x)) r)" in
  Alcotest.(check bool) "if0 false branch" true
    (match result with `Number n -> Float.( <= ) (Float.abs (n -. 1.0)) 0.001 | _ -> false)
;;

(* ------------------------------------------------------------------ *)
(* Object tests                                                        *)
(* ------------------------------------------------------------------ *)

let test_object_field_access () =
  let input =
    {|((tmodule M
        (class Point (x y)
               (method getX () (this --> x))
               (method getY () (this --> y)))
        (((x Number) (y Number)) ((getX () Number) (getY () Number))))
       (timport M (((x Number) (y Number)) ((getX () Number) (getY () Number))))
       (def a 3.0)
       (def b 4.0)
       (def p (new Point (a b)))
       (def px (p --> getX ()))
       (def py (p --> getY ()))
       (px + py))|}
  in
  let result = run_full_pipeline input in
  Alcotest.(check bool) "object field access" true
    (match result with `Number n -> Float.( <= ) (Float.abs (n -. 7.0)) 0.001 | _ -> false)
;;

let test_parser_error () =
  let input = "invalid" in
  let result = run_full_pipeline input in
  Alcotest.(check bool) "parser error" true
    (match result with `Parser_error -> true | _ -> false)
;;

let test_bad_import () =
  let input =
    {|((module Cowboy (class Cowboy () (method draw () 1.0)))
       (module Artist (class Artist () (method draw () 666.0)))
       (import Cowboy)
       (import Artist)
       (def a (new Artist ()))
       (def c (new Cowboy ()))
       (def x 0.0)
       (if0 1.0 (x = a) (x = c))
       (x = (x --> draw ()))
       x)|}
  in
  let result = run_full_pipeline input in
  Alcotest.(check bool) "bad import (untyped modules)" true
    (match result with `Bad_import -> true | _ -> false)
;;

(* ------------------------------------------------------------------ *)
(* Test runner                                                         *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "xtr"
    [ ( "parsing"
      , [ Alcotest.test_case "simple number" `Quick test_parse_simple_number
        ; Alcotest.test_case "addition" `Quick test_parse_addition
        ; Alcotest.test_case "division" `Quick test_parse_division
        ; Alcotest.test_case "division by zero" `Quick test_division_by_zero
        ; Alcotest.test_case "equality true" `Quick test_parse_equality_true
        ; Alcotest.test_case "equality false" `Quick test_parse_equality_false
        ; Alcotest.test_case "parser error" `Quick test_parser_error
        ] )
    ; ( "validation"
      , [ Alcotest.test_case "undeclared variable" `Quick test_undeclared_variable
        ; Alcotest.test_case "duplicate module name" `Quick test_duplicate_module_name
        ; Alcotest.test_case "bad import" `Quick test_bad_import
        ] )
    ; ( "control flow"
      , [ Alcotest.test_case "if0 true branch" `Quick test_if0_true_branch
        ; Alcotest.test_case "if0 false branch" `Quick test_if0_false_branch
        ] )
    ; ( "modules"
      , [ Alcotest.test_case "cross-module method call" `Quick test_simple_module
        ; Alcotest.test_case "typed module chain" `Quick test_typed_module_cross_module
        ] )
    ; ( "objects"
      , [ Alcotest.test_case "object field access" `Quick test_object_field_access ] )
    ]
;;
