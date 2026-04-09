open! Core

type t =
  | Name of string
  | ErrVar of string

let keywords =
  [ "def"
  ; "if0"
  ; "while0"
  ; "block"
  ; "="
  ; "/"
  ; "+"
  ; "=="
  ; "class"
  ; "method"
  ; "isa"
  ; "new"
  ; "-->"
  ; "module"
  ; "tmodule"
  ; "timport"
  ; "import"
  ]
;;

let is_number (s : string) : bool =
  match float_of_string_opt s with
  | Some _ -> true
  | None -> false
;;

let to_string (name : t) : string =
  match name with
  | Name s -> s
  | _ -> raise (Exceptions.UnexpectedError "Could not access this variable's name")
;;

let of_string (s : string) : t =
  if List.mem keywords s ~equal:String.equal
  then ErrVar "Keyword cannot be a variable name"
  else if is_number s
  then ErrVar "Number cannot be a variable name"
  else Name s
;;

let module_of_string (s : string) : t =
  if String.equal s "Body" then ErrVar "Module cannot be 'Body'" else of_string s
;;

let contain_err_name (name : t) : bool =
  match name with
  | ErrVar _ -> true
  | Name _ -> false
;;

let equal (n1 : t) (n2 : t) : bool =
  let str1 = to_string n1 in
  let str2 = to_string n2 in
  String.equal str1 str2
;;
