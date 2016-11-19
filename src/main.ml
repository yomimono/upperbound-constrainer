open OpamParserTypes

let find_mirage contents =
  let any ~str = function
  | String (_, name) when name = str -> true
  | Option (_, String (_pos, name), _ops) when name = str -> true
  | _ -> false
  in
  let relevant = function
  | Section _ -> false
  | Variable (_, "depends", List (_, deps)) -> List.exists (any ~str:"mirage-types") deps || List.exists (any ~str:"mirage-types-lwt") deps
  | Variable _ -> false
  in
  List.exists relevant contents

let () =
  (* opamfile is a record, containing file_contents (which we're interested in) and file_name (which we're not) *)
  Format.printf "%s" @@ string_of_bool @@ find_mirage @@ (OpamParser.file "opam").file_contents
