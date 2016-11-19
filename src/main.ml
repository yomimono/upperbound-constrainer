open OpamParserTypes
open Cmdliner

let find_mirage file =
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
  let e = List.exists relevant @@ (OpamParser.file file).file_contents in
  e

let file =
  let doc = "The OPAM file to check for Mirage dependencies." in
  Arg.(value & pos 0 string "opam" & info [] ~doc)

let info =
  let doc = "check a file for MirageOS API dependencies" in
  let man = [ `S "BUGS"; `P "Please report bugs at https://github.com/yomimono/upperbound-constrainer/issues"; ] in
  Term.info "upperbound-constrainer" ~version:"0.0.1" ~doc ~man

let () =
  (* opamfile is a record, containing file_contents (which we're interested in) and file_name (which we're not) *)
  let find_t = Term.(const find_mirage $ file) in
  match Term.eval (find_t, info) with
  | `Ok true -> exit 0
  | `Ok false -> exit 1
  | _ -> exit 127
