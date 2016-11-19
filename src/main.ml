open OpamParserTypes
open Cmdliner

let upperbound = "3.0.0" (* the whole point of this exercise *)

let find_mirage file =
  let has_upper_bound constraints =
    (* TODO: the actual rules of these constraints are potentially complex;
     * is there a way to evaluate them rather than checking them in text? *)
    (* things which express an upper bound constraint: LT and LEQ which are not negated,
     * GT or GEQ which are negated (but not sure if this is valid in opam) *)
    let rec is_upper_bound = function
    | Logop (_, `And, e, r) -> is_upper_bound e || is_upper_bound r
    | Prefix_relop (_, `Lt, _) | Prefix_relop (_, `Leq, _) -> true
    | _ -> false
    in
    (* this isn't right -- we need to descend into positive nodes *)
    List.exists is_upper_bound constraints
  in
  let add_upper_bound = function
  (* no constraints yet; we're the first one. *)
  | String (pos, name) -> Option (pos, (String (pos, name)), (* quite a bit more structure! *)
    [
      Prefix_relop (pos, `Lt,
        (String (pos, upperbound))
      )
    ]
  )
  | Option (pos, (String (p, name)), (Prefix_relop (a, b, c))::_) ->
    Option (pos, (String (p, name)), [ Logop (p, `And, Prefix_relop (a, b, c),
      Prefix_relop (pos, `Lt,
        (String (pos, upperbound))
      )
    )]
    )
  | other -> other
  in
  let any ~str = function
  | String (_, name) as l when name = str -> add_upper_bound l
  | Option (_, String (_pos, name), ops) as l when name = str && has_upper_bound ops -> l
  | Option (_, String (_pos, name), ops) as l when name = str -> add_upper_bound l
  | other -> other
  in
  let transform_dependencies = function
  | Variable (name, "depends", List (pos, deps)) ->
     Variable (name, "depends", List (pos, List.map (any ~str:"mirage-types-lwt") @@ List.map (any ~str:"mirage-types") deps))
  | other -> other
  in
  let l = (OpamParser.file file).file_contents in
  let new_contents = List.map transform_dependencies l in
  Some { file_name = file; file_contents = new_contents}

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
  | `Ok (Some corrected) -> Format.printf "%s" @@ OpamPrinter.opamfile corrected
  | _ -> exit 1
