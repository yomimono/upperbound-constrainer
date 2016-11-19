open OpamParserTypes
open Cmdliner

let upperbound = "3.0.0" (* the whole point of this exercise *)

let acted = ref false

let find_mirage file =
  let has_upper_bound constraints =
    (* TODO: the actual rules of these constraints are potentially complex;
     * is there a way to evaluate them rather than checking them in text? *)
    let rec is_upper_bound = function
    | Logop (_, `And, e, r) -> is_upper_bound e || is_upper_bound r
    | Prefix_relop (_, `Lt, _) | Prefix_relop (_, `Leq, _) | Prefix_relop (_, `Eq, _) -> true
    | _ -> false
    in
    List.exists is_upper_bound constraints
  in
  let add_upper_bound = function
  (* no constraints yet; we're the first one. *)
  | String (pos, name) -> acted := true; Option (pos, (String (pos, name)), (* quite a bit more structure! *)
    [
      Prefix_relop (pos, `Lt,
        (String (pos, upperbound))
      )
    ]
  )
  | Option (pos, (String (p, name)), (Prefix_relop (a, b, c))::_) -> acted := true;
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
  { file_name = file; file_contents = (List.map transform_dependencies l)}

let file =
  let doc = "The OPAM file to add upper bounds to." in
  Arg.(value & pos 0 string "opam" & info [] ~doc)

let info =
  let doc = "add constraints to mirage-types and mirage-types-lwt dependencies in opam files." in
  let man = [ `S "BUGS"; `P "Please report bugs at https://github.com/yomimono/upperbound-constrainer/issues"; ] in
  Term.info "upperbound-constrainer" ~version:"0.0.1" ~doc ~man

let () =
  let find_t = Term.(const find_mirage $ file) in
  match Term.eval (find_t, info) with
  | `Ok file -> begin
    match !acted with
    | true -> Format.printf "%s\n" @@ OpamPrinter.opamfile file
    | false -> exit 1
  end
  | _ -> exit 1
