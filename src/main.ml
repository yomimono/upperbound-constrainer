open OpamParserTypes
open Cmdliner

let acted = ref false

let find_mirage (file : string) (version : string) (packages : string list) =
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
        (String (pos, version))
      )
    ]
  )
  | Option (pos, (String (p, name)), (Prefix_relop (a, b, c))::_) -> acted := true;
    Option (pos, (String (p, name)), [ Logop (p, `And, Prefix_relop (a, b, c),
      Prefix_relop (pos, `Lt,
        (String (pos, version))
      )
    )]
    )
  | other -> other
  in
  let ensure_bounds ~strs = function
  (* Strings have no constraints on the dependency at all, so the name need only match *)
  | String (_, name) as l when List.mem name strs -> add_upper_bound l
  (* if the name matches, add an upper bound if there isn't one already *)
  | Option (_, String (_pos, name), ops) as l when List.mem name strs && has_upper_bound ops -> l
  | Option (_, String (_pos, name), ops) as l when List.mem name strs -> add_upper_bound l
  (* leave other nodes alone *)
  | other -> other
  in

  let transform_dependencies = function
  | Variable (name, "depends", List (pos, deps)) ->
     Variable (name, "depends", List (pos, List.map (ensure_bounds ~strs:packages) deps))
  (* don't bother acting on anything other than the list of dependencies *)
  | other -> other
  in
  let l = (OpamParser.file file).file_contents in
  { file_name = file; file_contents = (List.map transform_dependencies l)}

let file =
  let doc = "The OPAM file to add upper bounds to." in
  Arg.(value & pos 0 string "opam" & info [] ~doc)

let version_number =
  let doc = "The upper bound constraint to add." in
  Arg.(value & pos 1 string "3.0.0" & info [] ~doc)

let packages =
  let doc = "The package names to add upper bounds for." in
  Arg.(value & pos_right 1 string ["mirage-types"; "mirage-types-lwt"] & info [] ~doc)

let info =
  let doc = "add upper-bounds constraints to dependencies in opam files." in
  let man = [ `S "BUGS"; `P "Please report bugs at https://github.com/yomimono/upperbound-constrainer/issues"; ] in
  Term.info "upperbound-constrainer" ~version:"0.0.1" ~doc ~man

let () =
  let find_t = Term.(const find_mirage $ file $ version_number $ packages) in
  match Term.eval (find_t, info) with
  | `Ok file -> begin
    match !acted with
    | true ->
      (* overwrite previous contents *)
      Format.printf "writing to %s: %s\n" file.file_name @@ OpamPrinter.opamfile file;
      let fd = Unix.(openfile file.file_name [O_WRONLY; O_TRUNC] 0o755) in
      let ch = Unix.out_channel_of_descr fd in
      set_binary_mode_out ch false;
      let fmt = Format.formatter_of_out_channel ch in
      Format.(fprintf fmt "%s\n" @@ OpamPrinter.opamfile file);
      flush ch;
      Unix.close fd
    | false -> Format.eprintf "no action taken"; exit 1
  end
  | _ -> exit 1
