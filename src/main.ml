open OpamParserTypes
open Cmdliner

let acted = ref false

let find_mirage lower file version packages =
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
  let has_lower_bound constraints =
    let rec is_lower_bound = function
    | Logop (_, `And, e, r) -> is_lower_bound e || is_lower_bound r
    | Prefix_relop (_, `Gt, _) | Prefix_relop (_, `Geq, _) | Prefix_relop (_, `Eq, _) -> true
    | _ -> false
    in
    List.exists is_lower_bound constraints
  in
  let has_bound ~lower x = match lower with
  | false -> has_upper_bound x
  | true -> has_lower_bound x
  in
  let add_bound (op : relop) = function
  (* no constraints yet; we're the first one. *)
  | String (pos, name) ->
    acted := true;
    Option (pos, (String (pos, name)),
      [ Prefix_relop (pos, op, (String (pos, version))) ]
  )
  | Option (pos, (String (p, name)), (Prefix_relop (a, b, c))::_) -> acted := true;
    Option (pos, (String (p, name)), [ Logop (p, `And, Prefix_relop (a, b, c),
      Prefix_relop (pos, op, (String (pos, version)))
    )]
    )
  | other -> other
  in
  let relop_of_bound = function
  | false -> `Lt
  | true -> `Geq
  in
  let ensure_bounds ~strs = function
  (* Strings have no constraints on the dependency at all, so the name need only match *)
  | String (_, name) as l when List.mem name strs -> add_bound (relop_of_bound lower) l
  (* if the name matches, add an upper bound if there isn't one already *)
  | Option (_, String (_pos, name), ops) as l when List.mem name strs && has_bound ~lower ops -> l
  | Option (_, String (_pos, name), ops) as l when List.mem name strs -> add_bound (relop_of_bound lower) l
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
  let doc = "The OPAM file to add dependency version constraints to." in
  Arg.(value & pos 0 string "opam" & info [] ~docv:"FILE" ~doc)

let lower =
  let doc = "Impose lower-bound (>=) constraints instead of the default upper-bound (<)." in
  Arg.(value & flag & info ["l"; "lower"] ~doc)

let version_number =
  let doc = "The version constraint to add." in
  Arg.(value & pos 1 string "3.0.0" & info [] ~docv:"VERSION" ~doc)

let packages =
  let doc = "The package names to add constraints for when encountered in the dependencies list." in
  Arg.(value & pos_right 1 string ["mirage-types"; "mirage-types-lwt"] & info [] ~docv:"PACKAGES" ~doc)

let info =
  let doc = "add upper-bounds constraints to dependencies in opam files." in
  let man = [ `S "BUGS"; `P "Please report bugs at https://github.com/yomimono/upperbound-constrainer/issues"; ] in
  Term.info "upperbound-constrainer" ~version:"0.0.1" ~doc ~man

let () =
  let find_t = Term.(const find_mirage $ lower $ file $ version_number $ packages) in
  match Term.eval (find_t, info) with
  | `Ok file -> begin
    match !acted with
    | true ->
      (* overwrite previous contents *)
      let fd = Unix.(openfile file.file_name [O_WRONLY; O_TRUNC] 0o755) in
      let ch = Unix.out_channel_of_descr fd in
      set_binary_mode_out ch false;
      let fmt = Format.formatter_of_out_channel ch in
      Format.(fprintf fmt "%s\n" @@ OpamPrinter.opamfile file);
      flush ch;
      Unix.close fd
    | false -> exit 1
  end
  | _ -> exit 1
