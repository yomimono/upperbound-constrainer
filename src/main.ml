open OpamTypes
open Cmdliner

let acted = ref false

let depends_has_upper_bound constraints =
  (* TODO: the actual rules of these constraints are potentially complex;
   * is there a way to evaluate them rather than checking them in text? *)
  let rec aux = function
  | Empty -> false
  | Atom (Constraint ((`Lt|`Leq|`Eq), _))  -> true
  | Block b -> aux b
  | And (x, y) | Or (x, y) -> aux x || aux y
  | _ -> false
  in
  aux constraints

let depends_has_lower_bound constraints =
  let rec aux = function
  | Empty -> false
  | Atom (Constraint ((`Gt|`Geq|`Eq), _)) -> true
  | Block b -> aux b
  | And (x, y) | Or (x, y) -> aux x || aux y
  | _ -> false
  in
  aux constraints

let remove_depends_bounds constraints =
  let rec aux = function
  | Empty | Atom (Constraint _) -> Empty
  | Block b -> (match aux b with Empty -> Empty | b -> Block b)
  | And (x, y) -> OpamFormula.ands [aux x; aux y]
  | Or (x, y)  -> OpamFormula.ors  [aux x; aux y]
  | Atom (Filter _) as a -> a
  in
  aux constraints

let add_depends_bound name version op constraints =
  let constraints = remove_depends_bounds constraints in
  let atom = Atom (Constraint (op, FString version)) in
  Atom (name, OpamFormula.ands [constraints; atom])

let depends_has_bound ~lower x = match lower with
  | false -> depends_has_upper_bound x
  | true  -> depends_has_lower_bound x

let relop_of_bound = function
  | false -> `Lt
  | true  -> `Geq

let rec ensure_depends_bounds packages version lower = function
  (* Strings have no constraints on the dependency at all, so the name
     need only match *)
  | Atom (name, Empty) when List.mem name packages ->
      add_depends_bound name version (relop_of_bound lower) Empty
  
  (* if the name matches, add version constraint if there isn't one already *)
  | Atom (name, ops) as l when List.mem name packages
                            && depends_has_bound ~lower ops ->
      l
  | Atom (name, constraints) when List.mem name packages ->
      add_depends_bound name version (relop_of_bound lower) constraints
  
  (* leave other nodes alone *)
  | Atom _
  | Empty as f -> f
  | Block x    -> Block (ensure_depends_bounds packages version lower x)
  | And (x, y) -> And (ensure_depends_bounds packages version lower x,
                       ensure_depends_bounds packages version lower y)
  | Or (x, y)  -> Or (ensure_depends_bounds packages version lower x,
                       ensure_depends_bounds packages version lower y)

let ensure_conflicts_bounds packages version lower (f:formula): formula =
  let relop = relop_of_bound (not lower) in
  let version = Atom (relop, OpamPackage.Version.of_string version) in
  match f with
  | Atom (name, _) when List.mem name packages -> Atom (name, version)
  | a -> a

let find_mirage lower file version packages =
  let packages = List.map OpamPackage.Name.of_string packages in
  let transform_opam opam =
    let depends   = ensure_depends_bounds packages version lower opam.OpamFile.OPAM.depends in
    let conflicts = ensure_conflicts_bounds packages version lower opam.OpamFile.OPAM.conflicts in
    opam
    |> OpamFile.OPAM.with_conflicts conflicts
    |> OpamFile.OPAM.with_depends depends
  in
  let f = OpamFile.make (OpamFilename.of_string file) in
  let x = OpamFile.OPAM.read f in
  let y = transform_opam x in
  if x = y then ()
  else OpamFile.OPAM.write_with_preserved_format f y

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
  | `Ok () -> ()
  | _ -> exit 1
