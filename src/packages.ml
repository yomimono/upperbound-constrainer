open Cmdliner

module M = Map.Make(OpamPackage.Name)

let versions =
  let n = OpamPackage.Name.of_string in
  let map = M.empty in
  let map = M.add (n "mirage-vnetif") "0.3.0" map in
  let map = M.add (n "mirage-types-lwt") "3.0.0" map in
  let map = M.add (n "mirage-runtime") "3.0.0" map in
  let map = M.add (n "mirage-types") "3.0.0" map in
  let map = M.add (n "mirage") "3.0.0" map in
  let map = M.add (n "tcpip") "3.0.0" map in
  let map = M.add (n "functoria-runtime") "2.0.0" map in
  let map = M.add (n "functoria") "2.0.0" map in
  let map = M.add (n "mirage-unix") "3.0.0" map in
  let map = M.add (n "mirage-xen") "3.0.0" map in
  let map = M.add (n "mirage-xen-ocaml") "3.0.0" map in
  let map = M.add (n "mirage-block-lwt") "1.0.0" map in
  let map = M.add (n "mirage-block") "1.0.0" map in
  let map = M.add (n "mirage-block-ramdisk") "0.3" map in
  let map = M.add (n "mirage-block-unix") "2.5.0" map in
  let map = M.add (n "mirage-block-xen") "1.5.0" map in
  let map = M.add (n "mirage-bootvar-xen") "0.4.0" map in
  let map = M.add (n "mirage-channel") "3.0.0" map in
  let map = M.add (n "mirage-channel-lwt") "3.0.0" map in
  let map = M.add (n "mirage-clock") "1.2.0" map in
  let map = M.add (n "mirage-clock-freestanding") "1.2.0" map in
  let map = M.add (n "mirage-clock-lwt") "1.2.0" map in
  let map = M.add (n "mirage-clock-unix") "1.2.0" map in
  let map = M.add (n "mirage-console") "2.2.0" map in
  let map = M.add (n "mirage-console-lwt") "2.2.0" map in
  let map = M.add (n "mirage-console-unix") "2.2.0" map in
  let map = M.add (n "mirage-console-xen-backend") "2.2.0" map in
  let map = M.add (n "mirage-console-xen-cli") "2.2.0" map in
  let map = M.add (n "mirage-console-xen") "2.2.0" map in
  let map = M.add (n "mirage-console-xen-proto") "2.2.0" map in
  let map = M.add (n "mirage-device") "1.0.0" map in
  let map = M.add (n "mirage-entropy") "0.4.0" map in
  let map = M.add (n "mirage-flow-lwt") "1.2.0" map in
  let map = M.add (n "mirage-flow-unix") "1.2.0" map in
  let map = M.add (n "mirage-flow") "1.2.0" map in
  let map = M.add (n "mirage-fs") "1.0.0" map in
  let map = M.add (n "mirage-fs-lwt") "1.0.0" map in
  let map = M.add (n "mirage-fs-unix") "1.3.0" map in
  let map = M.add (n "mirage-http") "3.0.0" map in
  let map = M.add (n "mirage-kv") "1.0.0" map in
  let map = M.add (n "mirage-kv-lwt") "1.0.0" map in
  let map = M.add (n "mirage-logs") "0.3" map in
  let map = M.add (n "mirage-net") "1.0.0" map in
  let map = M.add (n "mirage-net-lwt") "1.0.0" map in
  let map = M.add (n "mirage-net-macosx") "1.3.0" map in
  let map = M.add (n "mirage-net-unix") "2.3.0" map in
  let map = M.add (n "mirage-net-xen") "1.7.0" map in
  let map = M.add (n "mirage-protocols") "1.0.0" map in
  let map = M.add (n "mirage-protocols-lwt") "1.0.0" map in
  let map = M.add (n "mirage-random") "1.0.0" map in
  let map = M.add (n "mirage-stack-lwt") "1.0.0" map in
  let map = M.add (n "mirage-stack") "1.0.0" map in
  let map = M.add (n "mirage-time") "1.0.0" map in
  let map = M.add (n "mirage-time-lwt") "1.0.0" map in
  let map = M.add (n "conduit") "0.15.0" map in
  let map = M.add (n "mirage-conduit") "2.3.0" map in
  let map = M.add (n "crunch") "2.0.0" map in
  let map = M.add (n "dns") "0.19.0" map in
  let map = M.add (n "mirage-dns") "2.6.0" map in
  let map = M.add (n "fat-filesystem") "0.12.0" map in
  let map = M.add (n "tar-format") "0.7.0" map in
  let map = M.add (n "vchan") "2.3.0" map in
  let map = M.add (n "charrua-client") "0.1.0" map in
  map

let transform_opam opam =
  let lower = true in
  M.fold (fun package version opam ->
          let depends = Main.ensure_depends_bounds [package] version lower opam.OpamFile.OPAM.depends in
          let conflicts = Main.ensure_conflicts_bounds [package] version lower opam.OpamFile.OPAM.conflicts in
          opam
          |> OpamFile.OPAM.with_conflicts conflicts
          |> OpamFile.OPAM.with_depends depends
  ) versions opam


let update file =
  let opam_file = OpamFile.make (OpamFilename.of_string file) in
  let x = OpamFile.OPAM.read opam_file in
  let y = transform_opam x in
  if x = y then ()
  else OpamFile.OPAM.write_with_preserved_format opam_file y

let file =
  let doc = "The OPAM file to lowerbound-constrain package versions in." in
  Arg.(value & pos 0 string "opam" & info [] ~docv:"FILE" ~doc)

let info = Term.info "mirageos3-universe-maker"

let () =
  let find_t = Term.(const update $ file) in
  match Term.eval (find_t, info) with
  | `Ok () -> ()
  | _ -> exit 1

