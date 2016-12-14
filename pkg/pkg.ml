#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe ~metas:[] "upperbound-constrainer" @@ fun c ->
  Ok [
       Pkg.bin "src/main" ~dst:"constrain"
  ]
