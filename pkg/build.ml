#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "bittrex" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.library "lib/bittrex_intf";
    Pkg.lib ~exts:Exts.module_library "lib/bittrex";
    Pkg.lib ~exts:Exts.module_library "lib/bittrex_async";
    Pkg.bin ~auto:true "lib_test/suite"
  ]
