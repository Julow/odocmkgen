let prelude =
  let open Makefile in
  concat
    [
      phony_rule "default" ~deps:[ "link" ] [];
      phony_rule "compile" ~oo_deps:[ "odocs" ] [];
      phony_rule "link" ~deps:[ "compile" ] ~oo_deps:[ "odocls" ] [];
      phony_rule "clean" [ cmd "rm" $ "-r" $ "odocs" $ "odocls" ];
      rule [ Fpath.v "odocs" ] [ cmd "mkdir" $ "odocs" ];
      rule [ Fpath.v "odocls" ] [ cmd "mkdir" $ "odocls" ];
    ]

let run dir dep_file =
  let inputs = Inputs.find_inputs dir in
  let tree = Inputs.make_tree inputs in
  let compile_deps =
    match dep_file with
    | Some file -> Compute_compile_deps.from_deps_file ~file inputs
    | None -> Compute_compile_deps.from_odoc inputs
  in
  let parent_childs = Inputs.find_parent_childs tree in
  let makefile =
    let open Makefile in
    concat
      [
        prelude;
        Compile.gen ~parent_childs ~compile_deps tree;
        Link.gen ~parent_childs ~compile_deps tree;
      ]
  in
  Format.printf "%a\n" Makefile.pp makefile
