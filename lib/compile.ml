open Listm
open Util

(* Rules for compiling cm{t,ti,i} files into odoc files *)
let compile_fragment ~inputs_by_digest info =
  (* Get the filename of the output odoc file *)
  let odoc_path = Inputs.compile_target info in

  (* Find by digest the [source_info] for each dependency in our source_info record *)
  let deps =
    info.deps >>= fun dep ->
    try [ StringMap.find dep.Odoc.c_digest inputs_by_digest ]
    with Not_found ->
      Format.eprintf "Warning, couldn't find dep %s of file %a\n"
        dep.Odoc.c_unit_name Fpath.pp info.inppath;
      []
  in

  (* Get a list of odoc files for the dependencies *)
  let dep_odocs = List.map Inputs.compile_target deps in

  (* Odoc requires the directories in which to find the odoc files of the dependencies *)
  let include_args =
    List.map Fpath.parent dep_odocs
    |> List.sort_uniq Fpath.compare
    |> List.concat_map (fun dir -> [ "-I"; Fpath.to_string dir ])
  in

  let parent_args =
    match info.parent with Some parent -> [ "--parent"; parent ] | None -> []
  in
  let child_args = List.concat_map (fun c -> [ "-c"; c ]) info.childs in

  let open Makefile in
  concat
    [
      rule odoc_path
        ~fdeps:(Inputs.input_file info :: dep_odocs)
        [
          cmd "odoc" $ "compile" $$ parent_args $$ child_args $ "$<"
          $$ include_args $ "-o" $ "$@";
        ];
      phony_rule ("compile-" ^ info.package) ~fdeps:[ odoc_path ] [];
    ]

let split_digest inputs =
  List.fold_left
    (fun acc inp -> StringMap.add inp.Inputs.digest inp acc)
    StringMap.empty inputs

let gen packages =
  let packages = StringMap.bindings packages in
  let inputs =
    List.fold_left (fun acc (_, inputs) -> inputs @ acc) [] packages
  in
  let inputs_by_digest = split_digest inputs in
  let package_rules_s = List.map (fun (pkg, _) -> "compile-" ^ pkg) packages in
  let open Makefile in
  concat
    [
      concat (List.map (compile_fragment ~inputs_by_digest) inputs);
      phony_rule "compile" ~deps:package_rules_s [];
    ]
