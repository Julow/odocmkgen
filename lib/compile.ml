(* Rules for compiling cm{t,ti,i} files into odoc files *)
let compile_fragment (info, deps) =
  (* Get the filename of the output odoc file *)
  let odoc_path = Inputs.compile_target info in
  (* Get a list of odoc files for the dependencies *)
  let dep_odocs = List.map Inputs.compile_target deps in
  (* Odoc requires the directories in which to find the odoc files of the dependencies *)
  let include_args =
    List.map Fpath.parent dep_odocs
    |> List.sort_uniq Fpath.compare
    |> List.concat_map (fun dir -> [ "-I"; Fpath.to_string dir ])
  in
  let open Makefile in
  concat
    [
      rule [ odoc_path ]
        ~fdeps:(Inputs.input_file info :: dep_odocs)
        [
          cmd "odoc" $ "compile" $ "--package" $ info.package $ "$<"
          $$ include_args $ "-o" $ "$@";
        ];
      phony_rule (Inputs.compile_rule info) ~fdeps:[ odoc_path ] [];
    ]

let gen inputs =
  let package_rules_s =
    List.map (fun (info, _) -> Inputs.compile_rule info) inputs
    |> List.sort_uniq String.compare
  in
  let open Makefile in
  concat
    [
      concat (List.map compile_fragment inputs);
      phony_rule "compile" ~deps:package_rules_s [];
    ]
