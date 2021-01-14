open Util

let split_packages inputs =
  let f inp = function Some lst -> Some (inp :: lst) | None -> Some [ inp ] in
  List.fold_left
    (fun acc inp -> StringMap.update inp.Inputs.package (f inp) acc)
    StringMap.empty inputs

let generate_package_page target pkg childs =
  let open Makefile in
  rule target
    [
      cmd "mkdir" $ "-p" $ "$(@D)";
      cmd ~stdout:"$@" "odocmkgen" $ "package-index" $ pkg $$ childs;
    ]

(** Hack, used to make sure the index page is compiled first *)
let add_deps dep inp =
  let dep = { Odoc.c_unit_name = dep; c_digest = dep } in 
  { inp with Inputs.deps = dep :: inp.Inputs.deps }

let add_package_page pkg inputs =
  let index_name = "page-" ^ pkg in
  let is_pkg_page inp = inp.Inputs.name = index_name in
  match List_util.find_remove is_pkg_page inputs with
  | Some (inputs, p) ->
      (* Found an index page, it's removed from [inputs]. *)
      let childs = List.map (fun inp -> inp.Inputs.name) inputs in
      ( { p with Inputs.parent = None; childs; digest = p.name }
        :: List.map (add_deps p.name) inputs,
        Makefile.empty )
  | None ->
      (* Generate an index page *)
      let childs = List.map (fun inp -> inp.Inputs.name) inputs in
      let target = Fpath.(v "odocs" / pkg / (pkg ^ ".mld")) in
      let reloutpath = Fpath.v ("page-" ^ pkg ^ ".odoc") in
      let inp =
        {
          Inputs.name = index_name;
          inppath = target;
          root = Fpath.v ".";
          reloutpath;
          digest = index_name;
          (* hack, the parent page must be compiled first *)
          deps = [];
          parent = None;
          package = pkg;
          childs;
        }
      in
      ( inp :: List.map (add_deps index_name) inputs,
        generate_package_page target pkg childs )

(** Add index pages to packages. Return the new packages and the rules to
    generate the missing pages. *)
let add_package_pages packages =
  StringMap.fold
    (fun pkg inputs (pkg_acc, mk_acc) ->
      let inputs, mk = add_package_page pkg inputs in
      (StringMap.add pkg inputs pkg_acc, Makefile.concat [ mk_acc; mk ]))
    packages
    (StringMap.empty, Makefile.empty)

let run whitelist roots =
  let inputs = Inputs.find_inputs ~whitelist roots in
  let packages, mk_package_pages = add_package_pages (split_packages inputs) in
  let oc = open_out "Makefile.gen" in
  let fmt = Format.formatter_of_out_channel oc in
  Fun.protect
    ~finally:(fun () ->
      Format.pp_print_flush fmt ();
      close_out oc)
    (fun () ->
      Makefile.(
        pp fmt
          (concat [ mk_package_pages; Compile.gen packages; Link.gen packages ])))
