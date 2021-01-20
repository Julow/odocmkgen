open Listm
open Util

(** Lower is better *)
let cm_file_preference = function
  | ".cmti" -> Some 1
  | ".cmt" -> Some 2
  | ".cmi" -> Some 3
  | _ -> None

(** Get cm* files out of a list of files.
    Given the choice between a cmti, a cmt and a cmi file, we chose them according to [cm_file_preference] above *)
let get_cm_files files =
  let rec skip f = function hd :: tl when f hd -> skip f tl | x -> x in
  (* Take the first of each group. *)
  let rec dedup acc = function
    | (base, _, p) :: tl ->
        let tl = skip (fun (base', _, _) -> base = base') tl in
        dedup (p :: acc) tl
    | [] -> acc
  in
  (* Sort files by their basename and preference, remove other files *)
  files
  >>= (fun p ->
        let without_ext, ext = Fpath.split_ext p in
        match cm_file_preference ext with
        | Some pref -> [ (Fpath.basename without_ext, pref, p) ]
        | None -> [])
  |> List.sort compare |> dedup []

(** Get mld files out of a list of files. *)
let get_mld_files = List.filter (Fpath.has_ext ".mld")

type t = {
  name : string;  (** 'Astring' *)
  inppath : Fpath.t;  (** Path to the input file, contains [root]. *)
  root : Fpath.t;
      (** Root path in which this was found, e.g. /home/opam/.opam/4.10.0/lib/package_name *)
  reloutpath : Fpath.t;
      (** Relative path to use for output, extension is the same as [inppath].
          May not correspond to a input file in [root]. *)
  package : string;  (** Package in which this file lives ("astring") *)
}
(** Represents the necessary information about a particular compilation unit *)

let pp fmt x =
  Format.fprintf fmt "@[<v 2>{ name: %s@,root: %a@,path: %a@,package:%s }@]"
    x.name Fpath.pp x.root Fpath.pp x.inppath x.package

let input_file t = t.inppath

let compile_path_of_relpath = Fpath.(( // ) (v "odocs"))

let link_path_of_relpath = Fpath.(( // ) (v "odocls"))

(** Returns the relative path to an odoc file based on an input file. For example, given
   `/home/opam/.opam/4.10.0/lib/ocaml/compiler-libs/lambda.cmi` it will return
   `odocs/ocaml/compiler-libs/lambda.odoc` *)
let compile_target t =
  compile_path_of_relpath (Fpath.set_ext "odoc" t.reloutpath)

(** Like [compile_target] but goes into the "odocls" directory. *)
let link_target t = link_path_of_relpath (Fpath.set_ext "odocl" t.reloutpath)

(* Get info given a base file (cmt, cmti or cmi) *)
let get_cm_info root inppath =
  let package = Fpath.(basename (parent inppath)) in
  let reloutpath =
    match Fpath.relativize ~root inppath with
    | Some p -> p
    | None -> failwith "odd"
  in
  let fname = Fpath.base reloutpath in
  let name = String.capitalize_ascii Fpath.(to_string (rem_ext fname)) in
  { name; inppath; root; reloutpath; package }

let get_mld_info root inppath =
  let package = Fpath.(basename (parent inppath)) in
  let relpath =
    match Fpath.relativize ~root inppath with
    | Some p -> p
    | None -> failwith "odd"
  in
  let fparent, fname = Fpath.split_base relpath in
  (* Prefix name and output file name with "page-" *)
  let outfname = Fpath.v ("page-" ^ Fpath.to_string fname) in
  let name = Fpath.to_string (Fpath.rem_ext outfname) in
  let reloutpath = Fpath.append fparent outfname in
  { name; inppath; root; reloutpath; package }

(** The name of phony rules for compiling every units in a directory. Works
    with [reloutpath]. These rules are defined by [Compile] and used by [Link]. *)
let compile_rule_of_path path =
  String.concat "-" ("compile" :: List.filter (( <> ) "") (Fpath.segs path))

let compile_rule inp = compile_rule_of_path (Fpath.parent inp.reloutpath)

module DigestMap = Map.Make (Digest)

(** Compute direct compile-dependencies for a list of inputs.
    Returns the list of inputs paired with its dependencies. *)
let compile_deps inputs =
  let deps_and_digests =
    List.map
      (fun inp ->
        let deps = Odoc.compile_deps inp.inppath in
        match List.partition (fun d -> d.Odoc.c_unit_name = inp.name) deps with
        | [ self ], deps -> Some (self.c_digest, deps)
        | _ ->
            Format.eprintf "Failed to find digest for self (%s)\n%!" inp.name;
            None)
      inputs
  in
  let inputs_by_digest =
    List.fold_left2
      (fun acc inp -> function
        | Some (digest, _) -> DigestMap.add digest inp acc | None -> acc)
      DigestMap.empty inputs deps_and_digests
  in
  let find_dep inp dep =
    match DigestMap.find_opt dep.Odoc.c_digest inputs_by_digest with
    | Some _ as x -> x
    | None ->
        Format.eprintf "Warning, couldn't find dep %s of file %a\n"
          dep.Odoc.c_unit_name Fpath.pp inp.inppath;
        None
  in
  List.map2
    (fun inp -> function
      | Some (_, deps) -> (inp, List.filter_map (find_dep inp) deps)
      | None -> (inp, []))
    inputs deps_and_digests

let find_inputs root =
  let files = Fs_util.dir_contents_rec root in
  compile_deps (get_cm_files files |> List.map (get_cm_info root))
  @ (get_mld_files files >>= fun mld -> [ (get_mld_info root mld, []) ])
