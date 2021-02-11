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
  inppath : Fpath.t;  (** Path to the input file. *)
  relinppath : Fpath.t;
      (** Relative path to the input file from the root. Used for interpreting
          compile-deps files. *)
  reloutpath : Fpath.t;
      (** Relative path to use for output, extension is the same as [inppath]. *)
}
(** Represents the necessary information about a particular compilation unit *)

let pp fmt x =
  Format.fprintf fmt "@[<v 2>{ name: %s@,path: %a }@]"
    x.name Fpath.pp x.inppath

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
  let relinppath =
    match Fpath.relativize ~root inppath with
    | Some p -> p
    | None -> failwith "odd"
  in
  let fname = Fpath.base relinppath in
  let name = String.capitalize_ascii Fpath.(to_string (rem_ext fname)) in
  { name; inppath; relinppath; reloutpath = relinppath }

let get_mld_info root inppath =
  let unit_name fname =
    (* Prefix name and output file name with "page-".
       Change '-' characters into '_'. *)
    "page-" ^ String.concat "_" (String.split_on_char '-' fname)
  in
  let relinppath =
    match Fpath.relativize ~root inppath with
    | Some p -> p
    | None -> failwith "odd"
  in
  let fparent, fname = Fpath.split_base relinppath in
  let outfname = Fpath.v (unit_name (Fpath.to_string fname)) in
  let name = Fpath.to_string (Fpath.rem_ext outfname) in
  let reloutpath = Fpath.append fparent outfname in
  { name; inppath; relinppath; reloutpath }

(** Segs without ["."] and [".."]. *)
let segs_of_path p =
  List.filter (fun s -> (not (Fpath.is_rel_seg s)) && s <> "") (Fpath.segs p)

(** A tree node's [id] corresponding to an input. *)
let tree_id_of_input inp =
  String.concat "-" (segs_of_path (Fpath.parent inp.reloutpath))

let find_inputs root =
  let files = Fs_util.dir_contents_rec root in
  (get_cm_files files |> List.map (get_cm_info root))
  @ (get_mld_files files |> List.map (get_mld_info root))

type tree = {
  id : string;
      (** Unique name derived from [reldir]. The root node has [id = ""]. *)
  reldir : Fpath.t;
  inputs : t list;
  parent_page : t option;
  childs : (string * tree) list;
}

(** Flatten a {!tree}. *)
let fold_tree f acc tree =
  let rec loop_node acc tree = loop_childs (f acc tree) tree.childs
  and loop_childs acc = function
    | [] -> acc
    | (_, child) :: tl -> loop_childs (loop_node acc child) tl
  in
  loop_node acc tree

(** Turn a list of path into a tree. The second component is passed as-is. *)
let make_tree_from_dirs (paths : (Fpath.t * 'a list) list) :
    [ `N of 'a list * (string * 'b) list ] as 'b =
  let rec loop_node direct acc = function
    | [] -> `N (direct, acc)
    | ([], direct') :: tl -> loop_node (direct' @ direct) acc tl
    | (seg :: seg_tl, p) :: tl ->
        let childs, tl = loop_same_seg [ (seg_tl, p) ] seg tl in
        loop_node direct ((seg, loop_node [] [] childs) :: acc) tl
  and loop_same_seg acc seg = function
    | (seg' :: seg_tl, p) :: tl when seg = seg' ->
        loop_same_seg ((seg_tl, p) :: acc) seg tl
    | tl -> (acc, tl)
  in
  List.sort_uniq (fun (a, _) (b, _) -> Fpath.compare a b) paths
  |> List.map (fun (p, inp) -> (segs_of_path p, inp))
  |> loop_node [] []

(** Turn a list of inputs ([with_deps]) into a tree following the directory
    structure ([reloutpath]). Detects parent pages. See {!tree}. *)
let make_tree inputs =
  let module M = Fpath.Map in
  let multi_add v vs = Some (v :: Option.value vs ~default:[]) in
  let group_by_dir acc ({ reloutpath; _ } as inp) =
    M.update (Fpath.parent reloutpath) (multi_add inp) acc
  in
  let find_parent_page gp_page parent_inputs dir_name =
    let parent_name = dir_name ^ ".mld" in
    let is_parent_page inp = Fpath.basename inp.inppath = parent_name in
    match List.find_opt is_parent_page parent_inputs with
    | Some p -> Some p
    | None -> gp_page
  in
  (* Finally construct the tree. *)
  let rec make_tree segs parent_page (`N (inputs, childs)) =
    let id = String.concat "-" (List.rev segs)
    and reldir =
      if segs = [] then Fpath.v "./"
      else Fpath.v (String.concat Fpath.dir_sep (List.rev ("" :: segs)))
    in
    let childs = List.map (make_child_tree segs parent_page inputs) childs in
    { id; reldir; inputs; parent_page; childs }
  and make_child_tree segs gp_page parent_inputs (dir_name, subtree) =
    let parent_page = find_parent_page gp_page parent_inputs dir_name in
    (dir_name, make_tree (dir_name :: segs) parent_page subtree)
  in
  M.bindings (List.fold_left group_by_dir M.empty inputs)
  |> make_tree_from_dirs |> make_tree [] None

(** The name of phony rules for compiling every units in a directory. These
    rules are defined by [Compile] and used by [Link]. *)
let compile_rule tree = "compile-" ^ tree.id

(** The list of childs per parent. The keys are parent's [reloutpath]. *)
let find_parent_childs tree =
  let module M = Fpath.Map in
  let ( ||| ) a b = match a with Some _ -> a | None -> b in
  let add_childs childs acc = function
    | Some parent ->
        let update_childs c' = Some (childs @ Option.value ~default:[] c') in
        M.update parent.reloutpath update_childs acc
    | None -> acc
  in
  let rec loop_node parent acc t =
    let parent = t.parent_page ||| parent in
    let acc = add_childs t.inputs acc parent in
    List.fold_left (loop_child parent) acc t.childs
  and loop_child parent acc (_, t) = loop_node parent acc t in
  loop_node None M.empty tree
