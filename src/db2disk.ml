(* $Id: db2disk.ml,v 5.1 2006-12-23 22:09:30 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Def;
open Mutil;
open Printf;

value magic_patch = "GwPt0001";

type patches =
  { nb_per : mutable int;
    nb_fam : mutable int;
    nb_per_ini : int;
    nb_fam_ini : int;
    h_person : Hashtbl.t iper (gen_person iper string);
    h_ascend : Hashtbl.t iper (gen_ascend ifam);
    h_union : Hashtbl.t iper (gen_union ifam);
    h_family : Hashtbl.t ifam (gen_family iper string);
    h_couple : Hashtbl.t ifam (gen_couple iper);
    h_descend : Hashtbl.t ifam (gen_descend iper);
    h_key : Hashtbl.t (string * string * int) iper;
    h_name : Hashtbl.t string (list iper) }
;

type db2 =
  { phony : unit -> unit; (* to prevent usage of "=" in the program *)
    bdir : string;
    cache_chan : Hashtbl.t (string * string * string) in_channel;
    patches : patches;
    parents_array : mutable option (array (option ifam));
    consang_array : mutable option (array Adef.fix);
    family_array : mutable option (array (array ifam));
    father_array : mutable option (array iper);
    mother_array : mutable option (array iper);
    children_array : mutable option (array (array iper)) }
;

(* reading in files style database 2 *)

value fast_open_in_bin_and_seek db2 f1 f2 f pos = do {
  let ic =
    try Hashtbl.find db2.cache_chan (f1, f2, f) with
    [ Not_found -> do {
        let ic =
          open_in_bin
            (List.fold_left Filename.concat db2.bdir [f1; f2; f])
        in
        Hashtbl.add db2.cache_chan (f1, f2, f) ic;
        ic
      } ]
  in
  seek_in ic pos;
  ic
};

value get_field_acc db2 i (f1, f2) =
  let ic = fast_open_in_bin_and_seek db2 f1 f2 "access" (4 * i) in
  input_binary_int ic
;

value get_field_data db2 pos (f1, f2) data =
  let ic = fast_open_in_bin_and_seek db2 f1 f2 data pos in
  Iovalue.input ic
;

value get_field_2_data db2 pos (f1, f2) data =
  let ic = fast_open_in_bin_and_seek db2 f1 f2 data pos in
  let r = Iovalue.input ic in
  let s = Iovalue.input ic in
  (r, s)
;

value get_field db2 i path =
  let pos = get_field_acc db2 i path in
  get_field_data db2 pos path "data"
;

value string_of_istr2 db2 f pos =
  if pos = -1 || pos = Db2.empty_string_pos then ""
  else get_field_data db2 pos f "data"
;

(* hash tables in disk *)

type key =
  [ Key of Adef.istr and Adef.istr and int
  | Key0 of Adef.istr and Adef.istr (* to save memory space *) ]
;

type bucketlist 'a 'b =
  [ Empty
  | Cons of 'a and 'b and bucketlist 'a 'b ]
;

value rec hashtbl_find_rec key =
  fun
  [ Empty -> raise Not_found
  | Cons k d rest ->
      if compare key k = 0 then d else hashtbl_find_rec key rest ]
;

value hashtbl_find dir file key = do {
  let ic_ht = open_in_bin (Filename.concat dir file) in
  let ic_hta = open_in_bin (Filename.concat dir (file ^ "a")) in
  let alen = input_binary_int ic_hta in
  let pos = int_size + (Hashtbl.hash key) mod alen * int_size in
  seek_in ic_hta pos;
  let pos = input_binary_int ic_hta in
  close_in ic_hta;
  seek_in ic_ht pos;
  let bl : bucketlist _ _ = Iovalue.input ic_ht in
  close_in ic_ht;
  hashtbl_find_rec key bl
};

value hashtbl_find_all dir file key = do {
  let rec find_in_bucket =
    fun
    [ Empty -> []
    | Cons k d rest ->
        if compare k key = 0 then [d :: find_in_bucket rest]
        else find_in_bucket rest ]
  in
  let ic_ht = open_in_bin (Filename.concat dir file) in
  let ic_hta = open_in_bin (Filename.concat dir (file ^ "a")) in
  let alen = input_binary_int ic_hta in
  let pos = int_size + (Hashtbl.hash key) mod alen * int_size in
  seek_in ic_hta pos;
  let pos = input_binary_int ic_hta in
  close_in ic_hta;
  seek_in ic_ht pos;
  let bl : bucketlist _ _ = Iovalue.input ic_ht in
  close_in ic_ht;
  find_in_bucket bl
};

value key_hashtbl_find dir file (fn, sn, oc) =
  let key = if oc = 0 then Key0 fn sn else Key fn sn oc in
  hashtbl_find dir file key
;

(* string person index version 2 *)

type string_person_index2 =
  { is_first_name : bool;
    index_of_first_char : list (string * int);
    ini : mutable string;
    curr : mutable int }
;

value start_with s p =
  String.length p < String.length s &&
  String.sub s 0 (String.length p) = p
;

value spi2_first db2 spi s = do {
  let i =
    (* to be faster, go directly to the first string starting with
       the same char *)
    if s = "" then 0
    else
      let nbc = Name.nbc s.[0] in
      loop spi.index_of_first_char where rec loop =
        fun
        [ [(s1, i1) :: list] ->
            if s1 = "" then loop list
            else
              let nbc1 = Name.nbc s1.[0] in
              if nbc = nbc1 && nbc > 0 && nbc <= String.length s &&
                 nbc <= String.length s1 &&
                 String.sub s 0 nbc = String.sub s1 0 nbc
              then i1
              else loop list
        | [] -> raise Not_found ]
  in
  let f1 = "person" in
  let f2 = if spi.is_first_name then "first_name" else "surname" in
  let ic = fast_open_in_bin_and_seek db2 f1 f2 "index.acc" (4 * i) in
  let pos = input_binary_int ic in
  let ic = fast_open_in_bin_and_seek db2 f1 f2 "index.dat" pos in
  let (pos, i) =
    try
      loop i where rec loop i =
        let (s1, pos) : (string * int) = Iovalue.input ic in
        if start_with s1 s then (pos, i) else loop (i + 1)
    with
    [ End_of_file -> raise Not_found ]
  in
  spi.ini := s;
  spi.curr := i;
  (f1, f2, pos)
};

value spi2_next db2 spi need_whole_list (f1, f2) =
  let i =
    if spi.ini = "" && not need_whole_list then
      loop spi.index_of_first_char where rec loop =
        fun
        [ [(_, i1) :: ([(_, i2) :: _] as list)] ->
            if spi.curr = i1 then i2 else loop list
        | [] | [_] -> raise Not_found ]
    else spi.curr + 1
  in
  try do {
    let ic =
      if i = spi.curr + 1 then
        Hashtbl.find db2.cache_chan (f1, f2, "index.dat")
      else
        let ic =
          fast_open_in_bin_and_seek db2 f1 f2 "index.acc" (i * 4)
        in
        let pos = input_binary_int ic in
        fast_open_in_bin_and_seek db2 f1 f2 "index.dat" pos
    in
    let (s, pos) : (string * int) = Iovalue.input ic in
    let dlen = i - spi.curr in
    spi.curr := i;
    (pos, dlen)
  }
  with
  [ End_of_file -> raise Not_found ]
;

value spi2_find db2 (f1, f2) pos =
  let dir = List.fold_left Filename.concat db2.bdir [f1; f2] in
  hashtbl_find_all dir "person_of_string.ht" pos
;

value spi2gen_find db2 spi s =
  let proj =
    if spi.is_first_name then fun p -> p.first_name
    else fun p -> p.surname
  in
  Hashtbl.fold
    (fun _ iper iperl ->
       try
         let p = Hashtbl.find db2.patches.h_person iper in
         if proj p = s then [iper :: iperl] else iperl
       with
       [ Not_found -> iperl ])
    db2.patches.h_key []
;

(* *)

value person2_of_key db2 fn sn oc =
  let fn = Name.lower (nominative fn) in
  let sn = Name.lower (nominative sn) in
  try Some (Hashtbl.find db2.patches.h_key (fn, sn, oc)) with
  [ Not_found ->
      let person_of_key_d = Filename.concat db2.bdir "person_of_key" in
      try do {
        let ifn = hashtbl_find person_of_key_d "istr_of_string.ht" fn in
        let isn = hashtbl_find person_of_key_d "istr_of_string.ht" sn in
        let key = (ifn, isn, oc) in
        Some (key_hashtbl_find person_of_key_d "iper_of_key.ht" key : iper)
      }
      with
      [ Not_found -> None ] ]
;

value strings2_of_fsname db2 f s =
  let k = Name.crush_lower s in
  let dir = List.fold_left Filename.concat db2.bdir ["person"; f] in
  hashtbl_find_all dir "string_of_crush.ht" k
;

value persons2_of_name db2 s =
  let s = Name.crush_lower s in
  let dir = Filename.concat db2.bdir "person_of_name" in
  List.rev_append
    (try Hashtbl.find db2.patches.h_name s with [ Not_found -> [] ])
    (hashtbl_find_all dir "person_of_name.ht" s)
;

value persons_of_first_name_or_surname2 db2 is_first_name = do {
  let f1 = "person" in
  let f2 = if is_first_name then "first_name" else "surname" in
  let fdir = List.fold_left Filename.concat db2.bdir [f1; f2] in
  let index_ini_fname = Filename.concat fdir "index.ini" in
  let ic = open_in_bin index_ini_fname in
  let iofc : list (string * int) = input_value ic in
  close_in ic;
  {is_first_name = is_first_name; index_of_first_char = iofc; ini = "";
   curr = 0}
};

value load_array2 bdir nb_ini nb f1 f2 get =
  if nb = 0 then [| |]
  else do {
    let ic_acc =
      open_in_bin (List.fold_left Filename.concat bdir [f1; f2; "access"])
    in
    let ic_dat =
      open_in_bin (List.fold_left Filename.concat bdir [f1; f2; "data"])
    in
    let tab = Array.create nb (get ic_dat (input_binary_int ic_acc)) in
    for i = 1 to nb_ini - 1 do {
      tab.(i) := get ic_dat (input_binary_int ic_acc);
    };
    close_in ic_dat;
    close_in ic_acc;
    tab
  }
;

value load_couples_array2 db2 = do {
  eprintf "*** loading couples array\n";
  flush stderr;
  let nb = db2.patches.nb_fam in
  match db2.father_array with
  [ Some _ -> ()
  | None ->
      let tab =
        load_array2 db2.bdir db2.patches.nb_fam_ini nb "family"
          "father"
          (fun ic_dat pos ->
             do { seek_in ic_dat pos; Iovalue.input ic_dat })
      in
      do {
        Hashtbl.iter (fun i c -> tab.(Adef.int_of_ifam i) := Adef.father c)
          db2.patches.h_couple;
        db2.father_array := Some tab
      } ];
  match db2.mother_array with
  [ Some _ -> ()
  | None ->
      let tab =
        load_array2 db2.bdir db2.patches.nb_fam_ini nb "family"
          "mother"
          (fun ic_dat pos ->
             do { seek_in ic_dat pos; Iovalue.input ic_dat })
      in
      do {
        Hashtbl.iter (fun i c -> tab.(Adef.int_of_ifam i) := Adef.mother c)
          db2.patches.h_couple;
        db2.mother_array := Some tab
      } ]
};

value parents_array2 db2 nb_ini nb = do {
  let arr =
    load_array2 db2.bdir nb_ini nb "person" "parents"
      (fun ic_dat pos ->
         if pos = -1 then None
         else do {
           seek_in ic_dat pos;
           Some (Iovalue.input ic_dat : ifam)
         })
  in
  Hashtbl.iter (fun i a -> arr.(Adef.int_of_iper i) := a.parents)
    db2.patches.h_ascend;
  arr
};

value no_consang = Adef.fix (-1);

value consang_array2 db2 nb =
  let cg_fname =
    List.fold_left Filename.concat db2.bdir ["person"; "consang"; "data"]
  in
  match try Some (open_in_bin cg_fname) with [ Sys_error _ -> None ] with
  [ Some ic -> do {
      let tab = input_value ic in
      close_in ic;
      tab
    }
  | None -> Array.make nb no_consang ]
;

value family_array2 db2 = do {
  let fname =
    List.fold_left Filename.concat db2.bdir ["person"; "family"; "data"]
  in
  let ic = open_in_bin fname in
  let tab = input_value ic in
  close_in ic;
  tab
};

value children_array2 db2 = do {
  let fname =
    List.fold_left Filename.concat db2.bdir ["family"; "children"; "data"]
  in
  let ic = open_in_bin fname in
  let tab = input_value ic in
  close_in ic;
  tab
};

value read_notes bname fnotes rn_mode =
  let fname =
    if fnotes = "" then "notes.txt"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
  let fname = Filename.concat "base_d" fname in
  match
    try Some (Secure.open_in (Filename.concat bname fname)) with
    [ Sys_error _ -> None ]
  with
  [ Some ic -> do {
      let str =
        match rn_mode with
        [ RnDeg -> if in_channel_length ic = 0 then "" else " "
        | Rn1Ln -> try input_line ic with [ End_of_file -> "" ]
        | RnAll ->
            loop 0 where rec loop len =
              match try Some (input_char ic) with [ End_of_file -> None ] with
              [ Some c -> loop (Buff.store len c)
              | _ -> Buff.get len ] ]
      in
      close_in ic;
      str
    }
  | None -> "" ]
;

value check_magic ic magic id = do {
  let b = String.create (String.length magic) in
  really_input ic b 0 (String.length b);
  if b <> magic then failwith (sprintf "bad %s magic number" id)
  else ();
};

value commit_patches2 db2 = do {
  let fname = Filename.concat db2.bdir "patches" in
  let oc = open_out_bin (fname ^ "1") in
  output_string oc magic_patch;
  output_value oc db2.patches;
  close_out oc;
  remove_file (fname ^ "~");
  try Sys.rename fname (fname ^ "~") with [ Sys_error _ -> () ];
  Sys.rename (fname ^ "1") fname
};

value base_of_base2 bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let bdir = Filename.concat bname "base_d" in
  let patches =
    let patch_fname = Filename.concat bdir "patches" in
    match try Some (open_in_bin patch_fname) with [ Sys_error _ -> None ] with
    [ Some ic -> do {
        check_magic ic magic_patch "patch";
        let p = input_value ic in
        close_in ic;
        flush stderr;
        p
      }
    | None ->
        let nb_per =
          let fname =
            List.fold_left Filename.concat bdir
              ["person"; "sex"; "access"]
          in
          let st = Unix.lstat fname in
          st.Unix.st_size / 4
        in
        let nb_fam =
          let fname =
            List.fold_left Filename.concat bdir
              ["family"; "marriage"; "access"]
          in
          let st = Unix.lstat fname in
          st.Unix.st_size / 4
        in
        let empty_ht () = Hashtbl.create 1 in
        {nb_per = nb_per; nb_fam = nb_fam;
         nb_per_ini = nb_per; nb_fam_ini = nb_fam;
         h_person = empty_ht (); h_ascend = empty_ht ();
         h_union = empty_ht (); h_family = empty_ht ();
         h_couple = empty_ht (); h_descend = empty_ht ();
         h_key = empty_ht (); h_name = empty_ht ()} ]
  in
  {bdir = bdir; cache_chan = Hashtbl.create 1; patches = patches;
   parents_array = None; consang_array = None; family_array = None;
   father_array = None; mother_array = None; children_array = None;
   phony () = ()}
;