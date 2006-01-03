(* $Id: iobase.ml,v 4.33.2.1 2006-01-03 12:04:10 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Gutil;

value magic_gwb = "GnWb001y";

(*
 Files in base (directory .gwb)

    base - the base itself
       magic number (magic_gwb)                 : string of length 8
       number of persons                        : binary_int
       number of families                       : binary_int
       number of strings                        : binary_int
       persons array offset in file             : binary_int
       ascends array offset in file             : binary_int
       unions array offset in file              : binary_int
       families array offset in file            : binary_int
       couples array offset in file             : binary_int
       descends array offset in file            : binary_int
       strings array offset in file             : binary_int
       notes origin file                        : value
       persons array                            : value
       ascends array                            : value
       unions array                             : value
       families array                           : value
       couples array                            : value
       descends array                           : value
       strings array                            : value

    base.acc - direct accesses to arrays inside base
       persons offsets   : array of binary_ints
       ascends offsets   : array of binary_ints
       unions offsets    : array of binary_ints
       families offsets  : array of binary_ints
       couples offsets   : array of binary_ints
       descends offsets  : array of binary_ints
       strings offsets   : array of binary_ints

    names.inx - index for names, strings of first names and surnames
       2nd index offset : binary_int
       1st index (names) : value
         array, length = "table_size", associating:
          - a hash value of a "crushed" (module "Name") name (modulo length)
          - to the array of indexes of the corresponding persons
       2nd index (first names and surnames strings) : value
         array, length = "table_size", associating:
          - a hash value of the "crushed" (module "Name") first name or
            surname (modulo length)
          - to the array of the corresponding string indexes

    names.acc - direct accesses to arrays inside names.inx

    strings.inx - index for strings, surnames, first names
       length of the strings offset array : binary_int
       offset of surnames index           : binary_int
       offset of first names index        : binary_int
       strings hash table index           : 2 arrays of binary_ints
         strings offset array (length = prime after 10 * strings array length)
           - associating a hash value of the string modulo length
           - to its index in the string array
         strings list array (length = string array length)
           - associating a string index
           - to the index of the next index holding the same hash value
       -- the following table has been obsolete since version 4.10
       -- it has been replaced by snames.inx/sname.dat which use
       -- much less memory
       surnames index                     : value
         binary tree
          - associating the string index of a surname
          - to the corresponding list of persons holding this surname
       -- the following table has been obsolete since version 4.10
       -- it has been replaced by fnames.inx/fname.dat which use
       -- much less memory
       first_names index                  : value
         binary tree
          - associating the string index of a first name
          - to the corresponding list of persons holding this first name

    snames.inx - index for surnames
       binary tree
        - associating the string index of a surname
        - to a pointer (int) to snames.dat

    snames.dat - data associated with snames.inx
      table of list of persons holding a surname

    fnames.inx - index for first names
       binary tree
        - associating the string index of a first name
        - to a pointer (int) to fnames.dat

    fnames.dat - data associated with fnames.inx
      table of list of persons holding a first name

the corresponding list of persons holding this surname

    patches - patches
       When updated, none of the previous files are modified. Only this one
       is written and rewritten. It holds a record of type "patches", composed
       of association lists "index" - "new value".
*)

value verbose = ref True;
value trace s =
  if verbose.val then do { Printf.eprintf "*** %s\n" s; flush stderr }
  else ()
;

value remove_file f = try Sys.remove f with [ Sys_error _ -> () ];

value output_value_header_size = 20;
value output_value_no_sharing oc v =
  Marshal.to_channel oc v [Marshal.No_sharing]
;

value array_header_size arr = if arr.len < 8 then 1 else 5;

value output_array_access oc arr pos =
  loop (pos + output_value_header_size + array_header_size arr) 0 where rec
  loop pos i =
    if i == arr.len then pos
    else do {
      output_binary_int oc pos; loop (pos + Iovalue.size (arr.get i)) (i + 1)
    }
;

value output_cache_array_no_sharing oc arr =
  do {
    for i = 1 to output_value_header_size + array_header_size arr do {
      output_byte oc 0;
    };
    for i = 0 to arr.len - 1 do {
      Iovalue.output oc (arr.get i);
    };
  }
;

(* Search index of a given string in file strings.inx *)

value int_size = 4;

value string_piece s =
  let s = String.escaped s in
  if String.length s > 20 then
    String.sub s 0 10 ^ " ... " ^ String.sub s (String.length s - 10) 10
  else s
;

exception Found of int;

value hashtbl_right_assoc s ht =
  try
    do {
      Hashtbl.iter
        (fun i1 s1 -> if s = s1 then raise (Found i1) else ()) ht;
      raise Not_found;
    }
  with
  [ Found x -> x ]
;

value index_of_string strings ic start_pos hash_len string_patches s =
  try Adef.istr_of_int (hashtbl_right_assoc s string_patches) with
  [ Not_found ->
      let ia = Hashtbl.hash s mod hash_len in
      do {
        seek_in ic (start_pos + ia * int_size);
        let i1 = input_binary_int ic in
        let rec loop i =
          if i == -1 then raise Not_found
          else if strings.get i = s then Adef.istr_of_int i
          else do {
            seek_in ic (start_pos + (hash_len + i) * int_size);
            loop (input_binary_int ic)
          }
        in
        loop i1
      } ]
;

(* Search index of a given surname or given first name in file strings.inx *)

value name_key s =
  let i = Gutil.initial s in
  let s =
    if i == 0 then s
    else String.sub s i (String.length s - i) ^ " " ^ String.sub s 0 i
  in
  Name.lower s
;

(**)
value initial s =
  loop 0 where rec loop i =
    if i == String.length s then 0
    else
      match s.[i] with
      [ 'A'..'Z' | '�'..'�' -> i
      | _ -> loop (succ i) ]
;

value unaccent =
  fun
  [ '�' | '�' | '�' | '�' | '�' | '�' | '�' -> 'a'
  | '�' -> 'c'
  | '�' | '�' | '�' | '�' -> 'e'
  | '�' | '�' | '�' | '�' -> 'i'
  | '�' -> 'd'
  | '�' -> 'n'
  | '�' | '�' | '�' | '�' | '�' | '�' -> 'o'
  | '�' | '�' | '�' | '�' -> 'u'
  | '�' | '�' -> 'y'
  | '�' -> 'p'
  | c -> c ]
;

value compare_names s1 s2 =
  let compare_aux e1 e2 =
    loop where rec loop i1 i2 =
      if i1 == e1 && i2 == e2 then 0
      else if i1 == e1 then -1
      else if i2 == e2 then 1
      else
        let c1 = unaccent (Char.lowercase s1.[i1]) in
        let c2 = unaccent (Char.lowercase s2.[i2]) in
        match (c1, c2) with
        [ ('a'..'z', 'a'..'z') ->
            if c1 < c2 then -1
            else if c1 > c2 then 1
            else loop (i1 + 1) (i2 + 1)
        | ('a'..'z', _) -> 1
        | (_, 'a'..'z') -> -1
        | _ -> loop (i1 + 1) (i2 + 1) ]
  in
  if s1 = s2 then 0
  else
    let i1 = initial s1 in
    let i2 = initial s2 in
    match compare_aux (String.length s1) (String.length s2) i1 i2 with
    [ 0 -> compare_aux i1 i2 0 0
    | x -> x ]
;
(*
value compare_names s1 s2 = compare (name_key s1) (name_key s2);
*)

value compare_istr_fun base_data is1 is2 =
  if is1 == is2 then 0
  else
    compare_names (base_data.strings.get (Adef.int_of_istr is1))
      (base_data.strings.get (Adef.int_of_istr is2))
;

value rec list_remove_elemq x =
  fun
  [ [y :: l] -> if x == y then l else [y :: list_remove_elemq x l]
  | [] -> [] ]
;

(* compatibility with databases created with versions <= 4.09 *)
(* should be removed after some time (when all databases will have
   been rebuilt with version >= 4.10 *)
value old_persons_of_first_name_or_surname base_data strings params =
  let (ic2, start_pos, proj, person_patches, _, _, _) = params in
  let module IstrTree =
    Btree.Make
      (struct type t = istr; value compare = compare_istr_fun base_data; end)
  in
  let bt =
    let btr = ref None in
    let completed = ref False in
    let update_bt gistro bt =
      do {
        let bt = ref bt in
        Hashtbl.iter
          (fun i p ->
             let istr = proj p in
             if gistro <> None && gistro <> Some istr then ()
             else
               let ipera =
                 try IstrTree.find istr bt.val with [ Not_found -> [] ]
               in
               if List.memq p.cle_index ipera then ()
               else
	         bt.val := IstrTree.add istr [p.cle_index :: ipera] bt.val)
          person_patches;
        if gistro = None then completed.val := True else ();
        bt.val
      }
    in
    fun gistro ->
      match btr.val with
      [ Some bt ->
          if completed.val then bt
          else
            let bt = update_bt gistro bt in
            do { btr.val := Some bt; bt }
      | None ->
          do {
            seek_in ic2 start_pos;
(*
let ab1 = Gc.allocated_bytes () in
*)
            let bt : IstrTree.t (list iper) = input_value ic2 in
(*
let ab2 = Gc.allocated_bytes () in
*)
            let bt = update_bt gistro bt in
            btr.val := Some bt;
(*
Printf.eprintf "*** old database created by version <= 4.09\n"; flush stderr;
Printf.eprintf "*** using index allocating here %.0f bytes\n"
  (ab2 -. ab1);
flush stderr;
*)
            bt
          } ]
  in
  let check_patches istr ipl =
    let ipl = ref ipl in
    do {
      Hashtbl.iter
        (fun i p ->
           if List.memq (Adef.iper_of_int i) ipl.val then
             if compare_istr_fun base_data istr p.first_name == 0 ||
                compare_istr_fun base_data istr p.surname == 0 then
               ()
             else ipl.val := list_remove_elemq (Adef.iper_of_int i) ipl.val
           else ())
        person_patches;
      ipl.val
    }
  in
  let find istr =
    try check_patches istr (IstrTree.find istr (bt (Some istr))) with
    [ Not_found -> [] ]
  in
  let cursor str =
    IstrTree.key_after
      (fun key -> compare_names str (strings.get (Adef.int_of_istr key)))
      (bt None)
  in
  let next key = IstrTree.next key (bt None) in
  {find = find; cursor = cursor; next = next}
;

value new_persons_of_first_name_or_surname base_data strings params =
  let (_, _, proj, person_patches, names_inx, names_dat, bname) = params in
  let module IstrTree =
    Btree.Make
      (struct type t = istr; value compare = compare_istr_fun base_data; end)
  in
  let fname_dat = Filename.concat bname names_dat in
  let bt =
    let btr = ref None in
    fun () ->
      match btr.val with
      [ Some bt -> bt
      | None ->
          do {
            let fname_inx = Filename.concat bname names_inx in
            let ic_inx = Secure.open_in_bin fname_inx in
(*
let ab1 = Gc.allocated_bytes () in
*)
            let bt : IstrTree.t int = input_value ic_inx in
(*
let ab2 = Gc.allocated_bytes () in
Printf.eprintf "*** new database created by version >= 4.10\n";
Printf.eprintf "*** using index '%s' allocating here only %.0f bytes\n"
  names_inx (ab2 -. ab1);
flush stderr;
*)
            close_in ic_inx;
            btr.val := Some bt;
            bt
          } ]
  in
  let find istr =
    let ipera =
      try
        let pos = IstrTree.find istr (bt ()) in
        let ic_dat = Secure.open_in_bin fname_dat in
        do {
          seek_in ic_dat pos;
          let len = input_binary_int ic_dat in
          let rec read_loop ipera len =
            if len = 0 then ipera
            else
              let iper = Adef.iper_of_int (input_binary_int ic_dat) in
              read_loop [iper :: ipera] (len - 1)
          in
          let ipera = read_loop [] len in
          close_in ic_dat;
          ipera
        }
      with
      [ Not_found -> [] ]
    in
    let ipera = ref ipera in
    do {
      Hashtbl.iter
        (fun i p ->
           let istr1 = proj p in
           if istr1 <> istr then ()
           else if List.memq p.cle_index ipera.val then ()
           else ipera.val := [p.cle_index :: ipera.val])
        person_patches;
      ipera.val
    }
  in
  let bt_patched =
    let btr = ref None in
    fun () ->
      match btr.val with
      [ Some bt -> bt
      | None ->
          let bt = ref (bt ()) in
          do {
            Hashtbl.iter
              (fun i p ->
                 let istr1 = proj p in
                 try
                   let _ = IstrTree.find istr1 bt.val in
                   ()
                 with
                 [ Not_found -> bt.val := IstrTree.add istr1 0 bt.val ])
              person_patches;
            btr.val := Some bt.val;
	    bt.val
          } ]
  in
  let cursor str =
    IstrTree.key_after
      (fun key -> compare_names str (strings.get (Adef.int_of_istr key)))
      (bt_patched ())
  in
  let next key = IstrTree.next key (bt_patched ()) in
  {find = find; cursor = cursor; next = next}
;

value persons_of_first_name_or_surname base_data strings params =
  let (_, _, _, _, names_inx, _, bname) = params in
  if Sys.file_exists (Filename.concat bname names_inx) then
    new_persons_of_first_name_or_surname base_data strings params
  else
    old_persons_of_first_name_or_surname base_data strings params
;

(* Search index for a given name in file names.inx *)

type name_index_data = array (array iper);
value table_size = 0x3fff;

value persons_of_name bname patches =
  let t = ref None in
  fun s ->
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    let ai =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =        
        let i = i mod table_size in
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          do {
            seek_in ic_inx_acc (Iovalue.sizeof_long * i);
            let pos = input_binary_int ic_inx_acc in
            close_in ic_inx_acc;
            seek_in ic_inx pos;
            (Iovalue.input ic_inx : array iper)
          }
        else (* compatibility *)
          let a =
            match t.val with
            [ Some a -> a
            | None ->
                do {
                  seek_in ic_inx int_size;
                  let a : name_index_data = input_value ic_inx in
                  t.val := Some a;
                  a
                } ]
          in
          a.(i)
      in
      do { close_in ic_inx; ai }          
    in
    try
      let l = Hashtbl.find patches i in
      l @ Array.to_list ai
    with
    [ Not_found -> Array.to_list ai ]
;

type strings_of_fsname = array (array istr);

value strings_of_fsname bname strings (_, person_patches) =
  let t = ref None in
  fun s ->
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    let r =
      let ic_inx = Secure.open_in_bin (Filename.concat bname "names.inx") in
      let ai =
        let i = i mod table_size in
        let fname_inx_acc = Filename.concat bname "names.acc" in
        if Sys.file_exists fname_inx_acc then
          let ic_inx_acc = Secure.open_in_bin fname_inx_acc in
          do {
            seek_in ic_inx_acc (Iovalue.sizeof_long * (table_size + i));
            let pos = input_binary_int ic_inx_acc in
            close_in ic_inx_acc;
            seek_in ic_inx pos;
            (Iovalue.input ic_inx : array istr)
          }
        else (* compatibility *)
          let a =
            match t.val with
            [ Some a -> a
            | None ->
                let pos = input_binary_int ic_inx in
                do {
                  seek_in ic_inx pos;
                  let a : strings_of_fsname = input_value ic_inx in
                  t.val := Some a;
                  a
                } ]
          in
          a.(i)
        in
        do { close_in ic_inx; ai }
    in
    let l = ref (Array.to_list r) in
    do {
      Hashtbl.iter
        (fun _ p ->
           do {
             if not (List.memq p.first_name l.val) then
               let s1 = strings.get (Adef.int_of_istr p.first_name) in
               let s1 = nominative s1 in
               if s = Name.crush_lower s1 then
                 l.val := [p.first_name :: l.val]
               else ()
             else ();
             if not (List.memq p.surname l.val) then
               let s1 = strings.get (Adef.int_of_istr p.surname) in
               let s1 = nominative s1 in
               if s = Name.crush_lower s1 then
                 l.val := [p.surname :: l.val]
               else ()
             else ();
           })
        person_patches;
      l.val
    }
;
(**)

value lock_file bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then
      Filename.chop_suffix bname ".gwb"
    else bname
  in
  bname ^ ".lck"
;

(* Restrict file *)

type visible_state = [ VsNone | VsTrue | VsFalse ];

value make_visible_cache bname persons =
  let visible_ref = ref None in
  let fname = Filename.concat bname "restrict" in
  let r = { v_write = fun []; v_get = fun [] } in
  let read_or_create_visible () =
    let visible =
      match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
      [ Some ic ->
          do {
            ifdef UNIX then
              if verbose.val then do {
                Printf.eprintf "*** read restrict file\n";
                flush stderr;
              }
              else ()
            else ();
            let visible = input_value ic;
            close_in ic;
            visible
          }
      | None -> Array.create persons.len VsNone ]
    in
    do { visible_ref.val := Some visible; visible }
  in
  let v_write () =
    match visible_ref.val with
    [ Some visible ->
        try do {
          let oc = Secure.open_out fname;
          ifdef UNIX then
            if verbose.val then do {
              Printf.eprintf "*** write restrict file\n";
              flush stderr;
            }
            else ()
          else ();
          output_value oc visible;
          close_out oc
        }
        with [ Sys_error _ -> () ]
    | None -> () ]
  in
  let v_get fct i =
    let visible =
      match visible_ref.val with
      [ Some visible -> visible
      | None -> read_or_create_visible () ]
    in
    if i < Array.length visible then
      match visible.(i) with
      [ VsNone ->
          let status = fct (persons.get i) in
          do {
            visible.(i) := if status then VsTrue else VsFalse;
            visible_ref.val := Some visible;
            status
          }
      | VsTrue -> True
      | VsFalse -> False ]
    else fct (persons.get i)
  in
  do { r.v_write := v_write; r.v_get := v_get; r }
;

(* Input *)

value apply_patches tab f patches plen =
  if plen = 0 then tab
  else do {
    let new_tab =
      if plen > Array.length tab then do {
        let new_tab = Array.create plen (Obj.magic 0) in
        Array.blit tab 0 new_tab 0 (Array.length tab);
        new_tab
      }
      else tab
    in
    Hashtbl.iter (fun i v -> new_tab.(i) := f v) patches;
    new_tab
  }
;

(*
value rec patch_len len =
  fun
  [ [] -> len
  | [(i, _) :: l] -> patch_len (max len (i + 1)) l ]
;
*)

(* patches data; should be saved instead of Old.patches, but not done yet
   for backward compatibility; in case of major change, it can be done
   and module Old deleted; think of output_value_no_sharing which may
   not be good for this type (anyway I think not useful for the file
   patches) *)

type patches_ht =
  { h_person : (ref int * Hashtbl.t int person);
    h_ascend : (ref int * Hashtbl.t int ascend);
    h_union : (ref int * Hashtbl.t int union);
    h_family : (ref int * Hashtbl.t int family);
    h_couple : (ref int * Hashtbl.t int couple);
    h_descend : (ref int * Hashtbl.t int descend);
    h_string : (ref int * Hashtbl.t int string);
    h_name : (ref int * Hashtbl.t int (list iper)) }
;

module Old =
  struct
    type patches =
      { p_person : ref (list (int * person));
        p_ascend : ref (list (int * ascend));
        p_union : ref (list (int * union));
        p_family : ref (list (int * family));
        p_couple : ref (list (int * couple));
        p_descend : ref (list (int * descend));
        p_string : ref (list (int * string));
        p_name : ref (list (int * list iper)) }
    ;
  end
;

value check_magic =
  let b = String.create (String.length magic_gwb) in
  fun ic ->
    do {
      really_input ic b 0 (String.length b);
      if b <> magic_gwb then
        if String.sub magic_gwb 0 4 = String.sub b 0 4 then
          failwith "this is a GeneWeb base, but not compatible"
        else
          failwith "this is not a GeneWeb base, or it is a very old version"
      else ()
    }
;

value phony_person =
  {first_name = 0; surname = 0;
   occ = 0; image = 0; first_names_aliases = [];
   surnames_aliases = []; public_name = 0; qualifiers = [];
   aliases = []; titles = []; rparents = []; related = [];
   occupation = 0; sex = Neuter; access = IfTitles;
   birth = Adef.codate_None; birth_place = 0;
   birth_src = 0; baptism = Adef.codate_None;
   baptism_place = 0; baptism_src = 0;
   death = DontKnowIfDead; death_place = 0;
   death_src = 0; burial = UnknownBurial;
   burial_place = 0; burial_src = 0;
   notes = 0; psources = 0;
   cle_index = Adef.iper_of_int 0}
;

value phony_family =
  {marriage = Adef.codate_None;
   marriage_place = 0; marriage_src = 0;
   witnesses = [| |]; relation = Married;
   divorce = NotDivorced; comment = 0;
   origin_file = 0; fsources = 0;
   fam_index = Adef.ifam_of_int 0}
;

value ext phony v =
  let rlen = Array.length (Obj.magic v) in
  let alen = Array.length (Obj.magic phony) in
  if rlen = alen then v
  else if rlen < alen then do {
    let x = Array.copy (Obj.magic phony) in
    Array.blit (Obj.magic v) 0 x 0 rlen;
    Obj.magic x
  }
  else
    failwith "this is a GeneWeb base, but not compatible; please upgrade"
;

value array_ext phony fa =
  let a = Obj.magic fa in
  if Array.length a = 0 then fa
  else
    let rlen = Array.length a.(0) in
    let alen = Array.length (Obj.magic phony) in
    if rlen = alen then fa
    else if rlen < alen then do {
      ifdef UNIX then
        if verbose.val then do {
          Printf.eprintf
            "*** extending records from size %d to size %d\n" 
            rlen alen;
          flush stderr;
        }
        else ()
      else ();      
      for i = 0 to Array.length a - 1 do {
        let x = Array.copy (Obj.magic phony) in
        Array.blit a.(i) 0 x 0 rlen;
        a.(i) := x;
      };
      fa
    }
    else
      failwith "this is a GeneWeb base, but not compatible; please upgrade"
;

value make_cache ic ic_acc shift array_pos (plenr, patches) len name =
  let v_ext v =
    if name = "persons" then ext phony_person v
    else if name = "families" then ext phony_family v
    else v
  in
  let v_arr_ext v =
    if name = "persons" then array_ext phony_person v
    else if name = "families" then array_ext phony_family v
    else v
  in
  let tab = ref None in
  let cleared = ref False in
  let r =
    {array = fun []; get = fun []; len = max len plenr.val;
     clear_array = fun _ -> do { cleared.val := True; tab.val := None }}
  in
  let array () =
    match tab.val with
    [ Some x -> x
    | None ->
        do {
          if name = "persons" then failwith "bug: access to person array"
          else if name = "families" then failwith "bug: access to family array"
	  else ();
          ifdef UNIX then
            if verbose.val then do {
              Printf.eprintf "*** read %s%s\n" name
                (if cleared.val then " (again)" else "");
              flush stderr;
            }
            else ()
          else ();
          do {
            seek_in ic array_pos;
            let v = input_value ic in
            let v = v_arr_ext v in
            let t = apply_patches v v_ext patches r.len in
            tab.val := Some t;
            t
          }
        } ]
  in
  let gen_get i =
    if tab.val <> None then (r.array ()).(i)
    else
      try
        let v = Hashtbl.find patches i in
        v_ext v
      with
      [ Not_found ->
          if i < 0 || i >= len then
            failwith
              ("access " ^ name ^ " out of bounds; i = " ^ string_of_int i)
          else do {
            seek_in ic_acc (shift + Iovalue.sizeof_long * i);
            let pos = input_binary_int ic_acc in
            seek_in ic pos;
            let v = Iovalue.input ic in
            v_ext v
          } ]
  in
  do { r.array := array; r.get := gen_get; r }
;

value input_patches bname =
  let patches =
    match
      try
        Some (Secure.open_in_bin (Filename.concat bname "patches"))
      with _ -> None
    with
    [ Some ic -> let p = input_value ic in do { close_in ic; p }
    | None ->
        {Old.p_person = ref []; Old.p_ascend = ref []; Old.p_union = ref [];
         Old.p_family = ref []; Old.p_couple = ref []; Old.p_descend = ref [];
         Old.p_string = ref []; Old.p_name = ref []} ]
  in
  let ht =
    {h_person = (ref 0, Hashtbl.create 101);
     h_ascend = (ref 0, Hashtbl.create 101);
     h_union = (ref 0, Hashtbl.create 101);
     h_family = (ref 0, Hashtbl.create 101);
     h_couple = (ref 0, Hashtbl.create 101);
     h_descend = (ref 0, Hashtbl.create 101);
     h_string = (ref 0, Hashtbl.create 101);
     h_name = (ref 0, Hashtbl.create 101)}
  in
  let add (ir, ht) (k, v) =
    do {
      if k >= ir.val then ir.val := k + 1 else ();
      Hashtbl.add ht k v;
    }
  in
  do {
    List.iter (add ht.h_person) patches.Old.p_person.val;
    List.iter (add ht.h_ascend) patches.Old.p_ascend.val;
    List.iter (add ht.h_union) patches.Old.p_union.val;
    List.iter (add ht.h_family) patches.Old.p_family.val;
    List.iter (add ht.h_couple) patches.Old.p_couple.val;
    List.iter (add ht.h_descend) patches.Old.p_descend.val;
    List.iter (add ht.h_string) patches.Old.p_string.val;
    List.iter (add ht.h_name) patches.Old.p_name.val;
    ht
   }
;

value patches_of_patches_ht patches =
  let p =
    {Old.p_person = ref [];
     Old.p_ascend = ref [];
     Old.p_union = ref [];
     Old.p_family = ref [];
     Old.p_couple = ref [];
     Old.p_descend = ref [];
     Old.p_string = ref [];
     Old.p_name = ref []}
  in
  let add r k v = r.val := [(k, v) :: r.val] in
  do {
    Hashtbl.iter (add p.Old.p_person) (snd patches.h_person);
    Hashtbl.iter (add p.Old.p_ascend) (snd patches.h_ascend);
    Hashtbl.iter (add p.Old.p_union) (snd patches.h_union);
    Hashtbl.iter (add p.Old.p_family) (snd patches.h_family);
    Hashtbl.iter (add p.Old.p_couple) (snd patches.h_couple);
    Hashtbl.iter (add p.Old.p_descend) (snd patches.h_descend);
    Hashtbl.iter (add p.Old.p_string) (snd patches.h_string);
    Hashtbl.iter (add p.Old.p_name) (snd patches.h_name);
    p
  }
;

value input bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let patches = input_patches bname in
  let ic =
    let ic = Secure.open_in_bin (Filename.concat bname "base") in
    do { check_magic ic; ic }
  in
  let persons_len = input_binary_int ic in
  let families_len = input_binary_int ic in
  let strings_len = input_binary_int ic in
  let persons_array_pos = input_binary_int ic in
  let ascends_array_pos = input_binary_int ic in
  let unions_array_pos = input_binary_int ic in
  let families_array_pos = input_binary_int ic in
  let couples_array_pos = input_binary_int ic in
  let descends_array_pos = input_binary_int ic in
  let strings_array_pos = input_binary_int ic in
  let norigin_file = input_value ic in
  let ic_acc = Secure.open_in_bin (Filename.concat bname "base.acc") in
  let ic2 = Secure.open_in_bin (Filename.concat bname "strings.inx") in
  let ic2_string_start_pos = 3 * int_size in
  let ic2_string_hash_len = input_binary_int ic2 in
  let ic2_surname_start_pos = input_binary_int ic2 in
  let ic2_first_name_start_pos = input_binary_int ic2 in
  let shift = 0 in
  let persons =
    make_cache ic ic_acc shift persons_array_pos patches.h_person persons_len
      "persons"
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let ascends =
    make_cache ic ic_acc shift ascends_array_pos patches.h_ascend persons_len
      "ascends"
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let unions =
    make_cache ic ic_acc shift unions_array_pos patches.h_union persons_len
      "unions"
  in
  let shift = shift + persons_len * Iovalue.sizeof_long in
  let families =
    make_cache ic ic_acc shift families_array_pos patches.h_family
      families_len "families"
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let couples =
    make_cache ic ic_acc shift couples_array_pos patches.h_couple
      families_len "couples"
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let descends =
    make_cache ic ic_acc shift descends_array_pos patches.h_descend
      families_len "descends"
  in
  let shift = shift + families_len * Iovalue.sizeof_long in
  let strings =
    make_cache ic ic_acc shift strings_array_pos patches.h_string
      strings_len "strings"
  in
  let cleanup_ref =
    ref (fun () -> do { close_in ic; close_in ic_acc; close_in ic2; })
  in
  let cleanup () = cleanup_ref.val () in
  let commit_patches () =
    let tmp_fname = Filename.concat bname "1patches" in
    let fname = Filename.concat bname "patches" in
    do {
      let oc9 =
        try Secure.open_out_bin tmp_fname with
        [ Sys_error _ ->
            raise (Adef.Request_failure "the database is not writable") ]
      in
      let patches = patches_of_patches_ht patches in
      remove_file (fname ^ "~");
      output_value_no_sharing oc9 (patches : Old.patches);
      close_out oc9;
      try Sys.rename fname (fname ^ "~") with [ Sys_error _ -> () ];
      try Sys.rename tmp_fname fname with [ Sys_error _ -> () ];
    }
  in
  let patched_ascends () =
    let r = ref [] in
    do {
      Hashtbl.iter (fun i _ -> r.val := [Adef.iper_of_int i :: r.val])
        (snd patches.h_ascend);
      r.val
    }
  in
  let patch_person i p =
    let i = Adef.int_of_iper i in
    do {
      persons.len := max persons.len (i + 1);
      (fst patches.h_person).val := persons.len;
      Hashtbl.replace (snd patches.h_person) i p;
    }
  in
  let patch_ascend i a =
    let i = Adef.int_of_iper i in
    do {
      ascends.len := max ascends.len (i + 1);
      (fst patches.h_ascend).val := ascends.len;
      Hashtbl.replace (snd patches.h_ascend) i a;
    }
  in
  let patch_union i a =
    let i = Adef.int_of_iper i in
    do {
      unions.len := max unions.len (i + 1);
      (fst patches.h_union).val := ascends.len;
      Hashtbl.replace (snd patches.h_union) i a;
    }
  in
  let patch_family i f =
    let i = Adef.int_of_ifam i in
    do {
      families.len := max families.len (i + 1);
      (fst patches.h_family).val := families.len;
      Hashtbl.replace (snd patches.h_family) i f;
    }
  in
  let patch_couple i c =
    let i = Adef.int_of_ifam i in
    do {
      couples.len := max couples.len (i + 1);
      (fst patches.h_couple).val := couples.len;
      Hashtbl.replace (snd patches.h_couple) i c;
    }
  in
  let patch_descend i c =
    let i = Adef.int_of_ifam i in
    do {
      descends.len := max descends.len (i + 1);
      (fst patches.h_descend).val := descends.len;
      Hashtbl.replace (snd patches.h_descend) i c;
    }
  in
  let patch_string i s =
    let i = Adef.int_of_istr i in
    do {
      strings.len := max strings.len (i + 1);
      (fst patches.h_string).val := strings.len;
      Hashtbl.replace (snd patches.h_string) i s;
    }
  in
  let patch_name s ip =
    let s = Name.crush_lower s in
    let i = Hashtbl.hash s in
    try
      let ipl = Hashtbl.find (snd patches.h_name) i in
      if List.memq ip ipl then ()
      else Hashtbl.replace (snd patches.h_name) i [ip :: ipl]
    with
    [ Not_found -> Hashtbl.add (snd patches.h_name) i [ip] ]
  in
  let read_notes mlen =
    match
      try Some (Secure.open_in (Filename.concat bname "notes")) with
      [ Sys_error _ -> None ]
    with
    [ Some ic ->
        let len = ref 0 in
        do {
          try
            while mlen = 0 || len.val < mlen do {
              len.val := Buff.store len.val (input_char ic)
            }
          with
          [ End_of_file -> () ];
          close_in ic;
          Buff.get len.val
        }
    | None -> "" ]
  in
  let commit_notes s =
    let fname = Filename.concat bname "notes" in
    do {
      try Sys.remove (fname ^ "~") with [ Sys_error _ -> () ];
      try Sys.rename fname (fname ^ "~") with _ -> ();
      if s = "" then ()
      else do {
        let oc = Secure.open_out fname in output_string oc s; close_out oc; ()
      }
    }
  in
  let bnotes = {nread = read_notes; norigin_file = norigin_file} in
  let base_data =
    {persons = persons; ascends = ascends; unions = unions;
     visible = make_visible_cache bname persons;
     families = families; couples = couples; descends = descends;
     strings = strings; bnotes = bnotes}
  in
  let base_func =
    {persons_of_name = persons_of_name bname (snd patches.h_name);
     strings_of_fsname = strings_of_fsname bname strings patches.h_person;
     index_of_string =
       index_of_string strings ic2 ic2_string_start_pos ic2_string_hash_len
         (snd patches.h_string);
     persons_of_surname =
       persons_of_first_name_or_surname base_data strings
         (ic2, ic2_surname_start_pos, fun p -> p.surname, snd patches.h_person,
          "snames.inx", "snames.dat", bname);
     persons_of_first_name =
       persons_of_first_name_or_surname base_data strings
         (ic2, ic2_first_name_start_pos, fun p -> p.first_name,
          snd patches.h_person, "fnames.inx", "fnames.dat", bname);
     patch_person = patch_person; patch_ascend = patch_ascend;
     patch_union = patch_union; patch_family = patch_family;
     patch_couple = patch_couple; patch_descend = patch_descend;
     patch_string = patch_string; patch_name = patch_name;
     patched_ascends = patched_ascends; commit_patches = commit_patches;
     commit_notes = commit_notes; cleanup = cleanup}
  in
  {data = base_data; func = base_func}
;

(* Output *)

value is_prime a =
  loop 2 where rec loop b =
    if a / b < b then True else if a mod b == 0 then False else loop (b + 1)
;

value rec prime_after n = if is_prime n then n else prime_after (n + 1);

value output_strings_hash oc2 base =
  let strings_array = base.data.strings.array () in
  let taba =
    Array.create
      (min Sys.max_array_length
	 (prime_after (max 2 (10 * Array.length strings_array))))
      (-1)
  in
  let tabl = Array.create (Array.length strings_array) (-1) in
  do {
    for i = 0 to Array.length strings_array - 1 do {
      let ia = Hashtbl.hash strings_array.(i) mod Array.length taba in
      tabl.(i) := taba.(ia);
      taba.(ia) := i;
    };
    do {
      output_binary_int oc2 (Array.length taba);
      output_binary_int oc2 0;
      output_binary_int oc2 0;
      for i = 0 to Array.length taba - 1 do {
        output_binary_int oc2 taba.(i)
      };
      for i = 0 to Array.length tabl - 1 do {
        output_binary_int oc2 tabl.(i)
      };
    }
  }
;

value output_surname_index oc2 base tmp_snames_inx tmp_snames_dat =
  let module IstrTree =
    Btree.Make
      (struct type t = istr; value compare = compare_istr_fun base.data; end)
  in
  let bt = ref IstrTree.empty in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = base.data.persons.get i in
      let a = try IstrTree.find p.surname bt.val with [ Not_found -> [] ] in
      bt.val := IstrTree.add p.surname [p.cle_index :: a] bt.val
    };
    (* obsolete table: saved by compatibility with GeneWeb versions <= 4.09,
       i.e. the created database can be still read by these versions but this
       table will not be used in versions >= 4.10 *)
    output_value_no_sharing oc2 (bt.val : IstrTree.t (list iper));
    (* new table created from version >= 4.10 *)
    let oc_sn_dat = Secure.open_out_bin tmp_snames_dat in
    let bt2 =
      IstrTree.map
        (fun ipl ->
           let i = pos_out oc_sn_dat in
           do {
             output_binary_int oc_sn_dat (List.length ipl);
             List.iter
               (fun ip -> output_binary_int oc_sn_dat (Adef.int_of_iper ip))
               ipl;
             i
           })
        bt.val
    in
    close_out oc_sn_dat;
    let oc_sn_inx = Secure.open_out_bin tmp_snames_inx in
    output_value_no_sharing oc_sn_inx (bt2 : IstrTree.t int);
    close_out oc_sn_inx;
  }
;

value output_first_name_index oc2 base tmp_fnames_inx tmp_fnames_dat =
  let module IstrTree =
    Btree.Make
      (struct type t = istr; value compare = compare_istr_fun base.data; end)
  in
  let bt = ref IstrTree.empty in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = base.data.persons.get i in
      let a = try IstrTree.find p.first_name bt.val with [ Not_found -> [] ] in
      bt.val := IstrTree.add p.first_name [p.cle_index :: a] bt.val
    };
    (* obsolete table: saved by compatibility with GeneWeb versions <= 4.09,
       i.e. the created database can be still read by these versions but this
       table will not be used in versions >= 4.10 *)
    output_value_no_sharing oc2 (bt.val : IstrTree.t (list iper));
    (* new table created from version >= 4.10 *)
    let oc_fn_dat = Secure.open_out_bin tmp_fnames_dat in
    let bt2 =
      IstrTree.map
        (fun ipl ->
           let i = pos_out oc_fn_dat in
           do {
             output_binary_int oc_fn_dat (List.length ipl);
             List.iter
               (fun ip -> output_binary_int oc_fn_dat (Adef.int_of_iper ip))
               ipl;
             i
           })
        bt.val
    in
    close_out oc_fn_dat;
    let oc_fn_inx = Secure.open_out_bin tmp_fnames_inx in
    output_value_no_sharing oc_fn_inx (bt2 : IstrTree.t int);
    close_out oc_fn_inx;
  }
;

value make_name_index base =
  let t = Array.create table_size [| |] in
  let add_name key valu =
    let key = Name.crush (Name.abbrev key) in
    let i = Hashtbl.hash key mod Array.length t in
    if array_memq valu t.(i) then ()
    else t.(i) := Array.append [| valu |] t.(i)
  in
  let rec add_names ip =
    fun
    [ [] -> ()
    | [n :: nl] -> do { add_name n ip; add_names ip nl } ]
  in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = base.data.persons.get i in
      let first_name = p_first_name base p in
      let surname = p_surname base p in
      if first_name <> "?" && surname <> "?" then
        let names =
          [Name.lower (first_name ^ " " ^ surname) ::
           person_misc_names base p]
        in
        add_names p.cle_index names
      else ()
    };
    t
  }
;

value count_error computed found =
  do {
    Printf.eprintf "Count error. Computed %d. Found %d.\n" computed found;
    flush stderr;
    exit 2
  }
;

value cache_of tab =
  let c =
    {array = fun _ -> tab; get = fun []; len = Array.length tab;
     clear_array = fun _ -> ()}
  in
  do { c.get := fun i -> (c.array ()).(i); c }
;

value create_name_index oc_inx oc_inx_acc base =
  let ni = make_name_index base in
  let bpos = pos_out oc_inx in
  do {
    output_value_no_sharing oc_inx (ni : name_index_data);
    let epos = output_array_access oc_inx_acc (cache_of ni) bpos in
    if epos <> pos_out oc_inx then count_error epos (pos_out oc_inx)
    else ()
  }
;

value add_name t key valu =
  let key = Name.crush_lower key in
  let i = Hashtbl.hash key mod Array.length t in
  if array_memq valu t.(i) then ()
  else t.(i) := Array.append [| valu |] t.(i)
;

value make_strings_of_fsname base =
  let t = Array.create table_size [| |] in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let p = base.data.persons.get i in
      let first_name = p_first_name base p in
      let surname = p_surname base p in
      if first_name <> "?" then add_name t first_name p.first_name else ();
      if surname <> "?" then do {
        add_name t surname p.surname;
        List.iter (fun sp -> add_name t sp p.surname)
          (surnames_pieces surname);
      }
      else ();
      ()
    };
    t
  }
;

value create_strings_of_fsname oc_inx oc_inx_acc base =
  let t = make_strings_of_fsname base in
  let bpos = pos_out oc_inx in
  do {
    output_value_no_sharing oc_inx (t : strings_of_fsname);
    let epos = output_array_access oc_inx_acc (cache_of t) bpos in
    if epos <> pos_out oc_inx then count_error epos (pos_out oc_inx)
    else ()
  }
;

value just_copy bname what oc oc_acc =
  do {
    Printf.eprintf "*** copying %s\n" what;
    flush stderr;
    let ic =
      let ic = Secure.open_in_bin (Filename.concat bname "base") in
      do { check_magic ic; ic }
    in
    let ic_acc = Secure.open_in_bin (Filename.concat bname "base.acc") in
    let persons_len = input_binary_int ic in
    let families_len = input_binary_int ic in
    let strings_len = input_binary_int ic in
    let persons_array_pos = input_binary_int ic in
    let ascends_array_pos = input_binary_int ic in
    let unions_array_pos = input_binary_int ic in
    let families_array_pos = input_binary_int ic in
    let couples_array_pos = input_binary_int ic in
    let descends_array_pos = input_binary_int ic in
    let strings_array_pos = input_binary_int ic in
    let _ = input_value ic in
    let (beg_pos, end_pos, beg_acc_pos, array_len) =
      match what with
      [ "persons" ->
          let pos = 0 in
          (persons_array_pos, ascends_array_pos, pos, persons_len)
      | "ascends" ->
          let pos = persons_len * Iovalue.sizeof_long in
          (ascends_array_pos, unions_array_pos, pos, persons_len)
      | "unions" ->
          let pos = 2 * persons_len * Iovalue.sizeof_long in
          (unions_array_pos, families_array_pos, pos, persons_len)
      | "families" ->
          let pos = 3 * persons_len * Iovalue.sizeof_long in
          (families_array_pos, couples_array_pos, pos, families_len)
      | "couples" ->
          let pos = (3 * persons_len + families_len) * Iovalue.sizeof_long in
          (couples_array_pos, descends_array_pos, pos, families_len)
      | "descends" ->
          let pos =
            (3 * persons_len + 2 * families_len) * Iovalue.sizeof_long
          in
          (descends_array_pos, strings_array_pos, pos, families_len)
      | "strings" ->
          let pos =
            (3 * persons_len + 3 * families_len) * Iovalue.sizeof_long
          in
          (strings_array_pos, in_channel_length ic, pos, strings_len)
      | _ -> failwith ("just copy " ^ what) ]
    in
    let shift = pos_out oc - beg_pos in
    seek_in ic beg_pos;
    let rec loop pos =
      if pos = end_pos then close_in ic
      else do { output_char oc (input_char ic); loop (pos + 1) }
    in
    loop beg_pos;
    seek_in ic_acc beg_acc_pos;
    let rec loop len =
      if len = array_len then close_in ic_acc
      else do {
        output_binary_int oc_acc (input_binary_int ic_acc + shift);
        loop (len + 1)
      }
    in
    loop 0;
  }
;

value save_mem = ref False;

value gen_output no_patches bname base =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  do {
    try Unix.mkdir bname 0o755 with _ -> ();
    let tmp_base = Filename.concat bname "1base" in
    let tmp_base_acc = Filename.concat bname "1base.acc" in
    let tmp_names_inx = Filename.concat bname "1names.inx" in
    let tmp_names_acc = Filename.concat bname "1names.acc" in
    let tmp_snames_inx = Filename.concat bname "1snames.inx" in
    let tmp_snames_dat = Filename.concat bname "1snames.dat" in
    let tmp_fnames_inx = Filename.concat bname "1fnames.inx" in
    let tmp_fnames_dat = Filename.concat bname "1fnames.dat" in
    let tmp_strings_inx = Filename.concat bname "1strings.inx" in
    let tmp_notes = Filename.concat bname "1notes" in
    if not no_patches then
(*
      let _ = base.data.persons.array () in
*)
      let _ = base.data.ascends.array () in
      let _ = base.data.unions.array () in
(*
      let _ = base.data.families.array () in
*)
      let _ = base.data.couples.array () in
      let _ = base.data.descends.array () in
      let _ = base.data.strings.array () in ()
    else ();
    let oc = Secure.open_out_bin tmp_base in
    let oc_acc = Secure.open_out_bin tmp_base_acc in
    let output_array arrname arr =
      let bpos = pos_out oc in
      do {
        Printf.eprintf "*** saving %s array\n" arrname;
        flush stderr;
        match try Some (arr.array ()) with [ Failure _ -> None ] with
        [ Some a -> output_value_no_sharing oc a
	| None -> output_cache_array_no_sharing oc arr ];
        let epos = output_array_access oc_acc arr bpos in
        if epos <> pos_out oc then count_error epos (pos_out oc) else ()
      }
    in
    try
      do {
        output_string oc magic_gwb;
        output_binary_int oc base.data.persons.len;
        output_binary_int oc base.data.families.len;
        output_binary_int oc base.data.strings.len;
        let array_start_indexes = pos_out oc in
        output_binary_int oc 0;
        output_binary_int oc 0;
        output_binary_int oc 0;
        output_binary_int oc 0;
        output_binary_int oc 0;
        output_binary_int oc 0;
        output_binary_int oc 0;
        output_value_no_sharing oc base.data.bnotes.norigin_file;
        let persons_array_pos = pos_out oc in
        if not no_patches then output_array "persons" base.data.persons
        else just_copy bname "persons" oc oc_acc;
        let ascends_array_pos = pos_out oc in
        if not no_patches then () else trace "saving ascends";
        output_array "ascends" base.data.ascends;
        let unions_array_pos = pos_out oc in
        if not no_patches then output_array "unions" base.data.unions
        else just_copy bname "unions" oc oc_acc;
        let families_array_pos = pos_out oc in
        if not no_patches then output_array "families" base.data.families
        else just_copy bname "families" oc oc_acc;
        let couples_array_pos = pos_out oc in
        if not no_patches then output_array "couples" base.data.couples
        else just_copy bname "couples" oc oc_acc;
        let descends_array_pos = pos_out oc in
        if not no_patches then output_array "descends" base.data.descends
        else just_copy bname "descends" oc oc_acc;
        let strings_array_pos = pos_out oc in
        if not no_patches then output_array "strings" base.data.strings
        else just_copy bname "strings" oc oc_acc;
        seek_out oc array_start_indexes;
        output_binary_int oc persons_array_pos;
        output_binary_int oc ascends_array_pos;
        output_binary_int oc unions_array_pos;
        output_binary_int oc families_array_pos;
        output_binary_int oc couples_array_pos;
        output_binary_int oc descends_array_pos;
        output_binary_int oc strings_array_pos;
        base.data.families.clear_array ();
        base.data.descends.clear_array ();
        close_out oc;
        close_out oc_acc;
        if not no_patches then
          let oc_inx = Secure.open_out_bin tmp_names_inx in
          let oc_inx_acc = Secure.open_out_bin tmp_names_acc in
          let oc2 = Secure.open_out_bin tmp_strings_inx in
          try
            do {
              trace "create name index";
              output_binary_int oc_inx 0;
              create_name_index oc_inx oc_inx_acc base;
              base.data.ascends.clear_array ();
              base.data.unions.clear_array ();
              base.data.couples.clear_array ();
              if save_mem.val then do { trace "compacting"; Gc.compact () }
              else ();
              let surname_or_first_name_pos = pos_out oc_inx in
              trace "create strings of fsname";
              create_strings_of_fsname oc_inx oc_inx_acc base;
              seek_out oc_inx 0;
              output_binary_int oc_inx surname_or_first_name_pos;
              close_out oc_inx;
              close_out oc_inx_acc;
              if save_mem.val then do { trace "compacting"; Gc.compact () }
              else ();
              trace "create string index";
              output_strings_hash oc2 base;
              if save_mem.val then do { trace "compacting"; Gc.compact () }
              else ();
              let surname_pos = pos_out oc2 in
              trace "create surname index";
              output_surname_index oc2 base tmp_snames_inx tmp_snames_dat;
              if save_mem.val then do {
                trace "compacting"; Gc.compact ()
              }
              else ();
              let first_name_pos = pos_out oc2 in
              trace "create first name index";
	      output_first_name_index oc2 base tmp_fnames_inx tmp_fnames_dat;
              seek_out oc2 int_size;
              output_binary_int oc2 surname_pos;
              output_binary_int oc2 first_name_pos;
              let s = base.data.bnotes.nread 0 in
              if s = "" then ()
              else do {
                let oc_not = Secure.open_out tmp_notes in
                output_string oc_not s;
                close_out oc_not;
              };
              close_out oc2;
            }
          with e ->
            do {
              try close_out oc_inx with _ -> ();
              try close_out oc_inx_acc with _ -> ();
              try close_out oc2 with _ -> ();
              raise e
            }
        else ();
        trace "ok";
      }
    with e ->
      do {
        try close_out oc with _ -> ();
        try close_out oc_acc with _ -> ();
        remove_file tmp_base;
        remove_file tmp_base_acc;
        if not no_patches then do {
          remove_file tmp_names_inx;
          remove_file tmp_names_acc;
          remove_file tmp_strings_inx;
        }
        else ();
        raise e
      };
    base.func.cleanup ();
    remove_file (Filename.concat bname "base");
    Sys.rename tmp_base (Filename.concat bname "base");
    remove_file (Filename.concat bname "base.acc");
    Sys.rename tmp_base_acc (Filename.concat bname "base.acc");
    if not no_patches then do {
      remove_file (Filename.concat bname "names.inx");
      Sys.rename tmp_names_inx (Filename.concat bname "names.inx");
      remove_file (Filename.concat bname "names.acc");
      Sys.rename tmp_names_acc (Filename.concat bname "names.acc");
      remove_file (Filename.concat bname "snames.dat");
      Sys.rename tmp_snames_dat (Filename.concat bname "snames.dat");
      remove_file (Filename.concat bname "snames.inx");
      Sys.rename tmp_snames_inx (Filename.concat bname "snames.inx");
      remove_file (Filename.concat bname "fnames.dat");
      Sys.rename tmp_fnames_dat (Filename.concat bname "fnames.dat");
      remove_file (Filename.concat bname "fnames.inx");
      Sys.rename tmp_fnames_inx (Filename.concat bname "fnames.inx");
      remove_file (Filename.concat bname "strings.inx");
      Sys.rename tmp_strings_inx (Filename.concat bname "strings.inx");
      remove_file (Filename.concat bname "notes");
      if Sys.file_exists tmp_notes then
        Sys.rename tmp_notes (Filename.concat bname "notes")
      else ();
      remove_file (Filename.concat bname "patches");
      remove_file (Filename.concat bname "patches~");
      remove_file (Filename.concat bname "tstab");
      remove_file (Filename.concat bname "tstab_visitor");
      remove_file (Filename.concat bname "restrict")
    }
    else ();
  }
;

value output = gen_output False;
