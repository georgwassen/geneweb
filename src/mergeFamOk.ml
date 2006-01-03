(* camlp4r ./pa_html.cmo *)
(* $Id: mergeFamOk.ml,v 4.10.2.1 2006-01-03 12:04:10 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;
open Util;
open Gutil;

value cat_strings base is1 sep is2 =
  let n1 = sou base is1 in
  let n2 = sou base is2 in
  if n1 = "" then n2 else if n2 = "" then n1 else n1 ^ sep ^ n2
;

value merge_strings base is1 sep is2 =
  if is1 = is2 then sou base is1 else cat_strings base is1 sep is2
;

value sorp base ip =
  let p = poi base ip in
  (sou base p.first_name, sou base p.surname, p.occ, Update.Link, "")
;

value merge_witnesses base wit1 wit2 =
  let list =
    List.fold_right
      (fun wit list -> if List.mem wit list then list else [wit :: list])
      (List.map (sorp base) (Array.to_list wit1))
      (List.map (sorp base) (Array.to_list wit2))
  in
  Array.of_list list
;

value reconstitute conf base fam1 des1 fam2 des2 =
  let field name proj null =
    let x1 = proj fam1 in
    let x2 = proj fam2 in
    match p_getenv conf.env name with
    [ Some "1" -> x1
    | Some "2" -> x2
    | _ -> if null x1 then x2 else x1 ]
  in
  let fam =
    {marriage = field "marriage" (fun f -> f.marriage) ( \= Adef.codate_None);
     marriage_place =
       field "marriage_place" (fun f -> sou base f.marriage_place) ( \= "");
     marriage_src =
       merge_strings base fam1.marriage_src ", " fam2.marriage_src;
     witnesses = merge_witnesses base fam1.witnesses fam2.witnesses;
     relation = field "relation" (fun f -> f.relation) ( \= Married);
     divorce = field "divorce" (fun f -> f.divorce) ( \= NotDivorced);
     comment = merge_strings base fam1.comment ", " fam2.comment;
     origin_file = sou base fam1.origin_file;
     fsources = merge_strings base fam1.fsources ", " fam2.fsources;
     fam_index = fam1.fam_index}
  in
  let des =
    {children =
       Array.map (UpdateFam.person_key base)
         (Array.append des1.children des2.children)}
  in
  (fam, des)
;

value print_merge conf base =
  match (p_getint conf.env "i", p_getint conf.env "i2") with
  [ (Some f1, Some f2) ->
      let fam1 = base.data.families.get f1 in
      let des1 = base.data.descends.get f1 in
      let fam2 = base.data.families.get f2 in
      let des2 = base.data.descends.get f2 in
      let (sfam, sdes) = reconstitute conf base fam1 des1 fam2 des2 in
      let digest =
        Update.digest_family fam1 (base.data.couples.get f1) des1
      in
      let scpl =
        Gutil.map_couple_p conf.multi_parents (UpdateFam.person_key base)
          (coi base sfam.fam_index)
      in
      UpdateFam.print_update_fam conf base (sfam, scpl, sdes) digest
  | _ -> incorrect_request conf ]
;

value print_mod_merge_ok conf base wl cpl des =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "merge done")) in
  do {
    header conf title;
    print_link_to_welcome conf True;
    UpdateFamOk.print_family conf base wl cpl des;
    match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
    [ (Some ini1, Some ini2) ->
        let p1 = base.data.persons.get ini1 in
        let p2 = base.data.persons.get ini2 in
        do {
          Wserver.wprint "\n";
          html_p conf;
          stag "a" "href=%sm=MRG_IND;i=%d;i2=%d" (commd conf) ini1 ini2 begin
            Wserver.wprint "%s" (capitale (transl conf "continue merging"));
          end;
          Wserver.wprint "\n";
          Merge.print_someone conf base p1;
          Wserver.wprint "\n%s\n" (transl_nth conf "and" 0);
          Merge.print_someone conf base p2;
          Wserver.wprint "\n";
        }
    | _ -> () ];
    trailer conf;
  }
;

value effective_mod_merge conf base sfam scpl sdes =
  match p_getint conf.env "i2" with
  [ Some i2 ->
      let fam2 = base.data.families.get i2 in
      do {
        UpdateFamOk.effective_del conf base fam2;
        let (fam, cpl, des) =
          UpdateFamOk.effective_mod conf base sfam scpl sdes
        in
        let wl =
          UpdateFamOk.all_checks_family conf base fam cpl des
            (scpl, sdes, None (* should be Some *))
        in
        let (fn, sn, occ, _, _) =
          match p_getint conf.env "ip" with
          [ Some i when Adef.int_of_iper (mother cpl) = i -> (mother scpl)
          | _ -> (father scpl) ]
        in
        Util.commit_patches conf base;
        History.record conf base (fn, sn, occ) "ff";
        print_mod_merge_ok conf base wl cpl des;
      }
  | None -> incorrect_request conf ]
;

value print_mod_merge o_conf base =
  let conf = Update.update_conf o_conf in
  UpdateFamOk.print_mod_aux conf base (effective_mod_merge conf base)
;
