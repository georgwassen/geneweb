(* camlp4r ./pa_html.cmo ./pa_lock.cmo *)
(* $Id: mergeInd.ml,v 4.26.2.1 2006-01-03 12:04:10 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;
open Util;
open Gutil;

value print_differences conf base branches p1 p2 =
  let gen_string_field chk1 chk2 str_orig title name proj =
    let x1 = proj p1 in
    let x2 = proj p2 in
    if x1 <> "" && x1 <> "?" && x2 <> "" && x2 <> "?" && x1 <> x2 then do {
      Wserver.wprint "<h4>%s</h4>\n" (capitale title);
      tag "ul" begin
        html_li conf;
        Wserver.wprint "<input type=radio name=\"%s\" value=1%s>\n" name chk1;
        Wserver.wprint "%s\n" x1;
        html_li conf;
        Wserver.wprint "<input type=radio name=\"%s\" value=2%s>\n" name chk2;
        Wserver.wprint "%s\n" x2;
      end;
    }
    else ()
  in
  let string_field = gen_string_field " checked" "" in
  tag "form" "method=POST action=\"%s\"" conf.command begin
    Util.hidden_env conf;
    Wserver.wprint "<input type=hidden name=m value=MRG_IND_OK>\n";
    Wserver.wprint "<input type=hidden name=i1 value=%d>\n"
      (Adef.int_of_iper p1.cle_index);
    Wserver.wprint "<input type=hidden name=i2 value=%d>\n"
      (Adef.int_of_iper p2.cle_index);
    loop branches where rec loop =
      fun
      [ [(ip1, ip2)] ->
          do {
            Wserver.wprint "<input type=hidden name=ini1 value=%d>\n"
              (Adef.int_of_iper ip1);
            Wserver.wprint "<input type=hidden name=ini2 value=%d>\n"
              (Adef.int_of_iper ip2);
          }
      | [_ :: branches] -> loop branches
      | _ -> () ];
    html_p conf;
    string_field True (transl_nth conf "first name/first names" 0)
      "first_name" (fun p -> p_first_name base p);
    string_field True (transl_nth conf "surname/surnames" 0) "surname"
      (fun p -> p_surname base p);
    let select_smallest_num = p_first_name base p1 = p_first_name base p2 in
    gen_string_field
      (if p1.occ < p2.occ || not select_smallest_num then " checked" else "")
      (if p1.occ > p2.occ && select_smallest_num then " checked" else "")
      False (transl conf "number") "number" (fun p -> string_of_int p.occ);
    string_field True (transl_nth conf "image/images" 0) "image"
      (fun p -> sou base p.image);
    string_field True (transl conf "public name") "public_name"
      (fun p -> sou base p.public_name);
    string_field True (transl conf "occupation") "occupation"
      (fun p -> sou base p.occupation);
    string_field False (transl conf "sex") "sex"
      (fun p ->
         match p.sex with
         [ Male -> "M"
         | Female -> "F"
         | Neuter -> "" ]);
(*
    string_field False (transl conf "access") "access"
      (fun p ->
         match p.access with
         [ IfTitles -> transl conf "if titles"
         | Private -> "private"
         | Public -> "public" ]);
*)
    string_field False (transl conf "birth") "birth"
      (fun p ->
         match Adef.od_of_codate p.birth with
         [ None -> ""
         | Some d -> Date.string_of_ondate conf d ]);
    string_field True (transl conf "birth" ^ " / " ^ transl conf "place")
      "birth_place" (fun p -> sou base p.birth_place);
    string_field False (transl conf "baptism") "baptism"
      (fun p ->
         match Adef.od_of_codate p.baptism with
         [ None -> ""
         | Some d -> Date.string_of_ondate conf d ]);
    string_field True (transl conf "baptism" ^ " / " ^ transl conf "place")
      "baptism_place" (fun p -> sou base p.baptism_place);
    string_field False (transl conf "death") "death"
      (fun p ->
         let is = 2 in
         match p.death with
         [ NotDead -> transl_nth conf "alive" is
         | Death dr cd ->
             let s =
               match dr with
               [ Killed -> transl_nth conf "killed (in action)" is
               | Murdered -> transl_nth conf "murdered" is
               | Executed -> transl_nth conf "executed (legally killed)" is
               | Disappeared -> transl_nth conf "disappeared" is
               | Unspecified -> transl_nth conf "died" is ]
             in
             s ^ " " ^ Date.string_of_ondate conf (Adef.date_of_cdate cd)
         | DeadYoung -> transl_nth conf "died young" is
         | DeadDontKnowWhen -> transl_nth conf "died" is
         | DontKnowIfDead -> "" ]);
    string_field True (transl conf "death" ^ " / " ^ transl conf "place")
      "death_place" (fun p -> sou base p.death_place);
    string_field False (transl conf "burial") "burial"
      (fun p ->
         let is = 2 in
         match p.burial with
         [ UnknownBurial -> ""
         | Buried cod ->
             transl_nth conf "buried" is ^
               (match Adef.od_of_codate cod with
                [ None -> ""
                | Some d -> " " ^ Date.string_of_ondate conf d ])
         | Cremated cod ->
             transl_nth conf "cremated" is ^
               (match Adef.od_of_codate cod with
                [ None -> ""
                | Some d -> " " ^ Date.string_of_ondate conf d ]) ]);
    string_field True (transl conf "burial" ^ " / " ^ transl conf "place")
      "burial_place" (fun p -> sou base p.burial_place);
    html_p conf;
    Wserver.wprint "<input type=submit value=Ok>\n";
  end
;

value compatible_codates cd1 cd2 =
  cd1 = cd2 || cd2 = Adef.codate_None || cd1 = Adef.codate_None;

value compatible_cdates cd1 cd2 = cd1 = cd2;

value compatible_death_reasons dr1 dr2 = dr1 = dr2 || dr2 = Unspecified;

value compatible_deaths d1 d2 =
  if d1 = d2 then True
  else
    match (d1, d2) with
    [ (Death dr1 cd1, Death dr2 cd2) ->
        compatible_death_reasons dr1 dr2 && compatible_cdates cd1 cd2
    | (Death _ _, NotDead) -> False
    | (Death _ _, _) -> True
    | (_, DontKnowIfDead) -> True
    | (DontKnowIfDead, _) -> True
    | _ -> False ]
;

value compatible_burials b1 b2 =
  if b1 = b2 then True
  else
    match (b1, b2) with
    [ (_, UnknownBurial) -> True
    | (UnknownBurial, _) -> True
    | (Buried cd1, Buried cd2) -> compatible_codates cd1 cd2
    | (Cremated cd1, Cremated cd2) -> compatible_codates cd1 cd2
    | _ -> False ]
;

value compatible_strings s1 s2 =
  s1 = s2 || s2 = Adef.istr_of_int 0 || s1 = Adef.istr_of_int 0;

value compatible_divorces d1 d2 = d1 = d2;

value compatible_relation_kinds rk1 rk2 = rk1 = rk2;

value compatible_accesses a1 a2 = (*a1 = a2*)True;

value compatible_titles t1 t2 = t1 = t2 || t2 = [];

value compatible_strings_lists sl1 sl2 = sl2 = [] || sl1 = sl2;

value compatible_ind base p1 p2 =
  p1.first_name = p2.first_name && p1.surname = p2.surname &&
  compatible_strings p1.image p2.image &&
  compatible_strings p1.public_name p2.public_name &&
  compatible_strings_lists p1.qualifiers p2.qualifiers &&
  compatible_strings_lists p1.aliases p2.aliases &&
  compatible_strings_lists p1.first_names_aliases p2.first_names_aliases &&
  compatible_strings_lists p1.surnames_aliases p2.surnames_aliases &&
  compatible_titles p1.titles p2.titles && p2.rparents = [] &&
  p2.related = [] && compatible_strings p1.occupation p2.occupation &&
  compatible_accesses p1.access p2.access &&
  compatible_codates p1.birth p2.birth &&
  compatible_strings p1.birth_place p2.birth_place &&
  compatible_codates p1.baptism p2.baptism &&
  compatible_strings p1.baptism_place p2.baptism_place &&
  compatible_deaths p1.death p2.death &&
  compatible_strings p1.death_place p2.death_place &&
  compatible_burials p1.burial p2.burial &&
  compatible_strings p1.burial_place p2.burial_place &&
  compatible_strings p1.notes p2.notes 
(*
 &&
  compatible_strings p1.psources p2.psources
*)
;

value compatible_fam base fam1 fam2 =
  compatible_codates fam1.marriage fam2.marriage &&
  compatible_strings fam1.marriage_place fam2.marriage_place &&
  Array.length fam2.witnesses = 0 &&
  compatible_relation_kinds fam1.relation fam2.relation &&
  compatible_divorces fam1.divorce fam2.divorce &&
  compatible_strings fam1.fsources fam2.fsources
;

value propose_merge_ind conf base branches p1 p2 =
  let title h =
    let s = transl_nth conf "person/persons" 1 in
    Wserver.wprint "%s" (capitale (transl_decline conf "merge" s))
  in
  do {
    header conf title;
    if branches <> [] then do {
      Wserver.wprint "%s:\n" (capitale (transl conf "you must first merge"));
      tag "ul" begin
        html_li conf;
        stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p1) begin
          Merge.print_someone conf base p1;
        end;
        Wserver.wprint "\n%s\n" (transl_nth conf "and" 0);
        stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p2) begin
          Merge.print_someone conf base p2;
        end;
        Wserver.wprint "\n";
      end;
      html_p conf;
    }
    else ();
    print_differences conf base branches p1 p2;
    if branches <> [] then do {
      html_p conf;
      Wserver.wprint "<hr>";
      html_p conf;
      Wserver.wprint "%s:\n" (capitale (transl_nth conf "branch/branches" 1));
      html_p conf;
      tag "table" begin
        List.iter
          (fun (ip1, ip2) ->
             let p1 = poi base ip1 in
             let p2 = poi base ip2 in
             do {
               tag "tr" "align=left" begin
                 tag "td" begin
                   Wserver.wprint "\n%s" (referenced_person_text conf base p1);
                   Wserver.wprint "%s" (Date.short_dates_text conf base p1);
                 end;
                 tag "td" begin
                   Wserver.wprint "\n%s" (referenced_person_text conf base p2);
                   Wserver.wprint "%s" (Date.short_dates_text conf base p2);
                 end;
               end;
             })
          [(p1.cle_index, p2.cle_index) :: branches];
      end;
    }
    else ();
    trailer conf;
  }
;

value reparent_ind base p1 p2 =
  let a1 = aoi base p1.cle_index in
  let a2 = aoi base p2.cle_index in
  match (parents a1, parents a2) with
  [ (None, Some ifam) ->
      let des = doi base ifam in
      do {
        let rec replace i =
          if des.children.(i) = p2.cle_index then
            des.children.(i) := p1.cle_index
          else replace (i + 1)
        in
        replace 0;
        set_parents a1 (Some ifam);
        set_consang a1 (Adef.fix (-1));
        base.func.patch_ascend p1.cle_index a1;
        base.func.patch_descend ifam des;
      }
  | _ -> () ]
;

value effective_merge_ind conf base p1 p2 =
  do {
    reparent_ind base p1 p2;
    let u2 = uoi base p2.cle_index in
    if Array.length u2.family <> 0 then do {
      for i = 0 to Array.length u2.family - 1 do {
        let ifam = u2.family.(i) in
        let cpl = coi base ifam in
        if p2.cle_index = (father cpl) then set_father cpl p1.cle_index
        else if p2.cle_index = (mother cpl) then set_mother cpl p1.cle_index
        else assert False;
        base.func.patch_couple ifam cpl;
      };
      let u1 = uoi base p1.cle_index in
      u1.family := Array.append u1.family u2.family;
      base.func.patch_union p1.cle_index u1;
      u2.family := [| |];
      base.func.patch_union p2.cle_index u2;
    }
    else ();
    if p2.sex <> Neuter then p1.sex := p2.sex else ();
    if p1.birth = Adef.codate_None then p1.birth := p2.birth else ();
    if p1.birth_place = Adef.istr_of_int 0 then
      p1.birth_place := p2.birth_place
    else ();
    if p1.birth_src = Adef.istr_of_int 0 then
      p1.birth_src := p2.birth_src
    else ();
    if p1.baptism = Adef.codate_None then p1.baptism := p2.baptism else ();
    if p1.baptism_place = Adef.istr_of_int 0 then
      p1.baptism_place := p2.baptism_place
    else ();
    if p1.baptism_src = Adef.istr_of_int 0 then
      p1.baptism_src := p2.baptism_src
    else ();
    if p1.death = DontKnowIfDead then p1.death := p2.death else ();
    if p1.death_place = Adef.istr_of_int 0 then
      p1.death_place := p2.death_place
    else ();
    if p1.death_src = Adef.istr_of_int 0 then
      p1.death_src := p2.death_src
    else ();
    if p1.burial = UnknownBurial then p1.burial := p2.burial else ();
    if p1.burial_place = Adef.istr_of_int 0 then
      p1.burial_place := p2.burial_place
    else ();
    if p1.burial_src = Adef.istr_of_int 0 then
      p1.burial_src := p2.burial_src
    else ();
    if p1.occupation = Adef.istr_of_int 0 then
      p1.occupation := p2.occupation
    else ();
    if p1.notes = Adef.istr_of_int 0 then p1.notes := p2.notes else ();
    UpdateIndOk.effective_del conf base p2;
    base.func.patch_person p1.cle_index p1;
    base.func.patch_person p2.cle_index p2;
  }
;

value merge_ind conf base branches ip1 ip2 changes_done =
  let p1 = poi base ip1 in
  let p2 = poi base ip2 in
  if compatible_ind base p1 p2 then do {
    effective_merge_ind conf base p1 p2; (True, True)
  }
  else do {
    propose_merge_ind conf base branches p1 p2; (False, changes_done)
  }
;

value propose_merge_fam conf base branches fam1 fam2 p1 p2 =
  let title h =
    let s = transl_nth conf "family/families" 1 in
    Wserver.wprint "%s" (capitale (transl_decline conf "merge" s))
  in
  do {
    header conf title;
    Wserver.wprint "%s:\n"
      (capitale (transl conf "you must first merge the 2 families"));
    tag "ul" begin
      html_li conf;
      stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p1) begin
        Merge.print_someone conf base p1;
      end;
      Wserver.wprint "\n%s\n" (transl conf "with");
      stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p2) begin
        Merge.print_someone conf base p2;
      end;
      Wserver.wprint "\n";
    end;
    html_p conf;
    MergeFam.print_differences conf base branches fam1 fam2;
    trailer conf;
  }
;

value effective_merge_fam conf base fam1 fam2 p1 p2 =
  let des1 = doi base fam1.fam_index in
  let des2 = doi base fam2.fam_index in
  do {
    if fam1.marriage = Adef.codate_None then fam1.marriage := fam2.marriage
    else ();
    if fam1.marriage_place = Adef.istr_of_int 0 then
      fam1.marriage_place := fam2.marriage_place
    else ();
    if fam1.marriage_src = Adef.istr_of_int 0 then
      fam1.marriage_src := fam2.marriage_src
    else ();
    if fam1.fsources = Adef.istr_of_int 0 then
      fam1.fsources := fam2.fsources
    else ();
    base.func.patch_family fam1.fam_index fam1;
    des1.children := Array.append des1.children des2.children;
    base.func.patch_descend fam1.fam_index des1;
    for i = 0 to Array.length des2.children - 1 do {
      let ip = des2.children.(i) in
      let a = aoi base ip in
      set_parents a (Some fam1.fam_index);
      base.func.patch_ascend ip a;
    };
    des2.children := [| |];
    base.func.patch_descend fam2.fam_index des2;
    UpdateFamOk.effective_del conf base fam2;
  }
;

value merge_fam conf base branches ifam1 ifam2 ip1 ip2 changes_done =
  let p1 = poi base ip1 in
  let p2 = poi base ip2 in
  let fam1 = foi base ifam1 in
  let fam2 = foi base ifam2 in
  if compatible_fam base fam1 fam2 then do {
    effective_merge_fam conf base fam1 fam2 p1 p2; (True, True)
  }
  else do {
    propose_merge_fam conf base branches fam1 fam2 p1 p2;
    (False, changes_done)
  }
;

value not_found_or_incorrect conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Wserver.wprint "%s %s %s %s %s\n" (capitale (transl conf "not found"))
      (transl conf "or") (transl conf "several answers") (transl conf "or")
      (transl conf "incorrect request");
    trailer conf;
  }
;

value same_person conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Wserver.wprint "%s\n" (capitale (transl conf "it is the same person!"));
    trailer conf;
  }
;

value different_sexes conf =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Wserver.wprint "%s.\n" (capitale (transl conf "incompatible sexes"));
    trailer conf;
  }
;

value rec try_merge conf base branches ip1 ip2 changes_done =
  let a1 = aoi base ip1 in
  let a2 = aoi base ip2 in
  let ok_so_far = True in
  let (ok_so_far, changes_done) =
    match (parents a1, parents a2) with
    [ (Some ifam1, Some ifam2) when ifam1 <> ifam2 ->
        let branches = [(ip1, ip2) :: branches] in
        let cpl1 = coi base ifam1 in
        let cpl2 = coi base ifam2 in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            if (father cpl1) = (father cpl2) then (True, changes_done)
            else
              try_merge conf base branches (father cpl1) (father cpl2)
                changes_done
          else (False, changes_done)
        in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            if (mother cpl1) = (mother cpl2) then (True, changes_done)
            else
              try_merge conf base branches (mother cpl1) (mother cpl2)
                changes_done
          else (False, changes_done)
        in
        let (ok_so_far, changes_done) =
          if ok_so_far then
            merge_fam conf base branches ifam1 ifam2 (father cpl1) (mother cpl1)
              changes_done
          else (False, changes_done)
        in
        (ok_so_far, changes_done)
    | _ -> (ok_so_far, changes_done) ]
  in
  if ok_so_far then merge_ind conf base branches ip1 ip2 changes_done
  else (False, changes_done)
;

value print_merged conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "merge done")) in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "\n%s" (referenced_person_text conf base p);
    Wserver.wprint "\n";
    trailer conf;
  }
;

value is_ancestor base ip1 ip2 =
  let visited = Array.create base.data.persons.len False in
  let rec loop ip =
    if visited.(Adef.int_of_iper ip) then False
    else if ip = ip1 then True
    else do {
      visited.(Adef.int_of_iper ip) := True;
      match parents (aoi base ip) with
      [ Some ifam ->
          let cpl = coi base ifam in loop (father cpl) || loop (mother cpl)
      | None -> False ]
    }
  in
  loop ip2
;

value error_loop conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "<strong>%s%s %s</strong>" (p_first_name base p)
      (if p.occ = 0 then "" else "." ^ string_of_int p.occ)
      (p_surname base p);
    Wserver.wprint "\n%s\n" (transl conf "would be his/her own ancestor");
    Wserver.wprint "\n";
    trailer conf;
  }
;

value print conf base =
  let p1 =
    match p_getint conf.env "i" with
    [ Some i1 -> Some (base.data.persons.get i1)
    | None -> None ]
  in
  let p2 =
    match p_getint conf.env "i2" with
    [ Some i2 -> Some (base.data.persons.get i2)
    | None ->
        match (p_getenv conf.env "select", p_getenv conf.env "n") with
        [ (Some "input" | None, Some n) ->
            let ipl = Gutil.person_ht_find_all base n in
            match ipl with
            [ [ip2] -> Some (poi base ip2)
            | _ -> None ]
        | (Some x, Some "" | None) ->
            Some (base.data.persons.get (int_of_string x))
        | _ -> None ] ]
  in
  match (p1, p2) with
  [ (Some p1, Some p2) ->
      if p1.cle_index = p2.cle_index then same_person conf
      else if p1.sex <> p2.sex && p1.sex <> Neuter && p2.sex <> Neuter then
        different_sexes conf
      else if is_ancestor base p1.cle_index p2.cle_index then
        error_loop conf base p2
      else if is_ancestor base p2.cle_index p1.cle_index then
        error_loop conf base p1
      else
(*
        let bfile = Util.base_path [] (conf.bname ^ ".gwb") in
        lock (Iobase.lock_file bfile) with
        [ Accept ->
*)
            let (ok, changes_done) =
              try_merge conf base [] p1.cle_index p2.cle_index False
            in
            do {
              if changes_done then Util.commit_patches conf base else ();
              if ok then do {
                let key =
                  (sou base p1.first_name, sou base p1.surname, p1.occ)
                in
                History.record conf base key "fp";
                print_merged conf base p1;
              }
              else ();
            }
(*
        | Refuse -> Update.error_locked conf base ]
*)
  | _ -> not_found_or_incorrect conf ]
;

(* Undocumented feature... Kill someone's ancestors *)

value rec kill_ancestors conf base included_self p nb_ind nb_fam =
  do {
    match parents (aoi base p.cle_index) with
    [ Some ifam ->
        let cpl = coi base ifam in
        do {
          kill_ancestors conf base True (poi base (father cpl)) nb_ind nb_fam;
          kill_ancestors conf base True (poi base (mother cpl)) nb_ind nb_fam;
          UpdateFamOk.effective_del conf base (foi base ifam);
          incr nb_fam;
        }
    | None -> () ];
    if included_self then do {
      let ip = p.cle_index in
      UpdateIndOk.effective_del conf base p;
      base.func.patch_person ip p;
      incr nb_ind;
    }
    else ();
  }
;

value print_killed conf base p nb_ind nb_fam =
  let title _ = Wserver.wprint "Ancestors killed" in
  do {
    Util.header conf title;
    Wserver.wprint "%s's ancestors killed.<br>\n"
      (referenced_person_title_text conf base p);
    Wserver.wprint "%d persons and %d families deleted<p>\n" nb_ind nb_fam;
    Util.trailer conf;
  }
;

value print_kill_ancestors conf base =
  match p_getenv conf.base_env "can_kill_ancestors" with
  [ Some "yes" ->
      match find_person_in_env conf base "" with
      [ Some p ->
          let key = (sou base p.first_name, sou base p.surname, p.occ) in
          let _ = Util.base_path [] (conf.bname ^ ".lck") in
          let nb_ind = ref 0 in
          let nb_fam = ref 0 in
          do {
            kill_ancestors conf base False p nb_ind nb_fam;
            Util.commit_patches conf base;
            History.record conf base key "ka";
            print_killed conf base p nb_ind.val nb_fam.val;
          }
      | None -> incorrect_request conf ]
  | _ -> incorrect_request conf ]
;
