(* camlp4r ./def.syn.cmo ./pa_html.cmo *)
(* $Id: family.ml,v 2.38.2.1 1999-10-25 04:14:41 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;
open Config;
open Util;

value person_is_std_key base p k =
  let k = Name.strip_lower k in
  if k = Name.strip_lower (p_first_name base p ^ " " ^ p_surname base p)
  then True
  else if
    List.exists (fun n -> Name.strip n = k) (person_misc_names base p)
  then True
  else False
;

value select_std_eq base pl k =
  List.fold_right
    (fun p pl -> if person_is_std_key base p k then [p :: pl] else pl)
    pl []
;

value inconnu_au_bataillon conf =
  match (p_getenv conf.env "n", p_getenv conf.env "p") with
  [ (Some nom, Some prenom) ->
      let title _ =
        Wserver.wprint "%s: \"%s %s\"" (capitale (transl conf "not found"))
          prenom nom
      in
      do header conf title; trailer conf; return ()
  | _ -> incorrect_request conf ]
;

value inconnu conf n =
  let title _ =
    Wserver.wprint "%s: \"%s\"" (capitale (transl conf "not found")) n
  in
  do header conf title; trailer conf; return ()
;

value relation_print conf base p =
  let p1 =
    match p_getint conf.senv "ei" with
    [ Some i -> do conf.senv := []; return Some (base.data.persons.get i)
    | None ->
        match find_person_in_env conf base "1" with
        [ Some p1 -> do conf.senv := []; return Some p1
        | None -> None ] ]
  in
  Relation.print conf base p p1
;

value person_selected conf base p =
  match p_getenv conf.senv "em" with
  [ Some "R" -> relation_print conf base p
  | Some mode -> incorrect_request conf
  | None -> Perso.print conf base p ]
;

value compact_list conf base xl =
  let pl =
    Sort.list
      (fun p1 p2 ->
         match
          (Adef.od_of_codate p1.birth, p1.death,
           Adef.od_of_codate p2.birth, p2.death)
         with
         [ (Some d1, _, Some d2, _) -> d1 strictement_avant d2
         | (Some d1, _, _, Death _ d2) ->
             d1 strictement_avant Adef.date_of_cdate d2
         | (_, Death _ d1, Some d2, _) ->
             Adef.date_of_cdate d1 strictement_avant d2
         | (_, Death _ d1, _, Death _ d2) ->
             Adef.date_of_cdate d1 strictement_avant Adef.date_of_cdate d2
         | (Some _, _, _, _) -> False
         | (_, Death _ _, _, _) -> False
         | (_, _, Some _, _) -> True
         | (_, _, _, Death _ _) -> True
         | _ ->
             let c =
               alphabetique (p_surname base p1) (p_surname base p2)
             in
             if c == 0 then
               let c =
                 alphabetique (p_first_name base p1) (p_first_name base p2)
               in
               if c == 0 then p1.occ > p2.occ else c > 0
             else c > 0 ])
     xl
  in
  let pl =
    List.fold_right
      (fun p pl ->
         match pl with
         [ [p1 :: _] when p.cle_index == p1.cle_index -> pl
         | _ -> [p :: pl] ])
      pl []
  in
  pl
;

value cut_words str =
  loop 0 0 where rec loop beg i =
    if i < String.length str then
      match str.[i] with
      [ ' ' ->
          if beg == i then loop (succ beg) (succ i)
          else [String.sub str beg (i - beg) :: loop (succ i) (succ i)]
      | _ -> loop beg (succ i) ]
    else if beg == i then []
    else [String.sub str beg (i - beg)]
;

value try_find_with_one_first_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match lindex n1 ' ' with
  [ Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let (list, _) =
        Some.persons_of_fsname base base.func.persons_of_surname.find
          (fun x -> x.surname) sn
      in
      let pl =
        List.fold_left
          (fun pl (_, _, ipl) ->
             List.fold_left
               (fun pl ip ->
	          let p = poi base ip in
                  let fn1 = Name.abbrev (Name.lower (sou base p.first_name)) in
                  if List.mem fn (cut_words fn1) then [p :: pl] else pl)
               pl ipl)
          [] list
      in
      pl
  | None -> [] ]
;

value find_all conf base an =
  let sosa_ref = Util.find_person_in_env conf base "z" in
  let sosa_nb = try Some (Num.of_string an) with [ Failure _ -> None ] in
  match (sosa_ref, sosa_nb) with
  [ (Some p, Some n) ->
      match Util.branch_of_sosa base p.cle_index n with
      [ Some [(ip, _) :: _] -> [poi base ip]
      | _ -> [] ]
  | _ ->
      let ipl = person_ht_find_all base an in
      let pl = List.map (poi base) ipl in
      let spl = select_std_eq base pl an in
      let pl =
        if spl = [] then
          if pl = [] then try_find_with_one_first_name conf base an else pl
        else spl
      in
      compact_list conf base pl ]
;

value precisez conf base n pl =
  let title _ = Wserver.wprint "%s : %s" n (transl conf "specify") in
  let n = Name.crush_lower n in
  let ptll =
    List.map
      (fun p ->
         let tl = ref [] in
         let add_tl t =
           tl.val :=
             let rec add_rec =
               fun
               [ [t1 :: tl1] ->
                   if t1.t_ident = t.t_ident && t1.t_place = t.t_place then
                     [t1 :: tl1]
                   else [t1 :: add_rec tl1]
               | [] -> [t] ]
             in
             add_rec tl.val
         in
         let compare_and_add t pn =
           let pn = sou base pn in
           if Name.crush_lower pn = n then add_tl t
           else
             match p.nick_names with
             [ [nn :: _] ->
                 let nn = sou base nn in
                 if Name.crush_lower (pn ^ " " ^ nn) = n then add_tl t
                 else ()
             | _ -> () ]
         in
         do List.iter
              (fun t ->
                 match (t.t_name, p.public_name) with
                 [ (Tname s, _) -> compare_and_add t s
                 | (_,  pn) when sou base pn <> "" -> compare_and_add t pn
                 | _ -> () ])
              p.titles;
         return (p, tl.val))
      pl
  in
  do header conf title;
     conf.cancel_links := False;
     Wserver.wprint "<ul>\n";
     List.iter
       (fun (p, tl) ->
          do html_li conf;
             match tl with
             [ [] -> afficher_personne_titre_referencee conf base p
             | [t :: _] ->
                 do tag "a" "href=\"%s%s\"" (commd conf) (acces conf base p)
                    begin
                      Wserver.wprint "%s" (titled_person_text conf base p t);
                    end;
                    List.iter
                      (fun t ->
                         Wserver.wprint "%s" (one_title_text conf base p t))
                      tl;
                 return () ];
             Date.afficher_dates_courtes conf base p;
             match p.sex with
             [ Female ->
                 let husbands =
                   List.fold_right
                     (fun ifam husbands ->
                        let cpl = coi base ifam in
                        let husband = poi base cpl.father in
                        if p_surname base husband <> "?" then
                          [husband :: husbands]
                        else husbands)
                     (Array.to_list p.family) []
                 in
                 match husbands with
                 [ [] -> ()
                 | [h :: hl] ->
                     do Wserver.wprint ", <em>%s "
                          (transl_nth conf "spouse" 1);
                        afficher_personne_titre conf base h;
                        List.iter
                          (fun h ->
                             do Wserver.wprint ", %s\n" (transl conf "and");
                                afficher_personne_titre conf base h;
                             return ())
                          hl;
                        Wserver.wprint "</em>\n";
                     return () ]
             | _ -> () ];
          return ())
       ptll;
     Wserver.wprint "</ul>\n";
     trailer conf;
  return ()
;

(* Make the "special" environement;
     old system: "e=..." where ... is the coded environment
     new system: "em=mode;ei=n"
   The old system is kept by compatibility. *)

value set_senv conf vm vi =
  do conf.senv := [("em", vm); ("ei", vi)];
     match p_getenv conf.env "long" with
     [ Some "on" -> conf.senv := conf.senv @ [("long", "on")]
     | _ -> () ];
     match p_getenv conf.env "marr" with
     [ Some "on" -> conf.senv := conf.senv @ [("marr", "on")]
     | _ -> () ];
     match p_getenv conf.env "spouse" with
     [ Some "on" -> conf.senv := conf.senv @ [("spouse", "on")]
     | _ -> () ];
     match p_getenv conf.env "cgl" with
     [ Some "on" -> conf.senv := conf.senv @ [("cgl", "on")]
     | _ -> () ];
  return ()
;

value make_senv conf base =
  let get x = Util.p_getenv conf.env x in
  match (get "em", get "ei", get "ep", get "en", get "eoc") with
  [ (Some vm, Some vi, _, _, _) -> set_senv conf vm vi
  | (Some vm, None, Some vp, Some vn, voco) ->
      let voc =
        match voco with
        [ Some voc -> try int_of_string voc with [ Failure _ -> 0 ]
        | None -> 0 ]
      in
      let ip =
        try person_ht_find_unique base vp vn voc with
        [ Not_found -> do incorrect_request conf; return raise Exit ]
      in
      let vi = string_of_int (Adef.int_of_iper ip) in
      set_senv conf vm vi
  | _ ->
      let e =
        match get "e" with
        [ Some s -> Util.create_env (decode_varenv s)
        | _ -> [] ]
      in
      conf.senv := List.map (fun (x, v) -> ("e" ^ x, v)) e ]
;

value family_m conf base =
  do make_senv conf base; return
  match p_getenv conf.env "m" with
  [ Some "A" ->
      match find_person_in_env conf base "" with
      [ Some p -> Ascend.print conf base p
      | _ -> inconnu_au_bataillon conf ]
  | Some "ADD_FAM" when conf.wizard ->
      UpdateFam.print_add conf base
  | Some "ADD_FAM_OK" when conf.wizard ->
      UpdateFamOk.print_add conf base
  | Some "ADD_IND" when conf.wizard ->
      UpdateInd.print_add conf base
  | Some "ADD_IND_OK" when conf.wizard ->
      UpdateIndOk.print_add conf base
  | Some "ADD_PAR" when conf.wizard ->
      UpdateFam.print_add_parents conf base
  | Some "AN" ->
      match p_getenv conf.env "v" with
      [ Some x -> Birthday.print_birth conf base (int_of_string x)
      | _ -> Birthday.print_menu_birth conf base ]
  | Some "AD" ->
      match p_getenv conf.env "v" with
      [ Some x -> Birthday.print_dead conf base (int_of_string x)
      | _ -> Birthday.print_menu_dead conf base ]
  | Some "AM" ->
      match p_getenv conf.env "v" with
      [ Some x -> Birthday.print_marriage conf base (int_of_string x)
      | _ -> Birthday.print_menu_marriage conf base ]
  | Some "AS_OK" ->
      AdvSearchOk.print conf base
  | Some "B" when conf.wizard || conf.friend ->
      BirthDeath.print_birth conf base
  | Some "C" ->
      match find_person_in_env conf base "" with
      [ Some p -> Cousins.print conf base p
      | _ -> inconnu_au_bataillon conf ]
  | Some "CAL" ->
      Date.print_calendar conf base
  | Some "CHG_CHN" when conf.wizard ->
      ChangeChildren.print conf base
  | Some "CHG_CHN_OK" when conf.wizard ->
      ChangeChildren.print_ok conf base
  | Some "D" ->
      match find_person_in_env conf base "" with
      [ Some p -> Descend.print conf base p
      | _ -> inconnu_au_bataillon conf ]
  | Some "DEL_FAM" when conf.wizard ->
      UpdateFam.print_del conf base
  | Some "DEL_FAM_OK" when conf.wizard ->
      UpdateFamOk.print_del conf base
  | Some "DEL_IND" when conf.wizard ->
      UpdateInd.print_del conf base
  | Some "DEL_IND_OK" when conf.wizard ->
      UpdateIndOk.print_del conf base
  | Some "DEL_IMAGE" when conf.wizard && conf.can_send_image ->
      SendImage.print_del conf base
  | Some "DEL_IMAGE_OK" when conf.wizard && conf.can_send_image ->
      SendImage.print_del_ok conf base
  | Some "DOC" ->
      Doc.print conf
  | Some "FORUM" -> Forum.print conf base
  | Some "FORUM_ADD" -> Forum.print_add conf base
  | Some "FORUM_ADD_OK" -> Forum.print_add_ok conf base
  | Some "H" ->
      match p_getenv conf.env "v" with
      [ Some f -> Srcfile.print conf base f
      | None -> Util.incorrect_request conf ]
  | Some "HIST" ->
      History.print conf base
  | Some "LB" when conf.wizard || conf.friend ->
      BirthDeath.print_birth conf base
  | Some "LD" when conf.wizard || conf.friend ->
      BirthDeath.print_death conf base
  | Some "LEX" -> Srcfile.print_lexicon conf base
  | Some "MRG" when conf.wizard ->
      match find_person_in_env conf base "" with
      [ Some p -> Merge.print conf base p
      | _ -> inconnu_au_bataillon conf ]
  | Some "MRG_FAM" when conf.wizard ->
      MergeFam.print conf base
  | Some "MRG_FAM_OK" when conf.wizard ->
      MergeFamOk.print_merge conf base
  | Some "MRG_MOD_FAM_OK" when conf.wizard ->
      MergeFamOk.print_mod_merge conf base
  | Some "MRG_IND" when conf.wizard ->
      MergeInd.print conf base
  | Some "MRG_IND_OK" when conf.wizard ->
      MergeIndOk.print_merge conf base
  | Some "MRG_MOD_IND_OK" when conf.wizard ->
      MergeIndOk.print_mod_merge conf base
  | Some "MOD_FAM" when conf.wizard ->
      UpdateFam.print_mod conf base
  | Some "MOD_FAM_OK" when conf.wizard ->
      UpdateFamOk.print_mod conf base
  | Some "MOD_IND" when conf.wizard ->
      UpdateInd.print_mod conf base
  | Some "MOD_IND_OK" when conf.wizard ->
      UpdateIndOk.print_mod conf base
  | Some "MOD_NOTES" when conf.wizard ->
      Notes.print_mod conf base
  | Some "MOD_NOTES_OK" when conf.wizard ->
      Notes.print_mod_ok conf base
  | Some "N" ->
      match p_getenv conf.env "v" with
      [ Some v -> Some.surname_print conf base Some.surname_not_found v
      | _ -> Alln.print_surnames conf base ]
  | Some "NG" ->
      match (p_getenv conf.env "n", p_getenv conf.env "select") with
      [ (Some n, Some "input" | None) ->
          match p_getenv conf.env "t" with
          [ Some "P" ->
              do conf.cancel_links := False; return
              Some.first_name_print conf base n
          | Some "N" ->
              do conf.cancel_links := False; return
              Some.surname_print conf base Some.surname_not_found n
          | _ ->
              if n = "" then inconnu conf n
              else
                let pl = find_all conf base n in
                match pl with
                [ [] ->
                    do conf.cancel_links := False; return
                    Some.surname_print conf base inconnu n
                | [p] -> person_selected conf base p
                | pl -> precisez conf base n pl ] ]
      | (_, Some i) ->
          relation_print conf base (base.data.persons.get (int_of_string i))
      | _ -> () ]
  | Some "NOTES" -> Notes.print conf base
  | Some "P" ->
      match p_getenv conf.env "v" with
      [ Some v -> Some.first_name_print conf base v
      | None -> Alln.print_first_names conf base ]
  | Some "R" ->
      match find_person_in_env conf base "" with
      [ Some p -> relation_print conf base p
      | _ -> inconnu_au_bataillon conf ]
  | Some "REQUEST" when conf.wizard ->
      let title _ = () in
      do header conf title;
         Wserver.wprint "<pre>\n";
         List.iter (Wserver.wprint "%s\n") conf.request;
         Wserver.wprint "</pre>\n";
         trailer conf;
      return ()
  | Some "RL" -> RelationLink.print conf base
  | Some "SND_IMAGE" when conf.wizard && conf.can_send_image ->
      SendImage.print conf base
  | Some "SND_IMAGE_OK" when conf.wizard && conf.can_send_image ->
      SendImage.print_send_ok conf base
  | Some "SWI_FAM" when conf.wizard ->
      UpdateFam.print_swi conf base
  | Some "SWI_FAM_OK" when conf.wizard ->
      UpdateFamOk.print_swi conf base
  | Some "TREE" -> Tree.print conf base
  | Some "TT" -> Title.print conf base
  | Some "U" when conf.wizard ->
      match find_person_in_env conf base "" with
      [ Some p -> Update.print conf base p
      | _ -> inconnu_au_bataillon conf ]
  | Some mode -> incorrect_request conf
  | None ->
      match find_person_in_env conf base "" with
      [ Some p -> person_selected conf base p
      | _ -> inconnu_au_bataillon conf ] ]
;

value print_no_index conf base =
  let scratch s =
    code_varenv (Name.lower (sou base s))
  in
  let get_person v =
    match try Some (int_of_string v) with [ Failure _ -> None ] with
    [ Some i ->
        if i >= 0 && i < base.data.persons.len then
          let p = base.data.persons.get i in
          let f = scratch p.first_name in
          let s = scratch p.surname in
          let oc = string_of_int p.occ in
          Some (f, s, oc)
        else None
    | None -> None ]
  in
  let env =
    let rec loop =
      fun
      [ [] -> []
      | [("opt", "no_index") :: l] -> loop l
      | [("i", v) :: l] -> new_env "i" v (fun x -> x) l
      | [("ei", v) :: l] -> new_env "ei" v (fun x -> "e" ^ x) l
      | [(k, v) :: l] when String.length k == 2 && k.[0] == 'i' ->
          let c = String.make 1 k.[1] in new_env k v (fun x -> x ^ c) l
      | [kv :: l] -> [kv :: loop l] ]
    and new_env k v c l =
      match get_person v with
      [ Some (f, s, oc) ->
          if oc = "0" then [(c "p", f); (c "n", s) :: loop l]
          else [(c "p", f); (c "n", s); (c "oc", oc) :: loop l]
      | None -> [(k, v) :: loop l] ]
    in          
    loop conf.env
  in
  let link =
    let addr =
      let pref =
        let s = Util.get_request_string conf in
        match rindex s '?' with
        [ Some i -> String.sub s 0 i
        | None -> s ]
      in
      Util.get_server_string conf ^ pref
    in
    let suff =
      List.fold_right
        (fun (x, v) s ->
           let sep = if s = "" then "" else ";" in
           x ^ "=" ^ code_varenv v ^ sep ^ s)
        [("lang", conf.lang) :: env] ""
    in
    let suff =
      if conf.cgi then "b=" ^ conf.bname ^ ";" ^ suff else suff
    in
    "http://" ^ addr ^ "?" ^ suff
  in
  let title _ = Wserver.wprint "Link to use" in
  do header conf title;
     tag "ul" begin
       html_li conf;
       tag "a" "href=\"%s\"" link begin
         Wserver.wprint "%s" link;
       end;
     end;
     print_link_to_welcome conf False;
     trailer conf;
  return ()
;

value rec except_sosa_env =
  fun
  [ [("iz" | "nz" | "pz" | "ocz", _) :: env] -> except_sosa_env env
  | [x :: env] -> [env :: except_sosa_env env]
  | [] -> [] ]
;

value extract_sosa_henv conf base =
  match find_person_in_env conf base "z" with
  [ Some p ->
      conf.henv :=
        conf.henv @ [("iz", string_of_int (Adef.int_of_iper p.cle_index))]
  | None -> () ]
;

value family conf base =
  do extract_sosa_henv conf base; return
  let r =
    match p_getenv conf.env "opt" with
    [ Some "no_index" ->
        do print_no_index conf base; return None
    | _ ->
        if except_sosa_env conf.env = [] then
          let r = Srcfile.incr_welcome_counter conf in
          do Srcfile.print_start conf base; return r
        else
          let r = Srcfile.incr_request_counter conf in
          do family_m conf base; return r ]
  in
  do Wserver.wflush (); return r
;
