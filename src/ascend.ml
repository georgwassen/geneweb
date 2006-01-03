(* camlp4r ./def.syn.cmo ./pa_html.cmo *)
(* $Id: ascend.ml,v 4.39.2.1 2006-01-03 12:04:10 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;
open Gutil;
open Util;
open Printf;

value limit_by_list conf =
  match p_getint conf.base_env "max_anc_level" with
  [ Some x -> max 1 x
  | None -> 8 ]
;

value limit_by_tree conf =
  match p_getint conf.base_env "max_anc_tree" with
  [ Some x -> max 1 x
  | None -> 7 ]
;

value max_ancestor_level conf base ip =
(*
  let _ = base.data.ascends.array () in
  let _ = base.data.couples.array () in
*)
  let x = ref 0 in
  let mark = Array.create base.data.persons.len False in
  let rec loop level ip =
    if mark.(Adef.int_of_iper ip) then ()
    else do {
      mark.(Adef.int_of_iper ip) := True;
      x.val := max x.val level;
      match parents (aget conf base ip) with
      [ Some ifam ->
          let cpl = coi base ifam in
          do { loop (succ level) (father cpl); loop (succ level) (mother cpl) }
      | _ -> () ]
    }
  in
  do { loop 0 ip; x.val }
;

value text_to conf =
  fun
  [ 1 ->
      transl conf "specify" ^ " " ^ transl_nth conf "generation/generations" 0
  | 2 -> transl conf "to the parents"
  | 3 -> transl conf "to the grandparents"
  | 4 -> transl conf "to the great-grandparents"
  | i ->
      sprintf (ftransl conf "to the %s generation")
        (transl_nth conf "nth (generation)" i) ]
;

value text_level conf =
  fun
  [ 1 ->
      transl conf "specify" ^ " " ^ transl_nth conf "generation/generations" 0
  | 2 -> transl conf "the parents"
  | 3 -> transl conf "the grandparents"
  | 4 -> transl conf "the great-grandparents"
  | i ->
      sprintf (ftransl conf "the %s generation")
        (transl_nth conf "nth (generation)" i) ]
;

value print_choice conf base p effective_level =
  tag "form" "method=get action=\"%s\"" conf.command begin
    Util.hidden_env conf;
    Wserver.wprint "\n";
    Wserver.wprint "<input type=hidden name=m value=A>\n";
    wprint_hidden_person conf base "" p;
    tag "center" begin
      tag "select" "name=v" begin
        let rec loop i =
          if i > effective_level + 1 then ()
          else do {
            Wserver.wprint "  <option value=%d%s> %s\n" i
              (if i == 0 then " selected" else "")
              (capitale (text_to conf i));
            loop (succ i)
          }
        in
        loop 1;
      end;
      Wserver.wprint "<input type=submit value=\"Ok\">\n";
    end;
    html_br conf;
    tag "table" "border=%d width=\"90%%\"" conf.border begin
      tag "tr" "align=left" begin
        tag "td" "valign=top" begin
          Wserver.wprint
            "<input type=radio name=t value=N checked> %s (*)<br>\n"
            (capitale (transl conf "short display"));
          Wserver.wprint "<input type=radio name=t value=G> %s (*)<br>\n"
            (capitale (transl conf "long display"));
          Wserver.wprint
            "- %s <input type=checkbox name=siblings value=on checked><br>\n"
            (capitale (transl conf "siblings"));
          Wserver.wprint
            "- %s <input type=checkbox name=notes value=on checked><br>\n"
            (capitale (nominative (transl_nth conf "note/notes" 1)));
          Wserver.wprint "<input type=radio name=t value=T> %s\n"
            (capitale (transl conf "tree"));
          let limit = limit_by_tree conf in
          if effective_level < limit then ()
          else
            Wserver.wprint "(%s %d %s)\n" (transl conf "maximum") limit
              (transl_nth conf "generation/generations" 1);
          Wserver.wprint "<br>\n";
          Wserver.wprint "<input type=radio name=t value=A> %s\n"
            (capitale (transl_nth conf "male line/female line" 0));
          Wserver.wprint "<br>\n";
          Wserver.wprint "<input type=radio name=t value=C> %s\n"
            (capitale (transl_nth conf "male line/female line" 1));
          Wserver.wprint "<br>\n";
          Wserver.wprint
            "- %s <input type=checkbox name=image value=on><br>\n"
            (capitale (transl_nth conf "image/images" 1));
          Wserver.wprint
            "- %s <input name=bd size=1 maxlength=2 value=0><br>\n"
            (capitale (transl conf "border"));
          Wserver.wprint "\
<table><tr><td>-&nbsp;%s</td>
<td><input type=radio name=color value=\"\" checked></td>\n"
              (capitale (transl conf "color"));
          List.iter
            (fun c ->
               Wserver.wprint "\
<td bgcolor=%s><input type=radio name=color value=%s></td>\n" c c)
            ["FFC0C0"; "FFFFC0"; "C0FFC0"; "C0FFFF"; "C0C0FF"; "FFC0FF"];
          Wserver.wprint "</tr></table>\n";
        end;
        tag "td valign=top" begin
          Wserver.wprint "<input type=radio name=t value=L> %s\n"
            (capitale (transl_nth conf "list/list (ancestors)" 1));
          if effective_level < limit_by_list conf then ()
          else
            Wserver.wprint "(%s %d %s)\n" (transl conf "maximum")
              (limit_by_list conf)
              (transl_nth conf "generation/generations" 1);
          Wserver.wprint "<br>\n";
          Wserver.wprint "<input type=radio name=t value=H> %s\n"
            (capitale (transl conf "horizontally"));
          if effective_level < limit_by_list conf then ()
          else
            Wserver.wprint "(%s %d %s)\n" (transl conf "maximum")
              (limit_by_list conf)
              (transl_nth conf "generation/generations" 1);
          Wserver.wprint "<br>\n";
          Wserver.wprint "<input type=radio name=t value=F> %s\n"
            (capitale (transl conf "surnames list"));
          Wserver.wprint "<br>\n";
          Wserver.wprint "<input type=radio name=t value=M> %s<br>\n"
            (capitale (transl conf "missing ancestors"));
          Wserver.wprint "- %s\n" (capitale (transl conf "alphabetic order"));
          Wserver.wprint "<input type=checkbox name=al value=on><br>\n";
          Wserver.wprint "- %s\n"
            (capitale (transl conf "include missing spouses"));
          Wserver.wprint "<input type=checkbox name=ms value=on><br>\n";
          Wserver.wprint "- %s\n" (capitale (transl conf "after"));
          Wserver.wprint "<input name=after size=5 maxlength=5>\n";
          Wserver.wprint "%s\n" (capitale (transl conf "before"));
          Wserver.wprint "<input name=before size=5 maxlength=5><br>\n";
        end;
      end;
      tag "tr" "align=left" begin
        tag "td" "colspan=2 align=center" begin
          Wserver.wprint "%s\n"
            (capitale (transl conf "cancel GeneWeb links"));
          Wserver.wprint "<input type=checkbox name=cgl value=on>\n";
        end;
      end;
      tag "tr" "align=left" begin
        tag "td" "colspan=2 align=center" begin
          Wserver.wprint "(*) %s\n"
            (capitale (transl conf "only the generation selected"));
          Wserver.wprint "<input type=checkbox name=only value=on>\n";
        end;
      end;
    end;
  end
;

value display_ancestor_menu conf base p =
  let effective_level = max_ancestor_level conf base p.cle_index in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s"
      (capitale
         (transl_a_of_b conf
            (capitale (transl conf "ancestors"))
            (txt_fun raw_access conf base p)))
  in
  do {
    header conf title;
    tag "center" begin
      print_choice conf base p effective_level;
      html_p conf;
      Wserver.wprint
        (fcapitale (ftransl conf "navigation with %t as Sosa reference"))
        (fun _ ->
           do {
             conf.henv := List.remove_assoc "iz" conf.henv;
             let reference _ _ _ s =
               sprintf "<a href=\"%siz=%d\">%s</a>" (commd conf)
                 (Adef.int_of_iper p.cle_index) s
             in
             Wserver.wprint "%s"
               (gen_person_title_text reference std_access conf base p)
           });
      Wserver.wprint ".\n";
    end;
    Wserver.wprint "<br>\n";
    trailer conf
  }
;

value display_ancestor conf base p =
  do {
    Wserver.wprint "\n%s" (referenced_person_text conf base p);
    Wserver.wprint "%s" (Date.short_dates_text conf base p)
  }
;

value display_ancestors_upto conf base max_level p =
  let max_level = min (limit_by_list conf) max_level in
  let rec loop level ip =
    if level < max_level then
      let x = aget conf base ip in
      match parents x with
      [ Some ifam ->
          let cpl = coi base ifam in
          let pere = pget conf base (father cpl) in
          let mere = pget conf base (mother cpl) in
          let know_fath = know base pere in
          let know_moth = know base mere in
          if know_fath || know_moth then
            tag "ul" begin
              if know_fath then do {
                Wserver.wprint "<li type=square> ";
                display_ancestor conf base pere;
                Wserver.wprint "\n";
                loop (succ level) (father cpl)
              }
              else ();
              if know_moth then do {
                Wserver.wprint "<li type=circle> ";
                display_ancestor conf base mere;
                Wserver.wprint "\n";
                loop (succ level) (mother cpl)
              }
              else ();
            end
          else ()
      | None -> () ]
    else ()
  in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s"
      (capitale
         (transl_a_of_b conf (transl conf "ancestors")
            (txt_fun raw_access conf base p)))
  in
  do {
    header conf title;
    Wserver.wprint "%s.\n" (capitale (text_to conf max_level));
    loop 1 p.cle_index;
    trailer conf
  }
;

(* Print ancestors with numbers.
   The mark table holds the number of the ancestor after it has been
   printed or Num.zero if it has not yet been printed.
   At each generation, count and print a list of generation_person *)

type generation_person =
  [ GP_person of Num.t and iper and option ifam
  | GP_same of Num.t and Num.t and iper
  | GP_interv of option (Num.t * Num.t * option (Num.t * Num.t))
  | GP_missing of Num.t and iper ]
;

value next_generation conf base mark gpl =
  let gpl =
    List.fold_right
      (fun gp gpl ->
         match gp with
         [ GP_person n ip _ ->
             let n_fath = Num.twice n in
             let n_moth = Num.inc n_fath 1 in
             let a = aget conf base ip in
             match parents a with
             [ Some ifam ->
                 let cpl = coi base ifam in
                 [GP_person n_fath (father cpl) (Some ifam);
                  GP_person n_moth (mother cpl) (Some ifam) :: gpl]
             | None -> [GP_missing n ip :: gpl] ]
         | GP_interv None -> [gp :: gpl]
         | GP_interv (Some (n1, n2, x)) ->
             let x =
               match x with
               [ Some (m1, m2) -> Some (Num.twice m1, Num.twice m2)
               | None -> None ]
             in
             let gp = GP_interv (Some (Num.twice n1, Num.twice n2, x)) in
             [gp :: gpl]
         | _ -> gpl ])
      gpl []
  in
  let gpl =
    List.fold_left
      (fun gpl gp ->
         match gp with
         [ GP_person n ip _ ->
             let i = Adef.int_of_iper ip in
             let m = mark.(i) in
             if Num.eq m Num.zero then do { mark.(i) := n; [gp :: gpl] }
             else [GP_same n m ip :: gpl]
         | _ -> [gp :: gpl] ])
      [] gpl
  in
  List.rev gpl
;

value wpr s = Wserver.wprint "%s" s;

value print_generation_person conf base cnt gp =
  match gp with
  [ GP_person n ip _ ->
      let p = pget conf base ip in
      do {
        html_li conf;
        Num.print wpr (transl conf "(thousand separator)") n;
        Wserver.wprint " -\n";
        Wserver.wprint "%s" (referenced_person_title_text conf base p);
        Wserver.wprint "%s" (Date.short_dates_text conf base p);
        Wserver.wprint "\n";
        incr cnt
      }
  | GP_same n1 n2 ip ->
      let p = pget conf base ip in
      do {
        html_li conf;
        Num.print wpr (transl conf "(thousand separator)") n1;
        Wserver.wprint " =&gt; ";
        let s =
          let b = ref "" in
          do {
            Num.print (fun s -> b.val := b.val ^ s)
              (transl conf "(thousand separator)") n2;
            b.val
          }
        in
        Wserver.wprint "%s" (reference conf base p s);
        Wserver.wprint "\n\n"
      }
  | GP_interv None -> do { html_li conf; Wserver.wprint "...\n\n" }
  | GP_interv (Some (n1, n2, x)) ->
      do {
        html_li conf;
        Num.print wpr (transl conf "(thousand separator)") n1;
        Wserver.wprint "-";
        Num.print wpr (transl conf "(thousand separator)")
          (Num.sub n2 Num.one);
        Wserver.wprint " = ";
        match x with
        [ Some (m1, m2) ->
            do {
              Num.print wpr (transl conf "(thousand separator)") m1;
              Wserver.wprint "-";
              Num.print wpr (transl conf "(thousand separator)")
                (Num.sub m2 Num.one)
            }
        | None -> Wserver.wprint "..." ];
        Wserver.wprint "\n\n"
      }
  | _ -> () ]
;

value will_print =
  fun
  [ GP_person _ _ _ -> True
  | GP_same _ _ _ -> True
  | _ -> False ]
;

value display_ancestors_with_numbers conf base max_level p =
  let mark = Array.create base.data.persons.len Num.zero in
  let cnt = ref 0 in
  let rec generation level gpl =
    if level <= max_level then do {
      html_li conf;
      Wserver.wprint "%s %d\n"
        (capitale (transl_nth conf "generation/generations" 0)) level;
      tag "ul" begin
        List.iter (print_generation_person conf base cnt) gpl;
      end;
      let gpl = next_generation conf base mark gpl in
      if List.exists will_print gpl then generation (level + 1) gpl else ()
    }
    else ()
  in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s"
      (capitale
         (transl_a_of_b conf (transl conf "ancestors")
            (txt_fun raw_access conf base p)))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "%s.\n" (capitale (text_to conf max_level));
    tag "ul" begin
      mark.(Adef.int_of_iper p.cle_index) := Num.one;
      generation 1 [GP_person Num.one p.cle_index None];
    end;
    if cnt.val > 1 then do {
      html_p conf;
      Wserver.wprint "%s: %d %s\n" (capitale (transl conf "total")) cnt.val
        (nominative (transl_nth_def conf "person/persons" 2 1))
    }
    else ();
    trailer conf
  }
;

value link_long_txt conf n =
  let s =
    let r = ref "" in
    let print s = r.val := r.val ^ s in
    do { Num.print print (transl conf "(thousand separator)") n; r.val }
  in
  "<strong><a href=\"#" ^ Num.to_string n ^ "\">" ^ s ^ "</a></strong>"
;

value print_link_long conf n = Wserver.wprint "%s" (link_long_txt conf n);

value print_person_long_info conf base auth link p =
  do {
    List.iter
      (fun a ->
         Wserver.wprint ", %s <em>%s</em>" (nominative (transl conf "alias"))
           (sou base a))
      p.aliases;
    if link = None && List.length p.titles > 0 &&
       (p.access <> Private || conf.friend || conf.wizard) then
       do {
      Wserver.wprint ", <em>%s</em>"
        (Perso.string_of_titles conf base False (transl_nth conf "and" 0) p)
    }
    else ();
    if auth then Date.print_dates conf base p else ();
    let occu = sou base p.occupation in
    if auth && link = None && occu <> "" then Wserver.wprint ", %s" occu
    else ();
    match link with
    [ Some n ->
        if p_getenv conf.env "only" = Some "on" then ()
        else do {
          Wserver.wprint ".\n %s " (capitale (transl conf "see"));
          print_link_long conf n
        }
    | None -> () ]
  }
;

value title_reference conf base t =
  let ident = sou base t.t_ident in
  let place = sou base t.t_place in
  let s = if place = "" then ident else ident ^ " " ^ place in
  "<em>" ^
    geneweb_link conf
      ("m=TT;sm=S;t=" ^ code_varenv ident ^ ";p=" ^ code_varenv place) s ^
    "</em>"
;

value strong_referenced_person_title_text conf base p =
  if p.access <> Private || conf.friend || conf.wizard then
    match main_title base p with
    [ Some t ->
        "<strong>" ^
          reference conf base p (titled_person_text conf base p t) ^
          "</strong>,\n" ^ title_reference conf base t
    | None ->
        "<strong>" ^ reference conf base p (person_text conf base p) ^
          "</strong>" ]
  else
    "<strong>" ^ reference conf base p (person_text conf base p) ^ "</strong>"
;

value get_link all_gp ip =
  loop all_gp where rec loop =
    fun
    [ [GP_person n ip0 _ :: gpl] -> if ip = ip0 then Some n else loop gpl
    | [gp :: gpl] -> loop gpl
    | [] -> None ]
;

value print_persons_parents conf base all_gp p =
  let a = aget conf base p.cle_index in
  match parents a with
  [ Some ifam ->
      let cpl = coi base ifam in
      let string ip =
        let p = pget conf base ip in
        let link = get_link all_gp ip in
        strong_referenced_person_title_text conf base p ^
          Date.short_dates_text conf base p ^
          (match link with
           [ Some n ->
               "\n(" ^ transl conf "see" ^ " " ^ link_long_txt conf n ^ ")"
           | None -> "" ])
      in
      Wserver.wprint ", %s\n"
        (transl_a_of_gr_eq_gen_lev conf
           (transl_nth conf "son/daughter/child" (index_of_sex p.sex))
           (string (father cpl) ^ "\n" ^ transl_nth conf "and" 0 ^ "\n" ^
              string (mother cpl)))
  | None -> () ]
;

value print_marriage_long conf base all_gp auth marr_nb p ifam =
  let fam = foi base ifam in
  let ispouse = spouse p.cle_index (coi base ifam) in
  let spouse = pget conf base ispouse in
  let divorce = fam.divorce in
  let auth = auth && authorized_age conf base spouse in
  do {
    Wserver.wprint (fcapitale (relation_txt conf p.sex fam))
      (fun _ ->
         do {
           match marr_nb with
           [ Some n -> Wserver.wprint " (%d)" n
           | None -> () ];
           if auth then
             Wserver.wprint "%s" (Perso.string_of_marriage_text conf base fam)
           else ()
         });
    Wserver.wprint "\n";
    stag "strong" begin
      Wserver.wprint "%s"
        (reference conf base spouse (person_text conf base spouse));
    end;
    print_person_long_info conf base auth (get_link all_gp ispouse) spouse;
    print_persons_parents conf base all_gp spouse;
    Wserver.wprint ".\n";
    match divorce with
    [ Divorced d ->
        let d = Adef.od_of_codate d in
        do {
          Wserver.wprint "\n%s" (capitale (transl conf "divorced"));
          match d with
          [ Some d when auth ->
              Wserver.wprint " %s" (Date.string_of_ondate conf d)
          | _ -> () ];
          Wserver.wprint ".\n"
        }
    | _ -> () ]
  }
;

value person_has_notes conf base p =
  sou base p.notes <> "" || sou base p.psources <> "" ||
  sou base p.birth_src <> "" || sou base p.baptism_src <> "" ||
  sou base p.death_src <> "" || sou base p.burial_src <> "" ||
  List.exists
    (fun ifam ->
       let fam = foi base ifam in
       sou base fam.marriage_src <> "" || sou base fam.marriage_src <> "")
    (Array.to_list (uget conf base p.cle_index).family)
;
 
value has_notes conf base =
  List.exists
    (fun
     [ GP_person _ ip _ ->
         let p = pget conf base ip in
         authorized_age conf base p && person_has_notes conf base p
     | _ -> False ])
;

value print_other_marriages conf base ws all_gp auth ifamo p u =
  if ws && (ifamo = None || Array.length u.family >= 2) then
    for i = 0 to Array.length u.family - 1 do {
      if ifamo = None || ifamo <> Some u.family.(i) then
        let marr_nb = if ifamo = None then None else Some (i + 1) in
        print_marriage_long conf base all_gp auth marr_nb p u.family.(i)
      else ()
    }
  else ()
;

value print_notes_ref conf base p wn n child_n =
  if wn && authorized_age conf base p && person_has_notes conf base p then do {
    Wserver.wprint "[%s " (capitale (transl_nth conf "note/notes" 0));
    stag "strong" begin
      stag "a" "href=\"#notes-%s%s\"" (Num.to_string n) child_n begin
        Num.print wpr (transl conf "(thousand separator)") n;
        Wserver.wprint "%s" child_n;
      end;
    end;
    Wserver.wprint "].\n"
  }
  else ()
;

value print_family_long conf base ws wn all_gp ifam nth moth_nb =
  let cpl = coi base ifam in
  let des = doi base ifam in
  do {
    Wserver.wprint "<p>\n... ";
    Wserver.wprint "%s" (transl conf "having as children");
    match nth with
    [ Some (n, i) ->
        do {
          Wserver.wprint " ";
          print_link_long conf n;
          Wserver.wprint "-(X%d)" i
        }
    | None -> () ];
    Wserver.wprint ":\n<p>\n";
    let auth =
      List.for_all (fun ip -> authorized_age conf base (pget conf base ip))
        (Array.to_list des.children)
    in
    tag "ol" "type=a" begin
      for i = 0 to Array.length des.children - 1 do {
        let ipc = des.children.(i) in
        if ws || get_link all_gp ipc <> None then do {
          let pc = pget conf base ipc in
          let uc = uget conf base ipc in
          let n = get_link all_gp ipc in
          Wserver.wprint "<li>\n";
          stag "strong" begin
            if pc.surname = (pget conf base (father cpl)).surname then
              Wserver.wprint "%s"
                (referenced_person_text_without_surname conf base pc)
            else Wserver.wprint "\n%s" (referenced_person_text conf base pc);
          end;
          print_person_long_info conf base auth n pc;
          Wserver.wprint ".\n";
          match n with
          [ Some _ -> ()
          | None ->
              do {
                for i = 0 to Array.length uc.family - 1 do {
                  print_marriage_long conf base all_gp auth None pc
                    uc.family.(i)
                };
                let child_n =
                  let s = String.make 1 (Char.chr (Char.code 'a' + i)) in
                  match nth with
                  [ Some (_, i) -> "-" ^ string_of_int i ^ s
                  | None -> s ]
                in
                print_notes_ref conf base pc wn moth_nb child_n
              } ];
          if i <> Array.length des.children - 1 then
            Wserver.wprint "<br>&nbsp;\n"
          else ()
        }
        else ()
      };
    end
  }
;

value print_other_families conf base wn all_gp excl_ifam moth_nb =
  let print ip n =
    let u = uget conf base ip in
    for i = 0 to Array.length u.family - 1 do {
      let ifam = u.family.(i) in
      if ifam <> excl_ifam && Array.length (doi base ifam).children <> 0 then
        print_family_long conf base True wn all_gp ifam (Some (n, i + 1))
          moth_nb
      else ()
    }
  in
  let cpl = coi base excl_ifam in
  do { print (father cpl) (Num.sub moth_nb Num.one); print (mother cpl) moth_nb }
;

value print_generation_person_long conf base ws wn all_gp last_gen gp =
  match gp with
  [ GP_person n ip ifamo ->
      let p = pget conf base ip in
      let u = uget conf base ip in
      do {
        Wserver.wprint "<p>\n";
        match ifamo with
        [ Some ifam ->
            if not (Num.even n) then do {
              let fam = foi base ifam in
              let cpl = coi base ifam in
              let auth =
                authorized_age conf base (pget conf base (father cpl)) &&
                authorized_age conf base (pget conf base (mother cpl))
              in
              Wserver.wprint "... ";
              Wserver.wprint (relation_txt conf Male fam)
                (fun _ ->
                   if auth then
                     Wserver.wprint "%s"
                       (Perso.string_of_marriage_text conf base fam)
                   else ());
              Wserver.wprint "...\n<p>\n"
            }
            else ()
        | _ -> () ];
        stag "strong" begin
          stag "a" "name=\"%s\"" (Num.to_string n) begin
            Num.print wpr (transl conf "(thousand separator)") n;
          end;
        end;
        Wserver.wprint ":\n";
        stag "strong" begin
          Wserver.wprint "\n%s" (referenced_person_text conf base p);
        end;
        print_person_long_info conf base (authorized_age conf base p) None p;
        Wserver.wprint ".\n";
        match parents (aget conf base ip) with
        [ Some ifam ->
            let cpl = coi base ifam in
            let prec =
              if last_gen then
                (get_link all_gp (father cpl), get_link all_gp (mother cpl))
              else
                let n1 = Num.twice n in
                (Some n1, Some (Num.inc n1 1))
            in
            match prec with
            [ (Some n1, Some n2) ->
                do {
                  Wserver.wprint "%s: "
                    (capitale (nominative (transl conf "parents")));
                  print_link_long conf n1;
                  Wserver.wprint " %s " (transl_nth conf "and" 0);
                  print_link_long conf n2;
                  Wserver.wprint ".\n"
                }
            | _ -> () ]
        | None -> () ];
        print_notes_ref conf base p wn n "";
        print_other_marriages conf base ws all_gp (authorized_age conf base p)
          ifamo p u;
        match ifamo with
        [ Some ifam ->
            if not (Num.even n) then do {
              print_family_long conf base ws wn all_gp ifam None n;
              if ws then print_other_families conf base wn all_gp ifam n
              else ()
            }
            else ()
        | _ -> () ]
      }
  | GP_same n1 n2 ip ->
      do {
        Wserver.wprint "<p>\n";
        stag "strong" begin
          stag "a" "name=\"%s\"" (Num.to_string n1) begin
            Num.print wpr (transl conf "(thousand separator)") n1;
          end;
        end;
        Wserver.wprint ": %s " (transl conf "see");
        print_link_long conf n2;
        Wserver.wprint ".\n\n"
      }
  | _ -> () ]
;

value print_not_empty_src conf base new_parag first txt isrc =
  let src = sou base isrc in
  if src = "" then ()
  else do {
    if first.val then do {
      if new_parag then html_p conf else ();
      Wserver.wprint "<font size=-1><em>%s:</em></font>\n"
        (capitale (transl_nth conf "source/sources" 1))
    }
    else ();
    html_br conf;
    Wserver.wprint "-\n";
    first.val := False;
    Wserver.wprint "<font size=-1><em>%s: %s</em></font>\n" (txt ())
      (string_with_macros conf False [] src);
  }
;

value print_sources conf base new_parag p =
  let u = uget conf base p.cle_index in
  let first = ref True in
  let auth = authorized_age conf base p in
  do {
    print_not_empty_src conf base new_parag first
      (fun () -> nominative (transl_nth conf "person/persons" 0)) p.psources;
    if auth then do {
      print_not_empty_src conf base new_parag first
        (fun () -> transl_nth conf "birth" 0) p.birth_src;
      print_not_empty_src conf base new_parag first
        (fun () -> transl_nth conf "baptism" 0) p.baptism_src;
      print_not_empty_src conf base new_parag first
        (fun () -> transl_nth conf "death" 0) p.death_src;
      print_not_empty_src conf base new_parag first
        (fun () -> transl_nth conf "burial" 0) p.burial_src
    }
    else ();
    for i = 0 to Array.length u.family - 1 do {
      let fam = foi base u.family.(i) in
      if auth then
        print_not_empty_src conf base new_parag first
          (fun () ->
             nominative (transl_nth conf "marriage/marriages" 0) ^
               (if Array.length u.family == 1 then ""
                else " " ^ string_of_int (i + 1)))
          fam.marriage_src
      else ();
      print_not_empty_src conf base new_parag first
        (fun () ->
           nominative (transl_nth conf "family/families" 0) ^
             (if Array.length u.family == 1 then ""
              else " " ^ string_of_int (i + 1)))
        fam.fsources
    }
  }
;

value print_notes_for_someone conf base p n child_n =
  if authorized_age conf base p && person_has_notes conf base p then do {
    let notes = sou base p.notes in
    Wserver.wprint "<dt>\n";
    stag "strong" begin
      stag "a" "name=\"notes-%s%s\"" (Num.to_string n) child_n begin
        stag "a" "href=#%s" (Num.to_string n) begin
          Num.print wpr (transl conf "(thousand separator)") n;
          Wserver.wprint "%s" child_n;
        end;
      end;
    end;
    Wserver.wprint ": \n<dd>\n";
    Wserver.wprint "%s" (string_with_macros conf False [] notes);
    print_sources conf base (notes <> "") p;
    Wserver.wprint "<p>\n"
  }
  else ()
;

value print_notes_for_family conf base all_gp ifam nth moth_nb =
  let des = doi base ifam in
  for i = 0 to Array.length des.children - 1 do {
    let ipc = des.children.(i) in
    let pc = pget conf base ipc in
    let n = get_link all_gp ipc in
    match n with
    [ Some _ -> ()
    | None ->
        let child_n =
          let s = String.make 1 (Char.chr (Char.code 'a' + i)) in
          match nth with
          [ Some (_, i) -> "-" ^ string_of_int i ^ s
          | None -> s ]
        in
        print_notes_for_someone conf base pc moth_nb child_n ]
  }
;

value print_notes_for_other_families conf base all_gp excl_ifam moth_nb =
  let print ip n =
    let u = uget conf base ip in
    for i = 0 to Array.length u.family - 1 do {
      let ifam = u.family.(i) in
      if ifam <> excl_ifam && Array.length (doi base ifam).children <> 0 then
        print_notes_for_family conf base all_gp ifam (Some (n, i + 1)) moth_nb
      else ()
    }
  in
  let cpl = coi base excl_ifam in
  do { print (father cpl) (Num.sub moth_nb Num.one); print (mother cpl) moth_nb }
;

value print_notes conf base all_gp ws =
  fun
  [ GP_person n ip ifamo ->
      do {
        print_notes_for_someone conf base (pget conf base ip) n "";
        match ifamo with
        [ Some ifam ->
            if not (Num.even n) && ws then do {
              print_notes_for_family conf base all_gp ifam None n;
              print_notes_for_other_families conf base all_gp ifam n
            }
            else ()
        | _ -> () ]
      }
  | _ -> () ]
;

value display_ancestors_with_numbers_long conf base max_level ws wn p =
  let only = p_getenv conf.env "only" = Some "on" in
  let mark = Array.create base.data.persons.len Num.zero in
  let rec get_generations level gpll gpl =
    let gpll = [gpl :: gpll] in
    if level < max_level then
      let next_gpl = next_generation conf base mark gpl in
      if List.exists will_print next_gpl then
        get_generations (level + 1) gpll next_gpl
      else gpll
    else gpll
  in
  let rec generation level all_gp =
    fun
    [ [gpl :: gpll] ->
        do {
          if not only || level = max_level then do {
            tag "h3" begin
              Wserver.wprint "<em>%s %d</em>\n"
                (capitale (transl_nth conf "generation/generations" 0))
                level;
            end;
            List.iter
              (print_generation_person_long conf base ws wn all_gp
                 (gpll = []))
              gpl;
            Wserver.wprint "<p>";
            html_br conf
          }
          else ();
          generation (level + 1) all_gp gpll
        }
    | [] -> () ]
  in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s"
      (capitale
         (transl_a_of_b conf (transl conf "ancestors")
            (txt_fun raw_access conf base p)))
  in
  do {
    header conf title;
    if only then ()
    else Wserver.wprint "%s.\n" (capitale (text_to conf max_level));
    mark.(Adef.int_of_iper p.cle_index) := Num.one;
    let gpll1 = get_generations 1 [] [GP_person Num.one p.cle_index None] in
    let gpll = List.rev gpll1 in
    let all_gp = List.flatten gpll in
    generation 1 all_gp gpll;
    let all_gp = if only then List.hd gpll1 else all_gp in
    if wn && has_notes conf base all_gp then do {
      Wserver.wprint "<p><hr><p>\n";
      Wserver.wprint "<h3>%s</h3>\n"
        (capitale (nominative (transl_nth conf "note/notes" 1)));
      tag "dl" begin List.iter (print_notes conf base all_gp ws) all_gp; end
    }
    else ();
    trailer conf
  }
;

value print_ancestors_same_time_descendants conf base p a =
  let maxlen =
    match p_getint conf.env "l" with
    [ Some len -> len
    | None -> -1 ]
  in
  let predic =
    let tab = Array.create base.data.persons.len False in
    let rec mark_descendants len p =
      let i = Adef.int_of_iper p.cle_index in
      if maxlen > 0 && len > maxlen then ()
      else if tab.(i) then ()
      else do {
        tab.(i) := True;
        let u = uget conf base p.cle_index in
        for i = 0 to Array.length u.family - 1 do {
          let des = doi base u.family.(i) in
          for i = 0 to Array.length des.children - 1 do {
            mark_descendants (len + 1) (pget conf base des.children.(i))
          }
        }
      }
    in
    do { mark_descendants 0 a; fun ip -> tab.(Adef.int_of_iper ip) }
  in
  let will_print =
    fun
    [ GP_person _ ip _ -> predic ip
    | GP_same _ _ _ -> False
    | _ -> False ]
  in
  let mark = Array.create base.data.persons.len Num.zero in
  let cnt = ref 0 in
  let rec generation level gpl =
    if List.exists will_print gpl then do {
      html_li conf;
      Wserver.wprint "%s %d\n"
        (capitale (transl_nth conf "generation/generations" 0)) level;
      tag "ul" begin
        List.iter
          (fun gp ->
             if will_print gp then print_generation_person conf base cnt gp
             else ())
          gpl;
      end;
      let gpl = next_generation conf base mark gpl in
      generation (level + 1) gpl
    }
    else ()
  in
  let title h =
    if h then
      Wserver.wprint "%s... %s" (capitale (transl conf "ancestors"))
        (transl_decline conf "up to" "...")
    else do {
      Wserver.wprint "%s\n"
        (capitale
           (transl_a_of_b conf (transl conf "ancestors")
              (gen_person_text raw_access conf base p)));
      Wserver.wprint "%s"
        (transl_decline conf "up to" (person_text conf base a))
    }
  in
  do {
    header conf title;
    conf.senv := [];
    tag "ul" begin
      mark.(Adef.int_of_iper p.cle_index) := Num.one;
      generation 1 [GP_person Num.one p.cle_index None];
    end;
    trailer conf
  }
;

value display_ancestors_level conf base max_level p =
  let mark = Array.create base.data.persons.len Num.zero in
  let cnt = ref 0 in
  let rec generation level gpl =
    do {
      for i = 0 to base.data.persons.len - 1 do { mark.(i) := Num.zero };
      if level < max_level then
        let gpl =
          List.map
            (fun gp ->
               match gp with
               [ GP_same n m ip ->
                   GP_interv (Some (n, Num.inc n 1, Some (m, Num.inc m 1)))
               | _ -> gp ])
            gpl
        in
        let gpl = next_generation conf base mark gpl in
        let gpl =
          List.fold_right
            (fun gp gpl ->
               match (gp, gpl) with
               [ (GP_interv (Some (n1, n2, x)),
                  [GP_interv (Some (n3, n4, y)) :: gpl1]) ->
                   if Num.eq n2 n3 then
                     let z =
                       match (x, y) with
                       [ (Some (m1, m2), Some (m3, m4)) ->
                           if Num.eq m2 m3 then Some (m1, m4) else None
                       | _ -> None ]
                     in
                     [GP_interv (Some (n1, n4, z)) :: gpl1]
                   else [GP_interv None :: gpl1]
               | (GP_interv _, [GP_interv _ :: gpl]) ->
                   [GP_interv None :: gpl]
               | (GP_missing _ _, gpl) -> gpl
               | _ -> [gp :: gpl] ])
            gpl []
        in
        generation (level + 1) gpl
      else do {
        html_li conf;
        Wserver.wprint "%s\n" (capitale (text_level conf max_level));
        tag "ul" begin
          List.iter (print_generation_person conf base cnt) gpl;
        end
      }
    }
  in
  let title h =
    if h then
      Wserver.wprint "%s %d\n"
        (capitale (transl_nth conf "generation/generations" 0)) max_level
    else
      Wserver.wprint "%s"
        (capitale
           (transl_a_of_b conf (transl conf "ancestors")
              (gen_person_text raw_access conf base p)))
  in
  do {
    header conf title;
    tag "ul" begin
      mark.(Adef.int_of_iper p.cle_index) := Num.one;
      generation 1 [GP_person Num.one p.cle_index None];
    end;
    if cnt.val > 1 then do {
      html_p conf;
      Wserver.wprint "%s: %d %s\n" (capitale (transl conf "total")) cnt.val
        (nominative (transl_nth_def conf "person/persons" 2 1))
    }
    else ();
    trailer conf
  }
;

value print_generation_missing_persons conf base title sp_incl gp =
  let print_title () =
    match title.val with
    [ Some level ->
        do {
          html_li conf;
          Wserver.wprint "%s %d\n"
            (capitale (transl_nth conf "generation/generations" 0)) level;
          Wserver.wprint "<ul>\n";
          title.val := None
        }
    | _ -> () ]
  in
  match gp with
  [ GP_person n ip _ ->
      let p = pget conf base ip in
      let u = uget conf base ip in
      if sp_incl && p_first_name base p = "?" && p_surname base p = "?" then
         do {
        print_title ();
        html_li conf;
        Num.print wpr (transl conf "(thousand separator)") n;
        Wserver.wprint " -\n";
        if Array.length u.family > 0 then
          let cpl = coi base u.family.(0) in
          let (parent_name_index, conj) =
            match p.sex with
            [ Male -> (0, (mother cpl))
            | _ -> (1, (father cpl)) ]
          in
          Wserver.wprint "%s"
            (transl_a_of_b conf
               (geneweb_link conf (acces conf base p)
                  (transl_nth conf "husband/wife" parent_name_index))
               (person_title_text conf base (pget conf base conj) ^
                  Date.short_dates_text conf base (pget conf base conj)))
        else do {
          Wserver.wprint "\n%s" (referenced_person_title_text conf base p);
          Wserver.wprint "%s" (Date.short_dates_text conf base p)
        };
        Wserver.wprint "\n"
      }
      else ()
  | GP_missing n ip ->
      let p = pget conf base ip in
      if p_first_name base p = "?" && p_surname base p = "?" then ()
      else do {
        let n1 = Num.twice n in
        let n2 = Num.inc n1 1 in
        print_title ();
        html_li conf;
        Num.print wpr (transl conf "(thousand separator)") n1;
        Wserver.wprint "-";
        Wserver.wprint "%d" (Num.modl n2 10);
        Wserver.wprint " -\n";
        let s =
          referenced_person_title_text conf base p ^
            Date.short_dates_text conf base p
        in
        Wserver.wprint "%s\n"
          (if sp_incl then
             transl_a_of_b conf (nominative (transl conf "parents")) s
           else s)
      }
  | _ -> () ]
;

value one_year base p =
  match Adef.od_of_codate p.birth with
  [ Some (Dgreg d _) -> Some d.year
  | _ ->
      match Adef.od_of_codate p.baptism with
      [ Some (Dgreg d _) -> Some (year_of d)
      | _ ->
          match date_of_death p.death with
          [ Some (Dgreg d _) -> Some (year_of d)
          | _ ->
              match p.burial with
              [ Buried cod ->
                  match Adef.od_of_codate cod with
                  [ Some (Dgreg d _) -> Some (year_of d)
                  | _ -> None ]
              | Cremated cod ->
                  match Adef.od_of_codate cod with
                  [ Some (Dgreg d _) -> Some (year_of d)
                  | _ -> None ]
              | UnknownBurial -> None ] ] ] ]
;

value one_year_gp conf base =
  fun
  [ GP_person _ ip _ -> one_year base (pget conf base ip)
  | GP_same _ _ ip -> one_year base (pget conf base ip)
  | GP_interv _ -> None
  | GP_missing _ ip -> one_year base (pget conf base ip) ]
;

value print_missing_ancestors conf base v spouses_included p =
  let after = p_getint conf.env "after" in
  let before = p_getint conf.env "before" in
  let mark = Array.create base.data.persons.len Num.zero in
  let rec generation level gpl =
    if level > v + 1 then ()
    else if gpl <> [] then do {
      let title = ref (Some level) in
      let gpl_to_print =
        List.fold_left
          (fun gpl gp ->
             match (after, before) with
             [ (Some a1, Some a2) ->
                 match one_year_gp conf base gp with
                 [ Some a -> if a >= a1 && a <= a2 then [gp :: gpl] else gpl
                 | None -> gpl ]
             | (Some a1, None) ->
                 match one_year_gp conf base gp with
                 [ Some a -> if a >= a1 then [gp :: gpl] else gpl
                 | None -> gpl ]
             | (None, Some a2) ->
                 match one_year_gp conf base gp with
                 [ Some a -> if a <= a2 then [gp :: gpl] else gpl
                 | None -> gpl ]
             | (None, None) -> [gp :: gpl] ])
          [] gpl
      in
      let gpl_to_print = List.rev gpl_to_print in
      List.iter
        (print_generation_missing_persons conf base title spouses_included)
        gpl_to_print;
      if title.val = None then Wserver.wprint "</ul>\n" else ();
      let gpl = next_generation conf base mark gpl in
      generation (level + 1) gpl
    }
    else ()
  in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s"
      (capitale
         (transl_a_of_b conf
            (transl conf "missing ancestors")
            (txt_fun raw_access conf base p)))
  in
  do {
    header conf title;
    Wserver.wprint "%s" (capitale (text_to conf v));
    match after with
    [ Some a -> Wserver.wprint " %s %d" (transl conf "after") a
    | None -> () ];
    match before with
    [ Some a -> Wserver.wprint " %s %d" (transl conf "before") a
    | None -> () ];
    Wserver.wprint ".\n";
    if not spouses_included then do {
      html_br conf;
      html_br conf;
      Wserver.wprint "%s\n"
        (capitale
           (transl_a_of_b conf (nominative (transl conf "parents")) "..."))
    }
    else ();
    mark.(Adef.int_of_iper p.cle_index) := Num.one;
    tag "ul" begin generation 1 [GP_person Num.one p.cle_index None]; end;
    trailer conf
  }
;

type missing_type =
  [ A_person
  | A_surname_of_husband_of of string
  | A_surname_of_wife_of of string
  | A_husband_of
  | A_wife_of
  | A_parents_of ]
;

value add_missing conf base spouses_included list =
  fun
  [ GP_person n ip _ ->
      let p = pget conf base ip in
      let u = uget conf base ip in
      if spouses_included && p_first_name base p = "?" &&
         p_surname base p = "?" then
        if Array.length u.family > 0 then
          let cpl = coi base u.family.(0) in
          let (a, p) =
            match p.sex with
            [ Male -> (A_husband_of, pget conf base (mother cpl))
            | _ -> (A_wife_of, pget conf base (father cpl)) ]
          in
          [(a, p) :: list]
        else [(A_person, p) :: list]
      else list
  | GP_missing n ip ->
      let p = pget conf base ip in
      let u = uget conf base ip in
      if spouses_included &&
         (p_surname base p = "?" || p_surname base p = "N...") then
        if p_first_name base p = "?" then list
        else if Array.length u.family > 0 then
          let n = person_text_without_surname conf base p in
          let cpl = coi base u.family.(0) in
          let (a, p) =
            match p.sex with
            [ Male -> (A_surname_of_husband_of n, pget conf base (mother cpl))
            | _ -> (A_surname_of_wife_of n, pget conf base (father cpl)) ]
          in
          if p_surname base p = "?" then list else [(a, p) :: list]
        else [(A_parents_of, p) :: list]
      else if p_surname base p = "?" || p_surname base p = "?" then list
      else [(A_parents_of, p) :: list]
  | _ -> list ]
;

value val_of_mt =
  fun
  [ A_person -> 0
  | A_surname_of_husband_of _ -> 1
  | A_surname_of_wife_of _ -> 2
  | A_husband_of -> 3
  | A_wife_of -> 4
  | A_parents_of -> 5 ]
;

value compare base (mt1, p1) (mt2, p2) =
  let c = alphabetic (p_surname base p1) (p_surname base p2) in
  if c == 0 then
    let c = alphabetic (p_first_name base p1) (p_first_name base p2) in
    if c == 0 then
      if p1 == p2 then val_of_mt mt1 < val_of_mt mt2
      else
        match (Adef.od_of_codate p1.birth, Adef.od_of_codate p2.birth) with
        [ (Some d1, Some d2) -> d1 strictly_before d2
        | _ -> p1.occ < p2.occ ]
    else c < 0
  else c > 0
;

value print_missing_type conf =
  fun
  [ A_person -> ()
  | A_surname_of_husband_of x ->
      Wserver.wprint "%s %s"
        (transl_a_of_b conf
           (transl_nth conf "surname/surnames" 0)
           (transl_nth conf "husband/wife" 0))
        x
  | A_surname_of_wife_of x ->
      Wserver.wprint "%s %s"
        (transl_a_of_b conf
           (transl_nth conf "surname/surnames" 0)
           (transl_nth conf "husband/wife" 1))
        x
  | A_husband_of -> Wserver.wprint "%s" (transl_nth conf "husband/wife" 0)
  | A_wife_of -> Wserver.wprint "%s" (transl_nth conf "husband/wife" 1)
  | A_parents_of -> Wserver.wprint "%s" (nominative (transl conf "parents")) ]
;

value print_spouses conf base p u =
  Array.iter
    (fun ifam ->
       let fam = foi base ifam in
       let cpl = coi base ifam in
       let sp = pget conf base (spouse p.cle_index cpl) in
       if p_first_name base sp = "?" && p_surname base sp = "?" then ()
       else do {
         Wserver.wprint "\n&amp;%s\n"
           (Date.short_marriage_date_text conf base fam p sp);
         Wserver.wprint "%s" (person_title_text conf base sp);
         Wserver.wprint "%s" (Date.short_dates_text conf base sp)
       })
    u.family
;

value
  print_someone_missing conf base begin_surname spouses_incl (mt, mtl, p) =
  let href = "i=" ^ string_of_int (Adef.int_of_iper p.cle_index) in
  do {
    wprint_geneweb_link conf href (person_text_without_surname conf base p);
    Wserver.wprint "%s %s" begin_surname (person_title_text conf base p);
    Wserver.wprint "%s" (Date.short_dates_text conf base p);
    if spouses_incl then do {
      Wserver.wprint "\n=&gt; ";
      print_missing_type conf mt;
      List.iter
        (fun mt -> do { Wserver.wprint ", "; print_missing_type conf mt; () })
        mtl
    }
    else print_spouses conf base p (uget conf base p.cle_index)
  }
;

value print_alphabetic_missing conf base spouses_included (surname, list) =
  do {
    Wserver.wprint "%s " (surname_end surname);
    match list with
    [ [e] ->
        print_someone_missing conf base (surname_begin surname)
          spouses_included e
    | _ ->
        do {
          Wserver.wprint "%s\n" (surname_begin surname);
          tag "ul" begin
            List.iter
              (fun e ->
                 do {
                   html_li conf;
                   print_someone_missing conf base "" spouses_included e;
                   Wserver.wprint "\n"
                 })
              list;
          end
        } ]
  }
;

value print_missing_ancestors_alphabetically conf base v spouses_included p =
  let mark = Array.create base.data.persons.len Num.zero in
  let rec generation list level gpl =
    if level > v then list
    else if gpl <> [] then
      let list =
        List.fold_left (add_missing conf base spouses_included) list gpl
      in
      let gpl = next_generation conf base mark gpl in
      generation list (level + 1) gpl
    else list
  in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s"
      (capitale
         (transl_a_of_b conf
            (transl conf "missing ancestors")
            (txt_fun raw_access conf base p)))
  in
  let after = p_getint conf.env "after" in
  let before = p_getint conf.env "before" in
  do {
    header conf title;
    let list = generation [] 1 [GP_person Num.one p.cle_index None] in
    let list =
      List.fold_left
        (fun npl (n, p) ->
           match (after, before) with
           [ (Some a1, Some a2) ->
               match one_year base p with
               [ Some a -> if a >= a1 && a <= a2 then [(n, p) :: npl] else npl
               | None -> npl ]
           | (Some a1, None) ->
               match one_year base p with
               [ Some a -> if a >= a1 then [(n, p) :: npl] else npl
               | None -> npl ]
           | (None, Some a2) ->
               match one_year base p with
               [ Some a -> if a <= a2 then [(n, p) :: npl] else npl
               | None -> npl ]
           | (None, None) -> [(n, p) :: npl] ])
        [] list
    in
    let list = Sort.list (compare base) list in
    let list =
      List.fold_left
        (fun nell ((_, p) as elm) ->
           match nell with
           [ [(n, el) :: nell] when n = p_surname base p ->
               [(n, [elm :: el]) :: nell]
           | _ -> [(p_surname base p, [elm]) :: nell] ])
        [] list
    in
    let list =
      List.map
        (fun (n, el) ->
           let ell =
             List.fold_left
               (fun ell (a, p) ->
                  match ell with
                  [ [(a1, al, p1) :: el] when p1 == p ->
                      [(a, [a1 :: al], p) :: el]
                  | _ -> [(a, [], p) :: ell] ])
               [] el
           in
           (n, ell))
        list
    in
    let initials =
      List.fold_left
        (fun l (n, _) ->
           let i = n.[initial n] in
           match l with
           [ [] -> [i]
           | [x :: l'] -> if x = i then l else [i :: l] ])
        [] list
    in
    let print_initials = List.length initials > 3 && List.length list > 100 in
    if print_initials then do {
      html_p conf;
      List.iter
        (fun i ->
           do {
             stag "a" "href=\"#%c\"" i begin Wserver.wprint "%c" i; end;
             Wserver.wprint "\n"
           })
        (List.rev initials);
      html_p conf
    }
    else ();
    Wserver.wprint "%s" (capitale (text_to conf v));
    match after with
    [ Some a -> Wserver.wprint " %s %d" (transl conf "after") a
    | None -> () ];
    match before with
    [ Some a -> Wserver.wprint " %s %d" (transl conf "before") a
    | None -> () ];
    Wserver.wprint ".\n";
    if not spouses_included then do {
      html_br conf;
      html_br conf;
      Wserver.wprint "%s\n"
        (capitale
           (transl_a_of_b conf
              (nominative (transl conf "parents")) "..."))
    }
    else ();
    tag "ul" begin
      let _ =
        List.fold_left
          (fun prev_i ((n, _) as e) ->
             let i = n.[initial n] in
             do {
               if print_initials then
                 match prev_i with
                 [ Some pi ->
                     if i <> pi then do {
                       Wserver.wprint "</ul>\n";
                       html_li conf;
                       Wserver.wprint "<a name=\"%c\">%c</a>\n" i i;
                       Wserver.wprint "<ul>\n"
                     }
                     else ()
                 | None ->
                     do {
                       html_li conf;
                       Wserver.wprint "<a name=\"%c\">%c</a>\n" i i;
                       Wserver.wprint "<ul>\n"
                     } ]
               else ();
               html_li conf;
               print_alphabetic_missing conf base spouses_included e;
               Wserver.wprint "\n";
               Some i
             })
          None list
      in
      if print_initials then Wserver.wprint "</ul>\n" else ();
    end;
    trailer conf
  }
;

value tree_reference gv bd color conf base p s =
  if conf.cancel_links || is_hidden p then s
  else
    let im = p_getenv conf.env "image" = Some "on" in
    sprintf "<a href=\"%sm=A;t=T;v=%d;%s%s%s%s\">%s</a>"
      (commd conf) gv (acces conf base p) (if im then ";image=on" else "")
      (if bd > 0 then ";bd=" ^ string_of_int bd else "")
      (if color <> "" then ";color=" ^ color else "") s
;

type pos = [ Left | Right | Center | Alone ];

type cell = [ Cell of person and pos and bool and int | Empty ];

(* Ascendant tree:

  8 ? ? ? ? ? ? ?
   4   5   ?   7
     2       3 
         1

1) Build list of levels (t1 = True for parents flag, size 1)
   => [ [8At1 E E] [4Lt1 5Rt1 7At1] [2Lt1 3Rt1] [1Ct1] ] 

2) Enrich list of levels (parents flag, sizing)
   => [ [8At1 E E] [4Lt1 5Rf1 7Af1] [2Lt3 3Rt1] [1Ct5] ]

3) Display it
    For each cell:
      Top vertical bar if parents flag (not on top line)
      Person
      Person tree link (vertical bar) ) not on bottom line
      Horizontal line                 )

*)

value rec enrich lst1 lst2 =
  match (lst1, lst2) with
  [ (_, []) -> []
  | ([], lst) -> lst
  | (_, _) ->
      match (List.hd lst1, List.hd lst2) with
      [ (Cell _ Right _ s1, Cell p d u s2) ->
          [Cell p d u (s1 + s2 + 1) :: enrich (List.tl lst1) (List.tl lst2)]
      | (Cell _ Left _ s, Cell p d u _) ->
          enrich (List.tl lst1) [Cell p d u s :: List.tl lst2]
      | (Cell _ _ _ s, Cell p d u _) ->
          [Cell p d u s :: enrich (List.tl lst1) (List.tl lst2)]
      | (Empty, Cell p d _ s) ->
          [Cell p d False s :: enrich (List.tl lst1) (List.tl lst2)]
      | (_, Empty) -> [Empty :: enrich (List.tl lst1) (List.tl lst2)] ] ]
;
    
value is_empty lst =
  List.fold_left (fun test po -> test && po == Empty) True lst
;
     
value rec enrich_tree lst =
  match lst with
  [ [] -> []
  | [head :: tail] ->
      if is_empty head then enrich_tree tail
      else
        match tail with
        [ [] -> [head]
        | [thead :: ttail] ->
            [head :: enrich_tree [enrich head thead :: ttail]] ] ]
;

(* print_tree_with_table:
    conf: configuration parameters
    base: base name
    gv: number of generations
    p: person *)
value print_tree_with_table conf base gv p =
  let gv = min (limit_by_tree conf) gv in
  let bd = match p_getint conf.env "bd" with [ Some x -> x | None -> 0 ] in
  let color =
    match Util.p_getenv conf.env "color" with
    [ None -> ""
    | Some x -> x ]
  in
  let td_prop =
    match Util.p_getenv conf.env "td" with
    [ Some x -> " " ^ x
    | _ -> if color = "" then "" else " bgcolor=" ^ color ]
  in
  let next_gen pol =
    List.fold_right
      (fun po list ->
         match po with
         [ Empty -> [Empty :: list]
         | Cell p _ _ _ ->
             match parents (aget conf base p.cle_index) with
             [ Some ifam ->
                 let cpl = coi base ifam in
                 let fath =
                   let p = pget conf base (father cpl) in
                   if know base p then Some p else None
                 in
                 let moth =
                   let p = pget conf base (mother cpl) in
                   if know base p then Some p else None
                 in
                 match (fath, moth) with
                 [ (Some f, Some m) ->
                     [Cell f Left True 1; Cell m Right True 1 :: list]
                 | (Some f, None) -> [Cell f Alone True 1 :: list]
                 | (None, Some m) -> [Cell m Alone True 1 :: list]
                 | (None, None) -> [Empty :: list] ]
             | _ -> [Empty :: list] ] ])
      pol []
  in
  let gen =
    loop (gv - 1) [Cell p Center True 1] [] where rec loop i gen list =
      if i == 0 then [gen :: list]
      else loop (i - 1) (next_gen gen) [gen :: list]
  in
  let gen = enrich_tree gen in
  let down_reference p s =
    if conf.cancel_links then s else reference conf base p s
  in
  let colspan =
    fun
    [ Empty | Cell _ _ _ 1 -> ""
    | Cell _ _ _ s -> " colspan=" ^ string_of_int s ]
  in
  let align =
    fun
    [ Cell _ Center _ _ | Cell _ Alone _ _ -> "align=center"
    | Cell _ Left _ _ -> "align=right"
    | _ -> "" ]
  in
  let print_ancestor_link gen first po =
    do {
      if not first then Wserver.wprint "<td>&nbsp;&nbsp;</td>\n" else ();
      stag "td" "align=center%s" (colspan po) begin
        let txt =
          match po with
          [ Empty -> "&nbsp;"
          | Cell p _ _ _ -> tree_reference gv bd color conf base p "|" ]
        in
        Wserver.wprint "%s" txt;
      end;
      Wserver.wprint "\n"
    }
  in
  let print_ancestor gen first po =
    do {
      if not first then Wserver.wprint "<td>&nbsp;&nbsp;</td>\n" else ();
      stag "td" "align=center%s" (colspan po) begin
        let txt =
          match po with
          [ Empty -> "&nbsp;"
          | Cell p _ _ _ ->
              let txt = person_title_text conf base p in
              let txt = down_reference p txt in
              let txt = txt ^ Date.short_dates_text conf base p in
              let txt =
                if bd > 0 || td_prop <> "" then
                  sprintf
                    "<table border=%d><tr>\
                     <td align=center%s>%s</td></tr></table>"
                    bd td_prop txt
                else txt
              in
              txt ^ Dag.image_txt conf base p ]
        in
        Wserver.wprint "%s" txt;
      end;
      Wserver.wprint "\n"
    }
  in
  let print_vertical_bars gen first po =
    do {
      if not first then Wserver.wprint "<td>&nbsp;&nbsp;</td>\n" else ();
      stag "td" "align=center%s" (colspan po) begin
        let txt =
          match po with
          [ Empty | Cell _ _ False _ -> "&nbsp;"
          | _ -> "|" ]
        in
        Wserver.wprint "%s" txt;
      end;
      Wserver.wprint "\n"
    }
  in
  let print_horizontal_line gen first po =
    do {
      if not first then do {
        stag "td" "%s" (align po) begin
          let txt =
            match po with
            [ Cell _ Right _ _ | Cell _ Center _ _ ->
                "<hr noshade size=1 width=\"100%\">"
            | _ -> "&nbsp;" ]
          in
          Wserver.wprint "%s" txt;
        end;
        Wserver.wprint "\n"
      }
      else ();
      stag "td" "%s%s" (align po) (colspan po) begin
        let txt =
          match po with
          [ Empty -> "&nbsp;"
          | Cell _ Left _ _ ->
              "<hr noshade size=1 width=\"50%\" align=right>"
          | Cell _ Right _ _ ->
              "<hr noshade size=1 width=\"50%\" align=left>"
          | Cell _ Alone _ _ -> "|"
          | Cell _ Center _ _ -> "<hr noshade size=1>" ]
        in
        Wserver.wprint "%s" txt;
      end;
      Wserver.wprint "\n"
    }
  in
  tag "table" "border=%d cellspacing=0 cellpadding=0 width=\"100%%\""
    conf.border
  begin
    list_iter_first
      (fun firstline gen ->
         do {
           if not firstline then
             tag "tr" "align=left" begin
               list_iter_first
                 (fun first po -> print_vertical_bars gen first po) gen;
             end
           else ();
           tag "tr" "align=left" begin
             list_iter_first (fun first po -> print_ancestor gen first po)
               gen;
           end;
           match gen with
           [ [Cell _ Center _ _ :: _] -> ()
           | _ ->
               do {
                 tag "tr" "align=left" begin
                   list_iter_first
                     (fun first po -> print_ancestor_link gen first po) gen;
                 end;
                 tag "tr" "align=left" begin
                   list_iter_first
                     (fun first po -> print_horizontal_line gen first po) gen;
                 end
               } ]
         })
      gen;
  end
;

value print_normal_tree conf base v p =
  let title _ =
    Wserver.wprint "%s: %s" (capitale (transl conf "tree"))
      (person_text_no_html conf base p)
  in
  do {
    header_no_page_title conf title;
    Wserver.wprint "<div align=right><a href=\"%s" (commd conf);
    List.iter (fun (k, v) -> Wserver.wprint "%s=%s;" k v) conf.env;
    Wserver.wprint "dag=on;notab=on;slices=on";
    Wserver.wprint "\"><tt>//</tt></a></div>\n";
    print_tree_with_table conf base v p;
    trailer conf
  }
;

value print_tree conf base v p =
  let v = min (limit_by_tree conf) v in
  if p_getenv conf.env "dag" = Some "on" ||
     browser_doesnt_have_tables conf then
    let set =
      loop Dag.Pset.empty v p.cle_index where rec loop set lev ip =
        let set = Dag.Pset.add ip set in
        if lev <= 1 then set
        else
          match parents (aget conf base ip) with
          [ Some ifam ->
              let cpl = coi base ifam in
              let set = loop set (lev - 1) (mother cpl) in
              loop set (lev - 1) (father cpl)
          | None -> set ]
    in
    let d = Dag.make_dag conf base (Dag.Pset.elements set) in
    Dag.gen_print_dag conf base False True set [] d
  else print_normal_tree conf base v p
;

value no_spaces s =
  loop 0 0 where rec loop len i =
    if i == String.length s then Buff.get len
    else
      let len =
        match s.[i] with
        [ ' ' -> Buff.mstore len "&nbsp;"
        | x -> Buff.store len s.[i] ]
      in
      loop len (i + 1)
;

value htree_reference gv conf base p s =
  if conf.cancel_links || is_hidden p then s
  else
    "<a href=\"" ^ commd conf ^ "m=A;t=H;v=" ^ string_of_int gv ^ ";" ^
      acces conf base p ^ "\">" ^ s ^ "</a>"
;

value print_horizontally conf base max_level p =
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s"
      (capitale
         (transl_a_of_b conf (transl conf "ancestors")
            (txt_fun raw_access conf base p)))
  in
  let max_level = min (limit_by_list conf) max_level in
  let suff1 = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" in
  let suff2 = "&nbsp;+--&nbsp;" in
  let suff3 = "&nbsp;|&nbsp;&nbsp;&nbsp;" in
  let rec loop level s1 s2 s3 ip =
    if level >= max_level then ()
    else do {
      match parents (aget conf base ip) with
      [ Some ifam ->
          loop (level + 1) (s1 ^ suff1) (s1 ^ suff2) (s1 ^ suff3)
            (father (coi base ifam))
      | None -> () ];
      Wserver.wprint "<tt>%s</tt>" s2;
      let p = pget conf base ip in
      let ref = if level = 0 then reference else htree_reference max_level in
      Wserver.wprint "%s%s<br>\n"
        (ref conf base p (no_spaces (person_text conf base p)))
        (no_spaces (Date.short_dates_text conf base p));
      match parents (aget conf base ip) with
      [ Some ifam ->
          loop (level + 1) (s3 ^ suff3) (s3 ^ suff2) (s3 ^ suff1)
            (mother (coi base ifam))
      | None -> () ]
    }
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "%s.\n" (capitale (text_to conf max_level));
    Wserver.wprint "<table><tr><td nowrap>\n";
    let suff13 = "&nbsp;&nbsp;&nbsp;" in
    let suff2 = "--&nbsp;" in
    loop 0 suff13 suff2 suff13 p.cle_index;
    Wserver.wprint "</td></tr></table>\n";
    trailer conf
  }
;

value print_male_female_line male conf base v p =
  let list =
    loop [] v p.cle_index where rec loop list lev ip =
      let list = [ip :: list] in
      if lev <= 1 then list
      else
        match parents (aget conf base ip) with
        [ Some ifam ->
            let cpl = coi base ifam in
            loop list (lev - 1) (if male then (father cpl) else (mother cpl))
        | None -> list ]
  in
  let title _ =
    Wserver.wprint "%s: %s"
      (capitale
         (transl_nth conf "male line/female line" (if male then 0 else 1)))
      (person_text_no_html conf base p)
  in
  do {
    header_no_page_title conf title;
    tag "center" begin
      list_iter_first
        (fun first ip ->
           let p = pget conf base ip in
           do {
             if not first then Wserver.wprint "|<br>\n" else ();
             Wserver.wprint "%s\n%s<br>\n"
               (referenced_person_title_text conf base p)
               (Date.short_dates_text conf base p);
             Wserver.wprint "%s" (Dag.image_txt conf base p)
           })
        list;
    end;
    trailer conf
  }
;

value print_male_line = print_male_female_line True;
value print_female_line = print_male_female_line False;

(* Surnames list *)

value get_date_place conf base auth_for_all_anc p =
  if auth_for_all_anc || authorized_age conf base p then
    let d1 =
      match Adef.od_of_codate p.birth with
      [ None -> Adef.od_of_codate p.baptism
      | x -> x ]
    in
    let d1 =
      if d1 <> None then d1
      else
        List.fold_left
          (fun d ifam ->
             if d <> None then d
             else Adef.od_of_codate (foi base ifam).marriage)
          d1 (Array.to_list (uget conf base p.cle_index).family)
    in
    let d2 =
      match p.death with
      [ Death _ cd -> Some (Adef.date_of_cdate cd)
      | _ ->
          match p.burial with
          [ Buried cod -> Adef.od_of_codate cod
          | Cremated cod -> Adef.od_of_codate cod
          | _ -> None ] ]
    in
    let auth_for_all_anc =
      if auth_for_all_anc then True
      else
        match d2 with
        [ Some (Dgreg d _)
          when (time_gone_by d conf.today).year > conf.private_years ->
            True
        | _ -> False ]
    in
    let pl =
      let pl = "" in
      let pl = if pl <> "" then pl else sou base p.birth_place in
      let pl = if pl <> "" then pl else sou base p.baptism_place in
      let pl = if pl <> "" then pl else sou base p.death_place in
      let pl = if pl <> "" then pl else sou base p.burial_place in
      let pl =
        if pl <> "" then pl
        else
          List.fold_left
            (fun pl ifam ->
               if pl <> "" then pl
               else sou base (foi base ifam).marriage_place)
            pl (Array.to_list (uget conf base p.cle_index).family)
      in
      pl
    in
    ((d1, d2, pl), auth_for_all_anc)
  else ((None, None, ""), False)
;

value merge_date_place conf base surn ((d1, d2, pl), auth) p =
  let ((pd1, pd2, ppl), auth) = get_date_place conf base auth p in
  let nd1 =
    if pd1 <> None then pd1
    else if p.surname = surn then if pd2 <> None then pd2 else d1
    else None
  in
  let nd2 =
    if p.surname = surn then
      if d2 <> None then d2
      else if d1 <> None then d1
      else if pd1 <> None then pd2
      else pd1
    else if pd2 <> None then pd2
    else if pd1 <> None then pd1
    else d1
  in
  let pl = if ppl <> "" then ppl else if p.surname = surn then pl else "" in
  ((nd1, nd2, pl), auth)
;

value build_surnames_list conf base v p =
  let ht = Hashtbl.create 701 in
  let mark = Array.create base.data.persons.len 5 in
  let auth = conf.wizard || conf.friend in
  let add_surname sosa p surn dp =
    let r =
      try Hashtbl.find ht surn with
      [ Not_found ->
          let r = ref ((fst dp, p), []) in
          do { Hashtbl.add ht surn r; r } ]
    in
    r.val := (fst r.val, [sosa :: snd r.val])
  in
  let rec loop lev sosa p surn dp =
    if mark.(Adef.int_of_iper p.cle_index) = 0 then ()
    else if lev = v then
      if conf.hide_names && not (fast_auth_age conf p) then ()
      else add_surname sosa p surn dp
    else do {
      mark.(Adef.int_of_iper p.cle_index) :=
        mark.(Adef.int_of_iper p.cle_index) - 1;
      match parents (aget conf base p.cle_index) with
      [ Some ifam ->
          let cpl = coi base ifam in
          let fath = pget conf base (father cpl) in
          let moth = pget conf base (mother cpl) in
          do {
            if surn <> fath.surname && surn <> moth.surname then
              add_surname sosa p surn dp
            else ();
            let sosa = Num.twice sosa in
            if not (is_hidden fath) then
              let dp1 = merge_date_place conf base surn dp fath in
              loop (lev + 1) sosa fath fath.surname dp1
            else ();
            let sosa = Num.inc sosa 1 in
            if not (is_hidden moth) then
              let dp2 = merge_date_place conf base surn dp moth in
              loop (lev + 1) sosa moth moth.surname dp2
            else ();
          }
      | None -> add_surname sosa p surn dp ]
    }
  in
  do {
    loop 0 Num.one p p.surname (get_date_place conf base auth p);
    let list = ref [] in
    Hashtbl.iter
      (fun i dp ->
         let surn = sou base i in
         if surn <> "?" then list.val := [(surn, dp.val) :: list.val] else ())
      ht;
    Sort.list (fun (s1, _) (s2, _) -> Gutil.alphabetic s1 s2 <= 0) list.val
  }
;

value print_surnames_list conf base v p =
  let title h =
    do {
      if not h then Wserver.wprint "%s<br>" (person_text conf base p) else ();
      Wserver.wprint "- %s -" (capitale (transl conf "surnames list"))
    }
  in
  let list = build_surnames_list conf base v p in
  let with_tab =
    not (Util.browser_doesnt_have_tables conf) &&
    p_getenv conf.env "tab" = Some "on"
  in
  do {
    Util.header conf title;
    Util.print_link_to_welcome conf True;
    if with_tab then Wserver.wprint "<br>\n" else ();
    Wserver.wprint "<%s>\n"
      (if with_tab then "table border=1 width=90%%" else "ul");
    List.iter
      (fun (surn, (((d1, d2, pl), anc), sosa_list)) ->
         let d2 = if d2 = d1 then None else d2 in
         do {
           Wserver.wprint "<%s>\n" (if with_tab then "tr><td" else "li");
           if Util.browser_doesnt_have_tables conf then
             let sosa = List.hd (List.rev sosa_list) in
             wprint_geneweb_link conf
               ("m=RL;" ^ acces conf base anc ^ ";" ^
                  acces_n conf base "1" p ^ ";" ^ acces_n conf base "2" anc ^
                  ";b1=" ^ Num.to_string sosa ^ ";b2=1")
               (surname_end surn ^ surname_begin surn)
           else
             let (str, _) =
               List.fold_right
                 (fun sosa (str, n) ->
                    let str =
                      str ^ ";s" ^ string_of_int n ^ "=" ^ Num.to_string sosa
                    in
                    (str, n + 1))
                 sosa_list ("", 1)
             in
             wprint_geneweb_link conf
               ("m=DAG;" ^ acces_n conf base "1" p ^ str)
               (surname_end surn ^ surname_begin surn);
           if conf.cancel_links then ()
           else
             let comm =
               match List.length sosa_list with
               [ 1 -> ""
               | n -> " (" ^ string_of_int n ^ ")" ]
             in
             Wserver.wprint "%s" comm;
           Wserver.wprint "%s&nbsp;%s%s" (if with_tab then "<td>" else ";") pl
             (if with_tab then "<td>" else ";");
           Wserver.wprint "&nbsp;";
           match d1 with
           [ Some (Dgreg d _) -> Wserver.wprint "%d" d.year
           | Some (Dtext s) -> Wserver.wprint "%s" s
           | None -> () ];
           if d1 <> None && d2 <> None then Wserver.wprint "-" else ();
           match d2 with
           [ Some (Dgreg d _) -> Wserver.wprint "%d" d.year
           | Some (Dtext s) -> Wserver.wprint "%s" s
           | None -> () ];
           Wserver.wprint "\n"
         })
      list;
    Wserver.wprint "</%s>\n" (if with_tab then "table" else "ul");
    Util.trailer conf
  }
;

value print conf base p =
  match (p_getenv conf.env "t", p_getint conf.env "v") with
  [ (Some "L", Some v) -> display_ancestors_upto conf base v p
  | (Some "N", Some v) ->
      if p_getenv conf.env "only" = Some "on" then
        display_ancestors_level conf base v p
      else display_ancestors_with_numbers conf base v p
  | (Some "G", Some v) ->
      let ws =
        match p_getenv conf.env "siblings" with
        [ Some "on" -> True
        | _ -> False ]
      in
      let wn =
        match p_getenv conf.env "notes" with
        [ Some "on" -> True
        | _ -> False ]
      in
      display_ancestors_with_numbers_long conf base v ws wn p
  | (Some "M", Some v) ->
      let al =
        match p_getenv conf.env "al" with
        [ Some "on" -> True
        | _ -> False ]
      in
      let si =
        match p_getenv conf.env "ms" with
        [ Some "on" -> True
        | _ -> False ]
      in
      if al then print_missing_ancestors_alphabetically conf base v si p
      else print_missing_ancestors conf base v si p
  | (Some "T", Some v) -> print_tree conf base v p
  | (Some "H", Some v) -> print_horizontally conf base v p
  | (Some "A", Some v) -> print_male_line conf base v p
  | (Some "C", Some v) -> print_female_line conf base v p
  | (Some "D", x) ->
      match (find_person_in_env conf base "1", x) with
      [ (Some anc, _) -> print_ancestors_same_time_descendants conf base p anc
      | (_, Some v) ->
          print_ancestors_same_time_descendants conf base p
            (pget conf base (Adef.iper_of_int v))
      | _ -> display_ancestor_menu conf base p ]
  | (Some "F", Some v) -> print_surnames_list conf base v p
  | _ -> display_ancestor_menu conf base p ]
;
