(* camlp4r ./pa_html.cmo *)
(* $Id: birthday.ml,v 2.5.2.4 1999-10-25 04:14:40 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;
open Util;
open Gutil;

type date_event = [ DeBirth | DeDeath of death_reason ];

value afficher_anniversaires_jour conf base dead_people liste =
  do Wserver.wprint "<ul>\n";
     List.iter
       (fun (p, a, date_event) ->
          let is = index_of_sex p.sex in
          do html_li conf;
             afficher_personne_titre_referencee conf base p;
             if not dead_people then Wserver.wprint " <em>%d</em>\n" a
             else
               let txt =
                 match date_event with
                 [ DeBirth -> transl_nth conf "born" is
                 | DeDeath Unspecified -> transl_nth conf "died" is
                 | DeDeath Killed -> transl_nth conf "killed (in action)" is
                 | DeDeath Murdered -> transl_nth conf "murdered" is
                 | DeDeath Executed ->
                     transl_nth conf "executed (legally killed)" is
                 | DeDeath Disappeared -> transl_nth conf "disappeared" is ]
               in
               Wserver.wprint ", <em>%s %s %d</em>\n" txt
                 (transl conf "in (year)") a;
          return ())
       liste;
     Wserver.wprint "</ul>\n";
  return ()
;

value gen_print conf base mois dead_people =
  let tab = Array.create 31 [] in
  let title _ =
    let lab =
      if dead_people then transl conf "anniversaries"
      else transl conf "birthdays"
    in
    Wserver.wprint "%s %s %s" (capitale lab)
      (transl conf "in (month year)")
      (transl_nth conf "(month)" (mois - 1))
  in
  do for i = 0 to base.data.persons.len - 1 do
       let p = base.data.persons.get i in
       if not dead_people then
         match (Adef.od_of_codate p.birth, p.death) with
         [ (Some (Dgreg d _), NotDead | DontKnowIfDead) ->
             if d.prec = Sure && d.day <> 0 && d.month <> 0
             && d.month = mois then
               if age_autorise conf base p then
                 let j = d.day in
                 tab.(pred j) := [(p, d.year, DeBirth) :: tab.(pred j)]
               else ()
             else ()
         | _ -> () ]
       else
         match p.death with
         [ NotDead | DontKnowIfDead -> ()
         | _ ->
             do match Adef.od_of_codate p.birth with
                [ Some (Dgreg dt _) ->
                    if dt.prec = Sure && dt.day <> 0 && dt.month <> 0
                    && dt.month = mois then
                      if age_autorise conf base p then
                        let j = dt.day in
                        tab.(pred j) := [(p, dt.year, DeBirth) :: tab.(pred j)]
                      else ()
                    else ()
                | _ -> () ];
                match p.death with
                [ Death dr d ->
                    match Adef.date_of_cdate d with
                    [ Dgreg dt _ ->
                        if dt.prec = Sure && dt.day <> 0 && dt.month <> 0
                        && dt.month = mois then
                          if age_autorise conf base p then
                            let j = dt.day in
                            let a = dt.year in
                            tab.(pred j) :=
                              [(p, a, DeDeath dr) :: tab.(pred j)]
                          else ()
                        else ()
                    | _ -> () ]
                | _ -> () ];
             return () ];
     done;
     header conf title;
     Wserver.wprint "<ul>\n";
     for j = 1 to 31 do
       if tab.(pred j) <> [] then
         do html_li conf;
            Wserver.wprint "%d\n" j;
            let liste =
              Sort.list (fun (p1, a1, _) (p2, a2, _) -> a1 <= a2) tab.(pred j)
            in
            afficher_anniversaires_jour conf base dead_people liste;
         return ()
       else ();
     done;
     Wserver.wprint "</ul>\n";
     trailer conf;
  return ()
;

value anniversary_of conf base dead_people jj mm =
  let xx = ref [] in
  do for i = 0 to base.data.persons.len - 1 do
       let p = base.data.persons.get i in
       if not dead_people then
         match (Adef.od_of_codate p.birth, p.death) with
         [ (Some (Dgreg d _), NotDead | DontKnowIfDead) ->
             if d.prec = Sure && d.day <> 0 && d.month <> 0 then
               if d.day == jj && d.month == mm then
                 if age_autorise conf base p then
                   xx.val := [(p, d.year, DeBirth) :: xx.val]
                 else ()
               else ()
             else ()
         | _ -> () ]
       else
         match p.death with
         [ NotDead | DontKnowIfDead -> ()
         | _ ->
             do match Adef.od_of_codate p.birth with
                [ Some (Dgreg d _) ->
                    if d.prec = Sure && d.day <> 0 && d.month <> 0 then
                      if d.day == jj && d.month == mm then
                        if age_autorise conf base p then
                          xx.val := [(p, d.year, DeBirth) :: xx.val]
                        else ()
                      else ()
                    else ()
                | _ -> () ];
                match p.death with
                [ Death dr d ->
                    match Adef.date_of_cdate d with
                    [ Dgreg dt _ ->
                        if dt.prec = Sure && dt.day <> 0 && dt.month <> 0 then
                          if dt.day == jj && dt.month == mm then
                            if age_autorise conf base p then
                              xx.val := [(p, dt.year, DeDeath dr) :: xx.val]
                            else ()
                          else ()
                        else ()
                    | _ -> () ]
                | _ -> () ];
             return () ];
     done;
     xx.val := Sort.list (fun (p1, a1, _) (p2, a2, _) -> a1 <= a2) xx.val;
  return xx.val
;

value anniversaire_du conf base dead_people dt =
  let list = anniversary_of conf base dead_people dt.day dt.month in
  if not (leap_year dt.year) && dt.day = 1 && dt.month = 3 then
    list @ anniversary_of conf base dead_people 29 2
  else list
;

value afficher_liste_anniversaires conf base dead_people dt liste =
  let a_ref = dt.year in
  do Wserver.wprint "<ul>\n";
     List.iter
       (fun (p, a, date_event) ->
          do html_li conf;
             if dead_people then
               do Wserver.wprint "<em>";
                  match date_event with
                  [ DeBirth ->
                      Wserver.wprint "%s" (transl conf "of the birth")
                  | DeDeath (Unspecified | Killed) ->
                      Wserver.wprint "%s" (transl conf "of the death")
                  | DeDeath Murdered ->
                       Wserver.wprint "%s" (transl conf "of the murder")
                  | DeDeath Executed ->
                      Wserver.wprint "%s" (transl conf "of the execution")
                  | DeDeath Disappeared ->
                      Wserver.wprint "%s"
                        (transl conf "of the disappearance") ];
                  Wserver.wprint "</em>\n";
                  Wserver.wprint "%s " (transl_decline conf "of" "");
                  afficher_personne_titre_referencee conf base p;
                  Wserver.wprint "\n<em>%s %d" (transl conf "in (year)") a;
                  Wserver.wprint " (";
                  Wserver.wprint (ftransl conf "%d years ago")
                    (conf.today.year - a);
                  Wserver.wprint ")</em>";
               return ()
             else
               do afficher_personne_titre_referencee conf base p;
                  match p.death with
                  [ NotDead ->
                      do Wserver.wprint " <em>";
                         match a_ref - a with
                         [ 0 -> Wserver.wprint "%s" (transl conf "birth")
                         | 1 ->
                             Wserver.wprint "%s" (transl conf "one year old")
                         | n ->
                             Wserver.wprint "%d %s" n
                               (transl conf "years old") ];
                         Wserver.wprint "</em>";
                      return ()
                  | _ -> () ];
               return ();
             Wserver.wprint "\n";
          return ())
       liste;
     Wserver.wprint "</ul>\n";
  return ()
;

value print_birth conf base mois = gen_print conf base mois False;
value print_dead conf base mois = gen_print conf base mois True;

value print_birth_day conf base day_name verb wd dt list =
  do Wserver.wprint "\n"; html_p conf; return
  match list with
  [ [] ->
      Wserver.wprint "%s %s.\n"
        (capitale (transl conf "no birthday")) day_name
  | _ ->
      do Wserver.wprint "%s, %s%s %s %s:\n"
           (capitale day_name)
           (std_color
              ("<b>" ^ transl_nth conf "(week day)" wd ^ " " ^
               Date.string_of_date conf (Dgreg dt Dgregorian) ^ "</b>"))
           verb (transl conf "the birthday")
           (transl_decline conf "of" "");
         afficher_liste_anniversaires conf base False dt list;
      return () ]
;

value propose_months conf mode =
  tag "center" begin
    tag "form" "method=get action=\"%s\"" conf.command begin
      Srcfile.hidden_env conf;
      Wserver.wprint "<input type=hidden name=m value=%s>\n" mode;
      tag "select" "name=v" begin
        for i = 1 to 12 do
          Wserver.wprint "<option value=%d%s>%s\n" i
            (if i = conf.today.month then " selected" else "")
            (capitale (transl_nth conf "(month)" (i - 1)));
        done;
      end;
      Wserver.wprint "<input type=submit value=\"Ok\">\n";
    end;
  end
;

value day_after d =
  let (day, r) =
    if d.day >= nb_jours_dans_mois d.month d.year then (1, 1)
    else (succ d.day, 0)
  in
  let (month, r) = if d.month + r > 12 then (1, 1) else (d.month + r, 0) in
  let year = d.year + r in
  {day = day; month = month; year = year; prec = Sure; delta = 0}
;

value print_anniv conf base day_name verb wd dt list =
  do Wserver.wprint "\n"; html_p conf; return
  match list with
  [ [] ->
      Wserver.wprint "%s %s.\n"
        (capitale (transl conf "no anniversary")) day_name
  | _ ->
      do Wserver.wprint "%s, %s%s %s:\n"
           (capitale day_name)
           (std_color
              ("<b>" ^ transl_nth conf "(week day)" wd ^ " " ^
               Date.string_of_date conf (Dgreg dt Dgregorian) ^ "</b>"))
           verb (transl conf "the anniversary");
         afficher_liste_anniversaires conf base True dt list;
      return () ]
;

value print_marriage conf base month =
  let title _ =
    let lab = transl conf "anniversaries of marriage" in
    Wserver.wprint "%s %s %s" (capitale lab)
      (transl conf "in (month year)")
      (transl_nth conf "(month)" (month - 1))
  in
  let tab = Array.create 31 [] in
  do header conf title;
     for i = 0 to base.data.families.len - 1 do
       let fam = base.data.families.get i in
       if is_deleted_family fam then ()
       else
         match Adef.od_of_codate fam.marriage with
         [ Some (Dgreg {day = d; month = m; year = y; prec = Sure} _) when
           d <> 0 && m <> 0 ->
             let cpl = base.data.couples.get i in
             if m == month && age_autorise conf base (poi base cpl.father)
             && age_autorise conf base (poi base cpl.mother) then
               tab.(pred d) := [(cpl, y) :: tab.(pred d)]
             else ()
         | _ -> () ];
     done;
     Wserver.wprint "<ul>";
     for i = 1 to 31 do
       match tab.(i - 1) with
       [ [] -> ()
       | l ->
           let l = Sort.list (fun (fam1, y1) (fam2, y2) -> y1 < y2) l in
           do Wserver.wprint "\n";
              html_li conf;
              Wserver.wprint "%d\n<ul>" i;
              List.iter
                (fun (fam, year) ->
                   do Wserver.wprint "\n";
                      html_li conf;
                      afficher_personne_titre_referencee conf base
                        (poi base fam.father);
                      Wserver.wprint "\n%s\n" (transl conf "and");
                      afficher_personne_titre_referencee conf base
                        (poi base fam.mother);
                      Wserver.wprint ", <em>%s %d</em>\n"
                        (transl conf "in (year)") year;
                   return ())
               l;
               Wserver.wprint "</ul>\n";
           return () ];
     done;
     Wserver.wprint "</ul>\n";
     trailer conf;
  return ()
;

value anniversary_of_marriage_of_day conf base dt =
  let xx = ref [] in
  do for i = 0 to base.data.families.len - 1 do
       let fam = base.data.families.get i in
       if is_deleted_family fam then ()
       else
         match Adef.od_of_codate fam.marriage with
         [ Some (Dgreg {day = d; month = m; year = y; prec = Sure} _) when
           d <> 0 && m <> 0 ->
             let cpl = base.data.couples.get i in
             if age_autorise conf base (poi base cpl.father)
             && age_autorise conf base (poi base cpl.mother)
             && d == dt.day && m == dt.month then
               xx.val := [(cpl, y) :: xx.val]
             else ()
         | _ -> () ];
     done;
     xx.val := Sort.list (fun (fam1, y1) (fam2, y2) -> y1 <= y2) xx.val;
  return xx.val
;

value print_anniversaries_of_marriage conf base y list =
  do Wserver.wprint "<ul>";
     List.iter
       (fun (fam, year) ->
          do Wserver.wprint "\n";
             html_li conf;
             afficher_personne_titre_referencee conf base
               (poi base fam.father);
             Wserver.wprint "\n%s\n" (transl conf "and");
             afficher_personne_titre_referencee conf base
               (poi base fam.mother);
             Wserver.wprint ", <em>%s %d\n("
               (transl conf "in (year)") year;
             Wserver.wprint (ftransl conf "%d years ago")
               (conf.today.year - year);
             Wserver.wprint "</em>)\n";
          return ())
       list;
     Wserver.wprint "</ul>\n";
  return ()
;

value print_marriage_day conf base day_name verb wd dt list =
  do Wserver.wprint "\n"; html_p conf; return
  match list with
  [ [] ->
      Wserver.wprint "%s %s.\n"
        (capitale (transl conf "no anniversary")) day_name
  | _ ->
      do Wserver.wprint "%s, %s%s %s %s:\n"
           (capitale day_name)
           (std_color
              ("<b>" ^ transl_nth conf "(week day)" wd ^ " " ^
               Date.string_of_date conf (Dgreg dt Dgregorian) ^ "</b>"))
           verb (transl conf "the anniversary of marriage")
           (transl_decline conf "of" "");
         print_anniversaries_of_marriage conf base dt.year list;
      return () ]
;

value print_menu_birth conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "birthdays"))
  in
  do cheader conf title;
     let tom = day_after conf.today in
     let aft = day_after tom in
     let list_today = anniversaire_du conf base False conf.today in
     let list_tom = anniversaire_du conf base False tom in
     let list_aft = anniversaire_du conf base False aft in
     do print_birth_day conf base (transl conf "today") (transl conf ", it is")
          conf.today_wd conf.today list_today;
        print_birth_day conf base (transl conf "tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 1) mod 7)
          tom list_tom;
        print_birth_day conf base (transl conf "the day after tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 2) mod 7)
          aft list_aft;
     return ();
     Wserver.wprint "\n";
     html_p conf;
     propose_months conf "AN";
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_menu_dead conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "anniversaries of dead"))
  in
  do cheader conf title;
     let tom = day_after conf.today in
     let aft = day_after tom in
     let list_today = anniversaire_du conf base True conf.today in
     let list_tom = anniversaire_du conf base True tom in
     let list_aft = anniversaire_du conf base True aft in
     do print_anniv conf base (transl conf "today") (transl conf ", it is")
          conf.today_wd conf.today list_today;
        print_anniv conf base (transl conf "tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 1) mod 7)
          tom list_tom;
        print_anniv conf base (transl conf "the day after tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 2) mod 7)
          aft list_aft;
     return ();
     Wserver.wprint "\n";
     html_p conf;
     propose_months conf "AD";
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_menu_marriage conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "anniversaries of marriage"))
  in
  do cheader conf title;
     let tom = day_after conf.today in
     let aft = day_after tom in
     let list_today = anniversary_of_marriage_of_day conf base conf.today in
     let list_tomorrow = anniversary_of_marriage_of_day conf base tom in
     let list_after = anniversary_of_marriage_of_day conf base aft in
     do print_marriage_day conf base (transl conf "today")
          (transl conf ", it is") conf.today_wd conf.today list_today;
        print_marriage_day conf base (transl conf "tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 1) mod 7)
          tom list_tomorrow;
        print_marriage_day conf base (transl conf "the day after tomorrow")
          (transl conf ", it will be") ((conf.today_wd + 2) mod 7)
          aft list_after;
       return ();
     Wserver.wprint "\n";
     html_p conf;
     propose_months conf "AM";
     Wserver.wprint "\n";
     trailer conf;
  return ()
;
