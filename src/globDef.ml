(* camlp4r *)
(* $Id: globDef.ml,v 1.1.2.3 1999-04-13 05:27:59 ddr Exp $ *)

open Def;
open Gutil;
open Config;

type death =
  [ NotDead
  | Death of death_reason and date
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead ]
;

type burial =
  [ UnknownBurial | Buried of (option date) | Cremated of (option date) ]
;

value wprint fmt = Wserver.wprint fmt;

value indent ind =
  do wprint "\n"; for i = 1 to ind do wprint " "; done; return ()
;

value f_uncapitalize (s : format 'a 'b 'c) =
  (Obj.magic String.uncapitalize (Obj.magic s) : format 'a 'b 'c)
;

(*
value list_iter_first f =
  loop True where rec loop first =
    fun
    [ [x :: l] -> do f first x; return loop False l
    | [] -> () ]
;
*)

value access conf base p = Util.acces conf base p
and aliases conf base p =
  List.map (fun s -> Util.coa conf (sou base s)) p.aliases
and are_not_married conf base fam = fam.not_married
and auth conf base p = Util.age_autorise conf base p
and auto_image_file conf base p = Util.auto_image_file conf base p
and baptism_date conf base p = Adef.od_of_codate p.baptism
and baptism_place conf base p = Util.coa conf (sou base p.baptism_place)
and basename conf base s = Filename.basename s
and birth_date conf base p = Adef.od_of_codate p.birth
and birth_place conf base p = Util.coa conf (sou base p.birth_place)
and burial conf base p =
  match p.burial with
  [ Def.UnknownBurial -> UnknownBurial
  | Def.Buried cod -> Buried (Adef.od_of_codate cod)
  | Def.Cremated cod -> Cremated (Adef.od_of_codate cod) ]
and burial_place conf base p = Util.coa conf (sou base p.burial_place)
and children conf base fam = List.map (poi base) (Array.to_list fam.children)
and commd conf base = Util.commd conf
and day conf base d = d.day
and death conf base p =
  match p.death with
  [ Def.NotDead -> NotDead
  | Def.Death dr cd -> Death dr (Adef.date_of_cdate cd)
  | Def.DeadYoung -> DeadYoung
  | Def.DeadDontKnowWhen -> DeadDontKnowWhen
  | Def.DontKnowIfDead -> DontKnowIfDead ]
and death_place conf base p = Util.coa conf (sou base p.death_place)
and father conf base fam = poi base (coi base fam.fam_index).father
and families conf base p = List.map (foi base) (Array.to_list p.family)
and find_person_in_env conf base x = Util.find_person_in_env conf base x
and first_name conf base p = Util.coa conf (sou base p.first_name)
and first_names_aliases conf base p =
  List.map (fun s -> Util.coa conf (sou base s)) p.first_names_aliases
and ftransl conf base s =
  if String.length (Obj.magic s : string) > 0 then
    match (Obj.magic s : string).[0] with
    [ 'A'..'Z' -> Util.fcapitale (Util.ftransl conf (f_uncapitalize s))
    | _ -> Util.ftransl conf s ]
  else s
and ftransl_nth conf base s i =
  if String.length (Obj.magic s : string) > 0 then
    match (Obj.magic s : string).[0] with
    [ 'A'..'Z' ->
        Util.fcapitale (Util.ftransl_nth conf (f_uncapitalize s) i)
    | _ -> Util.ftransl_nth conf s i ]
  else s
and has_titles conf base p = List.length p.titles > 0
and image conf base p = sou base p.image
and image_file_name conf base s =
  let fname = Util.image_file_name conf.bname s in
  if Sys.file_exists fname then Some fname else None
and image_size conf base = Util.image_size
and index conf base p = Adef.int_of_iper p.cle_index
and is_anniversary conf base d =
  if d.prec = Sure then
    d.day = conf.today.day && d.month = conf.today.month &&
    d.year < conf.today.year ||
    not (leap_year conf.today.year) && d.day = 29 && d.month = 2 &&
    conf.today.day = 1 && conf.today.month = 3
  else False
and is_implicit_filename conf base = Filename.is_implicit
and is_private_access conf base p =
  p.access = Private && not conf.wizard && not conf.friend
and is_rtl conf base = conf.is_rtl
and is_wizard conf base = conf.wizard
and list_length conf base = List.length
and lower conf base s = Util.coa conf (Name.lower (Util.aoc conf s))
and main_title conf base = Util.main_title base
and marriage_date conf base fam = Adef.od_of_codate fam.marriage
and marriage_place conf base fam = Util.coa conf (sou base fam.marriage_place)
and modtime conf base f =
  let s = Unix.stat f in
  int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int))
and month conf base d = d.month
and mother conf base fam = poi base (coi base fam.fam_index).mother
and notes conf base p = Util.coa conf (sou base p.notes)
and occupation conf base p = Util.coa conf (sou base p.occupation)
and ondate conf base d = Date.string_of_ondate conf d
and parents conf base p =
  match (aoi base p.cle_index).parents with
  [ Some ifam -> Some (foi base ifam)
  | None -> None ]
and passed_time conf base d1 d2 =
  let a = temps_ecoule d1 d2 in (a.day, a.month, a.year)
and person_text conf base = Util.person_text conf base
and precision conf base d = d.prec
and public_name conf base p = Util.coa conf (sou base p.public_name)
and qualifiers conf base p =
  List.map (fun s -> Util.coa conf (sou base s)) p.nick_names
and sex conf base p = Util.index_of_sex p.sex
and spouse conf base p fam = poi base (Util.spouse p (coi base fam.fam_index))
and string_length conf base s = String.length s
and string_of_num conf base = Num.to_string
and string_of_num_sep conf base n =
  let r = ref "" in
  do Num.print (fun x -> r.val := r.val ^ x)
       (Util.transl conf "(thousand separator)") n;
  return r.val
and string_sub conf base s beg len = String.sub s beg len
and surname conf base p = Util.coa conf (sou base p.surname)
and surnames_aliases conf base p =
  List.map (fun s -> Util.coa conf (sou base s)) p.surnames_aliases
and title_ident conf base t = Util.coa conf (sou base t.t_ident)
and title_place conf base t = Util.coa conf (sou base t.t_place)
and titled_person_text conf base = Util.titled_person_text conf base
and today conf base = conf.today
and transl conf base s =
  if String.length s > 0 then
    match s.[0] with
    [ 'A'..'Z' -> Util.capitale (Util.transl conf (String.uncapitalize s))
    | _ -> Util.transl conf s ]
  else ""
and transl_nth conf base s i =
  if String.length s > 0 then
    match s.[0] with
    [ 'A'..'Z' ->
        Util.capitale (Util.transl_nth conf (String.uncapitalize s) i)
    | _ -> Util.transl_nth conf s i ]
  else ""
and url_encode conf base = Util.code_varenv
and year_p conf base d =
  let s =
    match d.prec with
    [ Before -> "/"
    | About | Maybe | OrYear _ | YearInt _ -> "ca&nbsp;"
    | _ -> "" ]
  in
  let s = s ^ string_of_int d.year in
  match d.prec with
  [ After -> s ^ "/"
  | _ -> s ]
;

(* "perso" specific *)

value expand_macros conf base s =
  "<em>Macros expansion to write...</em><br>" ^ s;
value find_sosa =
  Perso.find_sosa;
value has_grand_children conf base p =
  Perso.has_grand_children base p;
value has_grand_parents conf base p =
  Perso.has_grand_parents base p;
value print_titles conf base p =
  do Perso.print_titles conf base (Util.transl conf "and") p; return "";
