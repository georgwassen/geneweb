(* $Id: gutil.ml,v 4.26.2.1 2006-01-03 12:04:10 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;

value poi base i = base.data.persons.get (Adef.int_of_iper i);
value aoi base i = base.data.ascends.get (Adef.int_of_iper i);
value uoi base i = base.data.unions.get (Adef.int_of_iper i);

value foi base i = base.data.families.get (Adef.int_of_ifam i);
value coi base i = base.data.couples.get (Adef.int_of_ifam i);
value doi base i = base.data.descends.get (Adef.int_of_ifam i);

value sou base i = base.data.strings.get (Adef.int_of_istr i);

value rindex s c =
  pos (String.length s - 1) where rec pos i =
    if i < 0 then None else if s.[i] = c then Some i else pos (i - 1)
;

value lindex s c =
  pos 0 where rec pos i =
    if i == String.length s then None
    else if s.[i] == c then Some i
    else pos (i + 1)
;

value list_iter_first f al =
  let _ =
    List.fold_left (fun first a -> let () = f first a in False) True al
  in
  ()
;

value array_memq x a =
  loop 0 where rec loop i =
    if i == Array.length a then False
    else if x == a.(i) then True
    else loop (i + 1)
;

value string_sub s i len =
  let i = min (String.length s) (max 0 i) in
  let len = min (String.length s - i) (max 0 len) in String.sub s i len
;

value decline_word case s ibeg iend =
  let i =
    loop ibeg where rec loop i =
      if i + 3 > iend then ibeg
      else if s.[i] == ':' && s.[i + 1] == case && s.[i + 2] == ':' then i + 3
      else loop (i + 1)
  in
  let j =
    loop i where rec loop i =
      if i + 3 > iend then iend
      else if s.[i] == ':' && s.[i + 2] == ':' then i
      else loop (i + 1)
  in
  if i = ibeg then String.sub s ibeg (j - ibeg)
  else if s.[i] == '+' then
    let k =
      loop ibeg where rec loop i =
        if i == iend then i else if s.[i] == ':' then i else loop (i + 1)
    in
    let i = i + 1 in string_sub s ibeg (k - ibeg) ^ string_sub s i (j - i)
  else if s.[i] == '-' then
    let k =
      loop ibeg where rec loop i =
        if i == iend then i else if s.[i] == ':' then i else loop (i + 1)
    in
    let (i, cnt) =
      loop (i + 1) 1 where rec loop i cnt =
        if i < iend && s.[i] == '-' then loop (i + 1) (cnt + 1) else (i, cnt)
    in
    string_sub s ibeg (k - ibeg - cnt) ^ string_sub s i (j - i)
  else string_sub s i (j - i)
;

value decline case s =
  loop 0 0 where rec loop ibeg i =
    if i == String.length s then
      if i == ibeg then "" else decline_word case s ibeg i
    else
      match s.[i] with
      [ ' ' | '<' | '/' as sep ->
          decline_word case s ibeg i ^ String.make 1 sep ^
            loop (i + 1) (i + 1)
      | '>' -> String.sub s ibeg (i + 1 - ibeg) ^ loop (i + 1) (i + 1)
      | _ -> loop ibeg (i + 1) ]
;

value nominative s =
  match rindex s ':' with
  [ Some _ -> decline 'n' s
  | _ -> s ]
;

value p_first_name base p = nominative (sou base p.first_name);
value p_surname base p = nominative (sou base p.surname);

value leap_year a =
  if a mod 100 == 0 then a / 100 mod 4 == 0 else a mod 4 == 0
;

value nb_days_in_month =
  let tb = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |] in
  fun m a ->
    if m == 2 && leap_year a then 29
    else if m >= 1 && m <= 12 then tb.(m - 1)
    else 0
;

value common_prec p1 p2 =
  if p1 = p2 then p1
  else
    match (p1, p2) with
    [ (Sure, _) -> p2
    | (_, Sure) -> p1
    | _ -> Maybe ]
;

value time_gone_by d1 d2 =
  let prec = common_prec d1.prec d2.prec in
  match d1 with
  [ {day = 0; month = 0; year = a1} ->
      {day = 0; month = 0; year = d2.year - a1; prec = prec; delta = 0}
  | {day = 0; month = m1; year = a1} ->
      match d2 with
      [ {day = 0; month = 0; year = a2} ->
          {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
          let r = 0 in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = 0; month = month; year = year; prec = prec; delta = 0}
      | {day = j2; month = m2; year = a2} ->
          let r = 0 in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = 0; month = month; year = year; prec = prec; delta = 0} ]
  | {day = j1; month = m1; year = a1} ->
      match d2 with
      [ {day = 0; month = 0; year = a2} ->
          {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
          let r = 0 in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = 0; month = month; year = year; prec = prec; delta = 0}
      | {day = j2; month = m2; year = a2} ->
          let (day, r) =
            if j1 <= j2 then (j2 - j1, 0)
            else (j2 - j1 + nb_days_in_month m1 a1, 1)
          in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = day; month = month; year = year; prec = prec; delta = 0} ] ]
;

value year_of d = d.year;

value strictly_before_dmy d1 d2 =
  let {day = d; month = m; year = y; prec = p} = time_gone_by d2 d1 in
  if y < 0 then True
  else if y > 0 then False
  else if m < 0 then True
  else if m > 0 then False
  else if d < 0 then True
  else if d > 0 then False
  else if d1.prec = d2.prec then False
  else if d1.prec = Before && d2.prec = After then True
  else False
;

value strictly_before d1 d2 =
  match (d1, d2) with
  [ (Dgreg d1 _, Dgreg d2 _) -> strictly_before_dmy d1 d2
  | _ -> False ]
;

value strictly_after_dmy d1 d2 =
  let {day = d; month = m; year = y; prec = p} = time_gone_by d1 d2 in
  if y < 0 then True
  else if y > 0 then False
  else if m < 0 then True
  else if m > 0 then False
  else if d < 0 then True
  else if d > 0 then False
  else if d2.prec = d1.prec then False
  else if d2.prec = Before && d1.prec = After then True
  else False
;

value strictly_after d1 d2 =
  match (d1, d2) with
  [ (Dgreg d1 _, Dgreg d2 _) -> strictly_after_dmy d1 d2
  | _ -> False ]
;

value designation base p =
  let prenom = p_first_name base p in
  let nom = p_surname base p in prenom ^ "." ^ string_of_int p.occ ^ " " ^ nom
;

value father = Adef.father;
value mother = Adef.mother;
value couple multi fath moth =
  if not multi then Adef.couple fath moth else Adef.multi_couple fath moth
;
value parent multi parent =
  if not multi then Adef.parent parent else Adef.multi_parent parent
;
value parent_array = Adef.parent_array;
value set_father = Adef.set_father;
value set_mother = Adef.set_mother;

value parents asc = asc.parents;
value consang asc = asc.consang;
value no_ascend () = {parents = None; consang = Adef.fix (-1)};
value set_parents asc v = asc.parents := v;
value set_consang asc v = asc.consang := v;

value spouse ip cpl =
  if ip == father cpl then mother cpl
(*
  else if ip == mother cpl then father cpl
*)
  else father cpl
;

value saints = ["saint"; "sainte"];

value surnames_pieces surname =
  let surname = Name.lower surname in
  let flush i0 i1 =
    if i1 > i0 then [String.sub surname i0 (i1 - i0)] else []
  in
  let rec loop i0 iw i =
    if i == String.length surname then
      if i0 == 0 then [] else if i > i0 + 3 then flush i0 i else []
    else if surname.[i] == ' ' then
      if i > iw + 3 then
        let w = String.sub surname iw (i - iw) in
        if List.mem w saints then loop i0 (i + 1) (i + 1)
        else flush i0 i @ loop (i + 1) (i + 1) (i + 1)
      else loop i0 (i + 1) (i + 1)
    else loop i0 iw (i + 1)
  in
  loop 0 0 0
;

value person_misc_names base p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if first_name = "?" || surname = "?" then []
  else
    let public_names =
      let titles_names =
        let tnl = ref [] in
        do {
          List.iter
            (fun t ->
               match t.t_name with
               [ Tmain | Tnone -> ()
               | Tname x -> tnl.val := [x :: tnl.val] ])
            p.titles;
          tnl.val
        }
      in
      if sou base p.public_name = "" || p.titles = [] then titles_names
      else [p.public_name :: titles_names]
    in
    let first_names =
      let pn =
        if sou base p.public_name <> "" && p.titles = [] then
          [p.public_name :: public_names]
        else public_names
      in
      [first_name :: List.map (sou base) (p.first_names_aliases @ pn)]
    in
    let surnames =
      [surname ::
       surnames_pieces surname @
         List.map (sou base) (p.surnames_aliases @ p.qualifiers)]
    in
    let surnames =
      if p.sex == Female then
        let u = uoi base p.cle_index in
        List.fold_left
          (fun list ifam ->
             let cpl = coi base ifam in
             let husband = poi base (father cpl) in
             let husband_surname = p_surname base husband in
             let husband_surnames_aliases =
               List.map (sou base) husband.surnames_aliases
             in
             if p_surname base husband = "?" then
               husband_surnames_aliases @ list
             else
               [husband_surname ::
                surnames_pieces husband_surname @ husband_surnames_aliases @
                  list])
          surnames (Array.to_list u.family)
      else surnames
    in
    let list = [] in
    let list =
      List.fold_left (fun list s -> [sou base s :: list]) list public_names
    in
    let list =
      List.fold_left
        (fun list f ->
           List.fold_left (fun list s -> [f ^ " " ^ s :: list]) list surnames)
        list first_names
    in
    let list =
      let first_names =
        [first_name :: List.map (sou base) p.first_names_aliases]
      in
      List.fold_left
        (fun list t ->
           let s = sou base t.t_place in
           if s = "" then list
           else
             let first_names =
               match t.t_name with
               [ Tname f -> [sou base f :: first_names]
               | _ ->
                   let f = sou base p.public_name in
                   if f = "" then first_names else [f :: first_names] ]
             in
             List.fold_left (fun list f -> [f ^ " " ^ s :: list]) list
               first_names)
        list p.titles
    in
    let list =
      match parents (aoi base p.cle_index) with
      [ Some ifam ->
          let cpl = coi base ifam in
          let fath = poi base (father cpl) in
          let first_names =
            [first_name :: List.map (sou base) p.first_names_aliases]
          in
          List.fold_left
            (fun list t ->
               let s = sou base t.t_place in
               if s = "" then list
               else
                 List.fold_left (fun list f -> [f ^ " " ^ s :: list]) list
                   first_names)
            list fath.titles
      | _ -> list ]
    in
    let list =
      List.fold_left (fun list s -> [sou base s :: list]) list p.aliases
    in
    let fn = Name.lower (first_name ^ " " ^ surname) in
    List.fold_left
      (fun list s ->
         let s = Name.lower s in
         if s = fn || List.mem s list then list else [s :: list])
      [] list
;

value person_ht_add base s ip = base.func.patch_name s ip;

value person_is_key base p k =
  let k = Name.crush_lower k in
  if k = Name.crush_lower (p_first_name base p ^ " " ^ p_surname base p) then
    True
  else if
    List.exists (fun x -> k = Name.crush_lower x)
      (person_misc_names base p) then
    True
  else False
;

value person_ht_find_unique base first_name surname occ =
  if first_name = "?" || surname = "?" then raise Not_found
  else
    let first_name = nominative first_name in
    let surname = nominative surname in
    let ipl = base.func.persons_of_name (first_name ^ " " ^ surname) in
    let first_name = Name.lower first_name in
    let surname = Name.lower surname in
    let rec find =
      fun
      [ [ip :: ipl] ->
          let p = poi base ip in
          if occ == p.occ && first_name = Name.lower (p_first_name base p) &&
             surname = Name.lower (p_surname base p) then
            p.cle_index
          else find ipl
      | _ -> raise Not_found ]
    in
    find ipl
;

value find_num s i =
  loop i i where rec loop start i =
    if i == String.length s then None
    else
      match s.[i] with
      [ '0'..'9' -> loop start (i + 1)
      | c ->
          if i == start then
            if c = ' ' then loop (start + 1) (start + 1) else None
          else Some (int_of_string (String.sub s start (i - start)), i) ]
;

value split_key s =
  match lindex s '.' with
  [ Some i ->
      match find_num s (i + 1) with
      [ Some (occ, j) ->
          let first_name = String.sub s 0 i in
          let surname = String.sub s j (String.length s - j) in
          (first_name, occ, surname)
      | None -> raise Not_found ]
  | None -> raise Not_found ]
;

value person_of_key base s =
  try
    let (first_name, occ, surname) = split_key s in
    Some (person_ht_find_unique base first_name surname occ)
  with
  [ Not_found -> None ]
;

value person_ht_find_all base s =
  match person_of_key base s with
  [ Some p -> [p]
  | _ ->
      let ipl = base.func.persons_of_name s in
      let rec select =
        fun
        [ [ip :: ipl] ->
            if person_is_key base (poi base ip) s then
              let ipl = select ipl in
              if List.mem ip ipl then ipl else [ip :: ipl]
            else select ipl
        | [] -> [] ]
      in
      select ipl ]
;

value find_same_name base p =
  let f = p_first_name base p in
  let s = p_surname base p in
  let ipl = person_ht_find_all base (f ^ " " ^ s) in
  let f = Name.strip_lower f in
  let s = Name.strip_lower s in
  let pl =
    List.fold_left
      (fun pl ip ->
         let p = poi base ip in
         if Name.strip_lower (p_first_name base p) = f &&
            Name.strip_lower (p_surname base p) = s then
           [p :: pl]
         else pl)
      [] ipl
  in
  Sort.list (fun p1 p2 -> p1.occ < p2.occ) pl
;

(* check base *)

type error 'person =
  [ AlreadyDefined of 'person
  | OwnAncestor of 'person
  | BadSexOfMarriedPerson of 'person ]
;
type base_error = error person;

type warning 'person =
  [ BirthAfterDeath of 'person
  | IncoherentSex of 'person and int and int
  | ChangedOrderOfChildren of ifam and descend and array iper
  | ChildrenNotInOrder of ifam and descend and 'person and 'person
  | DeadTooEarlyToBeFather of 'person and 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person and 'person
  | ParentBornAfterChild of 'person and 'person
  | ParentTooYoung of 'person and Def.dmy
  | TitleDatesError of 'person and title
  | UndefinedSex of 'person
  | YoungForMarriage of 'person and Def.dmy ]
;
type base_warning = warning person;

type visit = [ NotVisited | BeingVisited | Visited ];

value check_noloop base error =
  let tab = Array.create base.data.persons.len NotVisited in
  let rec noloop i =
    match tab.(i) with
    [ NotVisited ->
        do {
          match parents (base.data.ascends.get i) with
          [ Some fam ->
              let fath = father (coi base fam) in
              let moth = mother (coi base fam) in
              do {
                tab.(i) := BeingVisited;
                noloop (Adef.int_of_iper fath);
                noloop (Adef.int_of_iper moth);
                ()
              }
          | None -> () ];
          tab.(i) := Visited;
        }
    | BeingVisited -> error (OwnAncestor (base.data.persons.get i))
    | Visited -> () ]
  in
  for i = 0 to base.data.persons.len - 1 do {
    match tab.(i) with
    [ NotVisited -> noloop i
    | BeingVisited -> failwith "check_noloop algorithm error"
    | Visited -> () ]
  }
;

value check_noloop_for_person_list base error ipl =
  let tab = Array.create base.data.persons.len NotVisited in
  let rec noloop ip =
    let i = Adef.int_of_iper ip in
    match tab.(i) with
    [ NotVisited ->
        do {
          match parents (aoi base ip) with
          [ Some ifam ->
              let cpl = coi base ifam in
              do {
                tab.(i) := BeingVisited;
                Array.iter noloop (parent_array cpl);
              }
          | None -> () ];
          tab.(i) := Visited;
        }
    | BeingVisited -> error (OwnAncestor (poi base ip))
    | Visited -> () ]
  in
  List.iter noloop ipl
;

value date_of_death =
  fun
  [ Death _ cd -> Some (Adef.date_of_cdate cd)
  | _ -> None ]
;

value roman_of_arabian n =
  let build one five ten =
    fun
    [ 0 -> ""
    | 1 -> one
    | 2 -> one ^ one
    | 3 -> one ^ one ^ one
    | 4 -> one ^ five
    | 5 -> five
    | 6 -> five ^ one
    | 7 -> five ^ one ^ one
    | 8 -> five ^ one ^ one ^ one
    | _ -> one ^ ten ]
  in
  build "M" "M" "M" (n / 1000 mod 10) ^ build "C" "D" "M" (n / 100 mod 10) ^
    build "X" "L" "C" (n / 10 mod 10) ^ build "I" "V" "X" (n mod 10)
;

value arabian_of_roman s =
  let decode_digit one five ten r =
    loop 0 where rec loop cnt i =
      if i >= String.length s then (10 * r + cnt, i)
      else if s.[i] = one then loop (cnt + 1) (i + 1)
      else if s.[i] = five then
        if cnt = 0 then loop 5 (i + 1) else (10 * r + 5 - cnt, i + 1)
      else if s.[i] = ten then (10 * r + 10 - cnt, i + 1)
      else (10 * r + cnt, i)
  in
  let (r, i) = decode_digit 'M' 'M' 'M' 0 0 in
  let (r, i) = decode_digit 'C' 'D' 'M' r i in
  let (r, i) = decode_digit 'X' 'L' 'C' r i in
  let (r, i) = decode_digit 'I' 'V' 'X' r i in
  if i = String.length s then r else raise Not_found
;

value child_born_after_his_parent base error warning x iparent =
  let parent = poi base iparent in
  match
    (Adef.od_of_codate parent.birth, Adef.od_of_codate x.birth,
     date_of_death x.death)
  with
  [ (Some (Dgreg g1 _ as d1), Some (Dgreg g2 _ as d2), _) ->
      if strictly_after d1 d2 then warning (ParentBornAfterChild parent x)
      else
        let a = time_gone_by g1 g2 in
        if year_of a < 11 then warning (ParentTooYoung parent a) else ()
  | (Some (Dgreg g1 _ as d1), _, Some (Dgreg g2 _ as d2)) ->
      if strictly_after d1 d2 then warning (ParentBornAfterChild parent x)
      else
        let a = time_gone_by g1 g2 in
        if year_of a < 11 then warning (ParentTooYoung parent a) else ()
  | _ -> () ]
;

value born_after_his_elder_sibling base error warning x np ifam des =
  match (np, Adef.od_of_codate x.birth, x.death) with
  [ (None, _, _) -> ()
  | (Some (elder, d1), Some d2, _) ->
      if strictly_after d1 d2 then
        warning (ChildrenNotInOrder ifam des elder x)
      else ()
  | (Some (elder, d1), _, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then
        warning (ChildrenNotInOrder ifam des elder x)
      else ()
  | _ -> () ]
;

value child_born_before_mother_death base warning x imoth =
  let mother = poi base imoth in
  match (Adef.od_of_codate x.birth, mother.death) with
  [ (Some d1, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then
        warning (MotherDeadAfterChildBirth mother x)
      else ()
  | _ -> () ]
;

value child_has_sex warning child =
  if child.sex = Neuter then warning (UndefinedSex child)
  else ()
;

value possible_father base warning x ifath =
  let father = poi base ifath in
  match (Adef.od_of_codate x.birth, date_of_death father.death) with
  [ (Some (Dgreg {prec = Before} _), _) |
    (_, Some (Dgreg {prec = After} _)) ->
      ()
  | (Some (Dgreg d1 _), Some (Dgreg d2 _)) ->
      let a2 =
        match d2 with
        [ {prec = YearInt a2} -> a2
        | {prec = OrYear a2} -> a2
        | {year = a} -> a ]
      in
      if year_of d1 > a2 + 1 then warning (DeadTooEarlyToBeFather father x)
      else ()
  | _ -> () ]
;

value birth_before_death base warning p =
  match (Adef.od_of_codate p.birth, p.death) with
  [ (Some d1, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then warning (BirthAfterDeath p) else ()
  | _ -> () ]
;

value titles_after_birth base warning p t =
  let t_date_start = Adef.od_of_codate t.t_date_start in
  let t_date_end = Adef.od_of_codate t.t_date_end in
  do {
    match (t_date_start, t_date_end) with
    [ (Some d1, Some d2) ->
        if strictly_after d1 d2 then warning (TitleDatesError p t) else ()
    | _ -> () ];
    match Adef.od_of_codate p.birth with
    [ Some d1 ->
        do {
          match t_date_start with
          [ Some d ->
              if strictly_after d1 d then warning (TitleDatesError p t)
              else ()
          | None -> () ];
          match t_date_end with
          [ Some d ->
              if strictly_after d1 d then warning (TitleDatesError p t)
              else ()
          | None -> () ];
          ()
        }
    | _ -> () ];
  }
;

value try_to_fix_relation_sex base warning p_ref =
  do {
    let p_index = Some p_ref.cle_index in
    let fixed = ref 0 in
    let not_fixed = ref 0 in
    List.iter
      (fun ip ->
        let p = poi base ip in
        List.iter
          (fun rel ->
             match (p_index = rel.r_fath, p_index = rel.r_moth) with
             [ (True, False) ->
                 if p_ref.sex = Female then
                   match rel.r_moth with
                   [ Some ip ->
                       let oth_p = poi base ip in
                       if oth_p.sex = Male then do {
                         rel.r_fath := rel.r_moth;
                         rel.r_moth := p_index;
                         incr fixed;
                       }
                       else incr not_fixed
                   | None ->
                       do {
                         rel.r_fath := None;
                         rel.r_moth := p_index;
                         incr fixed;
                       } ]
                 else ()
             | (False, True) ->
                 if p_ref.sex = Male then
                   match rel.r_fath with
                   [ Some ip ->
                       let oth_p = poi base ip in
                       if oth_p.sex = Female then do {
                         rel.r_moth := rel.r_fath;
                         rel.r_fath := p_index;
                         incr fixed;
                       }
                       else incr not_fixed
                   | None ->
                       do {
                         rel.r_moth := None;
                         rel.r_fath := p_index;
                         incr fixed;
                       } ]
                  else ()
              | (False, False) -> ()
              | (True, True) -> incr not_fixed ])
          p.rparents)
      p_ref.related;
    warning (IncoherentSex p_ref fixed.val not_fixed.val);
  }
;

value related_sex_is_coherent base warning p_ref =
  let p_index = Some p_ref.cle_index in
  let merge_sex g1 g2 =
    match (g1, g2) with
    [ (Some Male, Some Male) -> Some Male
    | (Some Female, Some Female) -> Some Female
    | (Some Neuter, Some g) -> Some g
    | (Some g, Some Neuter) -> Some g
    | _ -> None ]
  in
  let check_sex sex rparents =
    List.fold_left
      (fun g rel ->
         match (p_index = rel.r_fath, p_index = rel.r_moth) with
         [ (True, False) -> merge_sex g (Some Male)
         | (False, True) -> merge_sex g (Some Female)
         | (False, False) -> g
         | (True, True) -> None ])
      sex rparents
  in
  let new_sex =
    List.fold_left (fun g ip -> let p = poi base ip in check_sex g p.rparents)
      (Some p_ref.sex) p_ref.related
  in
  match new_sex with
  [ Some g -> if p_ref.sex != g then p_ref.sex := g else ()
  | None -> try_to_fix_relation_sex base warning p_ref ]
;

value check_normal_marriage_date_for_someone base error warning fam ip =
  let p = poi base ip in
  match Adef.od_of_codate fam.marriage with
  [ Some d2 ->
      do {
        match Adef.od_of_codate p.birth with
        [ Some d1 ->
            if strictly_before d2 d1 then
              warning (MarriageDateBeforeBirth p)
            else ()
        | _ -> () ];
        match p.death with
        [ Death _ d3 ->
            let d3 = Adef.date_of_cdate d3 in
            if strictly_after d2 d3 then warning (MarriageDateAfterDeath p)
            else ()
        | _ -> () ];
      }
  | None -> () ]
;

value check_normal_marriage_date base error warning fam =
  let cpl = coi base fam.fam_index in
  do {
    check_normal_marriage_date_for_someone base error warning fam (father cpl);
    check_normal_marriage_date_for_someone base error warning fam (mother cpl);
  }
;

value sort_children base warning ifam des =
  let before = ref None in
  let a = des.children in
  do {
    for i = 1 to Array.length a - 1 do {
      let rec loop j =
        if j >= 0 then
          let p1 = poi base a.(j) in
          let p2 = poi base a.(j + 1) in
          let d1 =
            match Adef.od_of_codate p1.birth with
            [ Some d1 -> Some d1
            | None -> Adef.od_of_codate p1.baptism ]
          in
          let d2 =
            match Adef.od_of_codate p2.birth with
            [ Some d2 -> Some d2
            | None -> Adef.od_of_codate p2.baptism ]
          in
          match (d1, d2) with
          [ (Some d1, Some d2) ->
              if strictly_before d2 d1 then do {
                let ip = a.(j + 1) in
                match before.val with
                [ Some _ -> ()
                | None -> before.val := Some (Array.copy a) ];
                a.(j + 1) := a.(j);
                a.(j) := ip;
                loop (j - 1)
              }
              else ()
          | _ -> () ]
        else ()
      in
      loop (i - 1)
    };
    match before.val with
    [ None -> ()
    | Some a -> warning (ChangedOrderOfChildren ifam des a) ];
  }
;

value check_family base error warning fam cpl des =
  let ifam = fam.fam_index in
  let fath = poi base (father cpl) in
  let moth = poi base (mother cpl) in
  do {
    match fath.sex with
    [ Male -> birth_before_death base warning fath
    | _ ->
        if fam.relation = NoSexesCheck then ()
        else error (BadSexOfMarriedPerson fath) ];
    match moth.sex with
    [ Female -> birth_before_death base warning moth
    | _ ->
        if fam.relation = NoSexesCheck then ()
        else error (BadSexOfMarriedPerson moth) ];
    check_normal_marriage_date base error warning fam;
    sort_children base warning ifam des;
    let _ =
      List.fold_left
        (fun np child ->
           let child = poi base child in
           do {
             birth_before_death base warning child;
             born_after_his_elder_sibling base error warning child np ifam
               des;
             child_born_after_his_parent base error warning child (father cpl);
             child_born_after_his_parent base error warning child (mother cpl);
             child_born_before_mother_death base warning child (mother cpl);
             possible_father base warning child (father cpl);
             child_has_sex warning child;
             match Adef.od_of_codate child.birth with
             [ Some d -> Some (child, d)
             | _ -> np ]
           })
        None (Array.to_list des.children)
    in
    ();
  }
;

value check_person base error warning p =
  do {
    birth_before_death base warning p;
    List.iter (titles_after_birth base warning p) p.titles;
    related_sex_is_coherent base warning p;
  }
;

value is_deleted_family fam = fam.fam_index = Adef.ifam_of_int (-1);

value strip_all_trailing_spaces s =
  let b = Buffer.create (String.length s) in
  let len =
    loop (String.length s - 1) where rec loop i =
      if i < 0 then 0
      else
        match s.[i] with
        [ ' ' | '\t' | '\r' | '\n' -> loop (i - 1)
        | _ -> i + 1 ]
  in
  loop 0 where rec loop i =
    if i = len then Buffer.contents b
    else
      match s.[i] with
      [ '\r' -> loop (i + 1)
      | ' ' | '\t' ->
          loop0 (i + 1) where rec loop0 j =
            if j = len then Buffer.contents b
            else
              match s.[j] with
              [ ' ' | '\t' | '\r' -> loop0 (j + 1)
              | '\n' -> loop j
              | _ -> do { Buffer.add_char b s.[i]; loop (i + 1) } ]
      | c -> do { Buffer.add_char b c; loop (i + 1) } ]
;

value strip_spaces str =
  let start =
    loop 0 where rec loop i =
      if i == String.length str then i
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i + 1)
        | _ -> i ]
  in
  let stop =
    loop (String.length str - 1) where rec loop i =
      if i == -1 then i + 1
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1 ]
  in
  if start == 0 && stop == String.length str then str
  else if start > stop then ""
  else String.sub str start (stop - start)
;

value initial n =
  loop 0 where rec loop i =
    if i == String.length n then 0
    else
      match n.[i] with
      [ 'A'..'Z' | '�'..'�' -> i
      | _ -> loop (succ i) ]
;

value alphabetic_value =
  let tab = Array.create 256 0 in
  do {
    for i = 0 to 255 do { tab.(i) := 10 * i };
    tab.(Char.code '�') := tab.(Char.code 'a') + 1;
    tab.(Char.code '�') := tab.(Char.code 'a') + 2;
    tab.(Char.code '�') := tab.(Char.code 'a') + 3;
    tab.(Char.code '�') := tab.(Char.code 'e') + 1;
    tab.(Char.code '�') := tab.(Char.code 'e') + 2;
    tab.(Char.code '�') := tab.(Char.code 'e') + 3;
    tab.(Char.code '�') := tab.(Char.code 'e') + 4;
    tab.(Char.code '�') := tab.(Char.code 'o') + 1;
    tab.(Char.code '�') := tab.(Char.code 'A') + 2;
    tab.(Char.code '�') := tab.(Char.code 'A') + 5;
    tab.(Char.code '�') := tab.(Char.code 'E') + 1;
    tab.(Char.code '�') := tab.(Char.code 'E') + 2;
    tab.(Char.code '�') := tab.(Char.code 'O') + 4;
    tab.(Char.code '?') := 3000;
    fun x -> tab.(Char.code x)
  }
;

value alphabetic n1 n2 =
  let rec loop i1 i2 =
    if i1 == String.length n1 && i2 == String.length n2 then i1 - i2
    else if i1 == String.length n1 then -1
    else if i2 == String.length n2 then 1
    else
      let c1 = n1.[i1] in
      let c2 = n2.[i2] in
      if alphabetic_value c1 < alphabetic_value c2 then -1
      else if alphabetic_value c1 > alphabetic_value c2 then 1
      else loop (succ i1) (succ i2)
  in
  if n1 = n2 then 0 else loop (initial n1) (initial n2)
;

value map_title_strings f t =
  let t_name =
    match t.t_name with
    [ Tmain -> Tmain
    | Tname s -> Tname (f s)
    | Tnone -> Tnone ]
  in
  let t_ident = f t.t_ident in
  let t_place = f t.t_place in
  {t_name = t_name; t_ident = t_ident; t_place = t_place;
   t_date_start = t.t_date_start; t_date_end = t.t_date_end; t_nth = t.t_nth}
;

value map_relation_ps fp fs r =
  {r_type = r.r_type;
   r_fath =
     match r.r_fath with
     [ Some x -> Some (fp x)
     | None -> None ];
   r_moth =
     match r.r_moth with
     [ Some x -> Some (fp x)
     | None -> None ];
   r_sources = fs r.r_sources}
;

value map_person_ps fp fs p =
  {first_name = fs p.first_name; surname = fs p.surname; occ = p.occ;
   image = fs p.image;
   first_names_aliases = List.map fs p.first_names_aliases;
   surnames_aliases = List.map fs p.surnames_aliases;
   public_name = fs p.public_name; qualifiers = List.map fs p.qualifiers;
   titles = List.map (map_title_strings fs) p.titles;
   rparents = List.map (map_relation_ps fp fs) p.rparents;
   related = p.related; aliases = List.map fs p.aliases;
   occupation = fs p.occupation; sex = p.sex; access = p.access;
   birth = p.birth; birth_place = fs p.birth_place;
   birth_src = fs p.birth_src; baptism = p.baptism;
   baptism_place = fs p.baptism_place; baptism_src = fs p.baptism_src;
   death = p.death; death_place = fs p.death_place;
   death_src = fs p.death_src; burial = p.burial;
   burial_place = fs p.burial_place; burial_src = fs p.burial_src;
   notes = fs p.notes; psources = fs p.psources; cle_index = p.cle_index}
;

value map_family_ps fp fs fam =
  {marriage = fam.marriage; marriage_place = fs fam.marriage_place;
   marriage_src = fs fam.marriage_src; witnesses = Array.map fp fam.witnesses;
   relation = fam.relation; divorce = fam.divorce; comment = fs fam.comment;
   origin_file = fs fam.origin_file; fsources = fs fam.fsources;
   fam_index = fam.fam_index}
;

value map_couple_p multi_parents fp cpl =
  parent multi_parents (Array.map fp (parent_array cpl))
;

value map_descend_p fp des = {children = Array.map fp des.children};

value arg_list_of_string line =
  loop [] 0 0 None where rec loop list i len quote =
    if i == String.length line then
      if len == 0 then List.rev list else List.rev [Buff.get len :: list]
    else
      match (quote, line.[i]) with
      [ (Some c1, c2) ->
          if c1 == c2 then loop list (i + 1) len None
          else loop list (i + 1) (Buff.store len c2) quote
      | (None, ' ') ->
          let list = if len == 0 then list else [Buff.get len :: list] in
          loop list (i + 1) 0 quote
      | (None, ('"' | ''' as c)) -> loop list (i + 1) 0 (Some c)
      | (None, c) -> loop list (i + 1) (Buff.store len c) None ]
;

value sort_person_list base pl =
  Sort.list
    (fun p1 p2 ->
       match
         (Adef.od_of_codate p1.birth, p1.death, Adef.od_of_codate p2.birth,
          p2.death)
       with
       [ (Some d1, _, Some d2, _) -> strictly_before d1 d2
       | (Some d1, _, _, Death _ d2) ->
           strictly_before d1 (Adef.date_of_cdate d2)
       | (_, Death _ d1, Some d2, _) ->
           strictly_before (Adef.date_of_cdate d1) d2
       | (_, Death _ d1, _, Death _ d2) ->
           strictly_before (Adef.date_of_cdate d1) (Adef.date_of_cdate d2)
       | (Some _, _, _, _) -> False
       | (_, Death _ _, _, _) -> False
       | (_, _, Some _, _) -> True
       | (_, _, _, Death _ _) -> True
       | _ ->
           let c = alphabetic (p_surname base p1) (p_surname base p2) in
           if c == 0 then
             let c =
               alphabetic (p_first_name base p1) (p_first_name base p2)
             in
             if c == 0 then p1.occ > p2.occ else c > 0
           else c > 0 ])
    pl
;

value find_free_occ base f s i =
  let ipl = base.func.persons_of_name (f ^ " " ^ s) in
  let first_name = Name.lower f in
  let surname = Name.lower s in
  let list_occ =
    loop [] ipl where rec loop list =
      fun
      [ [ip :: ipl] ->
          let p = poi base ip in
          if not (List.mem p.occ list) &&
             first_name = Name.lower (p_first_name base p) &&
             surname = Name.lower (p_surname base p) then
            loop [p.occ :: list] ipl
          else loop list ipl
      | [] -> list ]
  in
  let list_occ = List.sort compare list_occ in
  loop 0 list_occ where rec loop cnt1 =
    fun
    [ [cnt2 :: list] ->
        if cnt1 = cnt2 then loop (cnt1 + 1) list else cnt1
    | [] -> cnt1 ]
;
