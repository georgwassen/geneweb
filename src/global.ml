(* camlp4r q_MLast.cmo *)
(* $Id: global.ml,v 1.1.2.2 1999-04-09 08:34:57 ddr Exp $ *)

open Def;
open Gutil;
open Config;

value loc = (0, 0);

value table =
  let ht = Hashtbl.create 73 in
  let list =
    [("access",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.acces conf base p :
            person -> string),
       <:ctyp< person -> string >>));
     ("age",
      (fun conf base ->
         Obj.repr
           (fun d -> temps_ecoule d conf.today :
            date -> date),
       <:ctyp< date -> date >>));
     ("aliases",
      (fun conf base ->
         Obj.repr
           (fun p ->
              List.map (fun s -> Util.coa conf (sou base s)) p.aliases :
            person -> list string),
       <:ctyp< person -> list string >>));
     ("are_not_married",
      (fun conf base ->
         Obj.repr
           (fun fam -> fam.not_married :
            family -> bool),
       <:ctyp< family -> bool >>));
     ("auth",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.age_autorise conf base p :
            person -> bool),
       <:ctyp< person -> bool >>));
     ("auto_image_file",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.auto_image_file conf base p :
            person -> option string),
       <:ctyp< person -> option string >>));
     ("baptism_date",
      (fun conf base ->
         Obj.repr
           (fun p -> Adef.od_of_codate p.baptism :
            person -> option date),
       <:ctyp< person -> option date >>));
     ("baptism_place",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.coa conf (sou base p.baptism_place) :
            person -> string),
       <:ctyp< person -> string >>));
     ("basename",
      (fun conf base ->
         Obj.repr
           (fun s -> Filename.basename s :
            string -> string),
       <:ctyp< string -> string >>));
     ("birth_date",
      (fun conf base ->
         Obj.repr
           (fun p -> Adef.od_of_codate p.birth :
            person -> option date),
       <:ctyp< person -> option date >>));
     ("birth_place",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.coa conf (sou base p.birth_place) :
            person -> string),
       <:ctyp< person -> string >>));
     ("children",
      (fun conf base ->
         Obj.repr
           (fun fam -> List.map (poi base) (Array.to_list fam.children) :
            family -> list person),
       <:ctyp< family -> list person >>));
     ("commd",
      (fun conf base ->
         Obj.repr
           (Util.commd conf :
            string),
       <:ctyp< string>>));
     ("day",
      (fun conf base ->
         Obj.repr
           (fun d -> d.day :
            date -> int),
       <:ctyp< date -> int >>));
     ("death",
      (fun conf base ->
         Obj.repr
           (fun p -> p.death :
            person -> death),
       <:ctyp< person -> death >>));
     ("death_place",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.coa conf (sou base p.death_place) :
            person -> string),
       <:ctyp< person -> string >>));
     ("father",
      (fun conf base ->
         Obj.repr
           (fun fam -> poi base (coi base fam.fam_index).father :
            family -> person),
       <:ctyp< family -> person >>));
     ("families",
      (fun conf base ->
         Obj.repr
           (fun p -> List.map (foi base) (Array.to_list p.family) :
            person -> list family),
       <:ctyp< person -> list family >>));
     ("find_person_in_env",
      (fun conf base ->
         Obj.repr
           (fun x -> Util.find_person_in_env conf base x :
            string -> option person),
       <:ctyp< string -> option person >>));
     ("first_name",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.coa conf (sou base p.first_name) :
            person -> string),
       <:ctyp< person -> string >>));
     ("first_names_aliases",
      (fun conf base ->
         Obj.repr
           (fun p ->
              List.map (fun s -> Util.coa conf (sou base s))
                p.first_names_aliases :
            person -> list string),
       <:ctyp< person -> list string >>));
     ("has_titles",
      (fun conf base ->
         Obj.repr
           (fun p -> List.length p.titles > 0 :
            person -> bool),
       <:ctyp< person -> bool >>));
     ("image",
      (fun conf base ->
         Obj.repr (fun p -> sou base p.image : person -> string),
       <:ctyp< person -> string >>));
     ("image_file_name",
      (fun conf base ->
         Obj.repr
           (fun s ->
              let fname = Util.image_file_name conf.bname s in
              if Sys.file_exists fname then Some fname else None :
            string -> option string),
       <:ctyp< string -> option string >>));
     ("image_size",
      (fun conf base ->
         Obj.repr
           (Util.image_size :
            string -> option (int * int)),
       <:ctyp< string -> option (int * int) >>));
     ("index",
      (fun conf base ->
         Obj.repr
           (fun p -> Adef.int_of_iper p.cle_index : person -> int),
       <:ctyp< person -> int >>));
     ("is_anniversary",
      (fun conf base ->
         Obj.repr
           (fun d ->
              if d.prec = Sure then
                d.day = conf.today.day && d.month = conf.today.month &&
                d.year < conf.today.year
              || not (leap_year conf.today.year) && d.day = 29 &&
                d.month = 2 && conf.today.day = 1 &&
                conf.today.month = 3
              else False :
            date -> bool),
       <:ctyp< date -> bool >>));
     ("is_implicit_filename",
      (fun conf base ->
         Obj.repr
           (Filename.is_implicit :
            string -> bool),
       <:ctyp< string -> bool >>));
     ("is_private_access",
      (fun conf base ->
         Obj.repr
           (fun p -> p.access = Private && not conf.wizard && not conf.friend :
            person -> bool),
       <:ctyp< person -> bool >>));
     ("is_rtl",
      (fun conf base -> Obj.repr (conf.is_rtl : bool),
       <:ctyp< bool >>));
     ("is_wizard",
      (fun conf base ->
         Obj.repr
           (conf.wizard :
            bool),
       <:ctyp< bool >>));
     ("list_length",
      (fun conf base ->
         Obj.repr
           (List.length :
            list 'a -> int),
       <:ctyp< list 'a -> int >>));
     ("lower",
      (fun conf base ->
         Obj.repr
           (fun s -> Name.lower s :
            string -> string),
       <:ctyp< string -> string >>));
     ("main_title",
      (fun conf base ->
         Obj.repr
           (Util.main_title base :
            person -> option title),
       <:ctyp< person -> option title >>));
     ("marriage_date",
      (fun conf base ->
         Obj.repr
           (fun fam -> Adef.od_of_codate fam.marriage :
            family -> option date),
       <:ctyp< family -> option date >>));
     ("marriage_place",
      (fun conf base ->
         Obj.repr
           (fun fam -> Util.coa conf (sou base fam.marriage_place) :
            family -> string),
       <:ctyp< family -> string >>));
     ("modtime",
      (fun conf base ->
         Obj.repr
           (fun f ->
              let s = Unix.stat f in
              int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)) :
            string -> int),
       <:ctyp< string -> int >>));
     ("mother",
      (fun conf base ->
         Obj.repr
           (fun fam -> poi base (coi base fam.fam_index).mother :
            family -> person),
       <:ctyp< family -> person >>));
     ("not",
      (fun conf base ->
         Obj.repr
           (not :
            bool -> bool),
       <:ctyp< bool -> bool >>));
     ("notes",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.coa conf (sou base p.notes) :
            person -> string),
       <:ctyp< person -> string >>));
     ("occupation",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.coa conf (sou base p.occupation) :
            person -> string),
       <:ctyp< person -> string >>));
     ("ondate",
      (fun conf base ->
         Obj.repr
           (fun d -> Date.string_of_ondate conf d :
            date -> string),
       <:ctyp< date -> string >>));
     ("parents",
      (fun conf base ->
         Obj.repr
           (fun p ->
              match (aoi base p.cle_index).parents with
              [ Some ifam -> Some (foi base ifam)
              | None -> None ] :
            person -> option family),
       <:ctyp< person -> option family >>));
     ("person_text",
      (fun conf base ->
         Obj.repr
           (Util.person_text conf base :
            person -> string),
       <:ctyp< person -> string >>));
     ("public_name",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.coa conf (sou base p.public_name) :
            person -> string),
       <:ctyp< person -> string >>));
     ("qualifiers",
      (fun conf base ->
         Obj.repr
           (fun p ->
              List.map (fun s -> Util.coa conf (sou base s)) p.nick_names :
            person -> list string),
       <:ctyp< person -> list string >>));
     ("sex",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.index_of_sex p.sex :
            person -> int),
       <:ctyp< person -> int >>));
     ("spouse",
      (fun conf base ->
         Obj.repr
           (fun p fam -> poi base (Util.spouse p (coi base fam.fam_index)) :
            person -> family -> person),
       <:ctyp< person -> family -> person >>));
     ("string_length",
      (fun conf base ->
         Obj.repr
           (fun s -> String.length s :
            string -> int),
       <:ctyp< string -> int >>));
     ("string_of_num",
      (fun conf base ->
         Obj.repr
           (fun n ->
              let r = ref "" in
              do Num.print (fun x -> r.val := r.val ^ x)
                   (Util.transl conf "(thousand separator)") n;
              return r.val :
            Num.t -> string),
       <:ctyp< num -> string >>));
     ("string_sub",
      (fun conf base ->
         Obj.repr
           (fun s beg len -> String.sub s beg len :
            string -> int -> int -> string),
       <:ctyp< string -> int -> int -> string >>));
     ("surname",
      (fun conf base ->
         Obj.repr
           (fun p -> Util.coa conf (sou base p.surname) :
            person -> string),
       <:ctyp< person -> string >>));
     ("surnames_aliases",
      (fun conf base ->
         Obj.repr
           (fun p ->
              List.map (fun s -> Util.coa conf (sou base s))
                p.surnames_aliases :
            person -> list string),
       <:ctyp< person -> list string >>));
     ("title_ident",
      (fun conf base ->
         Obj.repr
           (fun t -> Util.coa conf (sou base t.t_ident) :
            title -> string),
       <:ctyp< title -> string >>));
     ("title_place",
      (fun conf base ->
         Obj.repr
           (fun t -> Util.coa conf (sou base t.t_place) :
            title -> string),
       <:ctyp< title -> string >>));
     ("titled_person_text",
      (fun conf base ->
         Obj.repr
           (Util.titled_person_text conf base :
            person -> title -> string),
       <:ctyp< person -> title -> string >>));
     ("titles",
      (fun conf base ->
         Obj.repr
           (fun p -> p.titles :
            person -> list title),
       <:ctyp< person -> list title >>));
     ("transl",
      (fun conf base ->
         Obj.repr
           (fun s ->
              if String.length s > 0 then
                match s.[0] with
                [ 'A'..'Z' ->
                    Util.capitale (Util.transl conf (String.uncapitalize s))
                | _ -> Util.transl conf s ]
              else "" :
            string -> string),
       <:ctyp< string -> string >>));
     ("transl_nth",
      (fun conf base ->
         Obj.repr
           (fun s i ->
              if String.length s > 0 then
                match s.[0] with
                [ 'A'..'Z' ->
                    Util.capitale
                      (Util.transl_nth conf (String.uncapitalize s) i)
                | _ -> Util.transl_nth conf s i ]
              else "" :
            string -> int -> string),
       <:ctyp< string -> int -> string >>));
     ("url_encode",
      (fun conf base ->
         Obj.repr
           (Util.code_varenv :
            string -> string),
       <:ctyp< string -> string >>));
     ("year_p",
      (fun conf base ->
         Obj.repr
           (fun d ->
              let s =
                match d.prec with
                [ Before -> "/"
                | About | Maybe | OrYear _ | YearInt _ -> "ca&nbsp;"
                | _ -> "" ]
              in
              let s = s ^ string_of_int d.year in
              match d.prec with
              [ After -> s ^ "/"
              | _ -> s ] :
           date -> string),
       <:ctyp< date -> string >>));
     ("-",
      (fun conf base ->
         Obj.repr (fun x y -> x - y : int -> int -> int),
       <:ctyp< int -> int -> int >>));
     ("^",
      (fun conf base ->
         Obj.repr (fun x y -> x ^ y : string -> string -> string),
       <:ctyp< string -> string -> string >>));
     ("=",
      (fun conf base ->
         Obj.repr (fun x y -> x = y : 'a -> 'a -> bool),
       <:ctyp< 'a -> 'a -> bool >>));
     ("<>",
      (fun conf base ->
         Obj.repr (fun x y -> x <> y : 'a -> 'a -> bool),
       <:ctyp< 'a -> 'a -> bool >>));
     (">",
      (fun conf base ->
         Obj.repr (fun x y -> x > y : 'a -> 'a -> bool),
       <:ctyp< 'a -> 'a -> bool >>))]
  in
  do List.iter (fun (k, v) -> Hashtbl.add ht k v) list; return ht
;
