(* camlp4r q_MLast.cmo *)
(* $Id: global.ml,v 1.1.2.6 1999-04-11 01:19:14 ddr Exp $ *)

open Def;
open Gutil;
open Config;

value loc = (0, 0);

value list =
  [("access",
    (fun conf base -> Obj.repr (GlobDef.access conf base : person -> string),
     <:ctyp< person -> string >>));
   ("aliases",
    (fun conf base ->
       Obj.repr (GlobDef.aliases conf base : person -> list string),
     <:ctyp< person -> list string >>));
   ("are_not_married",
    (fun conf base ->
       Obj.repr (GlobDef.are_not_married conf base : family -> bool),
     <:ctyp< family -> bool >>));
   ("auth",
    (fun conf base -> Obj.repr (GlobDef.auth conf base : person -> bool),
     <:ctyp< person -> bool >>));
   ("auto_image_file",
    (fun conf base ->
       Obj.repr (GlobDef.auto_image_file conf base : person -> option string),
     <:ctyp< person -> option string >>));
   ("baptism_date",
    (fun conf base ->
       Obj.repr (GlobDef.baptism_date conf base : person -> option date),
     <:ctyp< person -> option date >>));
   ("baptism_place",
    (fun conf base ->
       Obj.repr (GlobDef.baptism_place conf base : person -> string),
     <:ctyp< person -> string >>));
   ("basename",
    (fun conf base ->
       Obj.repr (GlobDef.basename conf base : string -> string),
     <:ctyp< string -> string >>));
   ("birth_date",
    (fun conf base ->
       Obj.repr (GlobDef.birth_date conf base : person -> option date),
     <:ctyp< person -> option date >>));
   ("birth_place",
    (fun conf base ->
       Obj.repr (GlobDef.birth_place conf base : person -> string),
     <:ctyp< person -> string >>));
   ("burial",
    (fun conf base ->
       Obj.repr (GlobDef.burial conf base : person -> GlobDef.burial),
     <:ctyp< person -> burial >>));
   ("burial_place",
    (fun conf base ->
       Obj.repr (GlobDef.burial_place conf base : person -> string),
     <:ctyp< person -> string >>));
   ("children",
    (fun conf base ->
       Obj.repr (GlobDef.children conf base : family -> list person),
     <:ctyp< family -> list person >>));
   ("commd",
    (fun conf base -> Obj.repr (GlobDef.commd conf base : string),
     <:ctyp< string>>));
   ("day",
    (fun conf base -> Obj.repr (GlobDef.day conf base : date -> int),
     <:ctyp< date -> int >>));
   ("death",
    (fun conf base ->
       Obj.repr (GlobDef.death conf base : person -> GlobDef.death),
     <:ctyp< person -> death >>));
   ("death_place",
    (fun conf base ->
       Obj.repr (GlobDef.death_place conf base : person -> string),
     <:ctyp< person -> string >>));
   ("father",
    (fun conf base -> Obj.repr (GlobDef.father conf base : family -> person),
     <:ctyp< family -> person >>));
   ("families",
    (fun conf base ->
       Obj.repr (GlobDef.families conf base : person -> list family),
     <:ctyp< person -> list family >>));
   ("find_person_in_env",
    (fun conf base ->
       Obj.repr
         (GlobDef.find_person_in_env conf base : string -> option person),
     <:ctyp< string -> option person >>));
   ("first_name",
    (fun conf base ->
       Obj.repr (GlobDef.first_name conf base : person -> string),
     <:ctyp< person -> string >>));
   ("first_names_aliases",
    (fun conf base ->
       Obj.repr
         (GlobDef.first_names_aliases conf base : person -> list string),
     <:ctyp< person -> list string >>));
   ("has_titles",
    (fun conf base ->
       Obj.repr (GlobDef.has_titles conf base : person -> bool),
     <:ctyp< person -> bool >>));
   ("image",
    (fun conf base -> Obj.repr (GlobDef.image conf base : person -> string),
     <:ctyp< person -> string >>));
   ("image_file_name",
    (fun conf base ->
       Obj.repr (GlobDef.image_file_name conf base : string -> option string),
     <:ctyp< string -> option string >>));
   ("image_size",
    (fun conf base ->
       Obj.repr (GlobDef.image_size conf base : string -> option (int * int)),
     <:ctyp< string -> option (int * int) >>));
   ("index",
    (fun conf base -> Obj.repr (GlobDef.index conf base : person -> int),
     <:ctyp< person -> int >>));
   ("is_anniversary",
    (fun conf base ->
       Obj.repr (GlobDef.is_anniversary conf base : date -> bool),
     <:ctyp< date -> bool >>));
   ("is_implicit_filename",
    (fun conf base ->
       Obj.repr (GlobDef.is_implicit_filename conf base : string -> bool),
     <:ctyp< string -> bool >>));
   ("is_private_access",
    (fun conf base ->
       Obj.repr (GlobDef.is_private_access conf base : person -> bool),
     <:ctyp< person -> bool >>));
   ("is_rtl",
    (fun conf base -> Obj.repr (GlobDef.is_rtl conf base : bool),
     <:ctyp< bool >>));
   ("is_wizard",
    (fun conf base -> Obj.repr (GlobDef.is_wizard conf base : bool),
     <:ctyp< bool >>));
   ("list_length",
    (fun conf base ->
       Obj.repr (GlobDef.list_length conf base : list 'a -> int),
     <:ctyp< list 'a -> int >>));
   ("lower",
    (fun conf base -> Obj.repr (GlobDef.lower conf base : string -> string),
     <:ctyp< string -> string >>));
   ("main_title",
    (fun conf base ->
       Obj.repr (GlobDef.main_title conf base : person -> option title),
     <:ctyp< person -> option title >>));
   ("marriage_date",
    (fun conf base ->
       Obj.repr (GlobDef.marriage_date conf base : family -> option date),
     <:ctyp< family -> option date >>));
   ("marriage_place",
    (fun conf base ->
       Obj.repr (GlobDef.marriage_place conf base : family -> string),
     <:ctyp< family -> string >>));
   ("modtime",
    (fun conf base -> Obj.repr (GlobDef.modtime conf base : string -> int),
     <:ctyp< string -> int >>));
   ("month",
    (fun conf base -> Obj.repr (GlobDef.month conf base : date -> int),
     <:ctyp< date -> int >>));
   ("mother",
    (fun conf base -> Obj.repr (GlobDef.mother conf base : family -> person),
     <:ctyp< family -> person >>));
   ("not",
    (fun conf base -> Obj.repr (GlobDef.not conf base : bool -> bool),
     <:ctyp< bool -> bool >>));
   ("notes",
    (fun conf base -> Obj.repr (GlobDef.notes conf base : person -> string),
     <:ctyp< person -> string >>));
   ("occupation",
    (fun conf base ->
       Obj.repr (GlobDef.occupation conf base : person -> string),
     <:ctyp< person -> string >>));
   ("ondate",
    (fun conf base -> Obj.repr (GlobDef.ondate conf base : date -> string),
     <:ctyp< date -> string >>));
   ("parents",
    (fun conf base ->
       Obj.repr (GlobDef.parents conf base : person -> option family),
     <:ctyp< person -> option family >>));
   ("passed_time",
    (fun conf base ->
       Obj.repr
         (GlobDef.passed_time conf base : date -> date -> (int * int * int)),
     <:ctyp< date -> date -> (int * int * int) >>));
   ("person_text",
    (fun conf base ->
       Obj.repr (GlobDef.person_text conf base : person -> string),
     <:ctyp< person -> string >>));
   ("precision",
    (fun conf base ->
       Obj.repr (GlobDef.precision conf base : date -> precision),
     <:ctyp< date -> precision >>));
   ("public_name",
    (fun conf base ->
       Obj.repr (GlobDef.public_name conf base : person -> string),
     <:ctyp< person -> string >>));
   ("qualifiers",
    (fun conf base ->
       Obj.repr (GlobDef.qualifiers conf base : person -> list string),
     <:ctyp< person -> list string >>));
   ("sex",
    (fun conf base -> Obj.repr (GlobDef.sex conf base : person -> int),
     <:ctyp< person -> int >>));
   ("spouse",
    (fun conf base ->
       Obj.repr (GlobDef.spouse conf base : person -> family -> person),
     <:ctyp< person -> family -> person >>));
   ("string_length",
    (fun conf base ->
       Obj.repr (GlobDef.string_length conf base : string -> int),
     <:ctyp< string -> int >>));
   ("string_of_int",
    (fun conf base ->
       Obj.repr (GlobDef.string_of_int conf base : int -> string),
     <:ctyp< int -> string >>));
   ("string_of_num_sep",
    (fun conf base ->
       Obj.repr (GlobDef.string_of_num_sep conf base : Num.t -> string),
     <:ctyp< num -> string >>));
   ("string_sub",
    (fun conf base ->
       Obj.repr
         (GlobDef.string_sub conf base : string -> int -> int -> string),
     <:ctyp< string -> int -> int -> string >>));
   ("surname",
    (fun conf base -> Obj.repr (GlobDef.surname conf base : person -> string),
     <:ctyp< person -> string >>));
   ("surnames_aliases",
    (fun conf base ->
       Obj.repr (GlobDef.surnames_aliases conf base : person -> list string),
     <:ctyp< person -> list string >>));
   ("title_ident",
    (fun conf base ->
       Obj.repr (GlobDef.title_ident conf base : title -> string),
     <:ctyp< title -> string >>));
   ("title_place",
    (fun conf base ->
       Obj.repr (GlobDef.title_place conf base : title -> string),
     <:ctyp< title -> string >>));
   ("titled_person_text",
    (fun conf base ->
       Obj.repr
         (GlobDef.titled_person_text conf base : person -> title -> string),
     <:ctyp< person -> title -> string >>));
   ("today",
    (fun conf base -> Obj.repr (GlobDef.today conf base : date),
     <:ctyp< date >>));
   ("transl",
    (fun conf base -> Obj.repr (GlobDef.transl conf base : string -> string),
     <:ctyp< string -> string >>));
   ("transl_nth",
    (fun conf base ->
       Obj.repr (GlobDef.transl_nth conf base : string -> int -> string),
     <:ctyp< string -> int -> string >>));
   ("url_encode",
    (fun conf base ->
       Obj.repr (GlobDef.url_encode conf base : string -> string),
     <:ctyp< string -> string >>));
   ("year_p",
    (fun conf base -> Obj.repr (GlobDef.year_p conf base : date -> string),
     <:ctyp< date -> string >>));
   ("+",
    (fun conf base -> Obj.repr (fun x y -> x + y : int -> int -> int),
     <:ctyp< int -> int -> int >>));
   ("-",
    (fun conf base -> Obj.repr (fun x y -> x - y : int -> int -> int),
     <:ctyp< int -> int -> int >>));
   ("^",
    (fun conf base ->
       Obj.repr (fun x y -> x ^ y : string -> string -> string),
     <:ctyp< string -> string -> string >>));
   ("=",
    (fun conf base -> Obj.repr (fun x y -> x = y : 'a -> 'a -> bool),
     <:ctyp< 'a -> 'a -> bool >>));
   ("<>",
    (fun conf base -> Obj.repr (fun x y -> x <> y : 'a -> 'a -> bool),
     <:ctyp< 'a -> 'a -> bool >>));
   ("<",
    (fun conf base -> Obj.repr (fun x y -> x < y : 'a -> 'a -> bool),
     <:ctyp< 'a -> 'a -> bool >>));
   (">",
    (fun conf base -> Obj.repr (fun x y -> x > y : 'a -> 'a -> bool),
     <:ctyp< 'a -> 'a -> bool >>))]
;

value list =
  [("expand_macros",
    (fun conf base ->
       Obj.repr
         (GlobDef.expand_macros conf base : string -> string),
     <:ctyp< string -> string >>));
   ("find_sosa",
    (fun conf base ->
       Obj.repr
         (GlobDef.find_sosa conf base : person -> person -> option Num.t),
     <:ctyp< person -> person -> option num >>));
   ("has_grand_children",
    (fun conf base ->
       Obj.repr
         (GlobDef.has_grand_children conf base : person -> bool),
     <:ctyp< person -> bool >>));
   ("has_grand_parents",
    (fun conf base ->
       Obj.repr
         (GlobDef.has_grand_parents conf base : person -> bool),
     <:ctyp< person -> bool >>));
   ("print_titles",
    (fun conf base ->
       Obj.repr
         (GlobDef.print_titles conf base : person -> string),
     <:ctyp< person -> string >>))
   :: list]
;

value table =
  let ht = Hashtbl.create 73 in
  do List.iter (fun (k, v) -> Hashtbl.add ht k v) list; return ht
;
