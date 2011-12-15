(* camlp5r ./pa_html.cmo *)
(* $Id: alln.ml,v 1.1.1.1 2007/09/12 09:58:44 root Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;

value default_max_cnt = 2000;

(* tools *)

value string_start_with ini s =
  loop 0 0 where rec loop i1 i2 =
    if i1 = String.length ini then True
    else if i2 = String.length s then
      if ini.[i1] = '_' then loop (i1 + 1) i2 else False
    else if s.[i2] = ini.[i1] || s.[i2] = ' ' && ini.[i1] = '_' then
      loop (i1 + 1) (i2 + 1)
    else False
;

(* Algo de Knuth-Morris-Pratt *)
value init_next p =
  let m = String.length p in
  let next = Array.create m 0 in
  let i = ref 1 and j = ref 0 in
  do {
    while i.val < m - 1 do {
      if p.[i.val] = p.[j.val] then do { incr i; incr j; next.(i.val) := j.val } else
      if j.val = 0 then do { incr i; next.(i.val) := 0 } else j.val := next.(j.val)
    };
    next
  }
;

(* Algo de Knuth-Morris-Pratt *)
value kmp p =
  let next = init_next p
  and m = String.length p in
  fun s ->
    let n = String.length s
    and i = ref 0
    and j = ref 0 in
    do {
      while j.val < m && i.val < n do {
        if s.[i.val] = p.[j.val] then do { incr i; incr j } else
        if j.val = 0 then incr i else j.val := next.(j.val)
      };
      if j.val >= m then True else False
    }
;

value combine_by_ini ini list =
  let list =
    loop [] list where rec loop new_list =
      fun
      [ [] -> new_list
      | [(k, s, cnt) :: list] -> do {
          let ini_k =
            if String.length k > String.length ini then
              String.sub k 0 (index_of_next_char k (String.length ini))
            else k ^ String.make (String.length ini + 1 - String.length k) '_'
          in
          for i = 0 to String.length ini_k - 1 do {
            if ini_k.[i] = ' ' then ini_k.[i] := '_' else ()
          };
          let new_list =
            if ini_k = "_" then new_list
            else
              match new_list with
              [ [] -> [(ini_k, [(s, cnt)])]
              | [(ini_k1, l) :: ll] ->
                  if ini_k1 = ini_k then [(ini_k1, [(s, cnt) :: l]) :: ll]
                  else [(ini_k, [(s, cnt)]); (ini_k1, l) :: ll] ]
          in
          loop new_list list
        } ]
  in
  List.fold_left (fun new_l (ini_k, l) -> [(ini_k, List.rev l) :: new_l]) []
    list
;

value combine_by_count list =
  let list =
    loop [] list where rec loop new_list =
      fun
      [ [] -> new_list
      | [(_, s, cnt) :: list] ->
          let new_list =
            match new_list with
            [ [] -> [(cnt, [s])]
            | [(cnt1, l) :: ll] ->
                if cnt1 = cnt then [(cnt1, [s :: l]) :: ll]
                else [(cnt, [s]); (cnt1, l) :: ll] ]
          in
          loop new_list list ]
  in
  List.fold_left (fun new_l (cnt, l) -> [(cnt, List.rev l) :: new_l]) [] list
;

value alphab_string conf base is_surname s =
  if is_surname then
    if Mutil.utf_8_db.val then
      surname_end base s ^ surname_begin base s
    else old_surname_end s ^ old_surname_begin s
  else s
;

value lower_if_not_utf8 s =
  if Mutil.utf_8_db.val then s else Name.lower s
;

value capitalize_if_not_utf8 s =
  if Mutil.utf_8_db.val then s else String.capitalize s
;

value lowercase_if_not_utf8 s =
  if Mutil.utf_8_db.val then s else String.lowercase s
;

value new_name_key base s =
  let part = Util.get_particle base s in
  if part = "" then s
  else
    let i = String.length part in
    String.sub s i (String.length s - i) ^ " " ^ String.sub s 0 i
;

value name_key_compatible base s =
  if Mutil.utf_8_db.val then new_name_key base s else Mutil.name_key s
;

(* print *)

value print_title conf base is_surnames ini len = do {
  if len >= 2 then
    if is_surnames then
      Wserver.wprint (fcapitale (ftransl conf "the %d surnames")) len
    else Wserver.wprint (fcapitale (ftransl conf "the %d first names")) len
  else if is_surnames then
    Wserver.wprint "%s" (capitale (transl_nth conf "surname/surnames" 0))
  else
    Wserver.wprint "%s"
      (capitale (transl_nth conf "first name/first names" 0));
  if ini <> "" then
    Wserver.wprint " %s %s" (transl conf "starting with")
      (capitalize_if_not_utf8 ini)
  else
    Wserver.wprint " (%d %s)" (nb_of_persons base)
      (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1));
};

value displayify s =
  if Mutil.utf_8_db.val then
    loop 0 0 where rec loop i len =
      if i = String.length s then Buff.get len
      else
        let nbc = Name.nbc s.[i] in
        if nbc < 0 || i + nbc > String.length s then
          Buff.get (Buff.mstore len "...")
        else loop (i + nbc) (Buff.gstore len s i nbc)
  else String.capitalize s
;

value tr c1 s2 s =
  loop 0 0 where rec loop i len =
    if i = String.length s then Buff.get len
    else if String.unsafe_get s i = c1 then loop (i + 1) (Buff.mstore len s2)
    else loop (i + 1) (Buff.store len (String.unsafe_get s i))
;

value print_alphabetic_big conf base is_surnames ini list len too_big = do {
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  header conf title;
  tag "p" begin
    List.iter
      (fun (ini_k, _) ->
         stagn "a" "href=\"%sm=%s;tri=A;k=%s\"" (commd conf) mode
           (Util.code_varenv ini_k)
         begin
           Wserver.wprint "%s" (tr '_' "&nbsp;" (displayify ini_k));
         end)
      list;
  end;
  if len <= default_max_cnt && not too_big then do {
    stagn "p" begin
      Wserver.wprint "%s:" (capitale (transl conf "the whole list"));
    end;
    tag "ul" begin
      stagn "li" begin
        stag "a" "href=\"%sm=%s;tri=A;o=A;k=%s\"" (commd conf) mode ini begin
          Wserver.wprint "%s" (transl conf "long display");
        end;
      end;
      stagn "li" begin
        stag "a" "href=\"%sm=%s;tri=S;o=A;k=%s\"" (commd conf) mode ini begin
          Wserver.wprint "%s" (transl conf "short display");
        end;
      end;
      stagn "li" begin
        stag "a" "href=\"%sm=%s;tri=S;o=A;k=%s;cgl=on\"" (commd conf) mode ini
            begin
          Wserver.wprint "%s + %s" (transl conf "short display")
            (transl conf "cancel GeneWeb links");
        end;
      end;
    end;
  }
  else ();
  trailer conf;
};

value print_alphabetic_all conf base is_surnames ini list len = do {
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  header conf title;
  tag "p" begin
    List.iter
      (fun (ini_k, _) ->
         let ini = capitalize_if_not_utf8 ini_k in
         stagn "a" "href=\"#%s\"" ini begin
           Wserver.wprint "%s" (Mutil.tr '_' ' ' ini);
         end)
    list;
  end;
  tag "ul" begin
    List.iter
      (fun (ini_k, l) ->
         let ini = capitalize_if_not_utf8 ini_k in
         tag "li" begin
           stagn "a" "id=\"%s\"" ini_k begin
             Wserver.wprint "%s" (Mutil.tr '_' ' ' ini);
           end;
           tag "ul" begin
             List.iter
               (fun (s, cnt) ->
                  stagn "li" begin
                    let href =
                      "m=" ^ mode ^ ";v=" ^
                      code_varenv (lower_if_not_utf8 s) ^ ";t=A"
                    in
                    wprint_geneweb_link conf href
                      (alphab_string conf base is_surnames s);
                    Wserver.wprint " (%d)" cnt;
                  end)
               l;
           end;
         end)
      list;
  end;
  trailer conf;
};

value print_alphabetic_small conf base is_surnames ini list len = do {
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  header conf title;
  if list = [] then ()
  else
    tag "ul" begin
      List.iter
        (fun (_, s, cnt) ->
           stagn "li" begin
             stag "a" "href=\"%sm=%s;v=%s;t=A\"" (commd conf) mode
               (code_varenv (lower_if_not_utf8 s))
             begin
               Wserver.wprint "%s" (alphab_string conf base is_surnames s);
             end;
             Wserver.wprint " (%d)" cnt;
           end)
        list;
    end;
  trailer conf;
};

value print_frequency_any conf base is_surnames list len = do {
  let title _ = print_title conf base is_surnames "" len in
  let mode = if is_surnames then "N" else "P" in
  let n = ref 0 in
  header conf title;
  tag "ul" begin
    List.iter
      (fun (cnt, l) ->
         if n.val > default_max_cnt then ()
         else
           tag "li" begin
             Wserver.wprint "%d\n" cnt;
             tag "ul" begin
               List.iter
                 (fun s ->
                    stagn "li" begin
                      stag "a" "href=\"%sm=%s;v=%s\"" (commd conf) mode
                          (code_varenv (Name.lower s)) begin
                        Wserver.wprint "%s"
                          (alphab_string conf base is_surnames s);
                      end;
                      incr n;
                    end)
                 l;
             end;
           end)
      list;
  end;
  trailer conf;
};

(* selection *)

value select_names conf base is_surnames ini need_whole_list =
  let iii = 
    if is_surnames then persons_of_surname base
    else persons_of_first_name base
  in
  let (list, len) =
    let start_k = Mutil.tr '_' ' ' ini in
    match
      try Some (spi_first iii (capitalize_if_not_utf8 start_k)) with
      [ Not_found -> None ]
    with
    [ Some istr ->
        loop istr 0 [] where rec loop istr len list =
          let s = nominative (sou base istr) in
          let k = name_key_compatible base s in
          if string_start_with ini k then
            let (list, len) =
              if s <> "?" then
                let my_list = spi_find iii istr in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn =
                           if is_surnames then get_surname p
                           else get_first_name p
                         in
                         if eq_istr isn istr then [ip :: l] else l
                       else [ip :: l])
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in 
                let cnt = List.length my_list in
                if cnt = 0 then (list, len)
                else
                  match list with
                  [ [(k1, s1, cnt1) :: list1] ->
                      if k = k1 then
                        ([(k1, s1, cnt1 + cnt) :: list1], len - 1)
                      else ([(k, s, cnt) :: list], len)
                  | [] -> ([(k, s, cnt)], len) ]
              else (list, len)
            in
            match
              try Some (spi_next iii istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr (len + dlen) list
            | None -> (list, len) ]
          else (list, len)
    | None -> ([], 0) ]
  in
  let (list, len) =
    let lim =
      match p_getint conf.env "atleast" with
      [ Some x -> x
      | None -> 0 ]
    in
    List.fold_left
      (fun (list, len) (k, s, cnt) ->
         if cnt >= lim then ([(k, s, cnt) :: list], len) else (list, len - 1))
      ([], len) list
  in
  (list, if Mutil.utf_8_db.val then False else True, len)
;

value compare2 s1 s2 =
  if Mutil.utf_8_db.val then Gutil.alphabetic_utf_8 s1 s2 else compare s1 s2
;

value print_frequency conf base is_surnames =
  let () = load_strings_array base in
  let (list, _, len) = select_names conf base is_surnames "" True in
  let list =
    List.sort
      (fun (k1, _, cnt1) (k2, _, cnt2) ->
         if cnt1 > cnt2 then -1
         else if cnt1 < cnt2 then 1
         else compare2 k1 k2)
      list
  in
  let list = combine_by_count list in
  print_frequency_any conf base is_surnames list len
;

value print_alphabetic conf base is_surnames =
  let ini =
    match p_getenv conf.env "k" with
    [ Some k -> lowercase_if_not_utf8 k
    | _ -> "" ]
  in
  let fast =
    p_getenv conf.base_env "fast_alphabetic" = Some "yes" && ini = ""
  in
  let _ =
    if fast || String.length ini < 2 then load_strings_array base else ()
  in
  let all =
    match p_getenv conf.env "o" with
    [ Some "A" -> True
    | _ -> False ]
  in
  let (list, len) =
    if fast then
      loop [] 0 'Z' where rec loop list len c =
        let list = [(String.make 1 c, "", 1) :: list] in
        if c = 'A' then (list, len)
        else loop list (len + 1) (Char.chr (Char.code c - 1))
    else
      let (list, sorted, len) = select_names conf base is_surnames ini all in
      let list =
        if sorted then list
        else List.sort (fun (k1, _, _) (k2, _, _) -> compare2 k1 k2) list
      in
      (list, len)
  in
  if fast then
    let list = List.map (fun (s, _, _) -> (s, 1)) list in
    print_alphabetic_big conf base is_surnames ini list 1 True
  else if len >= 50 || ini = "" then
    let list = combine_by_ini ini list in
    if all then
      if len > default_max_cnt then incorrect_request conf
      else print_alphabetic_all conf base is_surnames ini list len
    else print_alphabetic_big conf base is_surnames ini list len False
  else print_alphabetic_small conf base is_surnames ini list len
;

(* short print *)

value print_alphabetic_short conf base is_surnames ini list len = do {
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  let need_ref = len >= 250 in
  header conf title;
  if need_ref then
    tag "p" begin
      List.iter
        (fun (ini_k, _) ->
           let ini = capitalize_if_not_utf8 ini_k in
           stagn "a" "href=\"#%s\"" ini begin
             Wserver.wprint "%s" (Mutil.tr '_' ' ' ini);
           end)
      list;
    end
  else ();
  List.iter
    (fun (ini_k, l) ->
       let ini = capitalize_if_not_utf8 ini_k in
       tag "p" begin
         list_iter_first
           (fun first (s, cnt) -> do {
              let href =
                if not conf.cancel_links then
                  " href=\"" ^ commd conf ^ "m=" ^ mode ^ ";v=" ^
                    code_varenv (lower_if_not_utf8 s) ^ ";t=A\""
                else ""
              in
              let name =
                if first && need_ref then " id=\"" ^ ini ^ "\"" else ""
              in
              if not first then Wserver.wprint ",\n" else ();
              if href <> "" || name <> "" then
                Wserver.wprint "<a%s%s>" href name
              else ();
              Wserver.wprint "%s" (alphab_string conf base is_surnames s);
              if href <> "" || name <> "" then Wserver.wprint "</a>"
              else ();
              Wserver.wprint " (%d)" cnt;
            })
           l;
         Wserver.wprint "\n";
       end)
    list;
  trailer conf;
};

value print_short conf base is_surnames =
  let ini =
    match p_getenv conf.env "k" with
    [ Some k -> lowercase_if_not_utf8 k
    | _ -> "" ]
  in
  let _ = if String.length ini < 2 then load_strings_array base else () in
  let (list, sorted, len) = select_names conf base is_surnames ini True in
  if len > default_max_cnt then incorrect_request conf
  else
    let list =
      if sorted then list
      else List.sort (fun (k1, _, _) (k2, _, _) -> compare2 k1 k2) list
    in
    let list = combine_by_ini ini list in
    print_alphabetic_short conf base is_surnames ini list len
;

(* main *)

value print_surnames conf base =
  match p_getenv conf.env "tri" with
  [ Some "F" -> print_frequency conf base True
  | Some "S" -> print_short conf base True
  | _ -> print_alphabetic conf base True ]
;

value print_first_names conf base =
  match p_getenv conf.env "tri" with
  [ Some "F" -> print_frequency conf base False
  | Some "S" -> print_short conf base False
  | _ -> print_alphabetic conf base False ]
;


(* FLH : recherche partielle *)
value select_both_start_with conf base ini_n ini_p need_whole_list =
  let name = persons_of_surname base in
  let list_maj =
    let start_n = Mutil.tr '_' ' ' ini_n in
    let letter = String.uppercase (String.sub start_n 0 1) in
    match
      try Some (spi_first name letter) with
      [ Not_found -> None ]
    with
    [ Some istr_n ->
        loop istr_n [] where rec loop istr_n list =
          let s = nominative (sou base istr_n) in
          let k = name_key_compatible base s in
(*          let _ = print_endline ((Name.lower ini_n) ^ "  --  " ^ (Name.lower k)) in   *)
if (String.sub k 0 1) = letter then
          if string_start_with (Name.lower ini_n) (Name.lower k) then
            let list =
              if s <> "?" then
                let my_list = spi_find name istr_n in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn = get_surname p in
                         let isp = sou base (get_first_name p) in
                         if (eq_istr isn istr_n) && 
                            (string_start_with (Name.lower ini_p) (Name.lower isp)) 
                         then [ip :: l] else l
                       else
                         let p = poi base ip in
                         let isp = sou base (get_first_name p) in
                         if string_start_with (Name.lower ini_p) (Name.lower isp) then 
                           [ip :: l]
                         else  l
                    )
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in
                my_list @ list
              else list
            in
            match
              try Some (spi_next name istr_n need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr_n, _) -> loop istr_n list
            | None -> list ]
          else 
            match
              try Some (spi_next name istr_n need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr_n, _) -> loop istr_n list
            | None -> list ]
else list
    | None -> [] ]
  in
  let list_min =
    let start_n = Mutil.tr '_' ' ' ini_n in
    let letter = String.lowercase (String.sub start_n 0 1) in
    match
      try Some (spi_first name letter) with
      [ Not_found -> None ]
    with
    [ Some istr_n ->
        loop istr_n [] where rec loop istr_n list =
          let s = nominative (sou base istr_n) in
          let k = name_key_compatible base s in
(*          let _ = print_endline ((Name.lower ini_n) ^ "  --  " ^ (Name.lower k)) in   *)
if (String.sub k 0 1) = letter then
          if string_start_with (Name.lower ini_n) (Name.lower k) then
            let list =
              if s <> "?" then
                let my_list = spi_find name istr_n in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn = get_surname p in
                         let isp = sou base (get_first_name p) in
                         if (eq_istr isn istr_n) && 
                            (string_start_with (Name.lower ini_p) (Name.lower isp)) 
                         then [ip :: l] else l
                       else
                         let p = poi base ip in
                         let isp = sou base (get_first_name p) in
                         if string_start_with (Name.lower ini_p) (Name.lower isp) then 
                           [ip :: l]
                         else  l
                    )
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in
                my_list @ list
              else list
            in
            match
              try Some (spi_next name istr_n need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr_n, _) -> loop istr_n list
            | None -> list ]
          else 
            match
              try Some (spi_next name istr_n need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr_n, _) -> loop istr_n list
            | None -> list ]
else list
    | None -> [] ]
  in
  list_maj @ list_min
;


value select_both_or conf base ini_n ini_p need_whole_list =
  let name = persons_of_surname base in
  let ini_n = code_varenv ini_n in
  let ini_n =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns [(String.sub s 0 index) :: acc]
      else [s::acc]
    in
    loop ini_n []
  in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = code_varenv ini_p in
  let ini_p =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns [(String.sub s 0 index) :: acc]
      else [s::acc]
    in
    loop ini_p []
  in
  let list =
    match
      try Some (spi_first name "A") with
      [ Not_found -> None ]
    with
    [ Some istr_n ->
        loop istr_n [] where rec loop istr_n list =
          let s = nominative (sou base istr_n) in
          let k = name_key_compatible base s in
(*          let _ = print_endline ((Name.lower ini_n) ^ "  --  " ^ (Name.lower k)) in   *)
(* let _ = (List.iter print_string ini_n) in *)
(* let _ = print_string (" -- " ^ (Name.lower k) ^ " -> ") in *)
(* let _ = print_endline (string_of_bool (List.exists (fun s -> kmp (Name.lower s) (Name.lower k)) ini_n)) in *)
          if List.exists (fun s -> kmp (Name.lower s) (Name.lower k)) ini_n then
            let list =
              if s <> "?" then
                let my_list = spi_find name istr_n in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn = get_surname p in
                         let isp = sou base (get_first_name p) in
                         if (eq_istr isn istr_n) && 
                            (List.exists (fun s -> kmp (Name.lower s) (Name.lower isp)) ini_p) 
                         then [ip :: l] else l
                       else
                         let p = poi base ip in
                         let isp = sou base (get_first_name p) in
                         if (List.exists (fun s -> kmp (Name.lower s) (Name.lower isp)) ini_p) then 
                           [ip :: l]
                         else  l
                    )
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in
                my_list @ list
              else list
            in
            match
              try Some (spi_next name istr_n need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr_n, _) -> loop istr_n list
            | None -> list ]
          else 
            match
              try Some (spi_next name istr_n need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr_n, _) -> loop istr_n list
            | None -> list ]
    | None -> [] ]
  in
  list
;

value select_both_all conf base ini_n ini_p need_whole_list = 
  let name = persons_of_surname base in
  let ini_n = code_varenv ini_n in
  let ini_n =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns [(String.sub s 0 index) :: acc]
      else [s::acc]
    in
    loop ini_n []
  in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = code_varenv ini_p in
  let ini_p =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns [(String.sub s 0 index) :: acc]
      else [s::acc]
    in
    loop ini_p []
  in
  let list =
    match
      try Some (spi_first name "A") with
      [ Not_found -> None ]
    with
    [ Some istr_n ->
        loop istr_n [] where rec loop istr_n list =
          let s = nominative (sou base istr_n) in
          let k = name_key_compatible base s in
(*          let _ = print_endline ((Name.lower ini_n) ^ "  --  " ^ (Name.lower k)) in   *)
(* let _ = (List.iter print_string ini_n) in *)
(* let _ = print_string (" -- " ^ (Name.lower k) ^ " -> ") in *)
(* let _ = print_endline (string_of_bool (List.exists (fun s -> kmp (Name.lower s) (Name.lower k)) ini_n)) in *)
          if List.for_all (fun s -> kmp (Name.lower s) (Name.lower k)) ini_n then
            let list =
              if s <> "?" then
                let my_list = spi_find name istr_n in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn = get_surname p in
                         let isp = sou base (get_first_name p) in
                         if (eq_istr isn istr_n) && 
                            (List.for_all (fun s -> kmp (Name.lower s) (Name.lower isp)) ini_p) 
                         then [ip :: l] else l
                       else
                         let p = poi base ip in
                         let isp = sou base (get_first_name p) in
                         if (List.for_all (fun s -> kmp (Name.lower s) (Name.lower isp)) ini_p) then 
                           [ip :: l]
                         else  l
                    )
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in
                my_list @ list
              else list
            in
            match
              try Some (spi_next name istr_n need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr_n, _) -> loop istr_n list
            | None -> list ]
          else 
            match
              try Some (spi_next name istr_n need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr_n, _) -> loop istr_n list
            | None -> list ]
    | None -> [] ]
  in
  list
; 

(* FLH : recherche partielle *)
value select_partial_1 conf base is_surnames ini need_whole_list =
  let name = 
    if is_surnames then persons_of_surname base
    else persons_of_first_name base
  in
  let (list, len) =
    let start_k = Mutil.tr '_' ' ' ini in
    match
      try Some (spi_first name (capitalize_if_not_utf8 start_k)) with
      [ Not_found -> None ]
    with
    [ Some istr ->
        loop istr 0 [] where rec loop istr len list =
          let s = nominative (sou base istr) in
          let k = name_key_compatible base s in
          if string_start_with ini k then
            let (list, len) =
              if s <> "?" then
                let my_list = spi_find name istr in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn =
                           if is_surnames then get_surname p
                           else get_first_name p
                         in
                         if eq_istr isn istr then [ip :: l] else l
                       else [ip :: l])
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in 
                let cnt = List.length my_list in
                if cnt = 0 then (list, len)
                else
                  match list with
                  [ [(k1, s1, cnt1) :: list1] ->
                      if k = k1 then
                        ([(k1, s1, cnt1 + cnt) :: list1], len - 1)
                      else ([(k, s, cnt) :: list], len)
                  | [] -> ([(k, s, cnt)], len) ]
              else (list, len)
            in
            match
              try Some (spi_next name istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr (len + dlen) list
            | None -> (list, len) ]
          else (list, len)
    | None -> ([], 0) ]
  in
  let (list, len) =
    let lim =
      match p_getint conf.env "atleast" with
      [ Some x -> x
      | None -> 0 ]
    in
    List.fold_left
      (fun (list, len) (k, s, cnt) ->
         if cnt >= lim then ([(k, s, cnt) :: list], len) else (list, len - 1))
      ([], len) list
  in
  (list, if Mutil.utf_8_db.val then False else True, len)
;

(* FLH : recherche partielle *)
value select_start_with conf base is_surnames ini need_whole_list =
  let name = 
    if is_surnames then persons_of_surname base
    else persons_of_first_name base
  in
  let list_maj =
    let start_k = Mutil.tr '_' ' ' ini  in
    let letter = String.uppercase (String.sub start_k 0 1) in
(*    let _ = print_endline ("list_maj commence par: " ^ start_k) in *)
    match
      try Some (spi_first name letter) with
      [ Not_found -> None ]
    with
    [ Some istr ->
        loop istr [] where rec loop istr list =
          let s = (sou base istr) in
          let k = (name_key_compatible base s) in
(*let _ = print_endline ( (String.sub k 0 1) ^ "  --  " ^ letter ^ " => " ^ string_of_bool ((String.sub k 0 1) = (letter))) in *)
if (String.sub k 0 1) = letter then
(*          let _ = print_endline ((Name.lower ini) ^ "  --  " ^ (Name.lower k) ^ " => " ^ string_of_bool (string_start_with (Name.lower ini) (Name.lower k))) in *)
          if string_start_with (Name.lower ini) (Name.lower k) then
            let list =
              if s <> "?" then
                let my_list = spi_find name istr in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn =
                           if is_surnames then get_surname p
                           else get_first_name p
                         in
                         if eq_istr isn istr then [ip :: l] else l
                       else [ip :: l])
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in 
                my_list @ list
              else list
            in
            match
              try Some (spi_next name istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr list
            | None -> list ]
          else 
            match
              try Some (spi_next name istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr list
            | None -> list ]
else list
    | None -> [] ]
  in
(*  let _ = print_endline ("") in *)
  let list_min =
    let start_k = Mutil.tr '_' ' ' ini  in
    let letter = String.lowercase (String.sub start_k 0 1) in
(*    let _ = print_endline ("list_min commence par: " ^ start_k) in *)
    match
      try Some (spi_first name letter) with
      [ Not_found -> None ]
    with
    [ Some istr ->
(*        let _ = print_endline ("istr: " ^ sou base istr) in *)
        loop istr [] where rec loop istr list =
          let s =  (sou base istr) in
          let k =  (name_key_compatible base s) in
(*let _ = print_endline ( (String.sub k 0 1) ^ "  --  " ^ letter ^ " => " ^ string_of_bool ((String.sub k 0 1) = (letter))) in *)
if (String.sub k 0 1) = letter then
(*          let _ = print_endline ((Name.lower ini) ^ "  --  " ^ (Name.lower k) ^ " => " ^ string_of_bool (string_start_with (Name.lower ini) (Name.lower k))) in *)
          if string_start_with (Name.lower ini) (Name.lower k) then
            let list =
              if s <> "?" then
                let my_list = spi_find name istr in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn =
                           if is_surnames then get_surname p
                           else get_first_name p
                         in
                         if eq_istr isn istr then [ip :: l] else l
                       else [ip :: l])
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in
                my_list @ list
              else list
            in
            match
              try Some (spi_next name istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr list
            | None -> list ]
          else
            match
              try Some (spi_next name istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr list
            | None -> list ]
else list
    | None -> [] ]
  in
  list_maj @ list_min
;


value select_or conf base is_surnames ini need_whole_list =
  let name = 
    if is_surnames then persons_of_surname base
    else persons_of_first_name base
  in
  let ini = code_varenv ini in
  let ini =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns [(String.sub s 0 index) :: acc]
      else [s::acc]
    in
    loop ini []
  in
  (* let _ =  *)
  (*   match *)
  (*      try Some (spi_first name "A") with *)
  (*      [ Not_found -> None] *)
  (*   with *)
  (*      [ Some istr ->  *)
  (*          loop istr where rec loop istr =  *)
  (*            let _ = print_endline (sou base istr) in *)
  (*            match *)
  (*               try Some (spi_next name istr need_whole_list) with *)
  (*                 [ Not_found -> None ] *)
  (*            with *)
  (*               [ Some (istr, dlen) -> loop istr *)
  (*             | None -> print_endline ("fin") ] *)
  (*      | None -> print_endline ("none") ] *)
  (* in *)
  let list_maj =
    match
      try Some (spi_first name "A") with
      [ Not_found -> None ]
    with
    [ Some istr ->
        loop istr [] where rec loop istr list =
          let s = (sou base istr) in
          let k = (name_key_compatible base s) in
(* let _ = (List.iter print_string ini) in *)
(* let _ = print_string (" -- " ^ (Name.lower k) ^ " -> ") in  *)
(* let _ = print_endline (string_of_bool (List.exists (fun s -> kmp (Name.lower s) (Name.lower k)) ini)) in *)
          if List.exists (fun s -> kmp (Name.lower s) (Name.lower k)) ini then
            let list =
              if s <> "?" then
                let my_list = spi_find name istr in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn =
                           if is_surnames then get_surname p
                           else get_first_name p
                         in
                         if eq_istr isn istr then [ip :: l] else l
                       else [ip :: l])
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in 
                my_list @ list
              else list
            in
            match
              try Some (spi_next name istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr list
            | None -> list ]
          else 
            match
              try Some (spi_next name istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr list
            | None -> list ]
    | None -> [] ]
  in
  list_maj
;

value select_all conf base is_surnames ini need_whole_list =
  let name = 
    if is_surnames then persons_of_surname base
    else persons_of_first_name base
  in
  let ini = code_varenv ini in
  let ini =
    let rec loop s acc =
      if String.contains s '+' then
        let index = String.index s '+' in
        let start = index + 1 in
        let len = String.length s - start in
        let ns = String.sub s start len in
        loop ns [(String.sub s 0 index) :: acc]
      else [s::acc]
    in
    loop ini []
  in
(*let _ = List.iter print_endline ini in *)
  let list_maj =
    match
      try Some (spi_first name "A") with
      [ Not_found -> None ]
    with
    [ Some istr ->
        loop istr [] where rec loop istr list =
          let s = (sou base istr) in
          let k = (name_key_compatible base s) in
(* let _ = (List.iter print_string ini) in *)
(* let _ = print_string (" -- " ^ (Name.lower k) ^ " -> ") in  *)
(* let _ = print_endline (string_of_bool (List.for_all (fun s -> kmp (Name.lower s) (Name.lower k)) ini)) in *)
          if List.for_all (fun s -> kmp (Name.lower s) (Name.lower k)) ini then
            let list =
              if s <> "?" then
                let my_list = spi_find name istr in
                let my_list =
                  List.fold_left
                    (fun l ip ->
                       if is_patched_person base ip then
                         let p = poi base ip in
                         let isn =
                           if is_surnames then get_surname p
                           else get_first_name p
                         in
                         if eq_istr isn istr then [ip :: l] else l
                       else [ip :: l])
                    [] my_list
                in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left
                      (fun l ip ->
                         if is_restricted conf base ip then l
                         else [ip :: l])
                      [] my_list
                  else my_list
                in 
                my_list @ list
              else list
            in
            match
              try Some (spi_next name istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr list
            | None -> list ]
          else 
            match
              try Some (spi_next name istr need_whole_list) with
              [ Not_found -> None ]
            with
            [ Some (istr, dlen) -> loop istr list
            | None -> list ]
    | None -> [] ]
  in
  list_maj
;


(* FLH : recherche partielle *)
value print_alph_all conf base is_surnames ini list len = do {
  let title _ = print_title conf base is_surnames ini len in
  header conf title;
    List.iter
      (fun (ini_k, l) ->
         List.iter
           (fun (s, cnt) ->
              stagn "li" begin
                Some.name_print conf base s;
              end)
           l )
      list;
  trailer conf; }
;

(* faire une value rec , ajouter la liste des conjoints *)
(* en parametre et faire : ^ liste conjoints ^ } *)
value person_to_JSON_string conf base p =
  let sep = '"' in
  let surname = Name.strip_c (p_surname base p) sep in
  let first_name = Name.strip_c (p_first_name base p) sep in
  let occ = string_of_int (get_occ p) in
  let key = code_varenv (Name.lower first_name) ^ "." ^ occ ^ " " ^ code_varenv (Name.lower surname) in
  let birth = Adef.od_of_codate (get_birth p) in
  let birth = match birth with
    [ Some d -> Date.string_of_date conf d
    | None -> "" ]
  in
  let death = CheckItem.date_of_death (get_death p) in
  let death = match death with
    [Some d -> Date.string_of_date conf d
    | None -> "" ]
  in
  let key = "\"key\":\"" ^ key ^ "\"," in
  let surname = "\"lastname\":\"" ^ surname ^ "\"," in
  let first_name = "\"firstname\":\"" ^ first_name ^ "\"," in
  let date_birth = "\"date_birth\":\"" ^ birth ^ "\"," in
  let date_death = "\"date_death\":\"" ^ death ^ "\"" in
  "{" ^ key ^ surname ^ first_name ^ date_birth ^ date_death ^ "}"
;

value fam_to_JSON_string conf base p fl =
  let sep = '"' in
  let surname = Name.strip_c (p_surname base p) sep in
  let first_name = Name.strip_c (p_first_name base p) sep in
  let occ = string_of_int (get_occ p) in
  let key = code_varenv (Name.lower first_name) ^ "." ^ occ ^ " " ^ code_varenv (Name.lower surname) in
  let birth = Adef.od_of_codate (get_birth p) in
  let birth = match birth with
    [ Some d -> Date.string_of_date conf d
    | None -> "" ]
  in
  let death = CheckItem.date_of_death (get_death p) in
  let death = match death with
    [Some d -> Date.string_of_date conf d
    | None -> "" ]
  in
  let key = "\"key\":\"" ^ key ^ "\"," in
  let surname = "\"lastname\":\"" ^ surname ^ "\"," in
  let first_name = "\"firstname\":\"" ^ first_name ^ "\"," in
  let date_birth = "\"date_birth\":\"" ^ birth ^ "\"," in
  let date_death = "\"date_death\":\"" ^ death ^ "\"" in
  let conj =
    if fl <> "" then 
      ",\"spouses\":[" ^ fl ^ "]"
    else ""
  in
  "{" ^ key ^ surname ^ first_name ^ date_birth ^ date_death ^ conj ^ "}"
;



value debug_l conf base list = do {
  Util.html conf ;
  Util.nl ();
  (* list = liste d'iper *)
  let person_l = List.map (fun i -> poi base i) list in
  let list =
  List.map
    (fun p ->
      let faml = Array.to_list (get_family p) in
      let fl =
        List.map
          (fun ifam ->
            let fam = foi base ifam in
            let c = spouse (get_key_index p) fam in
            pget conf base c)
          faml
      in
      let conj_l = 
        List.map
          (fun w -> person_to_JSON_string conf base w)
          fl
      in
      let s_w = String.concat "," conj_l in
      fam_to_JSON_string conf base p s_w )
    person_l
  in 
  let _ = Wserver.wprint "[" in
  let _ =
  Wserver.wprint "%s" (String.concat "," list)
  in
  Wserver.wprint "]" }
;


(* FLH : recherche partielle *)
value print_all conf base =
  match (p_getenv conf.env "n", p_getenv conf.env "p") with
  [ (Some n, Some fs) -> 
      let _ =
        if String.length n < 2 then load_strings_array base
        else ()
      in
      let list = match p_getenv conf.env "t" with
         [ Some "S" -> select_both_start_with conf base n fs True
         | Some "A" -> select_both_all conf base n fs True 
         | Some "O" -> select_both_or conf base n fs True 
         | _ -> [] ]
      in
      debug_l conf base list
  | (Some n, None) -> 
      let _ =
        if String.length n < 2 then load_strings_array base
        else ()
      in
      let list = match p_getenv conf.env "t" with
       [ Some "S" -> select_start_with conf base True n True
       | Some "A" -> select_all conf base True n True 
       | Some "O" -> select_or conf base True n True 
       | _ -> [] ]
      in
      debug_l conf base list
  | (None, Some fs) -> 
      let _ =
        if String.length fs < 2 then load_strings_array base
        else ()
      in
      let list = match p_getenv conf.env "t" with
       [ Some "S" -> select_start_with conf base False fs True
       | Some "A" -> select_all conf base False fs True 
       | Some "O" -> select_or conf base False fs True 
       | _ -> [] ]
      in
      debug_l conf base list
  | (None, None) -> () ]
;
  
