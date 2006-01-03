(* camlp4r ./pa_html.cmo *)
(* $Id: alln.ml,v 4.6.2.1 2006-01-03 12:04:10 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Config;
open Util;
open Gutil;

(* tools *)

value string_start_with ini s =
  loop 0 0 where rec loop i1 i2 =
    if i1 == String.length ini then True
    else if i2 == String.length s then
      if ini.[i1] == '_' then loop (i1 + 1) i2 else False
    else if s.[i2] == ini.[i1] || s.[i2] == ' ' && ini.[i1] == '_' then
      loop (i1 + 1) (i2 + 1)
    else False
;

value combine_by_ini ini list =
  let list =
    loop [] list where rec loop new_list =
      fun
      [ [] -> new_list
      | [(k, s, cnt) :: list] ->
          let ini_k =
            if String.length k > String.length ini then
              String.sub k 0 (String.length ini + 1)
            else k ^ String.make (String.length ini + 1 - String.length k) '_'
          in
          do {
            for i = 0 to String.length ini_k - 1 do {
              if ini_k.[i] == ' ' then ini_k.[i] := '_' else ()
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

value alphab_string conf is_surname s =
  if is_surname then surname_end s ^ surname_begin s else s
;

(* print *)

value print_title conf base is_surnames ini len =
  do {
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
        (String.capitalize ini)
    else
      Wserver.wprint " (%d %s)" base.data.persons.len
        (nominative (transl_nth_def conf "person/persons" 2 1));
  }
;

value print_alphabetic_big conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  do {
    header conf title;
    List.iter
      (fun (ini_k, _) ->
         do {
           stag "a" "href=\"%sm=%s;tri=A;k=%s\"" (commd conf) mode ini_k begin
             Wserver.wprint "%s" (String.capitalize ini_k);
           end;
           Wserver.wprint "\n";
         })
      list;
    html_p conf;
    Wserver.wprint "%s:\n" (capitale (transl conf "the whole list"));
    tag "ul" begin
      html_li conf;
      stag "a" "href=\"%sm=%s;tri=A;o=A;k=%s\"" (commd conf) mode ini begin
        Wserver.wprint "%s" (transl conf "long display");
      end;
      Wserver.wprint "\n";
      html_li conf;
      stag "a" "href=\"%sm=%s;tri=S;o=A;k=%s\"" (commd conf) mode ini begin
        Wserver.wprint "%s" (transl conf "short display");
      end;
      Wserver.wprint "\n";
      html_li conf;
      stag "a" "href=\"%sm=%s;tri=S;o=A;k=%s;cgl=on\"" (commd conf) mode ini
          begin
        Wserver.wprint "%s + %s" (transl conf "short display")
          (transl conf "cancel GeneWeb links");
      end;
      Wserver.wprint "\n";
    end;
    trailer conf;
  }
;

value print_alphabetic_all conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  do {
    header conf title;
    List.iter
      (fun (ini_k, _) ->
         do {
           stag "a" "href=\"#%s\"" ini_k begin
             Wserver.wprint "%s" (String.capitalize ini_k);
           end;
           Wserver.wprint "\n";
         })
      list;
    tag "ul" begin
      List.iter
        (fun (ini_k, l) ->
           do {
             html_li conf;
             stag "a" "name=\"%s\"" ini_k begin
               Wserver.wprint "%s" (String.capitalize ini_k);
             end;
             Wserver.wprint "\n";
             tag "ul" begin
               List.iter
                 (fun (s, cnt) ->
                    do {
                      html_li conf;
                      let href =
                        "m=" ^ mode ^ ";v=" ^ code_varenv (Name.lower s)
                      in
                      wprint_geneweb_link conf href
                        (alphab_string conf is_surnames s);
                      Wserver.wprint " (%d)\n" cnt;
                    })
                 l;
             end;
           })
        list;
    end;
    trailer conf;
  }
;

value print_alphabetic_small conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  do {
    header conf title;
    tag "ul" begin
      List.iter
        (fun (_, s, cnt) ->
           do {
             html_li conf;
             stag "a" "href=\"%sm=%s;v=%s\"" (commd conf) mode
                 (code_varenv (Name.lower s)) begin
               Wserver.wprint "%s" (alphab_string conf is_surnames s);
             end;
             Wserver.wprint " (%d)\n" cnt;
           })
        list;
    end;
    trailer conf;
  }
;

value print_frequency_any conf base is_surnames list len =
  let title _ = print_title conf base is_surnames "" len in
  let mode = if is_surnames then "N" else "P" in
  do {
    header conf title;
    tag "ul" begin
      List.iter
        (fun (cnt, l) ->
           do {
             html_li conf;
             Wserver.wprint "%d\n" cnt;
             tag "ul" begin
               List.iter
                 (fun s ->
                    do {
                      html_li conf;
                      stag "a" "href=\"%sm=%s;v=%s\"" (commd conf) mode
                          (code_varenv (Name.lower s)) begin
                        Wserver.wprint "%s"
                          (alphab_string conf is_surnames s);
                      end;
                      Wserver.wprint "\n";
                    })
                 l;
             end;
           })
        list;
    end;
    trailer conf;
  }
;

(* selection *)

(* version using the index *)
value select_names conf base is_surnames ini =
  let iii =
    if is_surnames then base.func.persons_of_surname
    else base.func.persons_of_first_name
  in
  let list =
    let start_k =
      if String.length ini > 0 && ini.[String.length ini - 1] == '_' then
        String.sub ini 0 (String.length ini - 1)
      else ini
    in
    match
      try Some (iii.cursor (String.capitalize start_k)) with
      [ Not_found -> None ]
    with
    [ Some istr ->
        loop istr [] where rec loop istr list =
          let s = nominative (sou base istr) in
          let k = Iobase.name_key s in
          if string_start_with ini k then
            let list =
              if s <> "?" then
                let my_list = iii.find istr in
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
                (*let cnt = List.length (iii.find istr) in*)
                if cnt = 0 then list
                else
                  match list with
                  [ [(k1, s1, cnt1) :: list1] ->
                      if k = k1 then [(k1, s1, cnt1 + cnt) :: list1]
                      else [(k, s, cnt) :: list]
                  | [] -> [(k, s, cnt)] ]
              else list
            in
            match try Some (iii.next istr) with [ Not_found -> None ] with
            [ Some istr -> loop istr list
            | None -> list ]
          else list
    | None -> [] ]
  in
  let list =
    let lim =
      match p_getint conf.env "atleast" with
      [ Some x -> x
      | None -> 0 ]
    in
    List.fold_left
      (fun list (k, s, cnt) ->
         if cnt >= lim then [(k, s, cnt) :: list] else list)
      [] list
  in
  (list, True)
;

value print_frequency conf base is_surnames =
  let _ = base.data.strings.array () in
  let list =
    let (list, _) = select_names conf base is_surnames "" in
    List.sort
      (fun (k1, _, cnt1) (k2, _, cnt2) ->
         if cnt1 > cnt2 then -1
         else if cnt1 < cnt2 then 1
         else if k1 < k2 then -1
         else if k1 > k2 then 1
         else 0)
      list
  in
  let len = List.length list in
  let list = combine_by_count list in
  print_frequency_any conf base is_surnames list len
;

value print_alphabetic conf base is_surnames =
  let ini =
    match p_getenv conf.env "k" with
    [ Some k -> String.lowercase k
    | _ -> "" ]
  in
  let fast =
    p_getenv conf.base_env "fast_alphabetic" = Some "yes" && ini = ""
  in
  let _ =
    if fast || String.length ini < 2 then
      let _ = base.data.strings.array () in ()
    else ()
  in
  let all =
    match p_getenv conf.env "o" with
    [ Some "A" -> True
    | _ -> False ]
  in
  let list =
    if fast then
      let list =
        loop [] 'Z' where rec loop list c =
          let list = [(String.make 1 c, "", 1) :: list] in
          if c = 'A' then list else loop list (Char.chr (Char.code c - 1))
      in
      list
    else
      let (list, sorted) = select_names conf base is_surnames ini in
      if sorted then list
      else Sort.list (fun (k1, _, _) (k2, _, _) -> k1 <= k2) list
  in
  let len = List.length list in
  if fast then
    let list = List.map (fun (s, _, _) -> (s, 1)) list in
    print_alphabetic_big conf base is_surnames ini list 1
  else if len >= 50 then
    let list = combine_by_ini ini list in
    if all then print_alphabetic_all conf base is_surnames ini list len
    else print_alphabetic_big conf base is_surnames ini list len
  else print_alphabetic_small conf base is_surnames ini list len
;

(* short print *)

value print_alphabetic_short conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  let need_ref = len >= 250 in
  do {
    header conf title;
    if need_ref then
      List.iter
        (fun (ini_k, _) ->
           do {
             stag "a" "href=\"#%s\"" ini_k begin
               Wserver.wprint "%s" (String.capitalize ini_k);
             end;
             Wserver.wprint "\n";
           })
        list
    else ();
    List.iter
      (fun (ini_k, l) ->
         do {
           html_p conf;
           list_iter_first
             (fun first (s, cnt) ->
                let href =
                  if not conf.cancel_links then
                    " href=\"" ^ commd conf ^ "m=" ^ mode ^ ";v=" ^
                      code_varenv (Name.lower s) ^ "\""
                  else ""
                in
                let name =
                  if first && need_ref then " name=" ^ ini_k else ""
                in
                do {
                  if not first then Wserver.wprint ",\n" else ();
                  if href <> "" || name <> "" then
                    Wserver.wprint "<a%s%s>" href name
                  else ();
                  Wserver.wprint "%s" (alphab_string conf is_surnames s);
                  if href <> "" || name <> "" then Wserver.wprint "</a>"
                  else ();
                  Wserver.wprint " (%d)" cnt;
                })
             l;
           Wserver.wprint "\n";
         })
      list;
    trailer conf;
  }
;

value print_short conf base is_surnames =
  let ini =
    match p_getenv conf.env "k" with
    [ Some k -> String.lowercase k
    | _ -> "" ]
  in
  let _ =
    if String.length ini < 2 then let _ = base.data.strings.array () in ()
    else ()
  in
  let list =
    let (list, sorted) = select_names conf base is_surnames ini in
    if sorted then list
    else Sort.list (fun (k1, _, _) (k2, _, _) -> k1 <= k2) list
  in
  let len = List.length list in
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
