(* $Id: doc.ml,v 2.7.2.1 1999-10-21 21:07:03 ddr Exp $ *)

open Config;

value start_with s i p =
  i + String.length p < String.length s
  && String.lowercase (String.sub s i (String.length p)) = p
;

value http = "http://";

value copy conf pref s =
  let last8 = String.make 8 ' ' in
  let insert c =
    let c = Char.lowercase c in
    let c = if c = '\n' || c = '\r' then ' ' else c in
    if c = ' ' && last8.[7] = ' ' then ()
    else do String.blit last8 1 last8 0 7; last8.[7] := c; return ()
  in
  loop 0 where rec loop i =
    if i == String.length s then ()
    else
      do insert s.[i]; return
      if last8 = "<a href=" then
        let i = do Wserver.wprint "%c" s.[i]; return i + 1 in
        let i =
          if s.[i] = '"' then do Wserver.wprint "\""; return i + 1 else i
        in
        do if s.[i] = '#' || start_with s i http || start_with s i "mailto:"
           then ()
           else Wserver.wprint "%s" pref;
        return
        loop i
      else if String.sub last8 2 6 = "<body>" then
        let s =
          try " " ^ List.assoc "body_prop" conf.base_env with
          [ Not_found -> Util.default_body_prop conf ]
        in
        do Wserver.wprint "%s>" s; return loop (i + 1)
      else do Wserver.wprint "%c" s.[i]; return loop (i + 1)
;

value has_dotslash s =
  loop 0 where rec loop i =
    if i == String.length s then False
    else if start_with s i "./" then True
    else loop (i + 1)
;

value print conf =
  let v =
    match Util.p_getenv conf.env "v" with
    [ Some f -> f
    | None -> "" ]
  in
  let v = if v = "" then "index.htm" else v in
  if Filename.is_implicit v && not (has_dotslash v) then
    let v =
      if Filename.check_suffix v ".htm" then v
      else v ^ ".htm"
    in 
    let fname = Filename.concat Util.doc_dir.val v in
    match try Some (open_in fname) with [ Sys_error _ -> None ] with
    [ Some ic ->
        do Util.html conf; return
        let s =
          let len = ref 0 in
          do try
               loop () where rec loop () =
                 do len.val := Buff.store len.val (input_char ic); return
                 loop ()
             with
             [ End_of_file -> close_in ic ];
          return Buff.get len.val
       in
       let pref =
         let dir = Filename.dirname v ^ "/" in
         let dir = if dir = "./" then "" else dir in
         conf.command ^ "?m=DOC;v=" ^ dir
       in
       copy conf pref s
    | None -> Util.incorrect_request conf ]
  else Util.incorrect_request conf
;
