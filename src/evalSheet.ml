(* camlp4r q_MLast.cmo *)
(* $Id: evalSheet.ml,v 1.1.2.10 1999-04-12 16:15:44 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Util;

type statement =
  [ Sif of string and list statement and list statement
  | Sfor of string and list statement
  | Sformat of string and list statement
  | Smatch of string and list (string * list statement)
  | Sxml of PXML.xast ]
;

(* Making statements *)

value stop_match_case =
  fun
  [ PXML.Xtag "with" _ | PXML.Xetag "match" -> True
  | _ -> False ]
;

value stop_if =
  fun
  [ PXML.Xtag "else" "" | PXML.Xetag "if" -> True
  | _ -> False ]
;

value rec make_sequence stats xast =
  match make_statement xast with
  [ Some (s, xast) -> make_sequence [s :: stats] xast
  | None -> List.rev stats ]
and make_statement =
  fun
  [ [(PXML.Xtag "if" s as a) :: xast] ->
      match make_if xast with
      [ Some (then_c, else_c, xast) -> Some (Sif s then_c else_c, xast)
      | None -> Some (Sxml a, xast) ]
  | [(PXML.Xtag "for" s as a) :: xast] ->
      match make_sequence_upto_etag "for" [] xast with
      [ Some (statl, xast) -> Some (Sfor s statl, xast)
      | None -> Some (Sxml a, xast) ]
  | [(PXML.Xtag "format" s as a) :: xast] ->
      match make_sequence_upto_etag "format" [] xast with
      [ Some (statl, xast) -> Some (Sformat s statl, xast)
      | None -> Some (Sxml a, xast) ]
  | [(PXML.Xtag "match" s as a) :: xast] ->
      match make_match [] xast with
      [ Some (cases, xast) -> Some (Smatch s cases, xast)
      | None -> Some (Sxml a, xast) ]
  | [a :: xast] -> Some (Sxml a, xast)
  | [] -> None ]
and make_if xast =
  match make_sequence_upto stop_if [] xast with
  [ Some (then_c, xast) ->
      match xast with
      [ [PXML.Xtag "else" "" :: xast] ->
          match make_sequence_upto_etag "if" [] xast with
          [ Some (else_c, xast) -> Some (then_c, else_c, xast)
          | _ -> None ]
      | [PXML.Xetag "if" :: xast] -> Some (then_c, [], xast)
      | _ -> None ]
  | _ -> None ]
and make_match cases =
  fun
  [ [PXML.Xtag "with" p :: xast] -> make_match_case cases p xast
  | [PXML.Xetag "match" :: xast] -> Some (List.rev cases, xast)
  | [PXML.Xtext _ | PXML.Xind _ :: xast] -> make_match cases xast
  | _ -> None ]
and make_match_case cases p xast =
  match make_sequence_upto stop_match_case [] xast with
  [ Some (s, xast) -> make_match [(p, s) :: cases] xast
  | None -> None ]
and make_sequence_upto stop stats xast =
  match make_statement xast with
  [ Some (Sxml a, xast) ->
      if stop a then Some (List.rev stats, [a :: xast])
      else make_sequence_upto stop [Sxml a :: stats] xast
  | Some (s, xast) -> make_sequence_upto stop [s :: stats] xast
  | None -> None ]
and make_sequence_upto_etag t stats xast =
  match make_statement xast with
  [ Some (Sxml (PXML.Xetag t1), xast) when t = t1 ->
      Some (List.rev stats, xast)
  | Some (s, xast) -> make_sequence_upto_etag t [s :: stats] xast
  | None -> None ]
;

value rec strip =
  fun
  [ [Sxml (PXML.Xind _) :: statl] -> strip statl
  | statl -> statl ]
;

(* Gadget to have an HTML well pretty printed; not necessary actually
   "reindent" could be the identity function *)

value setindent ind statl =
  let rec do_stat =
    fun
    [ Sif e then_c else_c ->
        Sif e (List.map do_stat then_c) (List.map do_stat else_c)
    | Sfor e body ->
        Sfor e (List.map do_stat body)
    | Sformat e body ->
        Sformat e (List.map do_stat body)
    | Smatch e casel ->
        Smatch e (List.map do_match_case casel)
    | Sxml (PXML.Xind ind0) ->
        let ind = max 0 (ind0 - ind) in
        Sxml (PXML.Xind ind)
    | x -> x ]
  and do_match_case (p, statl) =
    (p, List.map do_stat statl)
  in
  List.map do_stat statl
;

value rec get_min_indent =
  fun
  [ [(Sxml (PXML.Xind _) as stat); Sxml (PXML.Xind _) :: statl] ->
      get_min_indent [stat :: statl]
  | [Sxml (PXML.Xind ind) :: statl] ->
      match get_min_indent statl with
      [ Some sind -> Some (min ind sind)
      | None -> Some ind ]
  | [_ :: statl] -> get_min_indent statl
  | [] -> None ]
;

value reindent statl =
  let rev_statl =
    match List.rev statl with
    [ [Sxml (PXML.Xind ind) :: statl] ->
        match get_min_indent statl with
        [ Some sind -> setindent (sind - ind) statl
        | None -> statl ]
    | statl -> statl ]
  in
  List.rev rev_statl
;

(* Evaluating statements *)

value string_of_dyn d =
  match d.Eval.ctyp with
  [ <:ctyp< string >> -> (Obj.magic d.Eval.cval : string)
  | <:ctyp< int >> -> string_of_int (Obj.magic d.Eval.cval : int)
  | <:ctyp< num >> -> Num.to_string (Obj.magic d.Eval.cval : Num.t)
  | <:ctyp< unit >> -> ""
  | <:ctyp< $lid:i$ >> -> Eval.not_impl ("string_of_dyn ctyp " ^ i) 0
  | x -> Eval.not_impl "string_of_dyn ctyp" x ]
;

value bof = ref True;
value wprint fmt = do bof.val := False; return Wserver.wprint fmt;

value eval_string_with_antiquot global env s =
  eval 0 where rec eval i =
    if i < String.length s then
      if s.[i] == '`' then
        if i + 1 == String.length s then ()
        else
          let j =
            try String.index_from s (i + 1) '`' with
            [ Not_found -> String.length s ]
          in
          let d = Eval.expr global env (String.sub s (i + 1) (j - i - 1)) in
          do wprint "%s" (string_of_dyn d); return eval (j + 1)
      else do wprint "%c" s.[i]; return eval (i + 1)
    else ()
;

value flush_nl =
  fun
  [ [ind :: indl] ->
      do if bof.val then ()
         else List.iter (fun _ -> wprint "\n") [ind :: indl];
         for i = 1 to ind do wprint " "; done;
      return ()
  | [] -> () ]
;

value rec eval_sequence conf global env pend_nl =
  fun
  [ [] -> pend_nl
  | [stat :: statl] ->
      let (pend_nl, statl) =
        eval_statement conf global env pend_nl statl stat
      in
      eval_sequence conf global env pend_nl statl ]
and eval_statement conf global env pend_nl statl =
  fun
  [ Sif e then_c else_c ->
      do eval_if conf global env pend_nl e then_c else_c; return
      ([], statl)
  | Sfor e body ->
      do eval_for conf global env pend_nl e body; return ([], statl)
  | Sformat e body ->
      do eval_format conf global env pend_nl e body; return ([], statl)
  | Smatch e pwel ->
      do eval_match conf global env pend_nl (Eval.expr global env e) pwel;
      return ([], statl)
  | Sxml (PXML.Xtext s) ->
      do flush_nl pend_nl; wprint "%s" s; return ([], statl)
  | Sxml (PXML.Xetag "body") ->
      let pend_nl = match pend_nl with [ [_ :: l] -> l | [] -> [] ] in
      do flush_nl pend_nl; trailer conf; return ([], strip statl)
  | Sxml (PXML.Xtag t s) ->
      match String.lowercase t with
      [ "a" | "img" ->
          do flush_nl pend_nl;
             wprint "<%s" t;
             if s = "" then ()
             else
               do wprint " ";
                  eval_string_with_antiquot global env s;
               return ();
             wprint ">";
          return ([], statl)
      | "body" when s = "" ->
          do flush_nl pend_nl;
             let s =
               try " dir=" ^ Hashtbl.find conf.Config.lexicon " !dir" with
               [ Not_found -> "" ]
             in
             let s =
               try s ^ " " ^ List.assoc "body_prop" conf.Config.base_env with
               [ Not_found -> s ]
             in
             wprint "<body%s>" s;
          return ([], statl)
      | "comm" -> ([], statl)
      | "eval" ->
          do flush_nl pend_nl;
             wprint "%s" (string_of_dyn (Eval.expr global env s));
          return ([], statl)
      | "let" -> eval_let conf global env pend_nl statl s
      | "strip" -> ([], strip statl)
      | _ ->
          do flush_nl pend_nl;
             wprint "<%s" t;
             if s = "" then () else wprint " %s" s;
             wprint ">";
          return ([], statl) ]
  | Sxml (PXML.Xetag t) ->
      do flush_nl pend_nl; wprint "</%s>" t; return ([], statl)
  | Sxml (PXML.Xind ind) ->
      ([ind :: pend_nl], statl) ]
and eval_format conf global env pend_nl s statl =
  let (fast, astl) = Eval.simple_expr_list s in
  let x = Eval.eval_expr global env fast in
  match x.Eval.ctyp with
  [ <:ctyp< string >> ->
      let f = (Obj.magic x.Eval.cval : string) in
      loop 0 statl astl where rec loop i statl astl =
        if i + 1 < String.length f then
          if f.[i] = '%' then
            let (statl, astl) =
              match (f.[i+1], astl) with
              [ ('s', [ast :: astl]) ->
                  let x = Eval.eval_expr global env ast in
                  do wprint "%s" (string_of_dyn x); return (statl, astl)
              | ('t', _) ->
                  match strip statl with
                  [ [stat :: statl] ->
                      let (pend_nl, statl) =
                        eval_statement conf global env [] statl stat
                      in
                      (statl, astl)
                  | [] -> do wprint "%%t"; return (statl, astl) ]
              | (c, _) -> do wprint "%%%c" c; return (statl, astl) ]
            in
            loop (i + 2) statl astl
          else
            do wprint "%c" f.[i]; return loop (i + 1) statl astl
        else if i + 1 = String.length f then
          wprint "%c" f.[i]
        else ()
  | t -> Eval.error s "format not of type string" (Some t) ]
and eval_if conf global env pend_nl exp then_c else_c =
  let e = Eval.expr global env exp in
  match e.Eval.ctyp with
  [ <:ctyp< bool >> ->
      let seq =
        if (Obj.magic e.Eval.cval : bool) then then_c else else_c
      in
      let _ = eval_sequence conf global env pend_nl (strip (reindent seq)) in
      ()
  | t -> Eval.error exp "if condition is not of type bool" (Some t) ]
and eval_for conf global env pend_nl s body =
  let (p, e) = Eval.patt_in_expr global env s in
  match e.Eval.ctyp with
  [ <:ctyp< list $t$ >> ->
      let body = reindent body in
      let _ =
        List.fold_left
          (fun first x ->
             let body = if first then strip body else body in
             let x = {Eval.cval = x; Eval.ctyp = t} in
             do match Eval.eval_matching global env x (p, None) with
                [ Some new_env ->
                    let pend_nl = if first then pend_nl else [] in
                    let _ = eval_sequence conf global new_env pend_nl body in
                    ()
                | None ->
                    Eval.error s "pattern type does not match expression type"
                      (Some t) ];
             return False)
          True (Obj.magic e.Eval.cval : list Obj.t)
      in
      ()
  | t -> Eval.error s "for set is not of type list" (Some t) ]
and eval_let conf global env pend_nl statl s =
  let (p, e) = Eval.patt_eq_expr global env s in
  let statl = strip statl in
  match Eval.eval_matching global env e (p, None) with
  [ Some new_env -> (eval_sequence conf global new_env pend_nl statl, [])
  | None ->
      Eval.error s "pattern type does not match expression type"
        (Some e.Eval.ctyp) ]
and eval_match conf global env pend_nl exp =
  fun
  [ [(p, statl) :: pwel] ->
      match Eval.matching global env exp p with
      [ Some new_env ->
          let statl = strip (reindent statl) in
          let _ = eval_sequence conf global new_env pend_nl statl in ()
      | None ->
          eval_match conf global env pend_nl exp pwel ]
  | [] -> () ]
;

value f conf base env fname =
  let fname =
    List.fold_right Filename.concat [Util.lang_dir.val; "sheet"]
     (fname ^ ".txt")
  in
  try
    do Util.html conf; return
    let xast =
      let ic = open_in fname in
      let r = PXML.f (Stream.of_channel ic) in
      do close_in ic; return r
    in
    let global x =
      let (f, t) = Hashtbl.find Global.table x in
      {Eval.cval = f conf base; Eval.ctyp = t}
    in
    let seq = make_sequence [] xast in
    let pend_nl = eval_sequence conf global env [] seq in flush_nl pend_nl
  with
  [ Exit -> ()
  | Sys_error err ->
      do Printf.eprintf "*** Error while interpreting file \"%s\"\n" fname;
         Printf.eprintf "message: \"%s\"\n" err;
         flush stderr;
      return ()
  | Stdpp.Exc_located loc (Stream.Error err) ->
      do Printf.eprintf "*** Error while parsing file \"%s\"\n" fname;
         Printf.eprintf "at location: (%d, %d)\n" (fst loc) (snd loc);
         Printf.eprintf "message: \"%s\"\n" err;
         flush stderr;
      return () ]
;
