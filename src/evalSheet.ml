(* camlp4r q_MLast.cmo *)
(* $Id: evalSheet.ml,v 1.1.2.1 1999-04-08 16:54:11 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Util;

type statement =
  [ Sif of string and list statement and list statement
  | Sfor of string and list statement
  | Smatch of string and list (string * list statement)
  | Sxml of PXML.xast ]
;

(* Making statements *)

value stop_match_case =
  fun
  [ PXML.Xtag "case" [(_, _)] | PXML.Xetag "match" -> True
  | _ -> False ]
;

value stop_if =
  fun
  [ PXML.Xtag "else" [] | PXML.Xetag "if" -> True
  | _ -> False ]
;

value rec make_sequence stats xast =
  match make_statement xast with
  [ Some (s, xast) -> make_sequence [s :: stats] xast
  | None -> List.rev stats ]
and make_statement =
  fun
  [ [(PXML.Xtag "if" [(_, s)] as a) :: xast] ->
      match make_if xast with
      [ Some (then_c, else_c, xast) -> Some (Sif s then_c else_c, xast)
      | None -> Some (Sxml a, xast) ]
  | [(PXML.Xtag "for" [(_, s)] as a) :: xast] ->
      match make_sequence_upto_etag "for" [] xast with
      [ Some (statl, xast) -> Some (Sfor s statl, xast)
      | None -> Some (Sxml a, xast) ]
  | [(PXML.Xtag "match" [(_, s)] as a) :: xast] ->
      match make_match [] xast with
      [ Some (cases, xast) -> Some (Smatch s cases, xast)
      | None -> Some (Sxml a, xast) ]
  | [a :: xast] -> Some (Sxml a, xast)
  | [] -> None ]
and make_if xast =
  match make_sequence_upto stop_if [] xast with
  [ Some (then_c, xast) ->
      match xast with
      [ [PXML.Xtag "else" [] :: xast] ->
          match make_sequence_upto_etag "if" [] xast with
          [ Some (else_c, xast) -> Some (then_c, else_c, xast)
          | _ -> None ]
      | [PXML.Xetag "if" :: xast] -> Some (then_c, [], xast)
      | _ -> None ]
  | _ -> None ]
and make_match cases =
  fun
  [ [PXML.Xtag "case" [(_, p)] :: xast] -> make_match_case cases p xast
  | [PXML.Xetag "match" :: xast] -> Some (List.rev cases, xast)
  | [PXML.Xtext _ | PXML.Xnewl | PXML.Xind _ :: xast] -> make_match cases xast
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
  [ [Sxml (PXML.Xnewl | PXML.Xind _) :: statl] -> strip statl
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
    | Smatch e casel ->
        Smatch e (List.map do_match_case casel)
    | Sxml (PXML.Xind ind0) as a ->
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
  [ [Sxml (PXML.Xind ind) :: statl] ->
      match get_min_indent statl with
      [ Some sind -> Some (min ind sind)
      | None -> Some ind ]
  | [_ :: statl] -> get_min_indent statl
  | [] -> None ]
;

value reindent statl =
  let rev_statl =
    match List.rev statl with
    [ [Sxml (PXML.Xind ind); Sxml PXML.Xnewl :: statl] ->
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

value number_of_consecutive_nl = ref 0;
value wprint fmt =
  do number_of_consecutive_nl.val := 0; return Wserver.wprint fmt
;
value wprint_nl () =
  if number_of_consecutive_nl.val >= 2 then ()
  else do incr number_of_consecutive_nl; return Wserver.wprint "\n"
;

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

value rec eval_sequence conf global env =
  fun
  [ [] -> ()
  | [stat :: statl] ->
      let statl = eval_statement conf global env statl stat in
      eval_sequence conf global env statl ]
and eval_statement conf global env statl =
  fun
  [ Sif e then_c else_c ->
      do eval_if conf global env e then_c else_c; return statl
  | Sfor e body ->
      do eval_for conf global env e body; return statl
  | Smatch e pwel ->
      do eval_match conf global env (Eval.expr global env e) pwel; return statl
  | Sxml (PXML.Xtext s) -> do wprint "%s" s; return statl
  | Sxml (PXML.Xtag t tenv) ->
      match (String.lowercase t, tenv) with
      [ ("a" | "img", tenv) ->
          do wprint "<%s" t;
             List.iter
               (fun (k, s) ->
                  do wprint " %s=" k;
                     eval_string_with_antiquot global env s;
                  return ())
               tenv;
             wprint ">";
          return statl
      | ("comm", _) -> strip statl
      | ("strip", _) -> strip statl
      | ("eval", [(_, v)]) ->
          do wprint "%s" (string_of_dyn (Eval.expr global env v));
          return statl
      | ("format", [(_, v)]) ->
          Eval.wrap v (fun () -> eval_format conf global env statl v)
      | _ ->
          do wprint "<%s" t;
             List.iter (fun (p, e) -> wprint " %s=%s" p e) tenv;
             wprint ">";
          return statl ]
  | Sxml (PXML.Xetag "body") -> do trailer conf; return statl
  | Sxml (PXML.Xetag t) -> do wprint "</%s>" t; return statl
  | Sxml (PXML.Xind ind) ->
      do for i = 1 to ind do wprint " "; done; return statl
  | Sxml PXML.Xnewl -> do wprint_nl (); return statl ]
and eval_format conf global env statl v =
  let (fast, astl) = Eval.simple_expr_list v in
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
                      (eval_statement conf global env statl stat, astl)
                  | [] -> do wprint "%%t"; return (statl, astl) ]
              | (c, _) -> do wprint "%%%c" c; return (statl, astl) ]
            in
            loop (i + 2) statl astl
          else
            do wprint "%c" f.[i]; return loop (i + 1) statl astl
        else if i + 1 = String.length f then
          do wprint "%c" f.[i]; return statl
        else statl
  | t -> Eval.error v "format not of type string" (Some t) ]
and eval_if conf global env exp then_c else_c =
  let e = Eval.expr global env exp in
  match e.Eval.ctyp with
  [ <:ctyp< bool >> ->
      if (Obj.magic e.Eval.cval : bool) then
        eval_sequence conf global env (strip (reindent then_c))
      else
        eval_sequence conf global env (strip (reindent else_c))
  | t -> Eval.error exp "if condition is not of type bool" (Some t) ]
and eval_for conf global env s body =
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
                [ Some new_env -> eval_sequence conf global new_env body
                | None ->
                    Eval.error s "pattern type does not match expression type"
                      (Some t) ];
             return False)
          True (Obj.magic e.Eval.cval : list Obj.t)
      in
      ()
  | t -> Eval.error s "for set is not of type list" (Some t) ]
and eval_match conf global env exp =
  fun
  [ [(p, statl) :: pwel] ->
      match Eval.matching global env exp p with
      [ Some new_env ->
          let statl = strip (reindent statl) in
          eval_sequence conf global new_env statl
      | None ->
          eval_match conf global env exp pwel ]
  | [] -> () ]
;

value eval conf base env fname =
  let fname =
    List.fold_right Filename.concat [Util.lang_dir.val; "sheet"] fname
  in
  try
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
    eval_sequence conf global env seq
  with
  [ Exit -> ()
  | Stdpp.Exc_located loc (Stream.Error err) ->
      do Printf.eprintf "*** Error while parsing file \"%s\"\n" fname;
         Printf.eprintf "at location: (%d, %d)\n" (fst loc) (snd loc);
         Printf.eprintf "message: \"%s\"\n" err;
         flush stderr;
      return () ]
;
