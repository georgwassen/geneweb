(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id: pa_sheet.ml,v 1.1.2.2 1999-04-11 10:12:22 ddr Exp $ *)

value token_of_xast =
  fun
  [ PXML.Xtag "comm" p -> ("COMM", p)
  | PXML.Xtag "eval" p -> ("EVAL", p)
  | PXML.Xtag "for" p -> ("FOR", p)
  | PXML.Xtag "match" p -> ("MATCH", p)
  | PXML.Xtag "with" p -> ("WITH", p)
  | PXML.Xtag t "" -> ("TAG", t)
  | PXML.Xtag t p -> ("TAG", t ^ " " ^ p)
  | PXML.Xetag "for" -> ("EFOR", "")
  | PXML.Xetag "match" -> ("EMATCH", "")
  | PXML.Xetag t -> ("ETAG", t)
  | PXML.Xtext s -> ("TEXT", s)
  | PXML.Xind i -> ("IND", string_of_int i) ]
;

value locerr () = invalid_arg "Lexer: location function";
value loct_create () = ref (Array.create 1024 None);
value loct_func loct i =
  match
    if i < 0 || i >= Array.length loct.val then None
    else Array.unsafe_get loct.val i
  with
  [ Some loc -> loc
  | _ -> locerr () ]
;
value loct_add loct i loc =
  do if i >= Array.length loct.val then
       let new_tmax = Array.length loct.val * 2 in
       let new_loct = Array.create new_tmax None in
       do Array.blit loct.val 0 new_loct 0 (Array.length loct.val);
          loct.val := new_loct;
       return ()
     else ();
     loct.val.(i) := Some loc;
  return ()
;

value lexer_func strm =
  let loct = loct_create () in
  let token_stream =
    Stream.from
      (fun cnt ->
         let bp = Stream.count strm in
         let tok =
           if cnt = 0 then token_of_xast (PXML.indent 0 strm)
           else
             try token_of_xast (PXML.next_token strm) with
             [ Stream.Failure -> ("EOI", "") ]
         in
         do loct_add loct cnt (bp, Stream.count strm); return Some tok)
  in
  (token_stream, loct_func loct)
;

value lexer_using (p_con, p_prm) =
  match p_con with
  [ "COMM" | "EFOR" | "EMATCH" | "EOI" | "ETAG" | "EVAL" | "FOR" | "IND"
  | "MATCH" | "TAG" | "TEXT" | "WITH" -> ()
  | _ ->
      raise (Token.Error ("\
the constructor \"" ^ p_con ^ "\" is not recognized by Plexer")) ]
;

value lexer_tparse =
  fun
  [ (p_con, "") ->
      parser [: `(con, prm) when con = p_con :] -> prm
  | (p_con, p_prm) ->
      parser [: `(con, prm) when con = p_con && prm = p_prm :] -> prm ]
;

value lexer_text =
  fun
  [ ("EMATCH", "") -> "'</ematch>'"
  | (con, "") -> con
  | (con, prm) -> con ^ " \"" ^ prm ^ "\"" ]
;

value lexer =
  {Token.func = lexer_func;
   Token.using = lexer_using;
   Token.removing = fun [];
   Token.tparse = lexer_tparse;
   Token.text = lexer_text}
;

open Pcaml;

do Grammar.Unsafe.reinit_gram gram lexer;
   Grammar.Unsafe.clear_entry implem;
   Grammar.Unsafe.clear_entry expr;
return ();

value freloc loc sh (bp, ep) = (fst loc + sh + bp, fst loc + sh + ep);

value patt_of_string loc sh s =
  let (p, w) =
    Eval.G.Entry.parse Eval.patt_eoi (Eval.G.parsable (Stream.of_string s))
  in
  (Pcaml.patt_reloc (freloc loc sh) 0 p,
   match w with
   [ Some e -> Some (Pcaml.expr_reloc (freloc loc sh) 0 e)
   | None -> None ])
;

value expr_of_string loc sh s =
  let e =
    Eval.G.Entry.parse Eval.expr_eoi (Eval.G.parsable (Stream.of_string s))
  in
  Pcaml.expr_reloc (freloc loc sh) 0 e
;

value patt_in_expr_of_string s =
  Eval.G.Entry.parse Eval.patt_in_expr_eoi
    (Eval.G.parsable (Stream.of_string s))
;

value expr_of_expr_list loc =
  fun
  [ [] -> <:expr< () >>
  | [e] -> e
  | el -> <:expr< do $list:el$ return () >> ]
;

value defs =
  let loc = (0, 0) in
  List.map (fun x -> (x, loc))
    [<:str_item< open Def >>;
     <:str_item< open GlobDef >>]
;

value params = ref "";
value param_list () =
  loop [] 0 where rec loop pl i =
    match
      try Some (String.index_from params.val i ',') with
      [ Not_found -> None ]
    with
    [ Some j ->
        let p = String.sub params.val i (j - i) in
        loop [p :: pl] (j + 1)
    | None ->
        if i = String.length params.val then pl
        else
          let p =
            String.sub params.val i (String.length params.val - i)
          in
          [p :: pl] ]
;

value rec get_defined_ident =
  fun
  [ <:patt< $_$ . $_$ >> -> []
  | <:patt< _ >> -> []
  | <:patt< $lid:x$ >> -> [x]
  | <:patt< ($p1$ as $p2$) >> -> get_defined_ident p1 @ get_defined_ident p2
  | <:patt< $int:_$ >> -> []
  | <:patt< $str:_$ >> -> []
  | <:patt< $chr:_$ >> -> []
  | <:patt< [| $list:pl$ |] >> -> List.flatten (List.map get_defined_ident pl)
  | <:patt< ($list:pl$) >> -> List.flatten (List.map get_defined_ident pl)
  | <:patt< $uid:_$ >> -> []
  | <:patt< $p1$ $p2$ >> -> get_defined_ident p1 @ get_defined_ident p2
  | <:patt< { $list:lpl$ } >> ->
      List.flatten (List.map (fun (lab, p) -> get_defined_ident p) lpl)
  | <:patt< $p1$ | $p2$ >> -> get_defined_ident p1 @ get_defined_ident p2
  | <:patt< $p1$ .. $p2$ >> -> get_defined_ident p1 @ get_defined_ident p2
  | <:patt< ($p$ : $_$) >> -> get_defined_ident p
  | MLast.PaAnt _ p -> get_defined_ident p
  | MLast.PaXnd _ _ p -> get_defined_ident p ]
;

value adjust_expr =
  let rec expr env e =
    let loc = MLast.loc_of_expr e in
    match e with
    [ <:expr< $e1$ $e2$ >> -> <:expr< $expr env e1$ $expr env e2$ >>
    | <:expr< do $list:el$ return $e$ >> ->
        <:expr< do $list:List.map (expr env) el$ return $expr env e$ >>
    | <:expr< match $e$ with [ $list:ml$ ] >> ->
        let ml = List.map (match_case env) ml in
        <:expr< match $expr env e$ with [ $list:ml$ ] >>
    | <:expr< fun $p$ -> $e$ >> ->
        <:expr< fun $p$ -> $expr (get_defined_ident p @ env) e$ >>
    | <:expr< ($list:el$) >> -> <:expr< ($list:List.map (expr env) el$) >>
    | <:expr< $lid:s$ >> ->
        if List.mem s env then e else <:expr< $e$ conf base >>
    | _ -> e ]
  and match_case env (p, w, e) =
    let env = get_defined_ident p @ env in
    let w =
      match w with
      [ Some e -> Some (expr env e)
      | None -> None ]
    in
    (p, w, expr env e)
  in
  expr
;

value glob_no_conf_base =
  ["+"; "-"; "^"; "="; "<>"; "<"; ">"; "indent"; "wprint"]
;

EXTEND
  GLOBAL: implem expr;
  implem:
    [ [ el = LIST1 expr; EOI ->
          let env = param_list () in
          let el = List.map (adjust_expr (env @ glob_no_conf_base)) el in
          let e =
            List.fold_left (fun e p -> <:expr< fun $lid:p$ -> $e$ >>)
              <:expr< do $list:el$ return () >> env
          in
          let si = <:str_item< value f conf base = $e$ >> in
          defs @ [(si, loc)] ] ]
  ;
  expr:
    [ [ e = MATCH; ml = LIST0 match_case; EMATCH ->
          let e = expr_of_string loc (String.length "<match ") e in
          <:expr< match $e$ with [ $list:ml$ ] >>
      | e = FOR; el = LIST0 expr; EFOR ->
          let (p, e) = patt_in_expr_of_string e in
          let body = expr_of_expr_list loc el in
          <:expr< List.iter (fun $p$ -> $body$) $e$ >>
      | e = EVAL ->
          let e = expr_of_string loc (String.length "<eval ") e in
          <:expr< wprint "%s" $e$ >>
      | i = IND -> <:expr< indent $int:i$ >>
      | t = TAG -> <:expr< wprint "<%s>" $str:t$ >>
      | t = ETAG -> <:expr< wprint $str:"</" ^ t ^ ">"$ >>
      | t = TEXT -> <:expr< wprint $str:t$ >>
      | TAG "body" ->
          <:expr<
            let s =
              try " dir=" ^ Hashtbl.find conf.Config.lexicon " !dir" with
              [ Not_found -> "" ]
            in
            let s =
              try s ^ " " ^ List.assoc "body_prop" conf.Config.base_env with
              [ Not_found -> s ]
            in
            wprint "<body%s>" s >>
      | COMM; e = expr -> e ] ]
  ;
  match_case:
    [ [ p = WITH; el = LIST0 expr ->
          let (p, w) = patt_of_string loc (String.length "<with ") p in
          let e = expr_of_expr_list loc el in
          (p, w, e)
      | _ = TEXT; c = match_case -> c
      | _ = IND; c = match_case -> c ] ]
  ;
END;

Pcaml.add_option "-param" (Arg.String (fun s -> params.val := s))
  "<list> Parameters separated by commas."
;
