(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id: pa_sheet.ml,v 1.1.2.3 1999-04-11 19:28:13 ddr Exp $ *)

value token_of_xast =
  fun
  [ PXML.Xtag t p ->
      match String.lowercase t with
      [ "a" -> ("A", p)
      | "body" -> ("BODY", p)
      | "comm" -> ("COMM", p)
      | "else" -> ("ELSE", p)
      | "eval" -> ("EVAL", p)
      | "for" -> ("FOR", p)
      | "format" -> ("FORMAT", p)
      | "if" -> ("IF", p)
      | "img" -> ("IMG", p)
      | "let" -> ("LET", p)
      | "match" -> ("MATCH", p)
      | "with" -> ("WITH", p)
      | t -> if p = "" then ("TAG", t) else ("TAG", t ^ " " ^ p) ]
  | PXML.Xetag t ->
      match String.lowercase t with
      [ "body" -> ("E_BODY", "")
      | "for" -> ("E_FOR", "")
      | "format" -> ("E_FORMAT", "")
      | "if" -> ("E_IF", "")
      | "match" -> ("E_MATCH", "")
      | t -> ("ETAG", t) ]
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
  [ "A" | "BODY" | "COMM" | "E_BODY" | "E_FOR" | "E_FORMAT" | "E_IF"
  | "E_MATCH" | "ELSE" | "EOI" | "ETAG" | "EVAL" | "FOR" | "FORMAT" | "IF"
  | "IMG" | "IND" | "LET" | "MATCH" | "TAG" | "TEXT" | "WITH" -> ()
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
  [ ("E_FOR", "") -> "'</for>'"
  | ("E_FORMAT", "") -> "'</format>'"
  | ("E_IF", "") -> "'</if>'"
  | ("E_MATCH", "") -> "'</match>'"
  | ("EOI", "") -> "end of input"
  | ("IND", _) -> "tabulation"
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

value simple_expr_list_of_string loc sh s =
  let (e, el) =
    Eval.G.Entry.parse Eval.simple_expr_list_eoi
      (Eval.G.parsable (Stream.of_string s))
  in
  (Pcaml.expr_reloc (freloc loc sh) 0 e,
   List.map (Pcaml.expr_reloc (freloc loc sh) 0) el)
;

value patt_in_expr_of_string loc sh s =
  let (p, e) =
    Eval.G.Entry.parse Eval.patt_in_expr_eoi
      (Eval.G.parsable (Stream.of_string s))
  in
  (Pcaml.patt_reloc (freloc loc sh) 0 p,
   Pcaml.expr_reloc (freloc loc sh) 0 e)
;

value patt_eq_expr_of_string loc sh s =
  let (p, e) =
    Eval.G.Entry.parse Eval.patt_eq_expr_eoi
      (Eval.G.parsable (Stream.of_string s))
  in
  (Pcaml.patt_reloc (freloc loc sh) 0 p,
   Pcaml.expr_reloc (freloc loc sh) 0 e)
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
    | <:expr< let $p$ = $e1$ in $e2$ >> ->
        let nenv = get_defined_ident p @ env in
        <:expr< let $p$ = $expr env e1$ in $expr nenv e2$ >>
    | <:expr< match $e$ with [ $list:ml$ ] >> ->
        let ml = List.map (match_case env) ml in
        <:expr< match $expr env e$ with [ $list:ml$ ] >>
    | <:expr< if $e1$ then $e2$ else $e3$ >> ->
        <:expr< if $expr env e1$ then $expr env e2$ else $expr env e3$ >>
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

value print_with_antiquot loc sh s =
  eval 0 0 where rec eval i0 i =
    if i < String.length s then
      if s.[i] == '`' then
        let el1 =
          if i > i0 then
            let ss = String.sub s i0 (i - i0) in
            [<:expr< wprint "%s" $str:ss$ >>]
          else []
        in
        if i + 1 == String.length s then el1
        else
          let j =
            try String.index_from s (i + 1) '`' with
            [ Not_found -> String.length s ]
          in
          let ss = String.sub s (i + 1) (j - i - 1) in
          let e = expr_of_string loc (sh + i + 1) ss in
          el1 @ [<:expr< wprint "%s" $e$ >> :: eval (j + 1) (j + 1)]
      else eval i0 (i + 1)
    else if i > i0 then
      let ss = String.sub s i0 (i - i0) in
      [<:expr< wprint "%s" $str:ss$ >>]
    else []
;

value glob_no_conf_base =
  ["&&"; "||"; "+"; "-"; "^"; "="; "<>"; "<"; ">"; "not"; "string_of_int";
   "indent"; "wprint"]
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
    [ [ e = MATCH; ml = LIST0 match_case; E_MATCH ->
          let e = expr_of_string loc (String.length "<match ") e in
          <:expr< match $e$ with [ $list:ml$ ] >>
      | e = LET; el = LIST0 expr ->
          let (p, e) = patt_eq_expr_of_string loc (String.length "<let ") e in
          let body = expr_of_expr_list loc el in
          <:expr< let $p$ = $e$ in $body$ >>
      | e = FOR; el = LIST0 expr; E_FOR ->
          let (p, e) = patt_in_expr_of_string loc (String.length "<for ") e in
          let body = expr_of_expr_list loc el in
          <:expr< List.iter (fun $p$ -> $body$) $e$ >>
      | e = IF; el1 = LIST0 expr; ELSE; el2 = LIST0 expr; E_IF ->
          let e = expr_of_string loc (String.length "<if ") e in
          let e1 = expr_of_expr_list loc el1 in
          let e2 = expr_of_expr_list loc el2 in
          <:expr< if $e$ then $e1$ else $e2$ >>
      | e = IF; el1 = LIST0 expr; E_IF ->
          let e = expr_of_string loc (String.length "<if ") e in
          let e1 = expr_of_expr_list loc el1 in
          <:expr< if $e$ then $e1$ else () >>
      | e = EVAL ->
          let e = expr_of_string loc (String.length "<eval ") e in
          <:expr< wprint "%s" $e$ >>
      | e = A ->
          let s = print_with_antiquot loc (String.length "<a ") e in
          let el =
            [<:expr< wprint "<a " >> :: (s @ [<:expr< wprint ">" >>])]
          in
          <:expr< do $list:el$ return () >>
      | e = IMG ->
          let s = print_with_antiquot loc (String.length "<img ") e in
          let el =
            [<:expr< wprint "<img " >> :: (s @ [<:expr< wprint ">" >>])]
          in
          <:expr< do $list:el$ return () >>
      | e = FORMAT; tel = format_expr_list; E_FORMAT ->
          let (f, sel) =
            simple_expr_list_of_string loc (String.length "<format ") e
          in
          let e = <:expr< wprint $f$ >> in
          let e = List.fold_left (fun r e -> <:expr< $r$ $e$ >>) e sel in
          List.fold_left (fun r e -> <:expr< $r$ (fun _ -> $e$) >>) e tel
      | e = BODY ->
          if e = "" then
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
          else <:expr< wprint "<body%s>" $str:e$ >>
      | E_BODY -> <:expr< wprint "<p><em>Trailer to apply</em>\n</body>" >>
      | i = IND -> <:expr< indent $int:i$ >>
      | t = ETAG -> <:expr< wprint $str:"</" ^ t ^ ">"$ >>
      | t = TEXT -> <:expr< wprint $str:t$ >>
      | t = TAG -> <:expr< wprint "<%s>" $str:t$ >>
      | COMM; e = expr -> e ] ]
  ;
  format_expr_list:
    [ [ IND; el = format_expr_list -> el
      | e = expr; el = format_expr_list -> [e :: el]
      | -> [] ] ]
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
