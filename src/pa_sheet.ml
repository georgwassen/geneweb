(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id: pa_sheet.ml,v 1.1.2.6 1999-04-15 00:57:09 ddr Exp $ *)

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
      | "strip" -> ("STRIP", p)
      | "with" -> ("WITH", p)
      | _ -> if p = "" then ("TAG", t) else ("TAG", t ^ " " ^ p) ]
  | PXML.Xetag t ->
      match String.lowercase t with
      [ "body" -> ("E_BODY", "")
      | "for" -> ("E_FOR", "")
      | "format" -> ("E_FORMAT", "")
      | "if" -> ("E_IF", "")
      | "match" -> ("E_MATCH", "")
      | _ -> ("ETAG", t) ]
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
  | "IMG" | "IND" | "LET" | "MATCH" | "STRIP" | "TAG" | "TEXT" | "WITH" -> ()
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

value rec strip =
  fun
  [ [<:expr< indent $_$ >> :: el] -> strip el
  | el -> el ]
;

value adjust_sequence =
  let rec expr env e =
    let loc = MLast.loc_of_expr e in
    match e with
    [ <:expr< do $list:el$ return () >> ->
        let el = sequence [] env el in
        expr_of_expr_list loc el
    | <:expr< let $p$ = $e1$ in $e2$ >> ->
        let nenv = get_defined_ident p @ env in
        <:expr< let $p$ = $expr env e1$ in $expr nenv e2$ >>
    | <:expr< match $e$ with [ $list:ml$ ] >> ->
        let ml = List.map (match_case env) ml in
        <:expr< match $expr env e$ with [ $list:ml$ ] >>
    | <:expr< if $e1$ then $e2$ else $e3$ >> ->
        <:expr< if $expr env e1$ then $expr env e2$ else $expr env e3$ >>
    | <:expr< fun $p$ -> $body$ >> ->
        <:expr< fun $p$ -> $expr (get_defined_ident p @ env) body$ >>
    | <:expr< $e1$ $e2$ >> -> <:expr< $expr env e1$ $expr env e2$ >>
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
  and sequence pend_nl env =
    fun
    [ [<:expr< do $list:el1$ return () >> :: el] ->
        sequence pend_nl env (el1 @ el)
    | [(<:expr< indent $_$ >> as e) :: el] -> sequence [e :: pend_nl] env el
    | [e :: el] ->
        if pend_nl = [] then [expr env e :: sequence [] env el]
        else
          let loc = MLast.loc_of_expr e in
          let el1 =
            match e with
            [ <:expr< match $e1$ with [ $list:ml$ ] >> ->
                let ml = List.map (dispatch_pend_in_case pend_nl env) ml in
                [<:expr< match $expr env e1$ with [ $list:ml$ ] >>]
            | <:expr< if $e1$ then $e2$ else () >> ->
                let e2 =
                  expr_of_expr_list (MLast.loc_of_expr e2)
                    (sequence pend_nl env [e2])
                in
                [<:expr< if $expr env e1$ then $e2$ else () >>]
            | <:expr< List.iter (fun $p$ -> $body$) $e1$ >> ->
                let body =
                  let seq =
                    sequence pend_nl (get_defined_ident p @ ["first" :: env])
                      [body]
                  in
                  expr_of_expr_list (MLast.loc_of_expr body) seq
                in
                let e1 = expr env e1 in
                [<:expr< List.iter (fun $p$ -> $body$) $e1$ >>]
            | _ ->
                List.rev pend_nl @ [expr env e] ]
          in
          el1 @ sequence [] env el
    | [] -> List.rev pend_nl ]
  and dispatch_pend_in_case pend_nl env (p, w, e) =
    let env = get_defined_ident p @ env in
    let w =
      match w with
      [ Some e -> Some (expr env e)
      | None -> None ]
    in
    let e =
      match e with
      [ <:expr< () >> -> e
      | _ ->
          expr_of_expr_list (MLast.loc_of_expr e) (sequence pend_nl env [e]) ]
    in
    (p, w, e)
  in
  sequence []
;

value wprint_str loc s =
  if String.contains s '%' then <:expr< wprint "%s" $str:s$ >>
  else <:expr< wprint $str:s$ >>
;

value print_with_antiquot loc sh s =
  eval 0 0 where rec eval i0 i =
    if i < String.length s then
      if s.[i] == '`' then
        let el1 =
          if i > i0 then
            let ss = String.sub s i0 (i - i0) in
            [wprint_str loc ss]
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
      [wprint_str loc ss]
    else []
;

value setindent ind el =
  let rec do_e e =
    let loc = MLast.loc_of_expr e in
    match e with
    [ <:expr< do $list:el$ return $e$ >> ->
        <:expr< do $list:List.map do_e el$ return $e$ >>
    | <:expr< if $e$ then $then_c$ else $else_c$ >> ->
        <:expr< if $e$ then $do_e then_c$ else $do_e else_c$ >>
    | <:expr< List.iter (fun $p$ -> $body$) $e$ >> ->
        <:expr< List.iter (fun $p$ -> $do_e body$) $e$ >>
    | <:expr< match $e$ with [ $list:ml$ ] >> ->
        <:expr< match $e$ with [ $list:List.map do_match_case ml$ ] >>
    | <:expr< let $p$ = $e$ in $body$ >> ->
        <:expr< let $p$ = $e$ in $do_e body$ >>
    | <:expr< indent $int:ind0$ >> ->
        let ind = max 0 (int_of_string ind0 - ind) in
        <:expr< indent $int:string_of_int ind$ >>
    | x -> x ]
  and do_match_case (p, w, e) =
    (p, w, do_e e)
  in
  List.map do_e el
;

value rec get_min_indent =
  fun
  [ [(<:expr< indent $_$ >> as e); <:expr< indent $_$ >> :: el] ->
      get_min_indent [e :: el]
  | [<:expr< indent $int:ind$ >> :: el] ->
      let ind = int_of_string ind in
      match get_min_indent el with
      [ Some sind -> Some (min ind sind)
      | None -> Some ind ]
(*
  | [<:expr< let $_$ = $_$ in do $list:el0$; return () >> :: el] ->
      get_min_indent (el0 @ el)
*)
  | [_ :: el] -> get_min_indent el
  | [] -> None ]
;

value reindent el =
  let rev_el =
    match List.rev el with
    [ [<:expr< indent $int:ind$ >> :: el] ->
        let ind = int_of_string ind in
        match get_min_indent el with
        [ Some sind -> setindent (sind - ind) el
        | None -> el ]
    | el -> el ]
  in
  List.rev rev_el
;

(*
value strip x = x;
value reindent x = x;
value adjust_sequence env el = el;
*)

value glob_no_conf_base =
  ["&&"; "||"; "+"; "-"; "^"; "="; "<>"; "<"; ">"; "not"; "string_of_int";
   "indent"; "wprint"]
;

EXTEND
  GLOBAL: implem expr;
  implem:
    [ [ el = LIST1 expr; EOI ->
          let el =
            strip2 el where rec strip2 =
              fun
              [ [<:expr< indent $_$ >> ::
                 ([<:expr< indent $_$ >>:: _] as el)] ->
                  strip2 el
              | [<:expr< indent 0 >> :: el] -> el
              | el -> el ]
          in                  
          let env = param_list () in
          let el = adjust_sequence (env @ glob_no_conf_base) el in
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
      | e = LET; el = sequence ->
          let (p, e) = patt_eq_expr_of_string loc (String.length "<let ") e in
          let body = expr_of_expr_list loc (strip el) in
          <:expr< let $p$ = $e$ in $body$ >>
      | e = FOR; el = sequence; E_FOR ->
          let (p, e) = patt_in_expr_of_string loc (String.length "<for ") e in
          let body = expr_of_expr_list loc (strip (reindent el)) in
          <:expr< List.iter (fun $p$ -> $body$) $e$ >>
      | e = IF; el1 = sequence; ELSE; el2 = sequence; E_IF ->
          let e = expr_of_string loc (String.length "<if ") e in
          let e1 = expr_of_expr_list loc (strip (reindent el1)) in
          let e2 = expr_of_expr_list loc (strip (reindent el2)) in
          <:expr< if $e$ then $e1$ else $e2$ >>
      | e = IF; el1 = sequence; E_IF ->
          let e = expr_of_string loc (String.length "<if ") e in
          let e1 = expr_of_expr_list loc (strip (reindent el1)) in
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
      | t = TAG -> wprint_str loc ("<" ^ t ^ ">")
      | COMM; e = expr -> e ] ]
  ;
  sequence:
    [ [ STRIP; el = sequence -> strip el
      | e = expr; el = sequence -> [e :: el]
      | -> [] ] ]
  ;
  format_expr_list:
    [ [ IND; el = format_expr_list -> el
      | e = expr; el = format_expr_list -> [e :: el]
      | -> [] ] ]
  ;
  match_case:
    [ [ p = WITH; el = sequence ->
          let (p, w) = patt_of_string loc (String.length "<with ") p in
          let e = expr_of_expr_list loc (strip (reindent el)) in
          (p, w, e)
      | _ = TEXT; c = match_case -> c
      | _ = IND; c = match_case -> c ] ]
  ;
END;

Pcaml.add_option "-param" (Arg.String (fun s -> params.val := s))
  "<list> Parameters separated by commas."
;
