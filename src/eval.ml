(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id: eval.ml,v 1.1.2.8 1999-04-11 19:28:12 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

type dyn = { cval : Obj.t; ctyp : MLast.ctyp };

exception EvalError of string and option MLast.ctyp;
value eval_err s = raise (EvalError s None);
value type_err t s = raise (EvalError s (Some t));

value not_impl name x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.\tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  eval_err ("Eval." ^ name ^ ": not impl " ^ desc)
;

value not_impl2 name x y =
  let desc x =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.\tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  eval_err ("Eval." ^ name ^ ": not impl2 " ^ desc x ^ " " ^ desc y)
;

value rec print_type f =
  fun
  [ <:ctyp< $t1$ -> $t2$ >> ->
      do print_type1 f t1; f " -> "; return print_type f t2
  | x -> print_type1 f x ]
and print_type1 f =
  fun
  [ <:ctyp< $t1$ $t2$ >> ->
      do print_type1 f t1; f " "; return print_type2 f t2
  | x -> print_type2 f x ]
and print_type2 f =
  fun
  [ <:ctyp< $lid:s$ >> -> f s
  | <:ctyp< '$s$ >> -> f ("'" ^ s)
  | <:ctyp< ($list:tl$) >> ->
      let _ =
        List.fold_left (fun sep t -> do f sep; print_type f t; return " * ")
          "(" tl
      in
      f ")"
  | x -> not_impl "print_type2" x ]
;

value error s err t =
  do Printf.eprintf "*** while evaluating style sheet\n";
     Printf.eprintf "input: %s\n" s;
     Printf.eprintf "message: %s\n" err;
     match t with
     [ Some t ->
         do Printf.eprintf "type: ";
            print_type (fun x -> Printf.eprintf "%s" x) t;
            Printf.eprintf "\n";
         return ()
     | None -> () ];
     flush stderr;
  return raise Exit
;

module G = Grammar.Make (struct value lexer = Plexer.make (); end);
value expr_eoi = G.Entry.create "expr";
value patt_eoi = G.Entry.create "patt";
value patt_eq_expr_eoi = G.Entry.create "patt = expr";
value patt_in_expr_eoi = G.Entry.create "patt in expr";
value simple_expr_list_eoi = G.Entry.create "simple expr list";

GEXTEND G
  GLOBAL: expr_eoi patt_eoi patt_in_expr_eoi patt_eq_expr_eoi
    simple_expr_list_eoi;
  expr_eoi:
    [ [ e = expr; EOI -> e ] ]
  ;
  expr:
    [ [ e1 = expr; "OR"; e2 = expr -> <:expr< $e1$ || $e2$ >> ]
    | [ e1 = expr; "AND"; e2 = expr -> <:expr< $e1$ && $e2$ >> ]
    | [ e1 = expr; "!="; e2 = expr -> <:expr< $e1$ <> $e2$ >>
      | e1 = expr; "="; e2 = expr -> <:expr< $e1$ = $e2$ >>
      | e1 = expr; "LT"; e2 = expr -> <:expr< $e1$ < $e2$ >>
      | e1 = expr; "GT"; e2 = expr -> <:expr< $e1$ > $e2$ >> ]
    | [ e1 = expr; "+"; e2 = expr -> <:expr< $e1$ + $e2$ >>
      | e1 = expr; "-"; e2 = expr -> <:expr< $e1$ - $e2$ >>
      | e1 = expr; "^"; e2 = expr -> <:expr< $e1$ ^ $e2$ >> ]
    | [ e1 = expr; e2 = expr -> <:expr< $e1$ $e2$ >> ]
    | "simple"
      [ c = CHAR -> <:expr< $chr:c.[0]$ >>
      | s = STRING -> <:expr< $str:s$ >>
      | s = INT -> <:expr< $int:s$ >>
      | id = LIDENT -> <:expr< $lid:id$ >>
      | id = UIDENT -> <:expr< $uid:id$ >>
      | "["; "]" -> <:expr< [] >>
      | "["; e = expr; "]" -> <:expr< [$e$] >>
      | "["; e = expr; ";"; el = LIST1 expr SEP ";"; "]" ->
          List.fold_right (fun e el -> <:expr< [$e$ :: $el$] >>) [e :: el]
            <:expr< [] >>
      | "("; e = expr; ")" -> e
      | "("; e = expr; ","; el = LIST1 expr SEP ","; ")" ->
          <:expr< ($list:[e :: el]$) >> ] ]
  ;
  simple_expr_list_eoi:
    [ [ e = expr LEVEL "simple"; el = LIST0 (expr LEVEL "simple"); EOI ->
          (e, el) ] ]
  ;
  patt_eoi:
    [ [ p = patt; EOI -> (p, None)
      | p = patt; "when"; e = expr; EOI -> (p, Some e) ] ]
  ;
  patt:
    [ [ p1 = patt; p2 = patt -> <:patt< $p1$ $p2$ >> ]
    | [ id = LIDENT -> <:patt< $lid:id$ >>
      | id = UIDENT -> <:patt< $uid:id$ >>
      | s = INT -> <:patt< $int:s$ >>
      | s = STRING -> <:patt< $str:s$ >>
      | "_" -> <:patt< _ >>
      | "("; p = patt; ")" -> p
      | "("; p = patt; ","; pl = LIST1 patt SEP ","; ")" ->
          <:patt< ($list:[p :: pl]$) >>
      | "["; p1 = patt; "::"; p2 = patt; "]" ->
          <:patt< [$p1$ :: $p2$] >>
      | "["; "]" -> <:patt< [] >> ] ]
  ;
  patt_in_expr_eoi:
    [ [ p = patt; "in"; e = expr_eoi -> (p, e) ] ]
  ;
  patt_eq_expr_eoi:
    [ [ p = patt; "="; e = expr_eoi -> (p, e) ] ]
  ;
END;

value loc = (0, 0);

value rec unify t1 t2 =
  match (t1, t2) with
  [ (<:ctyp< $lid:s1$ >>, <:ctyp< $lid:s2$ >>) ->
      if s1 = s2 then []
      else eval_err ("failed to unify types \"" ^ s1 ^ "\" and \"" ^ s2 ^ "\"")
  | (<:ctyp< $t11$ $t12$ >>, <:ctyp< $t21$ $t22$ >>) ->
      unify t11 t21 @ unify t12 t22
  | (<:ctyp< '$x$ >>, t2) -> [(x, t2)]
  | (t1, <:ctyp< '$x$ >>) -> [(x, t1)]
  | (t1, t2) ->
      do Printf.eprintf "t1: "; print_type (fun s -> Printf.eprintf "%s" s) t1;
         Printf.eprintf "\n";
         Printf.eprintf "t2: "; print_type (fun s -> Printf.eprintf "%s" s) t2;
         Printf.eprintf "\n";
         flush stderr;
      return
      not_impl2 "unify" t1 t2 ]
;

value instance_cnt = ref 0;
value instance t =
  let rec loop assoc =
    fun
    [ <:ctyp< $t1$ $t2$ >> ->
        let (t1, assoc) = loop assoc t1 in
        let (t2, assoc) = loop assoc t2 in
        (<:ctyp< $t1$ $t2$ >>, assoc)
    | <:ctyp< $t1$ -> $t2$ >> ->
        let (t1, assoc) = loop assoc t1 in
        let (t2, assoc) = loop assoc t2 in
        (<:ctyp< $t1$ -> $t2$ >>, assoc)
    | <:ctyp< $lid:_$ >> as t -> (t, assoc)
    | <:ctyp< '$s$ >> ->
        try (<:ctyp< '$List.assoc s assoc$ >>, assoc) with
        [ Not_found ->
            let cnt = do incr instance_cnt; return instance_cnt.val in
            let var = "xy" ^ string_of_int cnt in
            (<:ctyp< '$var$ >>, [(s, var) :: assoc]) ]
    | <:ctyp< ($list:tl$) >> ->
        let (tl, assoc) =
          List.fold_right
            (fun t (tl, assoc) ->
               let (t, assoc) = loop assoc t in
               ([t :: tl], assoc))
            tl ([], assoc)
        in
        (<:ctyp< ($list:tl$) >>, assoc)
    | t -> not_impl "instance" t ]
  in
  fst (loop [] t)
;

value rec subst tenv t =
  match t with
  [ <:ctyp< $lid:_$ >> -> t
  | <:ctyp< $t1$ $t2$ >> -> <:ctyp< $subst tenv t1$ $subst tenv t2$ >>
  | <:ctyp< $t1$ -> $t2$ >> -> <:ctyp< $subst tenv t1$ -> $subst tenv t2$ >>
  | <:ctyp< '$x$ >> -> try List.assoc x tenv with [ Not_found -> t ]
  | <:ctyp< ($list:tl$) >> -> <:ctyp< ($list:List.map (subst tenv) tl$) >>
  | t -> not_impl "subst" t ]
;

value eval_expr global env =
  eval where rec eval =
    fun
    [ <:expr< $e1$ || $e2$ >> ->
        let v1 = eval e1 in
        match v1.ctyp with
        [ <:ctyp< bool >> ->
            match (Obj.magic v1.cval : bool) with
            [ False -> eval e2
            | True -> {cval = Obj.magic True; ctyp = <:ctyp< bool >>} ]
        | _ -> type_err v1.ctyp "bool type expected in OR" ]
    | <:expr< $e1$ && $e2$ >> ->
        let v1 = eval e1 in
        match v1.ctyp with
        [ <:ctyp< bool >> ->
            match (Obj.magic v1.cval : bool) with
            [ True -> eval e2
            | False -> {cval = Obj.magic False; ctyp = <:ctyp< bool >>} ]
        | _ -> type_err v1.ctyp "bool type expected in AND" ]
    | <:expr< [ $e$ :: $el$ ] >> ->
        let v = eval e in
        let vl = eval el in
        match vl.ctyp with
        [ <:ctyp< list $t$ >> ->
            let t = instance t in
            let tenv = unify t (instance v.ctyp) in
            let v = Obj.repr [v.cval :: Obj.magic vl.cval] in
            let t = subst tenv t in
            {cval = v; ctyp = <:ctyp< list $t$ >>}
        | _ -> type_err vl.ctyp "not list type" ]
    | <:expr< [] >> ->
        {cval = Obj.repr []; ctyp = <:ctyp< list 'a >>}
    | <:expr< $e1$ $e2$ >> ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        match instance v1.ctyp with
        [ <:ctyp< $t1$ -> $t$ >> ->
            let tenv = unify t1 (instance v2.ctyp) in
            let v = Obj.magic v1.cval v2.cval in
            let t = subst tenv t in
            {cval = v; ctyp = t}
        | _ -> type_err v1.ctyp "applied function not arrow type" ]
    | <:expr< $lid:s$ >> ->
        try List.assoc s env with
        [ Not_found ->
            try global s with
            [ Not_found -> eval_err ("unbound variable \"" ^ s ^ "\"") ] ]
    | <:expr< ($list:el$) >> ->
        let (len, vl, tl) =
          List.fold_left
            (fun (len, vl, tl) e ->
               let d = eval e in
               (len + 1, [d.cval :: vl], [d.ctyp :: tl]))
            (0, [], []) el
        in
        let v = Obj.repr (Array.of_list (List.rev vl)) in
        let t = <:ctyp< ($list:List.rev tl$) >> in
        {cval = v; ctyp = t}
    | <:expr< Sure >> ->
        {cval = Obj.repr Adef.Sure; ctyp = <:ctyp< precision >>}
    | <:expr< $str:s$ >> -> {cval = Obj.repr s; ctyp = <:ctyp< string >>}
    | <:expr< $int:s$ >> ->
        {cval = Obj.repr (int_of_string s); ctyp = <:ctyp< int >>}
    | e -> not_impl "eval_expr" e ]
;

value eval_simple_matching d p =
  matching d.cval (d.ctyp, p) where rec matching v =
    fun
    [ (t, <:patt< $lid:s$ >>) ->
(*
do Printf.eprintf "... binding %s\n" s; flush stderr; return
*)
        Some [(s, {cval = v; ctyp = t})]
    | (t, <:patt< _ >>) -> Some []
    | (<:ctyp< ($list:tl$) >>, <:patt< ($list:pl$) >>) ->
        let v = Array.to_list (Obj.magic v) in
        loop (Some [], v, tl, pl) where rec loop =
          fun
          [ (Some env, [v :: vl], [t :: tl], [p :: pl]) ->
              match matching v (t, p) with
              [ Some env1 -> loop (Some (env1 @ env), vl, tl, pl)
              | None -> None ]
          | (envo, [], [], []) -> envo
          | _ -> None ]
    | (<:ctyp< death >>, p) ->
        match ((Obj.magic v : Def.death), p) with
        [ (Def.NotDead, <:patt< NotDead >>) -> Some []
        | (Def.Death dr dd, <:patt< Death $p1$ $p2$ >>) ->
            match
              (matching (Obj.repr (dr : Def.death_reason))
                 (<:ctyp< death_reason >>, p1),
               matching (Obj.repr (Adef.date_of_cdate dd : Adef.date))
                 (<:ctyp< date >>, p2))
            with
            [ (Some env1, Some env2) -> Some (env1 @ env2)
            | _ -> None ]
        | (Def.DeadYoung, <:patt< DeadYoung >>) -> Some []
        | (Def.DeadDontKnowWhen, <:patt< DeadDontKnowWhen >>) -> Some []
        | (Def.DontKnowIfDead, <:patt< DontKnowIfDead >>) -> Some []
        | (_, <:patt< NotDead >> | <:patt< Death $_$ $_$ >>) -> None
        | (_, <:patt< DeadYoung >> | <:patt< DeadDontKnowWhen >>) -> None
        | (_, <:patt< DontKnowIfDead >>) -> None
        | _ -> eval_err "matching a death type with incompatible pattern" ]
    | (<:ctyp< death_reason >>, p) ->
        match ((Obj.magic v : Def.death_reason), p) with
        [ (Def.Unspecified, <:patt< Unspecified >>) -> Some []
        | (Def.Murdered, <:patt< Murdered >>) -> Some []
        | (Def.Killed, <:patt< Killed >>) -> Some []
        | (Def.Executed, <:patt< Executed >>) -> Some []
        | (Def.Disappeared, <:patt< Disappeared >>) -> Some []
        | (_, <:patt< Unspecified >> | <:patt< Murdered >>) -> None
        | (_, <:patt< Killed >> | <:patt< Executed >>) -> None
        | (_, <:patt< Disappeared >>) -> None
        | _ ->
            eval_err "matching a death_reason type with incompatible pattern" ]
    | (<:ctyp< burial >>, p) ->
        match ((Obj.magic v : Def.burial), p) with
        [ (Def.Buried d, <:patt< Buried $p$ >>) ->
            matching (Obj.repr (Adef.od_of_codate d : option Adef.date))
              (<:ctyp< option date >>, p)
        | (Def.Cremated d, <:patt< Cremated $p$ >>) ->
            matching (Obj.repr (Adef.od_of_codate d : option Adef.date))
              (<:ctyp< option date >>, p)
        | (_, <:patt< Buried $_$ >> | <:patt< Cremated $_$ >>) -> None
        | _ -> eval_err "matching a burial type with incompatible pattern" ]
    | (<:ctyp< int >>, <:patt< $int:s$ >>) ->
        if (Obj.magic v : int) = int_of_string s then Some [] else None
    | ((<:ctyp< list $t$ >> as tl), p) ->
        match ((Obj.magic v : list _), p) with
        [ ([x :: y], <:patt< [$p1$ :: $p2$] >>) ->
            match matching x (t, p1) with
            [ Some env1 ->
                match matching (Obj.magic y) (tl, p2) with
                [ Some env -> Some (env1 @ env)
                | None -> None ]
            | None -> None ]
        | ([], <:patt< [] >>) -> Some []
        | (_, <:patt< [$_$ :: $_$] >> | <:patt< [] >>) -> None
        | _ -> eval_err "matching a list with incompatible pattern" ]
    | (<:ctyp< option $t$ >>, p) ->
        match ((Obj.magic v : option _), p) with
        [ (Some v, <:patt< Some $p$ >>) -> matching v (t, p)
        | (None, <:patt< None >>) -> Some []
        | (_, <:patt< Some $_$ >> | <:patt< None >>) -> None
        | _ -> eval_err "matching an option with incompatible pattern" ]
    | (<:ctyp< bool >>, p) ->
        match ((Obj.magic v : bool), p) with
        [ (True, <:patt< True >>) -> Some []
        | (False, <:patt< False >>) -> Some []
        | (_, <:patt< True >> | <:patt< False >>) -> Some []
        | _ -> eval_err "matching a bool with incompatible pattern" ]
    | (<:ctyp< string >>, <:patt< $str:s$ >>) ->
        if (Obj.magic v : string) = s then Some [] else None
    | (t, p) -> type_err t "pattern and expression have incompatible types" ]
;

value eval_matching global env d (p, w) =
  match eval_simple_matching d p with
  [ Some nenv ->
      let env = nenv @ env in
      match w with
      [ Some s ->
          let d = eval_expr global env s in
          match d.ctyp with
          [ <:ctyp< bool >> ->
              let v = (Obj.magic d.cval : bool) in
              if v then Some env else None
          | _ -> eval_err "boolean expression expected after \'when\'" ]
      | None -> Some env ]
  | None -> None ]
;

value wrap s f =
  try f () with
  [ Stdpp.Exc_located loc (Stream.Error err) ->
      do Printf.eprintf "*** Error while parsing style sheet\n";
         Printf.eprintf "input: %s\n" s;
         Printf.eprintf "at location: (%d, %d)\n" (fst loc) (snd loc);
         Printf.eprintf "message: %s\n" err;
         flush stderr;
      return raise Exit
  | EvalError err t -> error s err t ]
;

value expr global env s =
  wrap s
    (fun () ->
       let ast = G.Entry.parse expr_eoi (G.parsable (Stream.of_string s)) in
       eval_expr global env ast)
;

value matching global env v s =
  wrap s
    (fun () ->
       let ast = G.Entry.parse patt_eoi (G.parsable (Stream.of_string s)) in
       eval_matching global env v ast)
;

value patt_in_expr global env s =
  wrap s
    (fun () ->
       let (id, ast) =
         G.Entry.parse patt_in_expr_eoi (G.parsable (Stream.of_string s))
       in
       (id, eval_expr global env ast))
;

value patt_eq_expr global env s =
  wrap s
    (fun () ->
       let (id, ast) =
         G.Entry.parse patt_eq_expr_eoi (G.parsable (Stream.of_string s))
       in
       (id, eval_expr global env ast))
;

value simple_expr_list s =
  wrap s
    (fun () ->
       G.Entry.parse simple_expr_list_eoi (G.parsable (Stream.of_string s)))
;
