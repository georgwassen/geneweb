(* camlp4r q_MLast.cmo *)

open Def;

value loc = (0, 0);

value perso_print conf base p =
  let env =
    [("p",
      {Eval.cval = Obj.repr (p : person); Eval.ctyp = <:ctyp< person >>})]
  in
  do EvalSheet.f conf base env "s_perso";
     Perso.print conf base p;
  return ()
;

value relation_print conf base p =
  fun
  [ Some p1 ->
      match
        try Some (Relation.compute_relationship conf base p1 p) with
        [ Consang.TopologicalSortError -> None ]
      with
      [ Some rel ->
          let env =
           [("result",
             {Eval.cval =
                Obj.repr
                  (rel :
                     option
                       (list (int * int * list (person * int)) * Num.t *
                        float));
              Eval.ctyp =
                <:ctyp<
                  option
                    (list (int * int * list (person * int)) * num *
                     float) >>});
            ("p2",
             {Eval.cval = Obj.repr (p : person);
              Eval.ctyp = <:ctyp< person >>});
            ("p1",
             {Eval.cval = Obj.repr (p1 : person);
              Eval.ctyp = <:ctyp< person >>})]
          in
          do EvalSheet.f conf base env "s_relation";
             Relation.print_main_relationship conf base p1 p rel;
          return ()
      | None -> Relation.print_base_loop conf base ]
  | None -> Relation.print_menu conf base p ]
;
