(* camlp4r *)
(* $Id: pXML.ml,v 1.1.2.1 1999-04-08 16:54:12 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

type xast =
  [ Xtag of string and list (string * string)
  | Xetag of string
  | Xtext of string
  | Xind of int
  | Xnewl ]
;

value buff = ref (String.create 80);
value store len x =
  do if len >= String.length buff.val then
       buff.val := buff.val ^ String.create (String.length buff.val)
     else ();
     buff.val.[len] := x;
  return succ len
;
value get_buff len = String.sub buff.val 0 len;

value rec ident len =
  parser
  [ [: `('a'..'z' | 'A'..'Z' | '0'..'9' | '_' as c);
       strm :] -> ident (store len c) strm
  | [: :] -> if len = 0 then raise Stream.Failure else get_buff len ]
;

value rec string q len =
  parser
  [ [: `x when x = q :] -> store len q
  | [: `'\\'; `c; strm :] ->
      let c =
        match c with
        [ 'n' -> '\n'
        | _ -> c ]
      in
      string q (store len c) strm
  | [: `x; strm :] -> string q (store len x) strm ]
;

value rec atom len =
  parser
  [ [: `x when x <> '>' && x <> ' '; strm :] -> atom (store len x) strm
  | [: :] -> get_buff len ]
;

value expr =
  parser
  [ [: `('"' | ''' | '`' as q); len = string q (store 0 q) :] -> get_buff len
  | [: e = atom 0 :] -> e ]
;

value rec any len =
  parser
  [ [: `'\\'; `c; strm :] -> any (store len c) strm
  | [: `('"' | ''' | '`' as q); strm :] ->
      any (string q (store len q) strm) strm
  | [: `x when x <> '>'; strm :] -> any (store len x) strm
  | [: :] -> len ]
;

value env strm =
  List.rev (loop [] strm) where rec loop bind =
    parser
    [ [: `' '|'\n' :] -> loop bind strm
    | [: p = ident 0;
         r =
           parser
           [ [: `'='; e = expr :] ->
               loop [(p, e) :: bind] strm
           | [: len = any 0 :] -> [("_", p ^ get_buff len) :: bind] ]
      :] -> r
    | [: `('"' | ''' | '`' as q); len = string q (store 0 q) :] ->
        [("_", get_buff len) :: bind]
    | [: len = any 0 :] ->
        if len = 0 then bind else [("_", get_buff len) :: bind]
    | [: :] -> bind ]
;

value tag =
  parser
  [ [: `'/'; t = ident 0?"ident expected" :] -> Xetag t
  | [: t = ident 0; e = env :] -> Xtag t e
  | [: `'!'; len = any (store 0 '!') :] -> Xtag (get_buff len) [] ]
;

value rec text len =
  parser
  [ [: `x when x <> '<' && x <> '\n'; strm :] -> text (store len x) strm
  | [: :] -> len ]
;

value rec elem ind strm =
  match ind with
  [ Some cnt ->
      match strm with parser
      [ [: `' ' :] -> elem (Some (cnt + 1)) strm
      | [: :] -> (Xind cnt, None) ]
  | None ->
      match strm with parser
      [ [: `'<'; t = tag?"tag expected"; `'>'?"'>' expected" :] -> (t, None)
      | [: `'\n' :] -> (Xnewl, Some 0)
      | [: `x; len = text (store 0 x) :] -> (Xtext (get_buff len), None) ] ]
;

value wrap strm =
  loop [] (Some 0) strm where rec loop r ind =
    parser
    [ [: (a, ind) = elem ind :] -> loop [a :: r] ind strm
    | [: `_ :] -> raise (Stream.Error "xml")
    | [: :] -> List.rev r ]
;

value f strm =
  try wrap strm with e ->
    let pos = Stream.count strm in
    Stdpp.raise_with_loc (pos, pos + 1) e
;
