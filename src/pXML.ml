(* camlp4r *)
(* $Id: pXML.ml,v 1.1.2.5 1999-04-11 19:28:13 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

type xast =
  [ Xtag of string and string
  | Xetag of string
  | Xtext of string
  | Xind of int ]
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

value rec skip_spaces =
  parser
  [ [: `' '|'\t'|'\n'; strm :] -> skip_spaces strm
  | [: :] -> () ]
;

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
        | '\n' -> do skip_spaces strm; return Stream.next strm
        | _ -> c ]
      in
      string q (store len c) strm
  | [: `x; strm :] -> string q (store len x) strm ]
;

value rec any len strm =
  match strm with parser
  [ [: `'\\'; `c :] -> any (store len c) strm
  | [: `('"' | ''' | '`' as q) :] -> any (string q (store len q) strm) strm
  | [: `(' '|'\t'|'\n' as c); _ = skip_spaces :] -> any (store len ' ') strm
  | [: `x when x <> '>' :] -> any (store len x) strm
  | [: :] -> len ]
;

value tag =
  parser
  [ [: `'/'; t = ident 0?"ident expected" :] -> Xetag t
  | [: t = ident 0; _ = skip_spaces; len = any 0 :] -> Xtag t (get_buff len)
  | [: `'!'; len = any (store 0 '!') :] -> Xtag (get_buff len) "" ]
;

value rec text len =
  parser
  [ [: `x when x <> '<' && x <> '\n'; strm :] -> text (store len x) strm
  | [: :] -> len ]
;

value rec indent i =
  parser
  [ [: `' '; strm :] -> indent (i + 1) strm
  | [: :] -> Xind i ]
;

value next_token =
  parser
  [ [: `'\n'; t = indent 0 :] -> t
  | [: `'<'; t = tag?"tag expected"; `'>'?"'>' expected" :] -> t
  | [: `x; len = text (store 0 x) :] -> Xtext (get_buff len) ]
;

value rec wrap tl =
  parser
  [ [: t = next_token; strm :] -> wrap [t :: tl] strm
  | [: :] -> List.rev tl ]
;

value f strm =
  try wrap [indent 0 strm] strm with e ->
    let pos = Stream.count strm in
    Stdpp.raise_with_loc (pos, pos + 1) e
;
