(* $Id: perso.mli,v 2.2.2.1 1999-04-11 01:19:15 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value find_sosa : config -> base -> person -> person -> option Num.t;
value has_grand_children : base -> person -> bool;
value has_grand_parents : base -> person -> bool;
value print_titles : config -> base -> string -> person -> unit;

value print : config -> base -> person -> unit;
