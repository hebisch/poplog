(* --- Copyright University of Sussex 1994. All rights reserved. ----------
 * File:            C.all/pml/lib/NJCompat.ml
 * Purpose:         Improve compatibility with SML/NJ
 * Author:          Robert John Duncan, Nov 22 1994
 * Documentation:   HELP * NJCOMPAT
 *)

(* Initial definitions in Pop-11 for time-critical functions *)
external structure NJCompat = struct

ml_val ordof : string * int -> int =
    procedure(s, i) with_props ordof;
        lvars s, i;
        if isinteger(i) then
            if i fi_>= 0 and i fi_< datalength(s) then
                return(fast_subscrs(i fi_+ 1, s));
            endif;
        endif;
        ;;; index out of range
        lconstant OrdExn = ml_valof('StdValues.Ord');
        ml_raise(OrdExn);
    endprocedure;

ml_val substring : string * int * int -> string =
    procedure(s, i, n);
        lvars s, i, n;
        if isinteger(i) and i fi_>= 0 then
            if isinteger(n) and n fi_>= 0 then
                if i fi_+ n fi_<= datalength(s) then
                    return(substring(i fi_+ 1, n, s));
                endif;
            endif;
        endif;
        ;;; arguments out of range
        lconstant SubstringExn = ml_valof('String.Substring');
        ml_raise(SubstringExn);
    endprocedure;

ml_val fold : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b =
    procedure(f, l, u) -> u with_props fold;
        lvars procedure f, l, u;
        popstackmark;
        until l == [] do
            fast_destpair(l) -> l;
        enduntil;
        until (() ->> l) == popstackmark do
            f(conspair(l, u)) -> u;
        enduntil;
    endprocedure;

ml_val revfold : (('a * 'b) -> 'b) -> 'a list -> 'b -> 'b =
    procedure(f, l, u) -> u with_props fold;
        lvars procedure f, l, u;
        until l == [] do
            f(conspair(fast_destpair(l) -> l, u)) -> u;
        enduntil;
    endprocedure;

ml_val app : ('a -> 'b) -> 'a list -> unit =
    procedure(f, l) with_props app;
        lvars procedure f, l;
        until l == [] do
            f(fast_destpair(l) -> l) -> ;
        enduntil;
        ml_unit;
    endprocedure;

ml_val revapp : ('a -> 'b) -> 'a list -> unit =
    procedure(f, l) with_props revapp;
        lvars procedure f, l;
        popstackmark;
        until l == [] do
            fast_destpair(l) -> l;
        enduntil;
        until (() ->> l) == popstackmark do
            f(l) -> ;
        enduntil;
        ml_unit;
    endprocedure;

end;

(* Now for the real definition in ML *)
structure NJCompat = struct

structure Integer = struct

    (* overloaded operators specialised on integers *)
    val ~ : int -> int = ~
    val abs : int -> int = abs
    val op div : int * int -> int = op div
    val op mod : int * int -> int = op mod
    val op quot : int * int -> int = Int.quot
    val op rem : int * int -> int = Int.rem
    val op* : int * int -> int = op*
    val op+ : int * int -> int = op+
    val op- : int * int -> int = op-
    val op> : int * int -> bool = op>
    val op>= : int * int -> bool = op>=
    val op< : int * int -> bool = op<
    val op<= : int * int -> bool = op<=
    val makestring : int -> string = makestring

    (* redefinition of print *)
    fun print(i) = output(std_out, makestring(i))

    (* new functions *)
    fun max(x,y) = if x >= y then x else y
    fun min(x,y) = if x <= y then x else y

end (* structure Integer *)

structure Real = struct

    (* include existing stuff *)
    open Real
    val real = real
    val floor = floor
    val sqrt = sqrt
    val sin = sin
    val cos = cos
    val arctan = arctan
    val exp = exp
    val ln = ln

    (* overloaded operators specialised on reals *)
    val ~ : real -> real = ~
    val abs : real -> real = abs
    val op/ : real * real -> real = op/
    val op* : real * real -> real = op*
    val op+ : real * real -> real = op+
    val op- : real * real -> real = op-
    val op> : real * real -> bool = op>
    val op>= : real * real -> bool = op>=
    val op< : real * real -> bool = op<
    val op<= : real * real -> bool = op<=
    val makestring : real -> string = makestring

    (* redefinition of print *)
    fun print(r) = output(std_out, makestring(r))

    (* new functions *)
    val truncate : real -> int = intof

end (* structure Real *)

structure String = struct

    (* include existing stuff *)
    open String
    val size = size
    val implode = implode
    val explode = explode
    val op^ = op^
    exception Chr = Chr
    val chr = chr
    exception Ord = Ord
    val ord = ord

    (* overloaded operators specialised on strings *)
    val op> : string * string -> bool = op>
    val op>= : string * string -> bool = op>=
    val op< : string * string -> bool = op<
    val op<= : string * string -> bool = op<=

    (* redefinition of print *)
    fun print(s) = output(std_out, s)

    (* new or revised functions *)
    val length = size
    val substring = NJCompat.substring
    val ordof = NJCompat.ordof

end (* structure String *)

structure List = struct

    (* include existing stuff *)
    open List
    val op@ = op@
    val map = map
    val rev = rev

    (* new or revised functions *)
    val fold = NJCompat.fold
    val revfold = NJCompat.revfold
    val app = NJCompat.app
    val revapp = NJCompat.revapp
    fun nth(l,n) = List.nth0 n l

    exception NthTail
    fun nthtail(l,0) = l
    |   nthtail(_::l,n) = nthtail(l,n-1)
    |   nthtail([],n) = raise NthTail

end (* structure List *)

structure Ref = struct

    (* include existing stuff *)
    val ! = !
    val op:= = op:=

    (* new functions *)
    fun inc(r as ref(n)) = (r := n+1)
    fun dec(r as ref(n)) = (r := n-1)

end (* structure Ref *)

(*  Add to the environment new names from structures open by default.
    We don't just open the ones defined here, because that would
    clobber overloaded names
 *)

(* From Integer *)
val max = Integer.max
val min = Integer.min

(* From String *)
val substring = String.substring
val ordof = String.ordof

(* From List *)
val fold = List.fold
val revfold = List.revfold
val app = List.app
val revapp = List.revapp
val nth = List.nth
exception NthTail = List.NthTail
val nthtail = List.nthtail

(* From Ref *)
val inc = Ref.inc
val dec = Ref.dec

(* From other structures not defined here *)
open Option;
val use = Compile.use
fun inputc is n = input(is,n)
fun outputc os s = output(os,s)
infix 0 before
fun x before y = x;

end (* structure NJCompat *);

infix 0 before
