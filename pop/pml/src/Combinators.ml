(* --- Copyright University of Sussex 1994.  All rights reserved. ---------
 * File:            C.all/pml/src/Combinators.ml
 * Purpose:         PML: Useful combinator definitions
 * Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 * Documentation:   HELP * COMBINATORS
 *)


signature Combinators = sig

    val I           : 'a -> 'a
    val id          : 'a -> 'a

    val K           : 'a -> 'b -> 'a
    val const       : 'a -> 'b -> 'a
    val unit        : 'a -> unit

    val B           : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
    val compose     : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
    val composel    : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

    val C           : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
    val commute     : ('a * 'b -> 'c) -> 'b * 'a -> 'c

    val curry       : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    val uncurry     : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

    val apply       : ('a -> 'b) -> 'a -> 'b
    val applyn      : int -> ('a -> 'a) -> 'a -> 'a
    val applywhile  : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
    val repeat      : ('a -> 'a) -> int -> 'a -> 'a
    val repeatwhile : ('a -> 'a) -> ('a -> bool) -> 'a -> 'a

    val pair        : 'a -> 'b -> 'a * 'b
    val fst         : 'a * 'b -> 'a
    val snd         : 'a * 'b -> 'b
    val before      : 'a * 'b -> 'a
    val swap        : 'a * 'b -> 'b * 'a

end;

infix 0 before;

pop11

section $-ml;

ml_structure Combinators : Combinators = struct

ml_val I : 'a -> 'a = identfn;

ml_val id : 'a -> 'a = ml_valof("I");

ml_val K : 'a -> 'b -> 'a =
procedure(/* k, x */) with_props K with_nargs 2;
    /* x */ ->
endprocedure;

ml_val const : 'a -> 'b -> 'a = ml_valof("K");

ml_val unit : 'a -> unit =
procedure(/* x */) with_props unit with_nargs 1;
    /* x */ -> ;
    ml_unit;
endprocedure;

ml_val B : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b =
procedure(f, g) with_props B with_nargs 2;
    lvars f, g;
    g <> f;
endprocedure;

ml_val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = ml_valof("B");

ml_val composel : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = nonop <>;

ml_val C : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c =
procedure(f, x, y) with_props C;
    lvars f, x, y;
    f(y)(x);
endprocedure;

ml_val commute : ('a * 'b -> 'c) -> 'b * 'a -> 'c =
procedure(f, arg) with_props commute;
    lvars f, arg;
    f(conspair(fast_back(arg), fast_front(arg)));
endprocedure;

ml_val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c =
procedure(f, x, y) with_props curry;
    lvars f, x, y;
    f(conspair(x,y));
endprocedure;

ml_val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c =
procedure(f, arg) with_props uncurry;
    lvars f, arg;
    f(fast_front(arg))(fast_back(arg));
endprocedure;

ml_val apply : ('a -> 'b) -> 'a -> 'b = identfn;

ml_val applyn : int -> ('a -> 'a) -> 'a -> 'a =
procedure(n, f, x) -> x with_props applyn;
    lvars procedure f, n, x;
    while n > 0 do
        f(x) -> x;
        n - 1 -> n;
    endwhile;
endprocedure;

ml_val applywhile : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a =
procedure(p, f, x) -> x with_props applywhile;
    lvars procedure f, procedure p, x;
    while p(x) do f(x) -> x endwhile;
endprocedure;

ml_val repeat : ('a -> 'a) -> int -> 'a -> 'a =
procedure(f, n, x) -> x with_props repeat;
    lvars procedure f, n, x;
    while n > 0 do
        f(x) -> x;
        n - 1 -> n;
    endwhile;
endprocedure;

ml_val repeatwhile : ('a -> 'a) -> ('a -> bool) -> 'a -> 'a =
procedure(f, p, x) -> x with_props repeatwhile;
    lvars procedure f, procedure p, x;
    while p(x) do f(x) -> x endwhile;
endprocedure;

ml_val pair : 'a -> 'b -> 'a * 'b = conspair;

ml_val fst : 'a * 'b -> 'a = fast_front;

ml_val snd : 'a * 'b -> 'b = fast_back;

ml_val swap : 'a * 'b -> 'b * 'a =
procedure(p) with_props swap;
    lvars p;
    conspair(fast_back(p), fast_front(p));
endprocedure;

ml_val before : 'a * 'b -> 'a = fast_front;

optimise("before", procedure(args) -> success;
    lvars arg, args, success = false;
    Destpair(args) -> args -> arg;
    if isTupleExp(arg) then
        gen_exp(dl(first(arg)) -> arg);
        gen_exp(arg);
        mlERASE(0);
        args, true -> success;
    endif;
endprocedure);

ml_endstructure;

endsection; /* $-ml */

ml


(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Jan 21 1991
        Changed optimiser for -before- to the new style.
--- Robert John Duncan, Oct 30 1990
        Added -before-
--- Rob Duncan, Aug 30 1989
        Moved out -impossible- and -error- to "StdValues.ml" and renamed
        from "Useful.ml".
 *)
