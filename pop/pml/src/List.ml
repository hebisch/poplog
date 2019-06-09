(* --- Copyright University of Sussex 1994.  All rights reserved. ---------
 * File:            C.all/pml/src/List.ml
 * Purpose:         PML: Functions on lists
 * Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 * Documentation:   HELP * LIST
 *)


signature List = sig

    exception Listof
    exception Hd
    exception Tl
    exception Last
    exception Nth
    exception Find
    exception Fold
    exception Assoc

    val null        : 'a list -> bool
    val cons        : 'a -> 'a list -> 'a list
    val append      : 'a list -> 'a list -> 'a list
    val zip         : 'a list -> 'b list -> ('a * 'b) list
    val fromto      : int -> int -> int list
    val listof      : int -> 'a -> 'a list
    val length      : 'a list -> int
    val hd          : 'a list -> 'a
    val tl          : 'a list -> 'a list
    val last        : 'a list -> 'a
    val nth         : int -> 'a list -> 'a
    val nth0        : int -> 'a list -> 'a
    val take        : int -> 'a list -> 'a list
    val drop        : int -> 'a list -> 'a list
    val takewhile   : ('a -> bool) -> 'a list -> 'a list
    val dropwhile   : ('a -> bool) -> 'a list -> 'a list
    val filter      : ('a -> bool) -> 'a list -> 'a list
    val any         : ('a -> bool) -> 'a list -> bool
    val exists      : ('a -> bool) -> 'a list -> bool
    val all         : ('a -> bool) -> 'a list -> bool
    val forall      : ('a -> bool) -> 'a list -> bool
    val find        : ('a -> bool) -> 'a list -> 'a
    val delete      : ''a -> ''a list -> ''a list
    val member      : ''a -> ''a list -> bool
    val map2        : ('a * 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val maptl       : ('a list -> 'b) -> 'a list -> 'b list
    val app         : ('a -> unit) -> 'a list -> unit
    val apptl       : ('a list -> unit) -> 'a list -> unit
    val foldl       : ('a * 'b -> 'a) -> 'a -> 'b list -> 'a
    val foldl'      : ('a * 'a -> 'a) -> 'a list -> 'a
    val foldr       : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldr'      : ('a * 'a -> 'a) -> 'a list -> 'a
    val assoc       : ''a -> (''a * 'b) list -> 'b
    val assocdef    : 'b -> ''a -> (''a * 'b) list -> 'b
    val sort        : ('a * 'a -> bool) -> 'a list -> 'a list

end;


pop11

section $-ml;

ml_structure List : List = struct

ml_exception Listof;
ml_exception Hd;
ml_exception Tl;
ml_exception Last;
ml_exception Nth;
ml_exception Find;
ml_exception Fold;
ml_exception Assoc;


ml_val null : 'a list -> bool =
procedure(/* l */) with_props null with_nargs 1;
    /* l */ == [];
endprocedure;

ml_val cons : 'a -> 'a list -> 'a list = conspair;

ml_val append : 'a list -> 'a list -> 'a list =
procedure(l1, l2) with_props append;
    lvars l1, l2;
    popstackmark;
    until l1 == [] do Destpair(l1) -> l1 enduntil;
    sysconslist_onto(l2);
endprocedure;

ml_val zip : 'a list -> 'b list -> ('a * 'b) list =
procedure(l1, l2) with_props zip;
    lvars l1, l2;
    [%
        until l1 == [] or l2 == [] do
            conspair(Destpair(l1) -> l1, Destpair(l2) -> l2);
        enduntil;
    %];
endprocedure;

ml_val fromto : int -> int -> int list =
procedure(n, m) with_props fromto;
    lvars n, m;
    [% until n > m do n, n + 1 -> n enduntil %];
endprocedure;

ml_val listof : int -> 'a -> 'a list =
procedure(n, x) with_props listof;
    lconstant ListofExn = exception("Listof");
    lvars n, x;
    unless isinteger(n) and n fi_>= 0 then raise(ListofExn) endunless;
    [% until n == 0 do x, n fi_- 1 -> n enduntil %];
endprocedure;

ml_val length : 'a list -> int =
procedure(l) -> n with_props length;
    lvars l, n = 0;
    until l == [] do
        n fi_+ 1 -> n;
        Back(l) -> l;
    enduntil;
endprocedure;

ml_val hd : 'a list -> 'a =
procedure(l) with_props hd;
    lconstant HdExn = exception("Hd");
    lvars l;
    if l == [] then raise(HdExn) else Front(l) endif;
endprocedure;

ml_val tl : 'a list -> 'a list =
procedure(l) with_props tl;
    lconstant TlExn = exception("Tl");
    lvars l;
    if l == [] then raise(TlExn) else Back(l) endif;
endprocedure;

ml_val last : 'a list -> 'a =
procedure(l) with_props last;
    lconstant LastExn = exception("Last");
    lvars l;
    if l == [] then raise(LastExn) endif;
    until Back(l) == [] do
        Back(l) -> l;
    enduntil;
    Front(l);
endprocedure;

ml_val nth : int -> 'a list -> 'a =
procedure(n, l) with_props nth;
    lconstant NthExn = exception("Nth");
    lvars n, l;
    unless isinteger(n) and n fi_>= 1 then raise(NthExn) endunless;
    until n == 1 or l == [] do
        n fi_- 1 -> n;
        Back(l) -> l;
    enduntil;
    if l == [] then raise(NthExn) else Front(l) endif;
endprocedure;

ml_val nth0 : int -> 'a list -> 'a =
procedure(n, l) with_props nth0;
    lconstant NthExn = exception("Nth");
    lvars n, l;
    unless isinteger(n) and n fi_>= 0 then raise(NthExn) endunless;
    until n == 0 or l == [] do
        n fi_- 1 -> n;
        Back(l) -> l;
    enduntil;
    if l == [] then raise(NthExn) else Front(l) endif;
endprocedure;

ml_val take : int -> 'a list -> 'a list =
procedure(n, l) with_props take;
    lvars n, l;
    returnunless(isinteger(n))(l);
    [%
        until n fi_<= 0 or l == [] do
            n fi_- 1 -> n;
            Destpair(l) -> l;
        enduntil;
    %];
endprocedure;

ml_val drop : int -> 'a list -> 'a list =
procedure(n, l) with_props drop;
    lvars n, l;
    returnunless(isinteger(n))([]);
    until n fi_<= 0 or l == [] do
        n fi_- 1 -> n;
        Back(l) -> l;
    enduntil;
    l;
endprocedure;

ml_val takewhile : ('a -> bool) -> 'a list -> 'a list =
procedure(p, l) with_props takewhile;
    lvars procedure p, l;
    [%
        while l /== [] and p(Front(l)) do
            Destpair(l) -> l;
        endwhile;
    %];
endprocedure;

ml_val dropwhile : ('a -> bool) -> 'a list -> 'a list =
procedure(p, l) with_props dropwhile;
    lvars procedure p, l;
    while l /== [] and p(Front(l)) do
        Back(l) -> l;
    endwhile;
    l;
endprocedure;

ml_val filter : ('a -> bool) -> 'a list -> 'a list =
procedure(p, l) with_props filter;
    lvars procedure p, l, i;
    [%
        until l == [] do
            if p(Destpair(l) -> l ->> i) then i endif;
        enduntil;
    %];
endprocedure;

ml_val any : ('a -> bool) -> 'a list -> bool =
procedure(p, l) with_props any;
    lvars procedure p, l;
    until l == [] do
        returnif(p(Destpair(l) -> l))(true);
    enduntil;
    false;
endprocedure;

ml_val exists : ('a -> bool) -> 'a list -> bool = ml_valof("any");

ml_val all : ('a -> bool) -> 'a list -> bool =
procedure(p, l) with_props all;
    lvars procedure p, l;
    until l == [] do
        returnunless(p(Destpair(l) -> l))(false);
    enduntil;
    true;
endprocedure;

ml_val forall : ('a -> bool) -> 'a list -> bool = ml_valof("all");

ml_val find : ('a -> bool) -> 'a list -> 'a =
procedure(p, l) with_props find;
    lconstant FindExn = ml_valof("Find");
    lvars i, l, procedure p;
    until l == [] do
        returnif(p(Destpair(l) -> l ->> i))(i);
    enduntil;
    ml_raise(FindExn);
endprocedure;

ml_val delete : ''a -> ''a list -> ''a list =
procedure(x, l) with_props delete;
    lvars y, l, x;
    [%
        until l == [] do
            if (Destpair(l) -> l ->> y) /= x then y endif;
        enduntil;
    %];
endprocedure;

ml_val member : ''a -> ''a list -> bool =
procedure(x, l) with_props member;
    lvars x, l;
    until l == [] do
        returnif((Destpair(l) -> l) = x)(true);
    enduntil;
    false;
endprocedure;

ml_val map2 : ('a * 'b -> 'c) -> 'a list -> 'b list -> 'c list =
procedure(f, l1, l2) with_props map2;
    lvars procedure f, l1, l2;
    [%
        until l1 == [] or l2 == [] do
            f(conspair(Destpair(l1) -> l1, Destpair(l2) -> l2));
        enduntil
    %];
endprocedure;

ml_val maptl : ('a list -> 'b) -> 'a list -> 'b list =
procedure(p, l) with_props maptl;
    lvars procedure p, l;
    [%
        until l == [] do
            p(l);
            Back(l) -> l;
        enduntil;
    %];
endprocedure;

ml_val app : ('a -> unit) -> 'a list -> unit =
procedure(p, l) with_props app;
    lvars procedure p, l;
    until l == [] do
        p(Destpair(l) -> l) -> ;
    enduntil;
    ml_unit;
endprocedure;

ml_val apptl : ('a list -> unit) -> 'a list -> unit =
procedure(p, l) with_props apptl;
    lvars procedure p, l;
    until l == [] do
        p(l) -> ;
        Back(l) -> l;
    enduntil;
    ml_unit;
endprocedure;

ml_val foldl : ('a * 'b -> 'a) -> 'a -> 'b list -> 'a =
procedure(f, u, l) with_props foldl;
    lvars procedure f, l, u;
    u, until l == [] do f(conspair(Destpair(l) -> l)) enduntil;
endprocedure;

ml_val foldr : ('b * 'a -> 'a) -> 'a -> 'b list -> 'a =
procedure(f, u, l) -> u with_props foldr;
    lvars l, u, procedure f;
    popstackmark;
    until l == [] do Destpair(l) -> l enduntil;
    until (->> l) == popstackmark do f(conspair(l, u)) -> u enduntil;
endprocedure;

ml_val foldl' : ('a * 'a -> 'a) -> 'a list -> 'a =
procedure(f, l) with_props foldl1;
    lconstant FoldExn = exception("Fold");
    lvars l, procedure f;
    if l == [] then raise(FoldExn) endif;
    Destpair(l) -> l;
    until l == [] do f(conspair(Destpair(l) -> l)) enduntil;
endprocedure;

ml_val foldr' : ('a * 'a -> 'a) -> 'a list -> 'a =
procedure(f, l) -> u with_props foldr1;
    lconstant FoldExn = exception("Fold");
    lvars l, u, procedure f;
    if l == [] then raise(FoldExn) endif;
    popstackmark;
    until (Destpair(l) ->> l) == [] do enduntil -> u;
    until (->> l) == popstackmark do f(conspair(l, u)) -> u enduntil;
endprocedure;

ml_val assoc : ''a -> (''a * 'b) list -> 'b =
procedure(x, l) with_props assoc;
    lconstant AssocExn = exception("Assoc");
    lvars pair, l, x;
    until l == [] do
        Destpair(l) -> l -> pair;
        returnif(x = Front(pair))(Back(pair));
    enduntil;
    raise(AssocExn);
endprocedure;

ml_val assocdef : 'b -> ''a -> (''a * 'b) list -> 'b =
procedure(def, x, l) with_props assoc;
    lvars pair, l, x, def;
    until l == [] do
        Destpair(l) -> l -> pair;
        returnif(x = Front(pair))(Back(pair));
    enduntil;
    def;
endprocedure;

ml_val sort : ('a * 'a -> bool) -> 'a list -> 'a list =
procedure(le, l) with_props sort;
    lconstant p = writeable conspair(undef,undef);
    dlvars procedure le;
    lvars l;
    syssort(l, procedure with_nargs 2;
        -> Back(p) -> Front(p);
        le(p);
    endprocedure);
    ;;; clear p for GC
    undef ->> Back(p) -> Front(p);
endprocedure;

ml_endstructure;

endsection; /* $-ml */

ml

(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Dec  7 1992
        Fixed types of fold[lr]'
--- Robert John Duncan, Sep 13 1990
        Added -find-, -delete- and -sort-.
        Changed order of arguments to -assoc-, -assocdef- and -member- to
        be more natural.
--- Robert John Duncan, Jul 17 1990
        Simplified exceptions and changed foldr1/foldl1 to foldr'/foldl'
--- Rob Duncan, Sep  4 1989
        Changed "Lists" to "List"
--- Rob Duncan, Sep  1 1989
        Reordered definitions and renamed some exceptions.
 *)
