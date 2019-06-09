/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/pml/src/util.p
 > Purpose:         PML: Utility procedures
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml =>
    ml_section
;

;;; ML section record

constant ml_section = current_section;

;;; Abbreviations for fast procedures

#_IF pop_debugging

constant macro (
    Front       = "front",
    Back        = "back",
    Destpair    = "destpair",
    Subscrv     = "subscrv",
    Subscrs     = "subscrs",
    Subscrw     = "subscrw",
    Cont        = "cont",
    For         = "for",
    Repeat      = "repeat",
    Lmember     = "lmember",
    Ncrev       = "ncrev",
);

#_ELSE

constant macro (
    Front       = "fast_front",
    Back        = "fast_back",
    Destpair    = "fast_destpair",
    Subscrv     = "fast_subscrv",
    Subscrs     = "fast_subscrs",
    Subscrw     = "fast_subscrw",
    Cont        = "fast_cont",
    For         = "fast_for",
    Repeat      = "fast_repeat",
    Lmember     = "fast_lmember",
    Ncrev       = "fast_ncrev",
);

#_ENDIF

;;; Standard functions on lists (fast, non-dynamic versions):

define llength(l) -> n;
    lvars n = 0, l;
    until l == [] do
        Back(l) -> l;
        n fi_+ 1 -> n;
    enduntil;
enddefine;

define filter(l, p);
    lvars i, procedure p, l;
    [% For i in l do if p(i) then i endif endfor %];
enddefine;

define map(l, p);
    lvars l, procedure p;
    [% until l == [] do p(Destpair(l) -> l) enduntil %];
enddefine;

define nc_map(l, p);
    lvars l, procedure p;
    until l == [] do
        p(Front(l)) -> Front(l);
        Back(l) -> l;
    enduntil;
enddefine;

define app(l, p);
    lvars l, procedure p;
    until l == [] do p(Destpair(l) -> l) enduntil;
enddefine;

define revapp(l, p);
    lvars l, p, x;
    unless l == [] then
        Destpair(l) -> l -> x;
        revapp(l, p);
        p(x);
    endunless;
enddefine;

define all(l, p);
    lvars l, procedure p;
    until l == [] do
        returnunless(p(Destpair(l) -> l))(false);
    enduntil;
    true;
enddefine;

define any(l, p);
    lvars l, procedure p;
    until l == [] do
        returnif(p(Destpair(l) -> l))(true);
    enduntil;
    false;
enddefine;


;;; position:
;;;     returns the position of -i- in -l-

define position(i, l);
    lvars n = 1, l, i;
    until l == [] do
        returnif((Destpair(l) -> l) == i)(n);
        n fi_+ 1 -> n;
    enduntil;
    false;
enddefine;

;;; rotate:
;;;     destructively updates the list -l- so that the i'th item is moved
;;;     to the front. Precondition: 1 <= i <= length(l).

define rotate(i, l);
    lvars i, l;
    l;
    unless i == 1 then
        repeat i fi_- 2 times Back(l) -> l endrepeat;
        Back(l) -> i;
        Back(i) -> Back(l);
        -> Back(i);
        i;
    endunless;
enddefine;

;;; fromto:
;;;     generates a list of integers from -i- to -j-

define fromto(i, j);
    lvars i, j;
    [% For i from i to j do i endfor %];
enddefine;

;;; acons:
;;;     adds a pair [k|v] to the association list l

define acons(k, v, l);
    lvars k, v, l;
    conspair(conspair(k, v), l);
enddefine;

;;; alookup ("assoc"):
;;;     looks up an entry in an association list

define alookup(k, l);
    lvars entry, l, k;
    until l == [] do
        Destpair(l) -> l -> entry;
        returnif(Front(entry) == k)(Back(entry));
    enduntil;
    false;
enddefine;


;;; stamp:
;;;     generates a sequence of integers

lvars
    stamp_cnt = 0,
;

define stamp();
    stamp_cnt + 1 ->> stamp_cnt;
enddefine;


;;; MLWID:
;;;     accesses an identifier in section ML

define syntax MLWID;
    lvars id;
    pop11_need_nextitem("(") -> ;
    readitem() -> id;
    pop11_need_nextitem(")") -> ;
    sysPUSHQ(word_identifier(id, ml_section, true));
enddefine;


;;; mlstring:
;;;     like -consstring-, but avoids creating new strings when the length
;;;     is 0 or 1. This is to minimise the damage caused by not having
;;;     real characters in ML.

lblock;

lvars i;

lconstant chartable = {% for i from 0 to 255 do consstring(i,1) endfor %};

define mlstring(n);
    lvars n;
    if n == 1 then
        Subscrv(() fi_+ 1, chartable);  ;;; assumes char in range
    elseif n == 0 then
        nullstring;
    else
        consstring(n);
    endif;
enddefine;

endlblock;


;;; make_disjoint:
;;;     transforms a list of lists into a list of disjoint sets (i.e. lists
;;;     which have no duplicate elements)
;;;     Elements in lists are assumed to compare with "=="

define make_disjoint(list);
    lvars i, j, k, item, list, set, s, N;

    define lconstant makeset(xs);
        lvars xs, x;
        [%
            until xs == [] do
                Destpair(xs) -> xs -> x;
                unless Lmember(x, xs) then x endunless;
            enduntil;
        %];
    enddefine;

    {% dl(list) %} -> list;
    {% For i to datalength(list) ->> N do i endfor %} -> set;
    For i from 2 to N do
        For item in Subscrv(i, list) do
            For j to i fi_- 1 do
                if Lmember(item, Subscrv(j, list)) then
                    ;;; list(i) and list(j) should be in the same set
                    unless (Subscrv(j, set) ->> s) == Subscrv(i, set) then
                        For k to i fi_- 1 do
                            if Subscrv(k, set) == s then
                                Subscrv(i, set) -> Subscrv(k, set);
                            endif;
                        endfor;
                    endunless;
                endif;
            endfor;
        endfor;
    endfor;
    [%
        For i to N do
            if (Subscrv(i, set) ->> s) == i then
                makeset(Subscrv(i, list));
            else
                Subscrv(i, list) <> Subscrv(s, list) -> Subscrv(s, list);
            endif;
        endfor;
    %];
enddefine;


;;; Boolean typespec for packing boolean flags in records

define lconstant pack_bool() with_nargs 1;
    if () == 0 then false else true endif;
enddefine;

define updaterof lconstant pack_bool() with_nargs 1;
    if () then 1 else 0 endif;
enddefine;

p_typespec bool : 1#pack_bool;


;;; ml_pr, ml_><:
;;;     prints an item in ML format.
;;;     Special printing procedures use the condition "pr == ml_pr" to
;;;     determine ML printing.

define ml_pr(item);
    lvars   item;
    dlocal  pr = ml_pr;
    syspr(item);
enddefine;

define 5 ml_><(item1, item2);
    lvars   item1, item2;
    dlocal  pr = ml_pr;
    item1 >< item2;
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised; ml_section now defined here
--- Robert John Duncan, Oct 24 1994
        Error procedures no longer forward declared here: defined as needed.
--- Robert John Duncan, Nov  1 1991
        Renamed warning and error procedures.
--- Robert John Duncan, Feb 22 1991
        Added ml_pr and ml_><.
        Reorganisation of error procedures.
--- Robert John Duncan, Feb  4 1991
        Definition of "bool" typespec for record definitions.
--- Robert John Duncan, Jan 21 1991
        Added -nc_map- and -rotate-
--- Robert John Duncan, Aug  8 1990
        Moved in forward declaration of error procedures from "ml.p".
        Used -pop_debugging- as debug flag.
 */
