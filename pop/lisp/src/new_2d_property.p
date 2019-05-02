/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/new_2d_property.p
 > Purpose:         2-dimensional property package (used in printcore.p)
 > Author:          John Williams, Nov 29 1995
 > Documentation:
 > Related Files:   C.all/lisp/src/printcore.p
 */

section $-lisp;

define lconstant Pair_==(p1, p2);
    if ispair(p1) and ispair(p2) then
        fast_back(p1) == fast_back(p2) and fast_front(p1) == fast_front(p2)
    else
        p1 == p2
    endif
enddefine;


define lconstant Pair_hash(p);
    fast_destpair(p), nonop fi_&& ()
enddefine;

lconstant Pair_hash_ref = consref(Pair_hash);


define lconstant Get_2d_property(key1, key2, prop);
    lconstant P = writeable conspair(0, 0);
    dlocal 2 % (fast_front(P), fast_back(P)) % = (key1, key2);
    fast_apply(P, prop)
enddefine;


define updaterof lconstant Get_2d_property(value, key1, key2, prop);
    value -> fast_apply(conspair(key1, key2), prop)
enddefine;


define global new_2d_property(size, default);
    Get_2d_property
        (% newanyproperty([], size, false, false,
                          Pair_hash_ref, Pair_==, "perm", default, false)
        %)
enddefine;


define global is_2d_property(p);
    isclosure(p) and pdpart(p) == Get_2d_property
enddefine;


define global clear_2d_property(p);
    if is_2d_property(p) then clearproperty(frozval(1, p)) endif
enddefine;


endsection;
