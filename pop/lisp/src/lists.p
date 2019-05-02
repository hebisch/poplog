/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/lists.p
 > Purpose:         Some list processing procedures
 > Author:          John Williams, Jul 22 1987 (see revisions)
 > Documentation:   CLtL, p207-223
 > Related Files:   C.all/lisp/src/lists.lsp
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, destpair, repeat;


define listp(item);
    ispair(item) or item == []
enddefine;


define nthcdr(n, list) -> list;
    check_positive(n) ->;
    until n == 0 or list == [] do
        cdr(list) -> list;
        n fi_- 1 -> n
    enduntil
enddefine;


define nth() with_nargs 2;
    car(nthcdr())
enddefine;


define updaterof nth() with_nargs 3;
    -> car(nthcdr())
enddefine;


define Nth(list, n);        /* For FIRST, SECOND, etc. */
    car(nthcdr(n, list))
enddefine;


define updaterof Nth(list, n) with_nargs 3;
    -> car(nthcdr(n, list))
enddefine;


define lastcons(list, n) -> result;
    lvars len;
    defaults n 1;
    check_positive(n) ->;
    list -> result;
    (#| while ispair(list) do
            list;
            back(list) -> list
        endwhile
    |#) -> len;
    returnif(len == 0);
    if n == 0 then
        back(dup()) -> result
    elseif n fi_<= len then
        subscr_stack(n) -> result
    endif;
    erasenum(len);
enddefine;


define list_*(N);
    fast_sysrepeat(N fi_- 1, conspair)
enddefine;


/* Joining lists */

define append(l1, l2) -> l2;
    repeat destlist(l1) times
        conspair(l2) -> l2
    endrepeat
enddefine;


define append_n(N);
    if N == 0 then
        []
    else
        fast_sysrepeat(N fi_- 1, append)
    endif
enddefine;


define nconc(l1, l2) -> l1;
    if endp(l1) then
        l2 -> l1
    else
        l2 -> back(lastpair(l1))
    endif
enddefine;


define nconc_n(N);
    if N == 0 then
        []
    else
        fast_sysrepeat(N fi_- 1, nconc)
    endif
enddefine;


/* Copying lists */

define copy_list(list);
    list_*
        (#| while ispair(list) do
                destpair(list) -> list
            endwhile;
            list
        |#)
enddefine;


define copy_alist(alist);
    list_*
        (#| while ispair(alist) then
                destpair(alist) -> alist;
                if ispair(dup()) then
                    conspair(destpair())
                endif
            endwhile;
            alist
        |#)
enddefine;


define copy_tree(tree);
    if ispair(tree) then
        conspair(
            copy_tree(front(tree)),
            copy_tree(back(tree)))
    else
        tree
    endif
enddefine;


define butlast(list, n);
    lvars len;
    defaults n 1;
    check_positive(n) ->;
    destlist(list) -> len;
    if len fi_<= n then
        erasenum(len);
        []
    else
        erasenum(n);
        conslist(len fi_- n)
    endif
enddefine;


define nbutlast(list, n) -> result;
    lvars len;
    defaults n 1;
    check_positive(n) ->;
    list -> result;
    (#| until endp(list) do
            list;
            back(list) -> list
        enduntil
    |#) -> len;
    if len fi_<= n then
        erasenum(len);
        [] -> result
    else
        erasenum(n);
        -> list;
        [] -> back(list);
        erasenum(len fi_- n fi_- 1)
    endif
enddefine;


/* For optimising */

define map_list(fn, list);
    lvars item;
    checkr_function(fn) -> fn;
    [% for item in_cl_list list do
        lisp_apply(item, fn, 1, 1)
    endfor %]
enddefine;


define map_2_lists(fn, l1, l2);
    checkr_function(fn) -> fn;
    [% repeat
        quitif(endp(l1));
        quitif(endp(l2));
        lisp_apply(destpair(l1) -> l1, destpair(l2) -> l2, fn, 2, 1)
    endrepeat %]
enddefine;


define eql_member(item, list);
    until endp(list) do
        returnif(item ==# front(list)) (list);
        back(list) -> list
    enduntil;
    false
enddefine;


define eql_assoc(item, list);
    lvars pair;
    for pair in_cl_list list do
        returnif(ispair(pair) and front(pair) ==# item) (pair)
    endfor;
    false
enddefine;


define eql_rassoc(item, list);
    lvars pair;
    for pair in_cl_list list do
        returnif(ispair(pair) and back(pair) ==# item) (pair)
    endfor;
    false
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Feb 23 1995
        Added map_2_lists (for optimising calls to MAPCAR with 2 list args).
--- John Williams, Aug 27 1993
        Upgraded to Steele 1990. Tidied up. append and nconc now take
        arguments off the stack. Added eql_rassoc.
 */
