/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/src/arrays.p
 > Purpose:        Arrays
 > Author:         John Gibson, John Williams (see revisions)
 > Related Files:  C.<processor>/src/array_cons.p, C.all/src/newanyarray.p
 */

;;; ---------------- RUN-TIME ARRAY PROCEDURES ----------------------------

#_INCLUDE 'declare.ph'

global constant
        procedure (isvectorclass, Sys$-Flush_procedure,
            Sys$-Hash_vector_between, Sys$-Array$-Cons),
        Sys$-Gc$-pd_table_noscan_region
    ;

;;; ------------------------------------------------------------------

section $-Sys$-Array => isarray, isarray_by_row, boundslist, arrayvector,
                        arrayvector_bounds, array_subscrp,
                        popc_is_array_upd;

vars popc_is_array_upd = false;

define isarray(item) -> item;
    lvars item;
    if isprocedure(item) then
        while item!PD_FLAGS _bitst _:M_PD_CLOSURE do
            item!PD_CLOS_PDPART -> item
        endwhile;
        if item!PD_FLAGS _bitst _:M_PD_ARRAY then
            return
        endif
    endif;
    false -> item
enddefine;

define lconstant Checkr_array(item) -> a;
    lvars item, a;
    unless (isarray(item) ->> a) do
        mishap(item, 1, 'ARRAY NEEDED')
    endunless
enddefine;

define isarray_by_row() with_nargs 1;
    Checkr_array()!PD_ARRAY_BY_ROW
enddefine;

define boundslist() with_nargs 1;
    Checkr_array()!PD_ARRAY_BOUNDSLIST
enddefine;

define Check_vec_size(vec, max_subscr);
    lvars vec, max_subscr;
    if iscompound(vec) and vec!KEY!K_FLAGS _bitst _:M_K_VECTOR
    and vec!V_LENGTH _lt _int(max_subscr) then
        mishap(vec, max_subscr, 2, 'ARRAYVECTOR TOO SMALL')
    endif
enddefine;

define arrayvector() with_nargs 1;
    Checkr_array()!PD_ARRAY_VECTOR
enddefine;
;;;
define updaterof arrayvector(new, array);
    lvars array, new, old;
    Checkr_array(array) -> array;
    array!PD_ARRAY_VECTOR -> old;
    if datakey(new) == datakey(old) then
        Check_vec_size(new, array!PD_ARRAY_MAX_SUBSCR);
        new ->> array!PD_ARRAY_VECTOR -> array!PD_UPDATER!PD_ARRAY_VECTOR
    else
        mishap(new, old, 2, 'NEW ARRAYVECTOR INCOMPATIBLE WITH OLD')
    endif
enddefine;

define arrayvector_bounds() with_nargs 1;
    lvars array;
    Checkr_array() -> array;
    array!PD_ARRAY_MAX_SUBSCR, array!PD_ARRAY_MIN_SUBSCR
enddefine;

define array_subscrp() with_nargs 1;
    Checkr_array()!PD_ARRAY_SUBSCR_PDR
enddefine;


    ;;; -p- is the outer procedure, -array- the inner array
    ;;; (may be the same)
define Hash(p, array);
    lvars procedure (p, array), vec = array!PD_ARRAY_VECTOR, _lo, _hi;
    array!PD_ARRAY_MIN_SUBSCR -> _lo;
    if isinteger(p!PD_PROPS) then
        p!PD_PROPS  ;;; for Clisp's benefit
    else
        array!PD_ARRAY_MAX_SUBSCR
    endif -> _hi;
    if iscompound(vec) and vec!KEY!K_FLAGS _bitst _:M_K_VECTOR then
        chain(vec, _lo, _hi, Hash_vector_between)
    elseif _lo fi_> _hi then
        chain(vec, syshash)
    else
        (_hi fi_- _lo)
            fi_+ syshash(fast_apply(_hi, vec, array!PD_ARRAY_SUBSCR_PDR))
    endif
enddefine;

    ;;; p is the outer procedure, array the inner array
    ;;; (may be the same)
define Print(p, array);
    lvars procedure (p, array);
    if pop_pr_level == 0 then return(Print_str('<array>')) endif;
    Print_str('<array ');
    while p!PD_FLAGS _bitst _:M_PD_CLOSURE and not(p!PD_PROPS) do
        p!PD_CLOS_PDPART -> p
    endwhile;
    if p!PD_PROPS then spr(recursive_front(p!PD_PROPS)) endif;
    pr(boundslist(array));
    cucharout(`>`)
enddefine;

define Dest(array);
    lvars array, _lo, _hi, procedure pdr;
    ;;; only explode used part of array vector
    array!PD_ARRAY_MIN_SUBSCR -> _lo;
    array!PD_ARRAY_MAX_SUBSCR -> _hi;
    array!PD_ARRAY_SUBSCR_PDR -> pdr;
    array!PD_ARRAY_VECTOR -> array;
    while _lo fi_<= _hi do
        _CHECKUSER;
        pdr(_lo, array);
        _lo fi_+ 1 -> _lo
    endwhile
enddefine;

define Fill(array);
    lvars array, _lo, _hi, procedure pdr;
    ;;; only fill used part of array vector
    array!PD_UPDATER -> array;
    array!PD_ARRAY_MIN_SUBSCR -> _lo;
    array!PD_ARRAY_MAX_SUBSCR -> _hi;
    array!PD_ARRAY_SUBSCR_PDR -> pdr;
    array!PD_ARRAY_VECTOR -> array;
    while _hi fi_>= _lo do
        pdr((), _hi, array);
        _hi fi_- 1 -> _hi
    endwhile
enddefine;

    /*  Allocate raw array procedure, with most of the header filled in
    */
define Get(bounds, offset, by_row) -> _arrayp;
    lvars   bounds, offset, by_row, rev_blst = rev(bounds), sfact_list,
            save1 = rev_blst, save2,
            _ptr, _arrayp, _hi, _lo, _len = 1, _sfact, _tabsize, _ndims;

    if by_row then
        [] -> sfact_list;
        until bounds = [] do
            fast_destpair(fast_destpair(bounds)) -> (_lo, _hi, bounds);
            conspair(_len, sfact_list) -> sfact_list;
            _len fi_* (_hi fi_- _lo fi_+ 1) -> _len
        enduntil
    else
        rev_blst -> bounds;
        [%  until bounds = [] do
                fast_destpair(fast_destpair(bounds)) -> (_hi, _lo, bounds);
                _len;
                _len fi_* (_hi fi_- _lo fi_+ 1) -> _len
            enduntil;
        %] -> sfact_list
    endif;
    sfact_list -> save2;

    _int(listlength(sfact_list)) -> _ndims;       ;;; number of dimensions

    ;;; word offset size of the parameter table (i.e. from PD_ARRAY_TABLE
    ;;; to PD_EXECUTE)
    @@(w)[_ndims _mult _3 _add _4] -> _tabsize;

    ;;; construct the raw procedure that calls _array_sub
    Cons(_tabsize) -> _arrayp;
    procedure_key   -> _arrayp!KEY;
    false           ->> _arrayp!PD_UPDATER -> _arrayp!PD_PROPS;
    _0              ->> _arrayp!PD_FLAGS -> _arrayp!PD_FLAGS2;
    _ndims          -> _arrayp!PD_NARGS;

    ;;; insert parameter table for the _array_sub routine
    lconstant macro _WORD = [_ptr!(w)++ -> _ptr];

    _arrayp@PD_ARRAY_TABLE -> _ptr;
    ;;; first param is offset plus base 1 subscript
    offset fi_+ 1 -> _WORD;

    until rev_blst == [] do
        fast_destpair(fast_destpair(rev_blst)) -> (_hi, _lo, rev_blst);
        ;;; length in this dimension represented so that
        ;;;     _0 _lteq (subscript _sub _lo) _lt _length
        (_hi fi_+ 1) _sub _lo -> _WORD;
        ;;; lower bound as popint
        _lo                   -> _WORD;
        fast_destpair(sfact_list) -> (_sfact, sfact_list);
        _int(_sfact) -> _sfact;         ;;; sysint scale factor
        ;;; plant 0 for scale factor of 1 (easy for _array_sub to test for)
        if _sfact == _1 then _0 else _sfact endif -> _WORD
    enduntil;

    _0       -> _WORD;              ;;; 0 length marks end of parameters
    _tabsize -> _WORD;              ;;; see next comment

    ;;; next value appearing in the word before PD_EXECUTE tells GC that
    ;;; previous word contains a word offset size to ignore at the end
    ;;; of PD_TABLE (i.e. _tabsize)
    $-Sys$-Gc$-pd_table_noscan_region -> _WORD;

    sys_grbg_list(save1);
    sys_grbg_list(save2);
enddefine;

define Copy(old) -> new;
    lvars old, new, _size;
    if old <@(w) _system_end then
        ;;; system procedure -- recreate from scratch
        Get(old!PD_ARRAY_BOUNDSLIST, old!PD_ARRAY_MIN_SUBSCR fi_- 1,
            old!PD_ARRAY_BY_ROW) -> new;
        ;;; fill in rest of header
        old!PD_PROPS            -> new!PD_PROPS;
        old!PD_ARRAY_VECTOR     -> new!PD_ARRAY_VECTOR;
        old!PD_ARRAY_SUBSCR_PDR -> new!PD_ARRAY_SUBSCR_PDR;
        old!PD_ARRAY_BOUNDSLIST -> new!PD_ARRAY_BOUNDSLIST;
        old!PD_ARRAY_MIN_SUBSCR -> new!PD_ARRAY_MIN_SUBSCR;
        old!PD_ARRAY_MAX_SUBSCR -> new!PD_ARRAY_MAX_SUBSCR;
        old!PD_ARRAY_BY_ROW     -> new!PD_ARRAY_BY_ROW;

        ;;; create updater (new not yet marked as array)
        copy(new) -> new!PD_UPDATER;
        old!PD_UPDATER!PD_ARRAY_SUBSCR_PDR -> new!PD_UPDATER!PD_ARRAY_SUBSCR_PDR;
        ;;; mark as array again
        old!PD_FLAGS -> new!PD_FLAGS;
    else
        Get_store(@@(w)[old!PD_LENGTH] ->> _size) -> new;
        _moveq(_size, old@POPBASE, new@POPBASE) -> ;
        ;;; adjust exec fields for copied procedure
        Adjust_pdr_exec(@@(w){new, old}, new);
        ;;; copy the updater (which is NOT marked as an array)
        copy(new!PD_UPDATER) -> new!PD_UPDATER;
    endif;
#_IF DEF CACHEFLUSH
    Flush_procedure(new);
#_ENDIF
    ;;; copy the base vector
    copy(new!PD_ARRAY_VECTOR) ->> new!PD_ARRAY_VECTOR
                                -> new!PD_UPDATER!PD_ARRAY_VECTOR;

    if popc_is_array_upd then
        ;;; defined as a property by Popc (this is unfortunately necessary
        ;;; to enable Popc to recognise array updaters separately from the
        ;;; base procedure)
        new -> popc_is_array_upd(new!PD_UPDATER)
    endif
enddefine;

endsection;     /* $-Sys$-Array */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 26 1995
        Added popc_is_array_upd code
--- John Gibson, May 27 1995
        Initialised new PD_FLAGS2 field to 0 in Get.
--- John Gibson, Aug 30 1994
        Rewrote Get procedure to do all the work of planting array parameters.
        (Removed V*AX dependency -- now same as other systems)
--- John Gibson, Aug 26 1994
        Changed Copy to copy header fields explicitly
--- Robert John Duncan, Mar 21 1994
        Added Copy procedure to rebuild system arrays from scratch:
        uses new procedure Get, with code extracted from newan*yarray
--- John Gibson, Mar 24 1990
        Amended -Hash- to cope with case where arrayvector isn't
        actually a vector.
--- John Gibson, Jun 19 1989
        Moved -newanyarray- to newanyarray.p
--- John Gibson, Feb 15 1989
        Moved Array$-Sub_error to errors.p
--- John Gibson, Feb 15 1989
        Updater of -arrayvector- wasn't assigning new vector into updater
        of array.
--- John Gibson, Feb 13 1989
        Added -array_subscrp- (so POPC can get at the subscriptor procedure)
--- John Gibson, Feb  5 1989
        Array by row now indicated by PD_ARRAY_BY_ROW field in procedure
        rather than a bit in PD_FLAGS
        Moved some array procedure header initialisations into newanyarray
        from Array$-Cons
--- John Williams, Jul 26 1988
        Fixed BR davidy@rsund.uucp.15   (-arrayvector- size check)
        Fixed BR aledm@tbobf.uucp.1     (updater for -arrayvector-)
--- John Gibson, Apr  5 1988
        Moved in array stuff from -explode-
--- John Gibson, Mar  9 1988
        Moved hashing and printing stuff to here from primkeys.p
--- John Gibson, Mar  8 1988
        Array constructor is now Sys$-Array$-Cons in array_cons.p
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
--- John Gibson, Dec 16 1987
        Added declaration at top of file for -isarray-
 */
