/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/src/newanyarray.p
 > Purpose:        Arrays
 > Author:         John Gibson, John Williams (see revisions)
 > Related Files:  C.all/src/arrays.p
 */

;;; ----------------- CONSTRUCTING NEW ARRAYS ----------------------------

#_INCLUDE 'declare.ph'

constant
        procedure (class_init, class_fast_subscr, isproperty, isarray,
        Sys$-Array$-Get, Sys$-Array$-Check_vec_size, Sys$-Flush_procedure
        )
    ;

vars
        popc_is_array_upd
    ;


;;; ------------------------------------------------------------------

section $-Sys$-Array => poparray_by_row, newanyarray, newarray;

vars
    poparray_by_row = true;


    /* Construct an array - possible arguments are:
        1   <boundslist>
        2   <optional initial value or initialiser procedure>
                cannot be a list - would confuse with 1
        3   <data specifier>
                key, vectorclass, array, or vector init procedure
        4   <optional subscriptor procedure>
                cannot be an array - would confuse with 3
                required if 3 is a vector init pdr
                defaulted from key, vectorclass, or array otherwise
        5   <optional integer offset into vector or array>
        6   <optional boolean value for -poparray_by_row->
    */

define newanyarray(arg) -> arraypdr with_nargs 2;
    lvars arg, arraydata, arraypdr, initialise, offset, subscrpdr,
            bounds, list, revbounds,
            _lo, _hi, _len, _rank,
        ;
    dlvars array_init, array_upd;
    dlocal poparray_by_row;

    ;;; ----------- get arguments
    false ->> arraypdr ->> arraydata -> subscrpdr;
    0 -> offset;
    if isboolean(arg) then              ;;; array ordering flag
        arg -> poparray_by_row;
        -> arg;
    endif;
    if isinteger(arg) then              ;;; index offset
        arg -> offset;
        if offset fi_< 0 then
            mishap(offset, 1, 'CANNOT SPECIFY NEGATIVE INDEX OFFSET');
        endif;
        -> arg;
    endif;
    if isprocedure(arg) and not(isarray(arg)) then      ;;; subscr pdr
        arg -> subscrpdr;
        -> arg;
    endif;
    if isprocedure(arg) and not(isproperty(arg)) then
        if (isarray(arg) ->> arraypdr) then             ;;; arraydata (array)
            arraypdr!PD_ARRAY_VECTOR -> arraydata;
            unless subscrpdr do
                arraypdr!PD_ARRAY_SUBSCR_PDR -> subscrpdr
            endunless;
            false -> arraypdr;
        else
            arg -> arraypdr;            ;;; init pdr to create arraydata
        endif;
    else
        if iskey(arg) then              ;;; key specifying initpdr & subscrpdr
            class_init(arg) -> arraypdr;
        else
            arg -> arraydata;           ;;; arraydata
            datakey(arraydata) -> arg;
        endif;
        unless subscrpdr do
            class_fast_subscr(arg) -> subscrpdr;
        endunless;
    endif;

    unless (subscrpdr!PD_UPDATER ->> array_upd) then
        mishap(subscrpdr, 1, 'ARRAY SUBSCR PROCEDURE HAS NO UPDATER');
    endunless;

    -> bounds;
    if islist(bounds) then
        false -> initialise;
    else
        true -> initialise;
        bounds -> array_init;
        -> bounds;
    endif;
    if (listlength(bounds) ->> _rank) &&=_0 1 then
        _rank fi_>> 1 -> _rank
    else
        mishap(bounds, 1, 'MISSING BOUND IN BOUNDSLIST')
    endif;

    ;;; ---------- compute total array length (ignoring offset)
    1 -> _len;
    bounds -> list;
    until list == [] do
        fast_destpair(fast_destpair(list)) -> list -> _hi -> _lo;
        Check_integer(_lo, false);
        Check_integer(_hi, false);
        if (_hi fi_- _lo) fi_< -1 then
            mishap(_lo, _hi, 2, 'ARRAY LOW BOUND GREATER THAN HIGH BOUND');
        endif;
        _len fi_* (_hi fi_- _lo fi_+ 1) -> _len;
    enduntil;

    ;;; ---------- get array vector
    offset fi_+ 1 -> _lo;                   ;;; PD_ARRAY_MIN_SUBSCR
    _len fi_+ offset -> _hi;                ;;; PD_ARRAY_MAX_SUBSCR
    if arraydata then
        Check_vec_size(arraydata, _hi)
    else
        arraypdr(_hi) -> arraydata
    endif;

    ;;; -- produce basic array procedure
    Get(bounds, offset, poparray_by_row) -> arraypdr;

    ;;; mustn't create any garbage until rest of fields safely filled in ...
    ;;; (no of dimensions is filled in PD_NARGS)
    arraydata   -> arraypdr!PD_ARRAY_VECTOR;
    subscrpdr   -> arraypdr!PD_ARRAY_SUBSCR_PDR;
    bounds      -> arraypdr!PD_ARRAY_BOUNDSLIST;
    _lo         -> arraypdr!PD_ARRAY_MIN_SUBSCR;
    _hi         -> arraypdr!PD_ARRAY_MAX_SUBSCR;
    poparray_by_row -> arraypdr!PD_ARRAY_BY_ROW;
    ;;; now OK

#_IF DEF CACHEFLUSH
    Flush_procedure(arraypdr);
#_ENDIF

    array_upd -> subscrpdr;     ;;; save subscriptor updater
    ;;; procedure not yet marked as an array, so copy won't think it is...
    copy(arraypdr) ->> array_upd -> arraypdr!PD_UPDATER;
    subscrpdr -> array_upd!PD_ARRAY_SUBSCR_PDR; ;;; subscr upd for updater
    ;;; now say it's an array procedure
    _:M_PD_ARRAY -> arraypdr!PD_FLAGS;

    if popc_is_array_upd then
        ;;; defined as a tmp property by Popc (unfortunately necessary
        ;;; to enable Popc to recognise array updaters separately from the
        ;;; base procedure)
        arraypdr -> popc_is_array_upd(array_upd)
    endif;


    ;;; ---------- do any initialisation

    define lconstant Array_scan(bounds, _dim, subscripts);
        lvars bounds, subscripts, _dim, _subscript, _upper;
        if bounds == [] then
            fast_apply(fast_apply(explode(subscripts), array_init),
                explode(subscripts), array_upd)
        else
            fast_destpair(fast_destpair(bounds))
                -> bounds -> _upper -> _subscript;
            until _subscript fi_> _upper do
                _subscript -> fast_subscrv(_dim, subscripts);
                Array_scan(bounds, _dim fi_+ 1, subscripts);
                _subscript fi_+ 1 -> _subscript
            enduntil
        endif
    enddefine;

    if initialise then
        if isprocedure(array_init) then
                Array_scan(bounds, 1, initv(_rank))
            else
            fast_for _len from _lo to _hi do
                fast_apply(array_init, _len, arraydata, subscrpdr)
            endfast_for
        endif
    endif
enddefine;

define newarray() with_nargs 1;
    newanyarray(vector_key)
enddefine;

endsection;     /* $-Sys$-Array */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 26 1995
        Added popc_is_array_upd code
--- Robert John Duncan, Mar 21 1994
        Moved out code for processing the bounds list and creating the
        raw array procedure into a new procedure Get in "arrays.p"
--- Robert John Duncan, Feb 11 1991
        Added cache flush
--- John Gibson, Jun 19 1989
        Split from arrays.p
 */
