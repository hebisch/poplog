/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/writeable.p
 > Purpose:
 > Author:          John Gibson, Jun 18 1989 (see revisions)
 > Documentation:   REF *SYSTEM
 */

;;; ----------- MARKING STRUCTURES WRITEABLE/NONWRITEABLE ----------------
;;;                 (for sys_lock_system and Popc)

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'

weak constant
        procedure Sys$-Set_writeable_prop
    ;

weak vars
        Sys$- _writeable_ignored
    ;

;;; --------------------------------------------------------------------

section $-Sys => pop_record_writeable, writeable, nonwriteable;

vars
    pop_record_writeable = false;

define 1 writeable with_nargs 1;
    lvars item, _key, _keyflags, _flag;
    returnunless(testdef Set_writeable_prop);
    if pop_record_writeable then
        weakref Set_writeable_prop((), true);
        return
    else
        () ->> item;
        returnunless(iscompound(item))
    endif;

    ;;; if a writeable is being ignored for a structure that would
    ;;; be made nonwriteable by sys_lock_system with flag X, set flag X
    ;;; in _writeable_ignored (will cause a sys_lock_system with
    ;;; that flag to mishap)
    item!KEY -> _key;
    _key!K_FLAGS -> _keyflags;
    returnif(_keyflags _bitst _:M_K_WRITEABLE);

    _0 -> _flag;
    unless _keyflags _bitst _:M_K_NONWRITEABLE then
        ;;; would use general default
        ;;; (exclude live dynamic list pairs)
        unless _key == pair_key and fast_front(item) == true
        and isprocedure(fast_back(item)) then
            _:SLS_NONWRITEABLE_DEFAULT -> _flag
        endunless
    elseif _key == procedure_key
    and item!PD_FLAGS _bitst _:M_PD_CLOSURE then
        ;;; would use closure default
        _:SLS_NONWRITEABLE_CLOSURES -> _flag
    else
        ;;; would always be nonwriteable
        _:SLS_NONWRITEABLE_CLASS -> _flag
    endunless;
    weakref[Set_writeable_prop] _writeable_ignored _biset _flag
            -> weakref[Set_writeable_prop] _writeable_ignored
enddefine;

define 1 nonwriteable with_nargs 1;
    if pop_record_writeable and testdef Set_writeable_prop then
        weakref Set_writeable_prop((), false)
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 15 1995
        Replaced boolean writeable_ignored with flags variable
        _writeable_ignored which has the appropriate SLS_ flag set by
        an ignored writeable.
--- John Gibson, Feb  2 1995
        Made writeable set writeable_ignored true when pop_record_writeable
        is false.
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
 */
