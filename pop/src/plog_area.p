/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/plog_area.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */


;;; ------------ PROLOG TRAIL/CONTINUATION STACK AREA -------------------------

#_INCLUDE 'declare.ph'

constant
        procedure (syssave, Sys$-Plog$-Reset_vars, prolog_set_contn_trap,
        Sys$-Move_callstack)
    ;

section $-Sys$-Sr;

weak constant
        procedure (Read, Read_idval, Write, Write_idval, Restore_cs_ptrs)
    ;

endsection;


;;; -------------------------------------------------------------------------

section $-Sys$-Plog => pop_prolog_lim, pop_prolog_size, prolog_reset;

    ;;; limit on size in words of the prolog area
vars
    pop_prolog_lim = 2:1e14;    ;;; 16384

    ;;; variables controlling prolog area
vars

    ;;; the following contain pointers
    $- _plog_area_hi,           ;;; top of prolog area
    $- _plog_contn_barrier,     ;;; base of current plog process' contns
    $- _plog_trail_lim,         ;;; limit for upward trail expansion
    $- _plog_trail_sp,          ;;; trail stack pointer (next free position)
    $- _plog_trail_barrier,     ;;; base of current plog process' trail
    $- _plog_area_lo,           ;;; bottom of prolog area

    ;;; the following contain word offsets relative to _plog_contn_barrier
    $- _plog_contn_sp,          ;;; contn stack pointer
    $- _plog_save_contn_sp,     ;;; saved value of contn_sp
    $- _plog_contn_top,         ;;; logical top of contn stack (where popping from)
    ;


    ;;; vectors of identifiers for save/restore
lconstant
    area_cs_ids =
        {%  ;;; pointers into the callstack area
            ident _plog_area_hi,
            ident _plog_contn_barrier,
            ident _plog_trail_lim,
            ident _plog_trail_sp,
            ident _plog_trail_barrier,
            ident _plog_area_lo
        %},

    area_ids =
        {%  ;;; these are relative to _plog_contn_barrier
            ident _plog_contn_sp,
            ident _plog_save_contn_sp,
            ident _plog_contn_top
        %};


    /*  Expand/contract the prolog area
    */
define Area_expand(_nwords);
    lvars _nwords, _contn_sp, _contn_size, _trail_size, _newfree;

    if _call_stack_seg_hi /== _call_stack_hi or _in_external_control then
        mishap(0, 'CAN\'T EXPAND PROLOG AREA DURING EXTERNAL CALLBACK',
                        'prolog-area:mem-fixedsize')
    endif;

    _plog_contn_barrier@(w){_plog_contn_sp} -> _contn_sp;
    @@(w){_plog_area_hi, _contn_sp} -> _contn_size;
    @@(w){_plog_trail_sp, _plog_area_lo} -> _trail_size;

    if _neg(_nwords) then
        ;;; contracting
        @@(w){_plog_area_hi, _plog_area_lo} _sub _contn_size _sub _trail_size
                    _sub _PLOG_SAFEGAP _add @@(w)[_nwords] -> _newfree;
        if _neg(_newfree) then
            _nwords _sub ##(w){_newfree} -> _nwords
        endif
    endif;

    returnif(_zero(_nwords));

    unless _neg(_nwords) then
        ;;; move the callstack down to make room
        unless Move_callstack(_negate(_nwords), _call_stack_hi, true) then
            mishap(0, 'rom: NO MORE MEMORY AVAILABLE (needing prolog area space)',
                        'prolog-area:mem-nomore')
        endunless
    endunless;

#_IF DEF STACK_GROWS_UP
    ;;; shift the continuation stack up/down
    _move(_contn_size, _contn_sp, _contn_sp@(w)[_nwords]) -> _plog_area_hi;
    _plog_trail_lim@(w)[_nwords] -> _plog_trail_lim;
    _plog_contn_barrier@(w)[_nwords] -> _plog_contn_barrier;
#_ELSE
    ;;; shift the trail down/up
    lvars _newlo = _plog_area_lo@(w)-[_nwords];
    _move(_trail_size, _plog_area_lo, _newlo) -> _plog_trail_sp;
    _plog_trail_barrier@(w)-[_nwords] -> _plog_trail_barrier;
    _newlo -> _plog_area_lo;
#_ENDIF

    if _neg(_nwords) then
        ;;; move the callstack up to fill room made
        Move_callstack(_negate(_nwords), _call_stack_hi, true) ->
    endif
enddefine;

    /* Deal with overflow in the prolog area
    */
define Area_overflow();
    lvars _lim;
    ;;; make sure _plog_trail_lim is correct
    _plog_contn_barrier@(w){_plog_contn_sp _sub _PLOG_SAFEGAP}
                                            ->> _lim -> _plog_trail_lim;
    ;;; then return if trail is actually below the limit
    returnif(_lim >@(w) _plog_trail_sp);
    if _pint(##(w){_plog_area_hi, _plog_area_lo}) fi_> pop_prolog_lim then
        ;;; can't expand any more
        Sysgarbage(true, 'plog');
        if _plog_trail_lim@(w)[_-64] <@(w) _plog_trail_sp then
            ;;; nothing free - expand by a bit to allow error
            Area_expand(_128);      ;;; expand 128 words
            mishap(0, 'rom: MEMORY LIMIT (pop_prolog_lim) EXCEEDED',
                        'prolog-area:mem-limit')
        endif
    else
        ;;; room to expand
        Area_expand(_2048)          ;;; expand 2048 words
    endif
enddefine;

define lconstant Reset_globals();
    _plog_area_lo ->> _plog_trail_barrier -> _plog_trail_sp;
    _0 ->> _plog_contn_top ->> _plog_contn_sp -> _plog_save_contn_sp;
    _plog_area_hi -> _plog_contn_barrier;
    _plog_area_hi@(w)-{_PLOG_SAFEGAP} -> _plog_trail_lim
enddefine;

    /*  Set up the prolog area initially (called from Setup_system)
    */
define Area_setup(_areap, _nwords);
    lvars _areap, _nwords;
    if _in_external_control then
        ;;; args give a fixed size area in ordinary mem
        _areap              -> _plog_area_lo;
        _areap@(w)[_nwords] -> _plog_area_hi;
        _pint(_nwords) -> pop_prolog_lim
    else
        ;;; args irrelevant -- set up necessary things to call Area_expand
        _call_stack_hi ->> _plog_area_hi ->> _plog_area_lo
                            ->> _plog_contn_barrier -> _plog_trail_sp;
        _0 -> _plog_contn_sp;
        Area_expand(_2:1e12)        ;;; allow 4096 words initially
    endif;
    Reset_globals()
enddefine;

    /*  Reset the prolog area, also prologvars and continuations
        if present
    */
define prolog_reset();
    Reset_globals();

    if testdef prologvar_key then
        weakref[prologvar_key] Reset_vars()
    endif;

    if testdef prolog_set_contn_trap then
        weakref prolog_set_contn_trap(true)
    endif
enddefine;

define active pop_prolog_size;
    _pint(##(w){_plog_area_hi, _plog_area_lo})
enddefine;
;;;
define updaterof active pop_prolog_size newval;
    lvars newval;
    Check_integer(newval, 0);
    Area_expand(_int(newval fi_- pop_prolog_size));
    if newval fi_> pop_prolog_lim then newval -> pop_prolog_lim endif
enddefine;


endsection;     /* $-Sys$-Plog */


;;; --- SAVE AND RESTORE ------------------------------------------------

section $-Sys;

lconstant macro SW = [weakref %"["% syssave %"]"%];

    /* Save the prolog area -- called from syssave
    */
define Plog$-Area_save();
    lvars _contn_sp;
    ;;; variables
    appdata(area_cs_ids, SW Sr$-Write_idval);
    appdata(area_ids, SW Sr$-Write_idval);
    ;;; trail
    SW Sr$-Write(_plog_area_lo, @@(w){_plog_trail_sp, _plog_area_lo});
    ;;; continuation stack
    _plog_contn_barrier@(w){_plog_contn_sp} -> _contn_sp;
    SW Sr$-Write(_contn_sp, @@(w){_plog_area_hi, _contn_sp})
enddefine;

    /*  Restore the prolog area -- called from sysrestore
    */
define Plog$-Area_restore();
    lvars _contn_sp;
    ;;; variables -- the callstack pointers have to be relocated
    SW Sr$-Restore_cs_ptrs(area_cs_ids);
    appdata(area_ids, SW Sr$-Read_idval);
    ;;; trail
    SW Sr$-Read(_plog_area_lo, @@(w){_plog_trail_sp, _plog_area_lo});
    ;;; continuation stack
    _plog_contn_barrier@(w){_plog_contn_sp} -> _contn_sp;
    SW Sr$-Read(_contn_sp, @@(w){_plog_area_hi, _contn_sp})
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  1 1996
        Added some mishap id-strings
--- John Gibson, Nov 12 1993
        Changed Area_setup to take args for fixed size area when in
        external control
--- John Gibson, Dec 13 1991
        Added -pop_prolog_size- and rewrote Area_expand to cope with
        negative expansion, i.e. contraction.
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Feb 27 1990
        Added extra arg to call of -Sysgarbage-.
--- John Gibson, Dec  6 1989
        Changes for new pop pointers
--- John Gibson, Nov  9 1989
        Added check in -Area_expand- for not being inside external
        callback.
--- John Gibson, Sep  2 1989
        Changes to -Area_expand- to deal with callstack limits
--- John Gibson, Mar  4 1988
        Created from stuff in plogcore.p, saverest.p
 */
