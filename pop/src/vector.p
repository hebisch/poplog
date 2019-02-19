/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/vector.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *VECTORS
 */

;;; ------------------- STANDARD FULL VECTORS ------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

section $-Sys;

constant
        procedure (Vector_apply, Vector_hash, Wordvec_getsize, Eq__Fullvec)
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys =>  isvector, initv, consvector, destvector, sysvecons,
                  subscrv, fast_subscrv, vector_key;

define Check_vector(item);
    lvars item;
    unless iscompound(item) and item!KEY == vector_key then
        mishap(item, 1, 'VECTOR NEEDED')
    endunless
enddefine;

define isvector(item);
    lvars item;
    if iscompound(item) and item!KEY==vector_key then true else false endif;
enddefine;

define Initv(_len, inititem) -> vec;
    lvars vec, inititem, _offs, _len;
    _int(_len) -> _len;
    @@(w)[_len] -> _offs;
    Get_store(@@V_WORDS{_offs} _sub @@POPBASE) -> vec;
    vector_key -> vec!KEY;
    _len -> vec!V_LENGTH;
    _fill(inititem, _offs, vec@V_WORDS)
enddefine;

define initv(_len);
    lvars _len;
    Check_integer(_len, 0);
    Initv(_len, undef)
enddefine;

define consvector(_len) -> _vec;
    lvars _vec, _len, _offs, _lim;
    Check_integer(_len,0);
    _int(_len) -> _len;
    @@(w)[_len] -> _offs;
    Get_store(@@V_WORDS{_offs} _sub @@POPBASE) -> _vec;
    vector_key -> _vec!KEY;
    _len -> _vec!V_LENGTH;
    _vec@V_WORDS -> _lim;
    _lim@(w){_offs} -> _len;
    ;;; _vec must be nonpop here because if we get stack underflow
    ;;; it won't have been completely initialised
    while _len >@(w) _lim do
        -> _len--!(w) -> _len
    endwhile
enddefine;

    ;;; dest a full vector
define destvector(vec);
    lvars vec, _offs, _lim;
    if isvector(vec) then
        @@V_WORDS[_0] -> _offs;
        @@V_WORDS[vec!V_LENGTH] -> _lim;
        while _offs _lt _lim do
            _CHECKUSER;
            vec!(w){_offs};
            @@(w){_offs}++ -> _offs
        endwhile;
        _pint(vec!V_LENGTH)
    else
        Check_vector(vec)
    endif
enddefine;

define subscrv(_vsub, vec);
    lvars vec, _vsub;
    if isvector(vec) then
        Check_vsubscr(_vsub, vec);
        _subsv(_vsub, vec)
    else
        Check_vector(vec)
    endif
enddefine;
;;;
define updaterof subscrv(val, _vsub, vec);
    lvars vec, val, _vsub;
    if isvector(vec) then
        Check_vsubscr(_vsub, vec);
        val -> _subsv(_vsub, vec)
    else
        Check_vector(vec)
    endif
enddefine;

define fast_subscrv() with_nargs 2;
    _subsv()
enddefine;
;;;
define updaterof fast_subscrv() with_nargs 3;
    -> _subsv()
enddefine;


;;; --- VECTOR KEY ---------------------------------------------------

define lconstant Vector_print(vec);
    lvars vec, _n = 1, _len = datalength(vec);
    if pop_pr_level == 0 then return(Minimal_print(vec)) endif;
    cucharout(`{`);
    while _n fi_<= _len do
        pr(fast_subscrv(_n, vec));
        unless _n == _len then cucharout(`\s`) endunless;
        _n fi_+ 1 -> _n
    endwhile;
    cucharout(`}`)
enddefine;

constant
    vector_key = struct KEY_V =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_VECTOR
            _biset _:M_K_FULL_VECTOR
            _biset _:M_K_COPY,  ;;; K_FLAGS
        _:GCTYPE_VECTOR,        ;;; K_GC_TYPE
        Wordvec_getsize,        ;;; K_GET_SIZE

        "vector",               ;;; K_DATAWORD
        "full",                 ;;; K_SPEC
        isvector,               ;;; K_RECOGNISER
        WREF Vector_apply,      ;;; K_APPLY
        Eq__Fullvec,            ;;; K_SYS_EQUALS
        WREF Eq__Fullvec,       ;;; K_EQUALS
        Vector_print,           ;;; K_SYS_PRINT
        WREF Vector_print,      ;;; K_PRINT
        WREF Vector_hash,       ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        _:t_WORD,               ;;; K_FIELD_CODE_V
        initv,                  ;;; K_INIT_V
        consvector,             ;;; K_CONS_V
        destvector,             ;;; K_DEST_V
        subscrv,                ;;; K_SUBSCR_V
        fast_subscrv            ;;; K_FAST_SUBSCR_V
        %};


;;; --- SPECIAL THINGS AT THE END --------------------------------------------
;;; N.B. Execute-level code that references this afterwards
;;; will not get the normal value (i.e. so leave it as the last thing
;;; in the file).

define sysvecons(stacklen);
    lvars stacklen;
    if (stacklength() fi_- stacklen ->> stacklen) fi_< 0 then
        mishap(0, 'STACK UNDERFLOW IN VECTOR CONSTRUCTOR')
    else
        consvector(stacklen)
    endif
enddefine;



endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  2 1989
        Changes for new pop pointers
--- John Gibson, Jan 25 1989
        Moved -sysvecons- to end of file so it compiles with new version
        of popc (see comment above).
--- John Gibson, Aug  5 1988
        Corrected potential problem in -consvector- (qv)
--- John Gibson, Apr 13 1988
        Moved out of vectors.p
 */
