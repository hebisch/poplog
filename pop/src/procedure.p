/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/procedure.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *PROCEDURE
*/

;;; --------------------- PROCEDURES -------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'gctypes.ph'

global constant
        procedure (isarray, Sys$-Array$-Copy, Sys$-Array$-Hash,
            Sys$-Array$-Print, Sys$-Prop$-Copy, $-Sys$-Cons_closure)
    ;

;;; ---------------------------------------------------------------------

section $-Sys =>    isprocedure, isclosure, isproperty, ispcomposite,
                    fast_frozval, procedure_key;

define Check_procedure(item);
    lvars item;
    unless iscompound(item) and item!KEY == procedure_key then
        mishap(item, 1, 'PROCEDURE NEEDED')
    endunless
enddefine;

    /*  Called by updaters of procedure fields (pdnargs, pdprops
        and updater)
    */
define Check_user_procedure(item);
    lvars item;
    Check_procedure(item);
    if item <@(w) _system_end and caller(2) >=@(w) _system_end then
        mishap(item, 1, 'USER PROCEDURE NEEDED')
    endif
enddefine;

define Check_closure(item);
    lvars item;
    unless iscompound(item) and item!KEY == procedure_key
        and item!PD_FLAGS _bitst _:M_PD_CLOSURE
    then
        mishap(item, 1, 'CLOSURE NEEDED')
    endunless
enddefine;

define isprocedure(item);
    lvars item;
    if iscompound(item) and item!KEY==procedure_key then true else false endif
enddefine;

define isclosure(item);
    lvars item;
    if isprocedure(item) and item!PD_FLAGS _bitst _:M_PD_CLOSURE then
        if item!PD_FLAGS _bitst _:M_PD_CLOS_PROTECT then
            1
        else
            true
        endif
    else
        false
    endif
enddefine;

define isproperty(item);
    lvars item;
    isclosure(item) and item!PD_FLAGS _bitst _:M_PD_CLOS_PROPERTY
enddefine;

define ispcomposite(item);
    lvars item;
    isprocedure(item) and not(item!PD_FLAGS _bitst _:M_PD_CLOSURE)
    and item!PD_FLAGS _bitst _:M_PD_COMPOSITE
enddefine;

define pdnargs(p);
    lvars p, _nargs;
    Check_procedure(p);
    p!PD_NARGS -> _nargs;
    if p!PD_FLAGS _bitst _:M_PD_CLOSURE and _nargs == _16:FF then
        ;;; compute from the pdpart and nfroz
        if p!PD_CLOS_PDPART == weakref systrace then
            pdnargs(fast_frozval(1, p))
        else
            pdnargs(p!PD_CLOS_PDPART) fi_- _pint(p!PD_CLOS_NFROZ)
        endif
    else
        _pint(_nargs)
    endif
enddefine;
;;;
define updaterof pdnargs(_nargs, p);
    lvars p, _nargs;
    Check_integer_range(_nargs, 0, 16:FF);
    Check_user_procedure(p);
    if p!PD_FLAGS _bitst _:M_PD_CLOSURE
    and p!PD_FLAGS _bitst _:M_PD_CLOS_PROTECT then
        mishap(p, 1, 'CAN\'T ALTER PDNARGS')
    else
        _int(_nargs) -> p!PD_NARGS
    endif
enddefine;

define fast_frozval();
    _subsfroz()
enddefine;
;;;
define updaterof fast_frozval();
    -> _subsfroz()
enddefine;


    /*  Search the dlocals of a procedure p to find if identifier
        _id is a dlocal, and if so return offset of saved value
        in p's stack frame (or zero otherwise).

        If _id is an active var and nonactiv is false, the offset
        returned is for the first saved value of the active var.

        N.B. nonactiv MUST be true if _id is a nonpop ident
        (otherwise the '_id!ID_IDENTPROPS' test will access junk)
    */
define Dlocal_frame_offset(_id, p, nonactiv);
    lvars p, nonactiv, _ptr, _id;
    if nonactiv or not(_id!ID_IDENTPROPS _bitst _:M_ID_ACTIVE) then
        ;;; search for ordinary dlocal identifier
        lblock lvars _lim, _save;
        p@PD_TABLE -> _lim;
        _lim@(w)[p!PD_NLOCALS] ->> _ptr -> _save;
        while _ptr >@(w) _lim do
            if (_ptr--!(w) -> _ptr) == _id then
                ;;; return the offset of the slot in the stack frame
                --@@(w){_save, _ptr} -> _save;     ;;; offset within dlocals
                return(@@SF_LOCALS[p!PD_NUM_STK_VARS] _add _save)
            endif
        endwhile
        endlblock
    elseif p!PD_FLAGS2 _bitst _:M_PD2_HAS_DLOCAL_ACTIVE then
        ;;; has dlocal active var data following the ordinary dlocals
        ;;; (see Stack_dlocal_actid_data in vm_conspdr.p and syscomp/m_trans.p)
        ;;; -- search for dlocal active identifier
        lblock lvars _index_data, _n;
        p@PD_TABLE[p!PD_NLOCALS] -> _ptr;
        repeat
            _int(_ptr!(w)++ -> _ptr) -> _index_data;
            _index_data _bimask _2:111 -> _n;   ;;; lo 3 bits is index count
            _shift(_index_data, _-3) -> _index_data;
            repeat
                ;;; lo byte of _index_data is index into stack frame of
                ;;; first save lvar
                returnif((_ptr!(w)++ -> _ptr) == _id)
                                        (@@(csword)[_index_data _bimask _16:FF]);
                quitif(_zero(_n _sub _1 ->> _n));
                _shift(_index_data, _-8) -> _index_data
            endrepeat;
            quitif(_neg(_index_data))   ;;; last index word is negative
        endrepeat
        endlblock
    endif;
    _0          ;;; zero if not local
enddefine;

    /*  Adjust the execute addresses in a procedure when copying, etc
    */
define Adjust_pdr_exec(_woffs_diff, _pdr);
    lvars _pdr, _woffs_diff;
    ;;; PD_EXECUTE field
    _pdr!PD_EXECUTE@(code){_woffs_diff|w} -> _pdr!PD_EXECUTE;
    unless _pdr!PD_FLAGS _bitst _:M_PD_CLOSURE then
        ;;; PD_EXIT field
        _pdr!PD_EXIT@(code){_woffs_diff|w} -> _pdr!PD_EXIT
    endunless
enddefine;

    /*  Flush procedure code addresses out of the I-cache (if needed)
    */
define Flush_procedure(p);
    lvars p;
#_IF DEF CACHEFLUSH
    CACHEFLUSH(
        p!PD_EXECUTE,
        @@(w)[p!PD_LENGTH] _add p _add @@POPBASE _sub p!PD_EXECUTE);
#_ENDIF
enddefine;

    /*  Copy a procedure
    */
define Procedure_copy(old) -> new;
    lvars old, new, _size;
    if old!PD_FLAGS _bitst _:M_PD_CLOSURE then
        old!PD_CLOS_NFROZ -> _size;
        Cons_closure(_size) -> new;
        procedure_key       -> new!KEY;
        old!PD_PROPS        -> new!PD_PROPS;
        old!PD_UPDATER      -> new!PD_UPDATER;
        old!PD_FLAGS        -> new!PD_FLAGS;
        old!PD_NARGS        -> new!PD_NARGS;
        _size               -> new!PD_CLOS_NFROZ;
        old!PD_CLOS_PDPART  -> new!PD_CLOS_PDPART;
        _moveq(@@(w)[_size], old@PD_CLOS_FROZVALS, new@PD_CLOS_FROZVALS) -> ;
        if new!PD_FLAGS _bitst _:M_PD_CLOS_PROPERTY then
            weakref[property_key] $-Sys$-Prop$-Copy(fast_frozval(1, new))
                                            -> fast_frozval(1, new);
            if new!PD_UPDATER then
                copy(new!PD_UPDATER) -> new!PD_UPDATER;
                fast_frozval(1, new) -> fast_frozval(1, new!PD_UPDATER);
            endif;
        endif;
    elseif old!PD_FLAGS _bitst _:M_PD_ARRAY then
        chain(old, weakref[isarray] Array$-Copy);
    elseif old >=@(w) _system_end then
        Get_store(@@(w)[old!PD_LENGTH] ->> _size) -> new;
        _moveq(_size, old@POPBASE, new@POPBASE) -> ;
        ;;; adjust exec fields for copied procedure
        Adjust_pdr_exec(@@(w){new, old}, new);
    else
        ;;; copying system procedures is forbidden
        mishap(old, 1, 'ILLEGAL ITEM FOR copy');
    endif;
#_IF DEF CACHEFLUSH
    Flush_procedure(new);
#_ENDIF
enddefine;


;;; --- PROCEDURE KEY -----------------------------------------------------

define lconstant Eq__Pdr(item, p);
    lvars item, p, _work, _lim;
    returnif(item == p) (true);
    returnif(issimple(item)) (false);
    if (item!KEY ->> _work) == procedure_key then
        item!PD_FLAGS -> _work;
        if p!PD_FLAGS _bitst _:M_PD_CLOSURE then
            if _work _bitst _:M_PD_CLOSURE
            and item!PD_CLOS_NFROZ == p!PD_CLOS_NFROZ then
                ;;; compare closures
                ;;; check for interrupt/recursion overflow
                _checkall();
                @@PD_CLOS_FROZVALS -> _work;
                @@PD_CLOS_FROZVALS[item!PD_CLOS_NFROZ] -> _lim;
                repeat
                    if _work _greq _lim then
                        ;;; frozvals equal so equal if pdparts equal
                        chain(item!PD_CLOS_PDPART, p!PD_CLOS_PDPART, Eq__Pdr)
                    elseif EQ( item!(w){_work}, p!(w){_work} ) then
                        @@(w){_work}++ -> _work;
                        _CHECKINTERRUPT
                    else
                        return(false)
                    endif
                endrepeat
            endif
        elseif p!PD_FLAGS _bitst _:M_PD_ARRAY then
            if not(_work _bitst _:M_PD_CLOSURE) and _work _bitst _:M_PD_ARRAY
            and p!PD_ARRAY_BY_ROW == item!PD_ARRAY_BY_ROW
            then
                ;;; compare the array vectors
                CHAIN_EQ(p!PD_ARRAY_VECTOR, item!PD_ARRAY_VECTOR)
            endif
        elseif p!PD_FLAGS _bitst _:M_PD_COMPOSITE then
            if not(_work _bitst _:M_PD_CLOSURE)
            and _work _bitst _:M_PD_COMPOSITE then
                ;;; compare composites
                return( EQ(item!PD_COMPOSITE_P1, p!PD_COMPOSITE_P1)
                        and EQ(item!PD_COMPOSITE_P2, p!PD_COMPOSITE_P2) )
            endif
        endif
    elseif _work!K_FLAGS _bitst _:M_K_MATCH_VAR then
        fast_chain(p, item, _work!K_SYS_EQUALS)
    endif;
    false
enddefine;

define lconstant Pdr_print(p);
    lvars p, arg = p, array;
    if testdef isarray and (weakref isarray(p) ->> array) then
        chain(p, array, weakref[isarray] Array$-Print)
    elseif p!PD_FLAGS _bitst _:M_PD_CLOSURE then
        if p!PD_FLAGS _bitst _:M_PD_CLOS_PROPERTY then
            "property" -> arg
        elseif p!PD_FLAGS _bitst _:M_PD_CLOS_UNDEF then
            chain(fast_frozval(1, p), sys_syspr)
        elseunless p!PD_PROPS then
            chain(p!PD_CLOS_PDPART, sys_syspr)
        endif
    endif;
    Default_print(arg, recursive_front(p!PD_PROPS))
enddefine;

define lconstant Pdr_hash(p);
    lvars p, array, _flags;
    if testdef isarray and (weakref isarray(p) ->> array) then
        weakref[isarray] Array$-Hash(p, array)
    elseif (p!PD_FLAGS ->> _flags) _bitst _:M_PD_CLOSURE
    and _flags _bitst _:M_PD_CLOS_PROPERTY then
        fast_apply(p!PD_CLOS_FROZVALS[_0], fast_cont(weakref property_key!K_HASH))
    else
        _pint(_shift(p!PD_LENGTH, p!PD_NARGS _bimask _2:11))
    endif
enddefine;

constant
    procedure_key = struct KEY =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL _biset _:M_K_COPY _biset _:M_K_NONWRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_PROCEDURE,     ;;; K_GC_TYPE
        procedure() with_nargs 1;
            @@(w)[()!PD_LENGTH]
        endprocedure,           ;;; K_GET_SIZE

        "procedure",            ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isprocedure,            ;;; K_RECOGNISER
        WREF apply,             ;;; K_APPLY
        Eq__Pdr,                ;;; K_SYS_EQUALS
        WREF Eq__Pdr,           ;;; K_EQUALS
        Pdr_print,              ;;; K_SYS_PRINT
        WREF Pdr_print,         ;;; K_PRINT
        WREF Pdr_hash,          ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE
        %};

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  8 1997
        Made Eq__Pdr on arrays return false unless PD_ARRAY_BY_ROW is the
        same for both arrays.
--- John Gibson, Jan  4 1996
        Changed to use EQ and CHAIN_EQ macros
--- John Gibson, May 27 1995
        Changed Dlocal_frame_offset to deal with dlocal active vars
--- John Gibson, Apr 10 1995
        Fixed Procedure_copy on closures (was using a _moveq to copy all
        fields from PD_CLOS_NFROZ to PD_CLOS_FROZVALS, which is liable to be
        invalid since PD_CLOS_NFROZ is not a word field)
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Jan 28 1995
        Changed Eq__Pdr to compare composite procedures
--- Robert John Duncan, Mar 21 1994
        Added Procedure_copy
--- John Williams, Dec 16 1993
        Fixed bug in Pdr_hash.
--- Robert John Duncan, Feb 11 1991
        Added -Flush_procedure-
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Nov 23 1989
        Changed -Check_user_procedure- to allow system procedures to
        (attempt to) update system procedures fields.
--- John Williams, Apr  4 1989
        -isclosure- now returns 1 for protected closures
--- John Gibson, Feb 13 1989
        Added -ispcomposite-
--- John Gibson, Apr 12 1988
        procedure_key moved in from primkeys.p and access procedures
        moved out to pdr_util.p.
            Eq__Pdr now checks closures for equlaity by comparing frozvals
        then pdparts.
--- John Gibson, Feb 28 1988
        Weakref'ed -systrace-
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
 */
