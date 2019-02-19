/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/data_generic.p
 > Purpose:         Generic datastructure procedures
 > Author:          John Gibson (see revisions)
 */


#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'

section $-Sys;

constant
        procedure (Process_copy, Procedure_copy, Get_store_dalign,
            Key_hash, Prop$-App_data, Prop$-Count,
        )
    ;

endsection;

weak constant
        procedure (copy_fixed, Sys$-Exptrmem_size),
        exptr_mem_key
    ;


;;; --------------------------------------------------------------------

section $-Sys => issimple, iscompound, isinheap, datakey, dataword
                 ==,  /==,  =,  /=,  =->, sys_=,  pop_matchvars_bound,
                 sys_finish_matchvars, syshash, appdata, datalength, copy;

vars
    pop_matchvars_bound = [],
;


;;; --- GENERAL PREDICATES ---------------------------------------------

define issimple() with_nargs 1;
    _issimple()
enddefine;

define iscompound() with_nargs 1;
    _iscompound()
enddefine;

define isinheap(item);
    lvars item;
    unless iscompound(item) then
        mishap(item, 1, 'STRUCTURE NEEDED')
    endunless;
    item >=@(w) _system_end
enddefine;

define datakey() with_nargs 1;
    _datakey()
enddefine;

define dataword() with_nargs 1;
    _datakey()!K_DATAWORD
enddefine;


;;; --- == ---------------------------------------------------------

define 7 == with_nargs 2;
    _eq
enddefine;

define 7 /== with_nargs 2;
    _neq
enddefine;


;;; --- = -----------------------------------------------------------

    /*  Must only be called when pop_matchvars_bound /== []
    */
define sys_finish_matchvars();
    lvars pmvb = pop_matchvars_bound, conv_assign = fast_front(pmvb);
    if conv_assign /== [] then
        if dup() then
            ;;; = succeeded, do assignments
            until conv_assign == [] do
                dest_assoc(conv_assign) -> conv_assign;
                () -> valof()
            enduntil
        endif;
        sys_grbg_list(fast_front(pmvb))
    endif;
    sys_grbg_list(pmvb)
enddefine;

define :inline lconstant EQ_BODY(x, y);
   (dlocal pop_matchvars_bound = [];
    EQ(x, y);
    if pop_matchvars_bound /== [] then sys_finish_matchvars() endif)
enddefine;

define 7 = with_nargs 2;
    EQ_BODY((), ())
enddefine;

define 7 /= with_nargs 2;
    not(EQ_BODY((), ()))
enddefine;

define 10 datum =-> pattern;
    lvars datum, pattern;
    unless EQ_BODY(datum, pattern) then
        mishap(datum, pattern, 2, 'NON-EQUAL ARGUMENTS FOR =->')
    endunless
enddefine;

define sys_=() with_nargs 2;
    dlocal pop_matchvars_bound = [];
    fast_apply(datakey(dup())!K_SYS_EQUALS);
    if pop_matchvars_bound /== [] then sys_finish_matchvars() endif
enddefine;


;;; --- HASHING ----------------------------------------------------------

vars _pop_hash_lim = 3;

define syshash(item);
    lvars item;
    dlocal _pop_hash_lim;
    if _pop_hash_lim == 0 then
        Key_hash(datakey(item))
    else
        _pop_hash_lim fi_- 1 -> _pop_hash_lim;
        fast_apply(item, fast_cont(datakey(item)!K_HASH))
    endif
enddefine;


;;; --- APPLY A PROCEDURE TO EACH ELEMENT OF AN ITEM -------------------------

define appdata(item, procedure pdr);
    lvars item, procedure pdr, accpdr, _offs, _work;
    lconstant errms = 'ILLEGAL ITEM FOR appdata';
    Check_procedure(pdr);
    datakey(item) -> _work;
    _work!K_FLAGS -> _offs;

    if _offs _bitst _:M_K_FULL_VECTOR then
        @@V_WORDS[_0] -> _offs;
        @@V_WORDS[item!V_LENGTH] -> _work;
        while _offs _lt _work do
            ;;; start addr of item must be added in each time
            ;;; 'cos it could change
            _CHECKUSER;
            pdr(item!(w){_offs});
            @@(w){_offs}++ -> _offs
        endwhile

    elseif _offs _bitst _:M_K_STRING then
        if _offs _bitst _:M_K_DSTRING then goto appvec endif;
appstring:
        if _offs _bitst _:M_K_STRING16 then
            @@V_SHORTS[_0] -> _offs;
            @@V_SHORTS[item!V_LENGTH] -> _work;
            while _offs _lt _work do
                _CHECKUSER;
                pdr(_pint( item!(w->s){_offs} ));
                @@(s){_offs}++ -> _offs
            endwhile
        else
            @@V_BYTES[_0] -> _offs;
            @@V_BYTES[item!V_LENGTH] -> _work;
            while _offs _lt _work do
                _CHECKUSER;
                pdr(_pint( item!(w->b){_offs} ));
                @@(b){_offs}++ -> _offs
            endwhile
        endif
    elseif _work == word_key then
        item!W_STRING -> item;
        item!KEY!K_FLAGS -> _offs;
        goto appstring

    elseif _work!K_FLAGS _bitst _:M_K_VECTOR then
appvec:
        _work!K_FAST_SUBSCR_V -> accpdr;
        1 -> _offs;
        _pint(item!V_LENGTH) -> _work;
appvec2:
        until _offs _gr _work do
            _CHECKUSER;
            pdr(fast_apply(_offs, item, accpdr));
            _offs fi_+ 1 -> _offs
        enduntil

    elseif _work!K_FLAGS _bitst _:M_K_RECORD then
        _work!K_ACCESS_R -> accpdr;
        @@V_WORDS[_0] -> _offs;
        @@V_WORDS[accpdr!V_LENGTH] -> _work;
        while _offs _lt _work do
            _CHECKUSER;
            pdr(fast_apply(item, accpdr!(w){_offs} ));
            @@(w){_offs}++ -> _offs
        endwhile

    elseif _work == procedure_key then
        item!PD_FLAGS -> _work;
        if _work _bitst _:M_PD_CLOSURE then
            if _work _bitst _:M_PD_CLOS_PROPERTY then
                weakref[property_key] Prop$-App_data(item, pdr)
            elseunless _work _bitst _:M_PD_CLOS_UNDEF then
                @@PD_CLOS_FROZVALS[_0] -> _offs;
                @@PD_CLOS_FROZVALS[item!PD_CLOS_NFROZ] -> _work;
                while _offs _lt _work do
                    ;;; start addr of item must be added in each time
                    ;;; 'cos it could change
                    _CHECKUSER;
                    pdr(item!(w){_offs});
                    @@(w){_offs}++ -> _offs
                endwhile
            else
                mishap(item, 1, errms)
            endif
        elseif _work _bitst _:M_PD_ARRAY then
            item!PD_ARRAY_MIN_SUBSCR -> _offs;
            item!PD_ARRAY_MAX_SUBSCR -> _work;
            item!PD_ARRAY_SUBSCR_PDR -> accpdr;
            item!PD_ARRAY_VECTOR -> item;
            goto appvec2
        else
            mishap(item, 1, errms)
        endif

    elseif _work == key_key and item!K_FLAGS _bitst _:M_K_RECORD then
        appdata(item!K_ACCESS_R, pdr)

    else
        mishap(item, 1, errms)
    endif;
enddefine;


;;; --- NUMBER OF ELEMENTS IN AN ITEM --------------------------------------

define datalength(item);
    lvars item, key = datakey(item), _flags;
    lconstant errms = 'ILLEGAL ITEM FOR datalength';
    if key!K_FLAGS _bitst _:M_K_VECTOR then
        _pint(item!V_LENGTH)
    elseif key == word_key then
        _pint(item!W_STRING!V_LENGTH)
    elseif key!K_FLAGS _bitst _:M_K_RECORD then
        _pint(key!K_ACCESS_R!V_LENGTH)
    elseif key == procedure_key then
        item!PD_FLAGS -> _flags;
        if _flags _bitst _:M_PD_CLOSURE then
            if _flags _bitst _:M_PD_CLOS_PROPERTY then
                ;;; Prop_count takes the property rec (1st frozval)
                weakref[property_key] Prop$-Count(item!PD_CLOS_FROZVALS[_0])
            elseunless _flags _bitst _:M_PD_CLOS_UNDEF then
                _pint(item!PD_CLOS_NFROZ)
            else
                mishap(item, 1, errms)
            endif
        elseif _flags _bitst _:M_PD_ARRAY then
            item!PD_ARRAY_MAX_SUBSCR fi_- item!PD_ARRAY_MIN_SUBSCR fi_+ 1
        else
            mishap(item, 1, errms)
        endif
    elseif key == weakref exptr_mem_key then
        weakref[exptr_mem_key] Exptrmem_size(item)
    elseif key == key_key and item!K_FLAGS _bitst _:M_K_RECORD then
        _pint(item!K_ACCESS_R!V_LENGTH)
    else
        mishap(item, 1, errms)
    endif
enddefine;


;;; --- COPYING AN ITEM -------------------------------------------------

define copy(item) -> new;
    lvars item, new, _size;
    lconstant errms = 'ILLEGAL ITEM FOR copy';

    unless iscompound(item) then
        mishap(item, 1, errms)
    elseif (item!KEY ->> new) == weakref process_key then
        ;;; process
        return(weakref[process_key] Process_copy(item) -> new)
    elseif new == procedure_key then
        ;;; procedure
        return(Procedure_copy(item) -> new);
    elseunless (new!K_FLAGS ->> _size) _bitst _:M_K_COPY then
        mishap(item, 1, errms)
    elseif _size _bitst _:M_K_ALWAYS_FIXED
    and (_curr_heap_seg == _NULL
            or not(_curr_heap_seg!SEG_FLAGS _bitst _M_SEG_FIXED_STRUCTS))
    then
        ;;; struct must be copied fixed-address, and not currently doing
        ;;; fixed allocation
        chain(item, weakref copy_fixed)
    elseif _size _bitst _:M_K_DOUBLE_ALIGN then
        ;;; doubleword-aligned structure
        fast_apply(item, new!K_GET_SIZE) -> _size;
        Get_store_dalign(_size) -> new;
        _moveq(_size, item@POPBASE, new@POPBASE) -> ;
        return
    endunless;

    fast_apply(item, new!K_GET_SIZE) -> _size;
    Get_store(_size) -> new;
    _moveq(_size, item@POPBASE, new@POPBASE) -> ;
    if new!KEY == word_key then
        ;;; erase the dictionary link field, etc
        false -> new!W_DICT_NEXT;
    endif;
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 11 1997
        String16 changes
--- John Gibson, Jan 15 1996
        Stopped appdata and datalength working on undef closures
--- John Gibson, Jan  6 1996
        Added =-> operator.
--- John Gibson, Jan  4 1996
        Removed procedure E*q (replaced by EQ macro)
--- John Gibson, Dec 22 1995
        Exported sys_finish_matchvars
--- John Gibson, Dec  9 1995
        Added pop_matchvars_bound and code for dealing with it in = etc.
--- Robert John Duncan, Mar 21 1994
        Copying of procedures moved out to Procedure_copy
--- John Gibson, Sep 14 1992
        o Made copy call copy_fixed for anything with M_K_ALWAYS_FIXED set.
        o Made datalength deal with exptr_mem structs
--- Robert John Duncan, Feb 11 1991
        Added cache flush
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Feb  5 1989
        Corrected potential bug in -copy- which was testing for
        _:M_PD_CLOS_PROPERTY without testing for _:M_PD_CLOSURE first.
--- John Gibson, Aug  8 1988
        Added support for doubleword-aligned structures to -copy-
--- John Williams, Jul  6 1988
        -copy- copies properties properly
--- John Gibson, Apr 14 1988
        Renamed data_generic.p
--- John Gibson, Apr  5 1988
        Moved -explode-, -fill- and -datalist- to explode.p
--- John Gibson, Mar 24 1988
        Weakref'ed property stuff
--- John Gibson, Mar 21 1988
        -move/set_bytes- to separate file
--- John Gibson, Feb 26 1988
        Weakref'ed prolog references
--- John Gibson, Feb 22 1988
        Various procedures into section Sys
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
 */
