/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/sr_incr.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSTEM
 */

;;; -------------- INCREMENTAL SAVE AND RESTORE ------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'process.ph'
#_INCLUDE 'gcdefs.ph'
#_INCLUDE 'destroy.ph'
#_INCLUDE 'sr.ph'

section $-Sys;

global vars
        _inhibit_gc, Gc$- _doing_saveincr
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys => syssaveincr, sysrestoreincr;



;;; --- SAVING --------------------------------------------------------

define syssaveincr(file, structure);
    lvars list, structure, file, swlist, _gcseg, _word;
    dlvars _item, _size, _nsegs;
    dlocal _disable, Sr$- _sr_device, Gc$- _doing_saveincr
        ;

    ;;; first scan structure if it's a list and create a list of
    ;;; any system words mentioned
    structure -> list;
    [%  if list <@(w) _system_end and isword(list) then
            ;;; single system word gets treated as if it's in a list
            list;
        else
            while ispair(list) do
                destpair(list) -> list -> _item;
                if _item <@(w) _system_end and isword(_item) then _item endif;
            endwhile
        endif;
    %] -> swlist;

    Sr$-Create(file, '.psi', PSI_MAGIC);    ;;; create incr save file

    ;;; now save everything that can be traced from structure by doing
    ;;; a pseudo garbage collection
    true -> Gc$- _doing_saveincr;   ;;; make gc treat temp props as permanent
    ;;; set up for a garbage collection
    unless Gc$-Setup(_0, true) then
        mishap(0, 'NO MEMORY AVAILABLE TO SAVE INCREMENT')
    endunless;

    ;;; scan the identifiers of the sys words etc
    for swlist -> list step fast_back(list) -> list till list == [] do
        fast_front(list)!W_IDENTIFIER -> _item; ;;; the id
        Gc$-Copyscan(ident _item);      ;;; MUSTN'T scan it in the sys word!!
        if _item <@(w) _system_end then
            ;;; system variable - scan idval
            fast_idval(_item) -> _item;
            Gc$-Copyscan(ident _item)   ;;; MUSTN'T scan it in the sys id!!
        endif
    endfor;

    structure -> _item;                 ;;; the structure
    Gc$-Copyscan(ident _item);          ;;; scan and copy from it
    Gc$-Copyscan_deferred();            ;;; complete any deferred

    ;;; compute total size of stuff copied
    _0 ->> _nsegs -> _size;
    Gc$- _gcseg_table -> _gcseg;
    until _gcseg!GC_SEG_BASE_PTR == _-1 do
        _nsegs _add _1 -> _nsegs;
        @@(w){_gcseg!GC_SEG_COPY_PTR, _gcseg!GC_SEG_COPY_BASE} _add _size
                                                                -> _size;
        _gcseg@(struct GC_SEG)++ -> _gcseg
    enduntil;
    Sr$-Write_idval(ident _nsegs);      ;;; write number of segs
    Sr$-Write_idval(ident _size);           ;;; write size of saved stuff

    ;;; now save stuff copied in all segments
    Gc$- _gcseg_table -> _gcseg;
    until _gcseg!GC_SEG_BASE_PTR == _-1 do
        Sr$-Write_word(_gcseg@GC_SEG_BASE_PTR);     ;;; write seg base address
        Sr$-Write_word(_gcseg@GC_SEG_LIM_PTR);      ;;; write seg lim address
        @@(w){_gcseg!GC_SEG_COPY_PTR, _gcseg!GC_SEG_COPY_BASE} -> _size;
        Sr$-Write_idval(ident _size);                   ;;; write the length copied
        Sr$-Write(_gcseg!GC_SEG_COPY_BASE@POPBASE, _size);  ;;; and the stuff copied
        _gcseg@(struct GC_SEG)++ -> _gcseg
    enduntil;

    ;;; after writing the saved stuff, reset the KEY fields of any dictionary
    ;;; words saved in the chain in _doing_saveincr
    Sr$-Write_idval(ident Gc$- _doing_saveincr);        ;;; write first in chain
    Gc$- _doing_saveincr -> _word;
    until _word == true do
        GC_SET_SEG _word;
        _word@(w){Gc$- _curr_gcseg_reloc} -> _word;
        _word!KEY, word_key -> _word!KEY -> _word
    enduntil;
    false -> Gc$- _doing_saveincr;

    ;;; complete the gc
    Gc$-Do_gc();

    ;;; save the identifiers of the sys words etc
    until swlist == [] do
        fast_destpair(swlist) -> swlist -> _item;       ;;; the word
        Sr$-Write_idval(ident _item);           ;;; write address of word
        _item!W_IDENTIFIER -> _item;
        Sr$-Write_idval(ident _item);           ;;; write ident
        if _item <@(w) _system_end then
            ;;; system variable - save idval and identprops
            Sr$-Write_word(_item@ID_VALOF); ;;; write idval
            Sr$-Write_word(_item@ID_INFO)   ;;; write identprops
        endif
    enduntil;
    _0 -> _item;
    Sr$-Write_idval(ident _item);           ;;; write zero address at end

    sysclose(Sr$- _sr_device)                   ;;; and it's done
enddefine;


;;; --- RESTORING ------------------------------------------------------

struct IR_SEG
  { (word)  IR_SEG_BASE_PTR,
            IR_SEG_LIM_PTR;
    int     IR_SEG_RELOC;
  };


lvars
    _irseg_table,
    _curr_irseg;


    ;;; relocate a pointer >= _system_end
define lconstant Reloc_ptr(_ptr);
    lvars _irseg = _curr_irseg, _ptr;
    unless _irseg!IR_SEG_BASE_PTR <=@(w) _ptr
    and _ptr <@(w) _irseg!IR_SEG_LIM_PTR then
        ;;; not in current segment
        _irseg_table -> _irseg;
        until _ptr <@(w) _irseg!IR_SEG_LIM_PTR do
            _irseg@(struct IR_SEG)++ -> _irseg
        enduntil;
        _irseg -> _curr_irseg
    endunless;

    _ptr@(w){_irseg!IR_SEG_RELOC}
enddefine;

define lconstant Reloc(_addr);
    lvars _addr, _item;
    _addr!(w) -> _item;
    if iscompound(_item) and _item >=@(w) _system_end then
        Reloc_ptr(_item) -> _item;
        if _item!KEY == word_key and not(_item!W_STRING) then
            ;;; replace with existing dictionary word
            _item!W_IDENTIFIER -> _item
        endif;
        _item -> _addr!(w)
    endif
enddefine;

define lconstant App_restored_area(_rec, _lim);
    lvars _rec, _key, _ptr, _work, _lim;
    dlocal Gc$- _lowest_garbageable = _system_end, Gc$- _copy_gc = true;

    ;;; procedure to fix the owner and return address in a process
    ;;; stack frame and return owner procedure's actual address
    ;;; (_retaddr_ptr is always _NULL)
    define lconstant Reloc_owner(_sframe, _retaddr_ptr) -> _owner;
        lvars _sframe, _owner, _owner, _retaddr_ptr;
        _sframe!SF_OWNER -> _owner;
        if _owner >=@(w) _system_end then
            Reloc_ptr(_owner) ->> _owner -> _sframe!SF_OWNER ;;; its correct addr
        endif
    enddefine;

    while _rec <@(w) _lim do
        _rec!KEY -> _key;
        if _key >=@(w) _system_end then Reloc_ptr(_key) -> _key endif;

        GCTYPE_GO_ON '' _pint(_key!K_GC_TYPE);  ;;; see gcdefs.ph

        PROCEDURE:
            Reloc(_rec@PD_PROPS);
            Reloc(_rec@PD_UPDATER);
            Reloc_ptr(_rec!PD_EXECUTE@(code->w) ->> _ptr) -> _work;
            Adjust_pdr_exec(@@(w){_work, _ptr}, _rec);
            if _rec!PD_FLAGS _bitst _:M_PD_CLOSURE then
                Reloc(_rec@PD_CLOS_PDPART);
                _rec@PD_CLOS_FROZVALS -> _ptr
            else
                _rec@PD_TABLE -> _ptr
            endif;
            Gc$-App_range(_ptr, @@(w){_work, _ptr}, Reloc);
            _rec@(w)[_rec!PD_LENGTH] -> _rec, nextloop;

        FULLREC1:
            Reloc(_rec@FIRST);
            _rec@(struct POPREC1)++ -> _rec, nextloop;

        FULLREC2:
            Reloc(_rec@FIRST); Reloc(_rec@SECOND);
        DOUBLE_PAD:
            _rec@(struct POPREC2)++ -> _rec, nextloop;

        FULLREC:
        USERREC:
            Gc$-App_range(_rec@POPBASE, _key!K_RECSIZE_R, Reloc);
            _rec@(w){_key!K_RECSIZE_R} -> _rec, nextloop;

        BYTEVEC:
            _rec@V_WORDS{_BYTEVEC_SIZE(_rec!V_LENGTH)} -> _rec, nextloop;

        VECTOR:
        USERVEC:
            Gc$-App_range(_rec@KEY, @@(w)[_rec!V_LENGTH]++, Reloc);
            _rec@V_WORDS[_rec!V_LENGTH]@~POPBASE -> _rec, nextloop;

        DDECIMAL:
            _rec@(struct DDECIMAL)++ -> _rec, nextloop;

        FULL2ND_REC2:
            Reloc(_rec@SECOND);
            _rec@(struct POPREC2)++ -> _rec, nextloop;

        WORD:
            if issimple(_rec!W_STRING) then
                ;;; restored word put into dictionary
                _mkcompound(_rec!W_STRING) -> _rec!W_STRING
            elseif _rec!W_STRING then
                ;;; non-dictionary word
                Reloc(_rec@W_IDENTIFIER);
                Reloc(_rec@W_STRING);
                Reloc(_rec@W_DICT_NEXT)
            endif;
            _rec@(struct WORD)++ -> _rec, nextloop;

        KEY:
            Gc$-App_key(_rec, Reloc);
            _rec@(w){fast_apply(_rec, _key!K_GET_SIZE)} -> _rec;
            nextloop;

        USERNFREC:
            _key -> _rec!KEY;   ;;; full offset table doesn't include key
        NFULLREC:
            _key@K_FULL_OFFS_TAB[_0] -> _ptr;       ;;; full offs table address
            _ptr@(-i){_key!K_FULL_OFFS_SIZE} -> _work;  ;;; lim address
            while _ptr <@(-i) _work do
                Reloc(_rec@(w){_ptr!(-i)++ -> _ptr})
            endwhile;
            _rec@(w){_key!K_RECSIZE_R} -> _rec, nextloop;

        PROPERTY:
            ;;; set to be rehashed
            unless _rec!PT_REHASH then true -> _rec!PT_REHASH endunless;
            Reloc(_rec@PT_DEFAULT);
            Reloc(_rec@PT_TABLE);
            Reloc(_rec@PT_ACTIVE);
            Reloc(_rec@PT_EQ_PDR);
            Reloc(_rec@PT_HASH_PDR);
            _rec@(struct PROP)++ -> _rec, nextloop;

        PROPENT_PERM:
        PROPENT_TMPARG:
        PROPENT_TMPVAL:
        PROPENT_TMPBOTH:
        PROPENT_TMPCLR:
            Reloc(_rec@PTE_ARG);
            Reloc(_rec@PTE_VALUE);
            Reloc(_rec@PTE_NEXT);
            Reloc(_rec@PTE_LINK);
            _rec@(struct PROP_ENTRY)++ -> _rec, nextloop;

        DESTROY_PROPENT:
            Reloc(_rec@PTE_ARG);
            Reloc(_rec@PTE_VALUE);
            Reloc(_rec@PTE_NEXT);
            Reloc(_rec@PTE_LINK);
            _rec@(struct DESTROY_PROP_ENTRY)++ -> _rec, nextloop;

        USERNFVEC:
            Reloc(_rec@KEY);
            _rec@(w){fast_apply(_rec, _key!K_GET_SIZE)} -> _rec;
            nextloop;

        PROCESS:
            PSWEAK Gc$-App_process(_rec, Reloc, Reloc_owner, false);
            unless _rec!PS_FLAGS _bitst _:M_PS_DEAD
            or (_rec!PS_STATE ->> _ptr) == _NULL then
                PSWEAK Gc$-App_proc_state(_rec,
                                            Reloc_ptr(_ptr@~PSS_DATA),
                                                    _0, Reloc, Reloc_owner);
            endunless;
            _rec@(struct PROCESS)++ -> _rec, nextloop;

        PROCSTATE:      ;;; dealt with from process record
        RAWSTRUCT:
        step_rawsize:
            _rec@(w){_rec!RAW_SIZE} -> _rec, nextloop;

        PLOGPROC:
            PSWEAK Gc$-App_plog_proc_state(_rec, Reloc);
            goto step_rawsize;

        DESCRIPTOR:
#_IF DEF VMS
            @@(w){_rec@DSC_SPEC!DSPEC_PTR, _rec!DSC_ITEM} -> _work;
            Reloc(_rec@DSC_ITEM);
            _rec!DSC_ITEM@(w){_work} -> _rec@DSC_SPEC!DSPEC_PTR;
            _rec@(struct DESCRIPTOR)++ -> _rec, nextloop;
#_ENDIF

        STACK:
        CONST_PLOGVAR:
        DESTROY_MARK:
        OBJMOD_PAD:
        FREE_BLOCK:
        ERROR:
            mishap(_rec, 1, 'App_restored_area: BAD STRUCTURE');
            setpop()
    endwhile
enddefine;

define sysrestoreincr(file);
    lvars list, file, newword, _w, _irseg, _ptr, _item, _tabsize, _base;
    dlvars _word, _size, _nsegs;
    dlocal _disable, Sr$- _sr_device, _inhibit_gc;

    ;;; open incr save file
    if Sr$-Open(file, '.psi', #_< [^PSI_MAGIC] >_#) then
        -> (,);     ;;; erase sysend and magicnum
    else
        return(false)
    endif;
    _DISABLE_ALL -> _disable;

    Sr$-Read_idval(ident _nsegs);       ;;; number of segs
    Sr$-Read_idval(ident _size);        ;;; total size of saved structures

    ;;; get space for ir seg table and saved stuff
    @@(struct IR_SEG)[_nsegs] -> _tabsize;
    ;;; allow extra for each seg for double aligning
    Get_store(@@V_WORDS{_tabsize} _sub @@POPBASE
                _add @@(struct DOUBLE_PAD)[_nsegs]
                _add _size) -> _ptr;

    ;;; set up ir seg table inside a rawstruct
    Make_filler_struct(_ptr, @@V_WORDS{_tabsize} _sub @@POPBASE);
    _ptr@V_WORDS ->> _irseg ->> _irseg_table -> _curr_irseg;
    _irseg@(struct IR_SEG){_tabsize}@~POPBASE ->> _ptr -> _base;

    ;;; read in saved segments and enter details into table
    until _zero(_nsegs) do
        if _ptr@(w.t->d) /== _ptr then
            ;;; make sure start of seg stuff is doubleword aligned
            ;;; (can't use an actual double pad rec for this, as these are
            ;;; assumed to be associated with individual records)
            Make_filler_struct(_ptr, @@(struct DOUBLE_PAD)++);
            _ptr@(struct DOUBLE_PAD)++ -> _ptr
        endif;
        Sr$-Read_word(_irseg@IR_SEG_BASE_PTR);  ;;; old base address
        Sr$-Read_word(_irseg@IR_SEG_LIM_PTR);   ;;; old lim address
        ;;; relocation to be added to old addresses to get new
        @@(w){_ptr, _irseg!IR_SEG_BASE_PTR} -> _irseg!IR_SEG_RELOC;
        Sr$-Read_idval(ident _size);                ;;; size of this seg
        Sr$-Read(_ptr@POPBASE, _size);          ;;; read it in
        _ptr@(w){_size} -> _ptr;
        _irseg@(struct IR_SEG)++ -> _irseg;
        _nsegs _sub _1 -> _nsegs
    enduntil;

    ;;; free any unused space
    _ptr -> Get_store();

    true -> _inhibit_gc;            ;;; make sure gcs don't occur

    ;;; get the chain of words that were in the dictionary, setting them
    ;;; to be replaced with any existing word now in the dictionary
    Sr$-Read_idval(ident _word);
    until _word == true do
        Reloc_ptr(_word) -> _w;
        _w!KEY -> _word;        ;;; next in chain
        word_key -> _w!KEY;
        Reloc(_w@W_STRING);
        Reloc(_w@W_IDENTIFIER);
        false -> _w!W_DICT_NEXT;
        if (Cons_word(_w, false) ->> newword) /== _w then
            false -> _w!W_STRING;   ;;; means replace with W_IDENTIFIER word
            if iscompound(_w!W_IDENTIFIER) then
                _w!W_IDENTIFIER -> newword!W_IDENTIFIER
            endif;
            newword -> _w!W_IDENTIFIER
        else
            ;;; mark not to be processed by App_restored_area
            _mksimple(_w!W_STRING) -> _w!W_STRING
        endif
    enduntil;

    ;;; scan the restored stuff, relocating addresses and linking
    ;;; words into the dictionary
    App_restored_area(_base, _ptr);

    ;;; then restore any system words - zero address terminates
    until (Sr$-Read_idval(ident _word), _zero(_word)) do
        Sr$-Read_word(_word@W_IDENTIFIER);  ;;; read the identifier
        if (_word!W_IDENTIFIER ->> _item) <@(w) _system_end then
            ;;; system ident - restore idval and identprops
            Sr$-Read_word(_item@ID_VALOF);  ;;; read the idval
            Sr$-Read_word(_item@ID_INFO);   ;;; and identprops etc
            Reloc(_item@ID_VALOF)           ;;; correct idval
        else
            Reloc(_word@W_IDENTIFIER)
        endif
    enduntil;

    sysclose(Sr$- _sr_device);
    if isboolean(file) then true endif
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 29 1996
        Included sr.ph
--- John Gibson, Apr 25 1995
        gctype ID*ENT -> FULL2ND_REC2
--- John Gibson, Apr  8 1995
        Change to layout of full-field offset table in not-all-full records
--- Robert John Duncan, Feb 17 1992
        Changed -syssaveincr- to treat a single system word the same as if
        it had been put in a list. This makes the example in the help file
        work again.
--- Robert John Duncan, Feb 14 1992
        Fixed -sysrestoreincr- to erase extra result returned by -Open-
--- John Gibson, Sep 14 1991
        Changes for new format bytevecs
--- John Gibson, Sep 12 1991
        Added RAWSTRUCT label to GCTYPE switches
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Jul 30 1989
        Added "tmpclr" property entry gc type
--- John Gibson, May 24 1989
        Added new property entry gc types
--- John Gibson, May 15 1989
        Included ident.ph
--- Roger Evans, Aug 30 1988
        Added DESTROY_MARK to GCTYPE_GO_ON
--- John Gibson, Aug 22 1988
        Added OBJMOD_PAD to GCTYPE_GO_ON
--- Roger Evans, Aug 19 1988
        Added DESTROY_PROPENT
--- John Gibson, Aug  8 1988
        Added support for doubleword alignment of structures.
--- John Gibson, Aug  6 1988
        Added DOUBLE_PAD type to -App_restored_area-
--- John Gibson, Jul  5 1988
        New treatment of return addresses in stack frames.
--- John Gibson, Mar 10 1988
        Renamed sr_incr.p (previously srincr.p)
--- John Gibson, Feb 25 1988
        Sectionised, etc
 */
