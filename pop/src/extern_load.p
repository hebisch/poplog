/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/extern_load.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *EXTERNAL
 */

;;;----------------- LOADING EXTERNAL PROCEDURES -----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'io.ph'

global constant
        procedure (sysseek, sysdelete, device_full_name, external_unload,
        isexternal_ptr, isexternal_ptr_class)
    ;

global vars
        poppid,
    ;

section $-Sys;

global constant
        procedure (Readerr_mishap, Coalesce_heap_segs, Ensure_open_seg_open),
        tmpval_prop_entry_key, Gc$-dead_prop_entry
    ;

endsection;

section $-Sys$-Extern;

constant
        procedure (Temp_name, Get_link_base, Restore_symtab, Save_symtab,
        Do_link_load, Undo_link_load, Undo_link_loads, Redo_link_loads,
        Reactivate_shrims
        )
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys$-Extern => external_do_load, external_unload,
                         external_load_mark, external_load_count,
                         external_show, external_load_consptr;

lconstant
    ;;; initial extern history
    init_extern_history = [^false],

    ;;; subscripts for load records on the extern_history list
    LOAD_MARK           = 1,    ;;; mark item
    LOAD_FIRST_SEG      = 2,    ;;; _mksimple(address of load first seg)
    LOAD_FILES          = 3,    ;;; object file list
    LOAD_SYMBOLS        = 4,    ;;; list of symbol name/value propents
    LOAD_VEC_LEN        = 4,
    ;

lvars
    extern_history  = init_extern_history,
    link_file_chain = [],
    restored_symtab = false,
    ;


define Readb(dev, _wptr, _nbytes);
    lvars dev, _wptr, _nbytes = _pint(_nbytes), _n;
    unless (fast_apply(dev, 1, _wptr, _nbytes, dev!D_READ) ->> _n) == _nbytes
    then
        Readerr_mishap(dev, _n, 'ERROR READING IMAGE FILE')
    endunless
enddefine;

define Writeb(dev, _wptr, _nbytes);
    lvars dev, _wptr, _nbytes;
    fast_apply(dev, 1, _wptr, _pint(_nbytes), dev!D_WRITE)
enddefine;

define lconstant Check_mark(mark);
    lvars mark, event, locked = false;
    for event in extern_history do
        if event then
            if event(LOAD_MARK) = mark then
                if locked then
                    mishap(mark, 1, 'EXTERNAL LOAD MARK IS LOCKED')
                else
                    return(event)
                endif
            endif
        else
            ;;; locked by a save
            true -> locked
        endif
    endfor;
    false
enddefine;

define external_load_consptr();
#_IF DEF VMS and not(DEF SHARED_LIBRARIES)
    ;;; pointers must be created by Do_link_load unless doing Popc
    returnunless(vm_pas_mode == "popc") (false);
#_ENDIF
    Ensure_writeable(Cons_extern_ptr(_NULL))
enddefine;

define Get_symbol_ptr(symspec) -> ptr;
    lvars symspec, ptr = symspec(3);
    unless isexternal_ptr(ptr) then
        Ensure_writeable(Cons_extern_ptr(_NULL)) ->> ptr -> symspec(3)
    endunless
enddefine;

    /*  Load external object modules/symbols
    */
define external_do_load(mark, objfiles, symbol_list);
    lvars   spec, symbol_list, objfiles, old_link_base, new_link_base,
            mark, value, value_list, first_seg, s, event, exptr, entry,
            lang, type
        ;

        ;;; make printing done by Do_link_load etc safe
    dlocal pr = sys_syspr, pop_pr_quotes = false;

        ;;; get base object file/symbol table for external link
    define lconstant Get_base() -> link_file;
        lvars save_dev, link_file;
        if link_file_chain /== [] then
            ;;; use the last one linked
            hd(link_file_chain) -> link_file
        elseunless restored_symtab then
            ;;; use the POPLOG base symbol table
            Get_link_base() -> link_file
        else
            ;;; restored a saved image with a symbol table appended
            ;;; copy this to a temporary file
            sysopen(front(restored_symtab), 0, true, `N`) -> save_dev;
            ;;; seek to start of symbol table
            sysseek(save_dev, back(restored_symtab), 0);
            Temp_name() <> '0' -> link_file;
            Restore_symtab(link_file, save_dev);
            sysclose(save_dev);
            link_file :: link_file_chain -> link_file_chain
        endif
    enddefine;      /* Get_base */


#_IF not(DEF BERKELEY or DEF SYSTEM_V or DEF VMS or DEF WIN32)
    mishap(0, 'EXTERNAL LOADING NOT IMPLEMENTED IN THIS SYSTEM');
#_ENDIF

    ;;; check object file args
    for s in objfiles do
        unless isstring(s) or isword(s) then
            mishap(s, 1, 'INVALID OBJECT FILE FOR EXTERNAL LOAD')
        endunless
    endfor;

    ;;; check symbol list
    for spec in symbol_list do
        unless isvector(spec) and datalength(spec) == 5
        and (isstring(spec(1)->>s) or isword(s))
        then
            mishap(spec, 1, 'INVALID SYMBOL SPEC FOR EXTERNAL LOAD')
        endunless
    endfor;


    if vm_pas_mode == "popc" then
        ;;; Just return null external pointers with symbol names of the form
        ;;;         [lang-name/]symbol[:type]
        ;;; (where lang-name = ASM means no conversion).
        fast_for spec in symbol_list do
            Get_symbol_ptr(spec) -> exptr;
            false -> type;
            if ispair(spec(2) ->> lang) then
                fast_destpair(lang) -> (lang, type)
            endif;
            if isstring(spec(1) ->> s) then 'ASM' -> lang endif;
            consstring(#| if lang then explode(lang), `/` endif,
                          explode(s),
                          if type then `:`, explode(type) endif
                |#) -> exptr!XP_PROPS       ;;; fill in name
        endfor;
        ;;; Give Popc a list of the objfile args so it can convert and
        ;;; save them
        valof("popc_exload_objfile_args")(objfiles);
        return
    endif;

    ;;; first check mark already exists
    if Check_mark(mark) then
        ;;; unload back to it first
        external_unload(mark)
    endif;

    ;;; get the old link file on which to base this link
    Get_base() -> old_link_base;
    ;;; name of base file for a subsequent link
    Temp_name() sys_>< listlength(link_file_chain) -> new_link_base;

    ;;; Do the load -- link files and read into memory.
    ;;; Do_link_load puts external pointer with symbol value in spec(3)

    ;;; when not SHARED_LIBRARIES we have to shift the open seg up
#_IF not(DEF SHARED_LIBRARIES)
    Ensure_open_seg_open();
#_ENDIF

    _mksimple(_seg_table_next_free) -> first_seg;   ;;; record segment address
    Do_link_load(objfiles, symbol_list, old_link_base, new_link_base);

    ;;; new_link_base is the symbol table input for a subsequent load
    new_link_base :: link_file_chain -> link_file_chain;

    [%  fast_for spec in symbol_list do
            ;;; symbol name and value "tmpval" prop entry for history list
            ;;; (returned in spec(4) so value can be assigned if wanted).
            Get_store(@@(struct PROP_ENTRY)++) ->> entry ->> spec(4);
            tmpval_prop_entry_key -> entry!KEY;
            spec(1) ->> entry!PTE_ARG -> spec(3)!XP_PROPS;
            $-Sys$-Gc$-dead_prop_entry -> entry!PTE_VALUE;
            0 -> entry!PTE_NEXT
        endfor
    %] -> value_list;

    ;;; record the event
    initv(LOAD_VEC_LEN) -> event;
    mark        -> event(LOAD_MARK);
    first_seg   -> event(LOAD_FIRST_SEG);
    objfiles    -> event(LOAD_FILES);
    value_list  -> event(LOAD_SYMBOLS);
    event :: extern_history -> extern_history
enddefine;      /* external_do_load */


    /*  Unload back to a given load mark
    */
define external_unload(mark);
    lvars   event, mark, _seg = _seg_table_next_free, _first, _flags,
            _make_heap
        ;

#_IF not(DEF BERKELEY or DEF SYSTEM_V or DEF VMS or DEF WIN32)
    mishap(0, 'EXTERNAL LOADING NOT IMPLEMENTED IN THIS SYSTEM');
#_ENDIF

    unless Check_mark(mark) ->> event then
        mishap(mark, 1, 'NONEXISTENT EXTERNAL LOAD MARK')
    endunless;

    ;;; Find reusable segs in seg table back to load first seg
    _mkcompound(event(LOAD_FIRST_SEG)) -> _first;
    true -> _make_heap;
    while _seg >@(struct SEG) _first do
        _seg--@(struct SEG) -> _seg;
        _seg!SEG_FLAGS -> _flags;
        unless _flags _bitst _M_SEG_NON_POP then
            ;;; ignore pop segs
            if _seg <@(struct SEG) _lowest_heap_seg then
                ;;; don't make heap segs below a locked one
                false -> _make_heap
            endif;
            if _flags _bitst _M_SEG_FIXED_EXPTRS then
                ;;; These contain fixed-address external pointers that were
                ;;; tied to external relocation (currently only VMS) -- can
                ;;; now turn into an ordinary seg
                _0 -> _seg!SEG_FLAGS
            endif;
            nextloop
        endunless;

        ;;; ignore external dynamic memory
        nextif(_flags _bitst _M_SEG_EXT_DYN_MEM);
#_IF DEF VMS
        ;;; in VMS, can reuse only if can make all pages writeable
        unless Set_mem_prot(_seg!SEG_BASE_PTR,
                           _seg!SEG_BASE_PTR@(w){_seg!SEG_SIZE}, _M_PROT_ALL)
        then
            _flags _biset _M_SEG_NO_SAVE -> _seg!SEG_FLAGS;
            nextloop
        endunless;
#_ENDIF
        ;;; else it's a reusable segment -- turn it into an empty pop one
        _0 -> _seg!SEG_FLAGS;
        _seg!SEG_BASE_PTR@~POPBASE ->> _seg!SEG_BASE_PTR -> _seg!SEG_FREE_PTR;
        if _make_heap and _seg <@(struct SEG) _lowest_heap_seg then
            _seg -> _lowest_heap_seg
        endif
    endwhile;

    ;;; Then try to combine adjacent pop heap segments
    Coalesce_heap_segs();

    ;;; Unwind the history stack
    lvars entry;
    repeat
        dest(extern_history) -> (event, extern_history);

#_IF DEF SHARED_LIBRARIES
        Undo_link_load();
#_ENDIF

        ;;; zap any external pointer records that become undefined
        fast_for entry in event(LOAD_SYMBOLS) do
            if isexternal_ptr_class(entry!PTE_VALUE) then
                _NULL -> entry!PTE_VALUE!XP_PTR
            endif
        endfor;

        ;;; delete the temporary linked entry table
        sysdelete(dest(link_file_chain) -> link_file_chain) -> ;

        ;;; have we reached the desired mark?
        quitif(event(LOAD_MARK) = mark)
    endrepeat
enddefine;      /* external_unload */

    /*  Delete any temporary files created by external_load
    */
define Delete_link_files();
    until link_file_chain == [] do
        sysdelete(destpair(link_file_chain) -> link_file_chain) ->
    enduntil
enddefine;

    /*  Save the current link file symbol table (if any) - called by syssave
        save_dev is the device for the save file
        returns 0 if no link file, or the position of link file copy
        on the save file
    */
define Save(save_dev, new_system);
    lvars save_dev, old_save_dev, buf, new_system, _n, _position;
    if new_system then
        ;;; creating new system, so can't unload after this ...
        if hd(extern_history) then
            false :: extern_history -> extern_history
        endif
    endif;
#_IF DEF SHARED_LIBRARIES
    return(0);
#_ENDIF
    if link_file_chain /== [] then
        ;;; copy the link file symtab onto the end of the save file
        ;;; when restoring, it can be recreated from there.
        sysseek(save_dev, 0, 2, true) -> _position;     ;;; get eof position
        Save_symtab(hd(link_file_chain), save_dev)
    elseif restored_symtab then
        ;;; restored saved image with symtab but symtab not yet copied from
        ;;; there -- copy it to the new save file
        sysseek(save_dev, 0, 2, true) -> _position;     ;;; get eof position
        sysopen(front(restored_symtab), 0, true, `N`) -> old_save_dev;
        ;;; seek to start of symbol table
        sysseek(old_save_dev, back(restored_symtab), 0);
        inits(512) -> buf;
        until (sysread(old_save_dev, buf, 512) ->> _n) == 0 do
            syswrite(save_dev, buf, _n)
        enduntil;
        sysclose(old_save_dev)
    else
        return(0)
    endif;
    ;;; return start position on save file
    _position
enddefine;

    /*  Restore external load - remember the save file name so
        it can be used to recreate a link file if external_load is used
    */
define Restore(_position, save_dev, new_system);
    lvars save_dev, new_system, _position;
    if _position == 0 then
        ;;; no link file symbol table saved
        false
    else
        ;;; remember filename and start byte position
        ;;; (the filename MUST be copied at this point, since the device
        ;;; and its filename are in the save/restore area; the filename
        ;;; will become garbage when a syssave happens)
        conspair(copy(device_full_name(save_dev)), _position)
    endif -> restored_symtab;
    ;;; can't unload after this
    if hd(extern_history) then
        false :: extern_history -> extern_history
    endif;
    [] -> link_file_chain;

#_IF DEF VMS
  #_IF not(DEF SHARED_LIBRARIES)    ;;; already done if SHARED_LIBRARIES
    ;;; reactivate any shareable images
    Reactivate_shrims();
  #_ENDIF

#_ELSEIF DEF SHARED_LIBRARIES
    ;;; Redo external loads from the restored image
    lvars link_file = Temp_name() <> '0';
    if Redo_link_loads(link_file) then
        link_file :: link_file_chain -> link_file_chain;
    endif;
#_ENDIF
enddefine;

    /*  Clean up external loads before restoring
    */
define Pre_restore();
#_IF DEF SHARED_LIBRARIES and not(DEF VMS)
    Undo_link_loads();
#_ENDIF
enddefine;



;;; -- FINDING OUT ABOUT THE EXTERNAL LOAD STATUS ----------------------------

    /*  Get mark item for the Nth load
        (where N=1 is the most recent load)
    */
define external_load_mark(_n);
    lvars _n, event, hist = extern_history;
    Check_integer(_n, 1);
    until null(hist) do
        if (dest(hist) -> hist ->> event) then _n fi_- 1 -> _n endif;
        if _n == 0 then return(event(LOAD_MARK)) endif
    enduntil;
    mishap(_n, 1, 'REFERENCE TO NONEXISTENT LOAD')
enddefine;

    /*  Return number of external loads that have been
    */
define external_load_count() -> _c;
    lvars _c = 0, hist = extern_history;
    until null(hist) do
        if (dest(hist) -> hist) then _c fi_+ 1 -> _c endif
    enduntil;
enddefine;

    /*  Show the state of external loading
    */
define external_show();
    lvars event, locked = false, f, maxlen, entry, value, name;
    dlocal pop_pr_quotes;
    printf('\n--- EXTERNAL LOAD HISTORY ------------------------------\n');
    for event in extern_history do
        unless event then
            ;;; lock point
            unless locked then 1 -> locked endunless;
            nextloop
        endunless;
        if locked == 1 then
            printf('\n>>> LOCKED FROM HERE (not unloadable)\n');
            true -> locked
        endif;
        printf('\nMark: ');
        true -> pop_pr_quotes;
        pr(event(LOAD_MARK));
        false -> pop_pr_quotes;
        cucharout(`\n`);
        printf('\s\s\s\sObject files: ');
        fast_for f in event(LOAD_FILES) do spr(f) endfor;
        8 -> maxlen;
        fast_for entry in event(LOAD_SYMBOLS) do
            max(datalength(entry!PTE_ARG)+2, maxlen) -> maxlen
        endfor;
        printf('\n\s\s\s\sSymbol');
        repeat maxlen-6 times cucharout(`\s`) endrepeat;
        printf('Value\n');
        fast_for entry in event(LOAD_SYMBOLS) do
            entry!PTE_VALUE -> value;
            nextif(value == $-Sys$-Gc$-dead_prop_entry);
            entry!PTE_ARG -> name;
            printf('\s\s\s\s');
            pr(name);
            fast_repeat maxlen-datalength(name) times
                cucharout(`\s`)
            endrepeat;
            pr(value);
            cucharout(`\n`)
        endfor
    endfor
enddefine;

endsection;     /* $-Sys$-Extern */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1996
        Changed behaviour of external_do_load for Popc.
--- John Gibson, May 28 1996
        Added external_load_consptr (plus Get_symbol_ptr to test if a symbol
        spec already contains a pointer and create one if not).
--- John Gibson, Nov 18 1994
        Alpha VMS changes
--- John Gibson, Nov 15 1994
        Moved code from Extern$-Post*_restore to end of Restore
--- Robert John Duncan, Sep  5 1994
        Enabled for Win32
--- John Gibson, Aug 16 1993
        Test for Popc now vm_pas_mode == "popc"
--- Robert John Duncan, Jun  8 1993
        SHARED_LIBRARIES flag now set for SunOS 5+
--- Simon Nichols, Jan 29 1993
        Changed name of conditional compilation flag SU*NOS_DYNAMIC to
        SHARED_LIBRARIES, as it's more descriptive and not SunOS specific.
--- Robert John Duncan, Aug  7 1992
        More changes for SunOS dynamic linking (now assumed for SunOS >= 5)
--- John Gibson, Jul 18 1992
        Added POPC code to external_do_load
--- Simon Nichols, Mar  2 1992
        Changes to support SunOS dynamic linking (temporarily flagged by
        SU*NOS_DYNAMIC).
--- John Gibson, Apr 27 1991
        -Do_link_load- now fills in external pointers in symbol specs
--- John Gibson, Oct 22 1990
        Changes to -Readb- and Writeb- to give new-style args to device
        read/write procedures.
--- John Gibson, Sep 21 1990
        Changed -external_do_load- to return exptrs WITH symbol values
        rather than TO symbol values.
--- Roger Evans, Jul 23 1990
        Initialised PTE_NEXT field in new prop entries (to 0)
--- John Gibson, Jul 23 1990
        Symbol names/values now kept in tmpval prop entries on history list
        so values can be garbaged.
--- John Gibson, Jul  7 1990
        New -external_do_load- replaces old -external_load- (the latter
        moved to extern_procedure.p).
--- John Gibson, Mar 21 1990
        Added "pointer" type to -external_load-.
--- John Gibson, Mar 14 1990
        External procedures now use XP_ fields (as external pointers)
--- John Gibson, Dec  1 1989
        Change to -external_unload- to deal with new pop pointers.
--- John Gibson, Apr 30 1989
        Put into section $-Sys$-Extern
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Williams, Apr 13 1988
        -external_load- localises -pr- and -pop_pr_quotes- for safety
--- John Gibson, Feb 16 1988
        Procedures into Sys$-Extern
--- John Gibson, Feb 11 1988
        external_apply and extern_procedure key moved to extern_apply.p
        This file renamed extern_load.p
--- John Gibson, Feb 10 1988
        Changes for sectioned identifiers, etc.
 */
