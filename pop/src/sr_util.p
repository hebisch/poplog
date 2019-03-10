/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/sr_util.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;---------------- ROUTINES USED BY SAVE AND RESTORE ------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'sr.ph'

constant
        procedure sysseek
    ;

section $-Sys;

constant
        procedure (Clear_freelists, Readerr_mishap,
        Endstring_to_num, Open_seg_expand_for, Expand_open_seg,
        Delete_mem, Sr$-Shrim_too_big
        )
    ;

vars
        _inhibit_gc, _nr_seg_entry
    ;

endsection;

;;; ------------------------------------------------------------------------

section $-Sys$-Sr;

vars
    _sr_seg_entry,  ;;; memory segment used for save/restore
    _sr_device,     ;;; holds the save/restore device

    _initial_mem_end,
    ;

    ;;; For VMS, __pop_shrim_start is a dummy procedure defined in c_core.c
    ;;; (should roughly precede the start of shareable images in the system).
lconstant macro _SHRIM_START = [ _extern __pop_shrim_start@(b.r->vpage) ];


;;; ----------------------------------------------------------------------

    /*  Seek to word offset _woff in the save file
    */
define Seek(_woff);
    lvars _woff;
    sysseek(_sr_device, _pint( ##(b){_woff|w} ), 0)
enddefine;


;;; --- SAVING -------------------------------------------------------------

    /*  Write out word offset length _woff from address _waddr
    */
define Write(_waddr, _woff);
    lvars _waddr, _woff;
    fast_chain(_sr_device, 1, _waddr, _pint(##(b){_woff|w}),
                                                        _sr_device!D_WRITE)
enddefine;

    /*  Write out a single word from address _waddr
    */
define Write_word(/* _waddr */) with_nargs 1;
    chain((), @@(w)[_1], Write)
enddefine;

    /*  Write out the idval of an identifier (used a lot)
    */
define Write_idval(_id);
    lvars _id;
    fast_chain(_sr_device, 1, _id@ID_VALOF, _pint(##(b)[_1|w]),
                                                        _sr_device!D_WRITE)
enddefine;

define lconstant Get_filename(file, ext) -> file;
    lvars file, ext;
    if isword(file) then file!W_STRING <> ext -> file endif;
    $-Sys$-Check_string(file)
enddefine;

define Create(file, ext, _magicnum);
    lvars file, ext, _magicnum, _len;
    dlocal _inhibit_gc;
    lstackmem struct IMAGE_INIT_HEADER _h;

    ;;; make sure the current directory doesn't get created in
    ;;; the save/restore segment
    current_directory -> ;
    sysfileok(Get_filename(file, ext)) -> file;

    ;;; switch to the save/restore seg to allocate the save device
    _curr_heap_seg -> _saved_curr_heap_seg;
    _sr_seg_entry!SEG_BASE_PTR -> _sr_seg_entry!SEG_FREE_PTR;
    Set_curr_heap_seg(_sr_seg_entry);
    true -> _inhibit_gc;            ;;; mishap will reset this on an error
    syscreate(file, 1, true) -> _sr_device;
    Set_curr_heap_seg(_saved_curr_heap_seg);
    ;;; clear free lists, in case they contain structures
    ;;; in the save/restore seg
    Clear_freelists(false);         ;;; (not free blocks)

    _magicnum   -> _h!IMG_MAGIC;        ;;; magic number
    _system_end -> _h!IMG_SYSTEM_END;   ;;; save value of _system_end
    _initial_mem_end
                -> _h!IMG_INITIAL_MEM_END;  ;;; save value of _initial_mem_end
    _0          -> _h!IMG_IMAGE_ARGS_OFFS;  ;;; filled in later

    ;;; system version id string
    pop_system_version!V_LENGTH -> _len;    ;;; length of it
    if _len _gr _:VERSION_LEN then _:VERSION_LEN -> _len endif;
    _len -> _h!IMG_VERSION_STRING_LEN;
    _bmove(@@(b)[_len], pop_system_version@V_BYTES, _h@IMG_VERSION_STRING) -> ;

    ;;; write initial header
    Write(_h, @@(struct IMAGE_INIT_HEADER)++)
enddefine;


;;; --- RESTORING -----------------------------------------------------------

define lconstant Do_read(_waddr, _woff, _nbytes);
    lvars _waddr, _woff, _nbytes, _n;
    _pint(_nbytes) -> _nbytes;
    unless (fast_apply(_sr_device, _pint(##(b){_woff|w} _add _1), _waddr,
                        _nbytes, _sr_device!D_READ) ->> _n) == _nbytes
    then
        Readerr_mishap(_sr_device, _n, 'ERROR READING SAVED IMAGE')
    endunless
enddefine;

    /*  Read word offset size _woff into address _waddr
    */
define Read(_waddr, _woff);
    lvars _waddr, _woff;
    chain(_waddr, _0, ##(b){_woff|w}, Do_read)
enddefine;

    /*  Read a single word into address _waddr
    */
define Read_word(/* _waddr */) with_nargs 1;
    chain((), _0, ##(b)[_1|w], Do_read)
enddefine;

    /*  Read the idval of an identifier
    */
define Read_idval(/* _id */) with_nargs 1;
    chain((), @@ID_VALOF, ##(b)[_1|w], Do_read)
enddefine;


define Open(file, ext, magicnums);
    lvars   file, ext, noerror = false, string, magicnums, args_id = false,
            _len, _magicnum, _sysend, _offs;
    lstackmem struct IMAGE_INIT_HEADER _h;

    define lconstant get_image_args(skipn) -> list;
        lvars skipn, pair, list, s, _len, _woff;
        lstackmem word _tmp;
        ;;; strings and pairs must go in the no-restore segment
        _curr_heap_seg -> _saved_curr_heap_seg;
        Set_curr_heap_seg(_nr_seg_entry);
        until (Read_word(_tmp), _zero(_tmp!(w) ->> _len)) do
            _BYTEVEC_DATA_SIZE(_len) -> _woff;
            if skipn fi_> 0 then
                sysseek(_sr_device, _pint(##(b){_woff|w}), 1)
            else
                inits(_pint(_len)) ->> s;
                Read(s@V_WORDS, _woff)
            endif;
            skipn fi_- 1 -> skipn
        enduntil;
        [] -> list;
        while skipn fi_< 0 do
            ;;; avoid putting pairs in the freelist
            Get_store(@@(struct PAIR)++) -> pair;
            pair_key -> pair!KEY;
            (), list -> (fast_front(pair), fast_back(pair));
            pair -> list;
            skipn fi_+ 1 -> skipn
        endwhile;
        Set_curr_heap_seg(_saved_curr_heap_seg)
    enddefine;


    ;;; make sure the current directory doesn't get created in
    ;;; the save/restore segment
    current_directory -> ;

    if isident(file) then
        ;;; being called from Init_restore
        file -> args_id;
        false -> file
    endif;
    if isboolean(file) then
        ;;; extra boolean argument to sysrestore means no error if
        ;;; file can't be opened, or different pop system
        (), if file then 2 else 1 endif -> (file, noerror)
    endif;
    sysfileok(Get_filename(file,ext)) -> file;

    ;;; switch to the save/restore seg to allocate the restore device
    _curr_heap_seg -> _saved_curr_heap_seg;
    _sr_seg_entry!SEG_BASE_PTR -> _sr_seg_entry!SEG_FREE_PTR;
    Set_curr_heap_seg(_sr_seg_entry);
    sysopen(file, 0, true, if noerror then `D` else `N` endif) -> _sr_device;
    Set_curr_heap_seg(_saved_curr_heap_seg);

    returnunless(_sr_device) (false);

    Read(_h, @@(struct IMAGE_INIT_HEADER)++);

    _h!IMG_MAGIC -> _magicnum;
    unless issimple(_magicnum) and fast_lmember(_magicnum, magicnums) then
        sysclose(_sr_device);
        mishap(file, 1, 'FILE IS NOT A POPLOG SAVED IMAGE')
    endunless;

    ;;; check image id
    _h!IMG_VERSION_STRING_LEN -> _len;  ;;; length of image id string
    if _len _gr _:VERSION_LEN then
        sysclose(_sr_device);
        mishap(file, 1, 'SAVED IMAGE CONTAINS INVALID DATA (ERROR 1)')
    endif;

    inits(_pint(_len)) -> string;
    _bmove(@@(b)[_len], _h@IMG_VERSION_STRING, string@V_BYTES) -> ;
    unless string = pop_system_version then
        if args_id and issimple(_h!IMG_IMAGE_ARGS_OFFS ->> _offs)
        and isinteger(_offs) then
            ;;; return the list of system image args this image used
            sysseek(_sr_device, _offs, 0);
            if (get_image_args(fast_idval(args_id)) ->> fast_idval(args_id))
                /== [] then
                ;;; try restoring the preceding images first
                2 -> noerror
            endif
        endif;
        sysclose(_sr_device);
        returnif(noerror == 2) (false);
        mishap(file, string, 2,
                    'crsv: CAN\'T RESTORE - NOT SAME SYSTEM AND VERSION')
    endunless;

    _h!IMG_SYSTEM_END -> _sysend;       ;;; saved value of _system_end

#_IF DEF VMS
    ;;; Check saved value of _system_end to be above where shareable images
    ;;; finish -- otherwise, we can't restore. This happens because the
    ;;; built-in shareable images have expanded by more than the allowed
    ;;; margin
    unless _sysend >=@(vpage) _initial_mem_end then
        sysclose(_sr_device);
        Shrim_too_big(_initial_mem_end, _sysend, _h!IMG_INITIAL_MEM_END,
                                                            _SHRIM_START)
    endunless;
#_ENDIF

    (_sysend, _magicnum, file)
enddefine;


;;; ----------------------------------------------------------------------

#_IF DEF VMS

    /*  Mishap when an area of shareable images has grown too big to fit
    */
define Shrim_too_big(_now, _allowed, _old, _base);
    lvars   _now, _allowed, _old, _base, _old_pages, _margin_needed,
            _margin_allowed;

    ##(vpage){_old, _base} -> _old_pages;
    (##(vpage){_now, _old} _mult _100) _div _old_pages -> _margin_needed;
    if _nonzero() then _margin_needed _add _1 -> _margin_needed endif;

    (##(vpage){_allowed, _old} _mult _100) _divq _old_pages -> _margin_allowed;

    mishap('expansion margin allowed ' sys_>< _pint(_margin_allowed)
                sys_>< '%, needed ' sys_>< _pint(_margin_needed)
                sys_>< '%', 1,
            'crsv: CAN\'T RESTORE - VMS SHAREABLE IMAGE(S) HAVE EXPANDED, NEED TO REBUILD SAVED IMAGE')
enddefine;

    /*  Calculate expansion margin for an area of shareable images
    */
define Shrim_margin(_shrsize);
    lvars s = systranslate('pop_shrim_margin'), _shrsize, _margin;
    if s then
        unless (Endstring_to_num(1, s) ->> _margin) and _margin _lt 500 then
            mishap(s, 1, 'INVALID VALUE FOR pop_shrim_margin')
        endunless;
    else
        ;;; not specified -- default 10%
        _10 -> _margin;
    endif;
    @@(vpage)[ (##(vpage){_shrsize} _mult _margin _add _99) _divq _100 ]
enddefine;

    /*  Adjust the start address of the initial open segment to allow
        a margin for shareable image expansion in saved images
        (called from Setup_system in setpop.p).
        The percentage margin can be specified by the logical name
        "pop_shrim_margin".
    */
define Adjust_heap_start();
    lvars _margin, _mwords;

    ;;; remember this for restoring a saved image
    _system_end@POPBASE -> _initial_mem_end;

    Shrim_margin( @@(vpage){_initial_mem_end, _SHRIM_START} ) -> _margin;
    returnif(_zero(_margin));
    ##(w){_margin | vpage} -> _mwords;
    Expand_open_seg(_mwords, _0) -> ;
    if Open_seg_expand_for(_mwords _add _2:1e12) _sgr _0 then
        mishap(0, 'CANNOT GET MEMORY FOR INITIAL HEAP')
    endif;
    _system_end@(w){_margin} -> _system_end;
    ;;; Delete the pages we don't need
    Delete_mem(_initial_mem_end, _margin)
enddefine;

#_ENDIF

endsection;     /* $-Sys$-Sr */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 29 1996
        Changes to Open to make it read new system image args table from
        image when version doesn't match.
--- John Gibson, Jan 27 1996
        Current seg now saved in _saved_curr_heap_seg while allocating from
        _sr_seg.
--- Robert John Duncan, Aug  7 1992
        Endstring_to_num now returns a system integer
--- Robert John Duncan, Feb 14 1992
        Changed -Open- always to read the saved value of -_initial_mem_end-
        even if it's not used, so that it always reads everything that
        -Create- writes. This fixes a bug in -sysrestoreincr- (isl-fr.4403).
--- John Gibson, Apr 23 1991
        More VMS things.
--- John Gibson, Apr 15 1991
        Added -Adjust_heap_start-, and other changes.
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Oct 22 1990
        Changed format of device read/write procedure args
--- John Gibson, May 28 1990
        Changed to used new -sysopen- arg so that initial restoring of
        saved images doesn't mishap for non-existent dir.
--- John Gibson, Dec  6 1989
        Changes for new pop pointers
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Gibson, Mar 10 1988
        Renamed sr_util.p (previously srneeds.p)
--- John Gibson, Feb 22 1988
        Check_string into section Sys
--- John Gibson, Sep 26 1987
        Changed layers.ph to memseg.ph
--- John Gibson, Aug 14 1987
        Changed for segmented system
 */
