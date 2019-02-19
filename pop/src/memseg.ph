/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/memseg.ph
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;------------------ MEMORY SEGMENT TABLE -------------------------------

    /*  Entries in _seg_table.
        Note that SEG_BASE_PTR and SEG_FREE_PTR are actual addresses for
        non-pop segments, and pop pointers for pop segments (i.e.
        <actual address>@~POPBASE ).
    */
struct SEG
  { (word)  SEG_BASE_PTR,       ;;; base ptr/address
            SEG_FREE_PTR;       ;;; next free ptr/address
    int     SEG_SIZE,           ;;; word offset size
            SEG_FLAGS;          ;;; flags
  };

    /*  Masks for flags in SEG_FLAGS
    */
lconstant macro (
    _M_SEG_CONSTANT         = _2:1e0,   ;;; non-writeable
    _M_SEG_NON_POP          = _2:1e1,   ;;; doesn't contain pop structs
    _M_SEG_NO_SAVE          = _2:1e2,   ;;; not saved/restored
    _M_SEG_EXT_DYN_MEM      = _2:1e3,   ;;; external memory
    _M_SEG_SPECIAL          = _2:1e4,   ;;; special, e.g. no restore seg
    _M_SEG_FIXED_STRUCTS    = _2:1e5,   ;;; contains fixed-address pop structs
    _M_SEG_HAS_LOCK_REC     = _2:1e6,   ;;; pop seg ends with a lock record
    _M_SEG_IMAGE_FIXUP      = _2:1e7,   ;;; image activator fixup section (VMS)
    _M_SEG_FIXED_EXPTRS     = _2:1e8,   ;;; contains fixed-address exptrs
                                        ;;; tied to external relocation
    );

    /*  Lock record created by sys_lock_heap (a string)
    */
struct LOCK_REC
  { word    V_LENGTH;
    full    KEY;
>-> int     LOCK_ALL_USED,      ;;; total used mem in this lock
            LOCK_FXD_USED;      ;;; fixed used mem in this lock
  };

    /*  Header on free blocks in fixed-address segments
    */
struct FREE_BLOCK_HEADER
  { (word)  FREEBLK_SIZE_NEXT;  ;;; ptr to next block of this size
    full    KEY;                ;;; only to here for blocksize = 2 words
>-> int     FREEBLK_SIZE;       ;;; word offset size
  };


lconstant macro (

    ;;; Memory protections
    _M_PROT_ALL     = _2:111,
    _M_PROT_NOWRITE = _2:101,
    _M_PROT_NONE    = _2:000,

    ;;; Flags used by sys_lock_system
    SLS_SHARE                   = 2:1e0,
    SLS_NONWRITEABLE_DEFAULT    = 2:1e1,
    SLS_NONWRITEABLE_CLOSURES   = 2:1e2,    ;;; inverted in flags to sys_lock_system
    SLS_NONWRITEABLE_CLASS      = 2:1e16,   ;;; not in flags to sys_lock_system
);


section $-Sys;

constant
        procedure (Add_seg_entry, Add_nonpop_seg_entry, Set_mem_prot)
    ;

vars
        _seg_table, _seg_table_next_free, _seg_table_lim,
        _lowest_heap_seg, _lowest_free_heap_seg,
        _curr_seg_free_ptr, _curr_seg_free_lim
    ;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 28 1996
        Added _M_SEG_SPECIAL
--- John Gibson, May 15 1995
        Added sys_lock_system flags
--- John Gibson, Apr 26 1991
        Added _M_SEG_IMAGE_FIXUP, _M_SEG_FIXED_EXPTRS
--- John Gibson, Jan 28 1990
        Added struct FREE_BLOCK_HEADER
--- John Gibson, Jan  4 1990
        Added _M_SEG_FIXED_STRUCTS.
--- John Gibson, Feb 23 1988
        Made declarations global
--- John Gibson, Sep 26 1987
        Changed name of this file to memseg.ph (previously layers.ph)
--- John Gibson, Aug 14 1987
        Changed for segmented system
 */
