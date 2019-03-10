/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/sr_sys.p
 > Purpose:         Saving and restoring images
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSTEM
 */

;;; -------------- SAVING AND RESTORING THE SYSTEM ------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'io.ph'
#_INCLUDE 'sr.ph'


constant
        procedure (sysseek, external_do_load, prolog_reset)
    ;

vars
        poppid, popusername, popversion, poparglist0, poparg0,
        poparglist, popenvlist, pop_runtime,
        popdirectory, popgctime, popheader, poplogfile,
        _plog_next_var, _system_stack_base, _pop_callstack_lim
    ;


section $-Sys;

constant
        procedure (Lock_heap_segs, Tab_open_seg, Detab_open_seg,
        Close_filetab_files, Ensure_min_userstack, Set_userhi_to,
        Ensure_open_seg_open, Reg_save_apply, Clear_freelists,
        Readerr_mishap, Move_callstack, Reset_last_gc_data,
        Init_arg_search, Process_percent_args, Do_runtime_startup,
        Initial_fixed_seg, Get_mem_break,
        Gc$-App_range, Extern$-Save, Extern$-Pre_restore, Extern$-Restore,
        Extern$-Reactivate_shrims, Io$-Kill_device,
        Plog$-Area_save, Plog$-Area_restore,
        ),

        _data_seg_start, _data_seg_end
    ;

vars
        default_device_encoding,
        Dir$-current, Extern$-encoding_funcs_loaded,
    ;

endsection;



#_IF DEF VMS

#_INCLUDE 'vmsdefs.ph'

vms_use_macdefs SEC;        ;;; for SEC$ constants

section $-Sys;

constant
        procedure (Delete_mem, Create_mem),

        /*  RMS control block set up in amisc.s
            A (writeable) FAB with User File Open option and a NAMe block
        */
        Io$- _work_ufofab
    ;

endsection;

#_ELSEIF DEF UNIX

#_INCLUDE 'unixdefs.ph'

constant
        procedure sys_reset_signal
    ;

#_ELSEIF DEF WIN32

#_INCLUDE 'win32defs.ph'

#_ENDIF



;;; -----------------------------------------------------------------------

section $-Sys$-Sr => pop_after_restore, syssave, sysrestore,
                     sys_install_image;

vars
    ;;; procedure to be run after a successful sysrestore
    procedure pop_after_restore = identfn,

    ;;; variable that if set true prohibits a sysrestore (e.g. because
    ;;; external memory has been used)
    _prohibit_restore = false,

    ;;; these variables are not used outside of this file, but CANNOT
    ;;; be lvars (because they'd get corrupted while restoring).
    _srp,
    _srlen,
    _new_system,
    _layering,
    _nuserhi,
    _usersize,
    _sysimage_arg_count = 0,
    ;


    ;;; permanent non-pop variables to be saved/restored
lconstant

    ;;; First, pointers into the system stack area (excluding
    ;;; -system_stack_base-). These must be relocated after restoring,
    ;;; since the stack end on system startup may have moved from its
    ;;; position on saving.
    sr_stkpt_nonpop_vars =
        {%  ident _call_stack_hi,
        %},

    ;;; and the rest
    sr_nonpop_vars =
        {%  ident _lowest_heap_seg, ident _lowest_free_heap_seg,
            ident _curr_heap_seg,
            ident _curr_seg_free_ptr, ident _curr_seg_free_lim,
            ident _file_tab_next_free, ident _file_tab_close_ptr,
            ident _pop_callstack_lim,
            ident weakref[prologvar_key] _plog_next_var,
        %};




#_IF DEF VMS

section $-Sys$-Io;

lconstant
    gsect_name          = writeable 'POP$123456789ABC_1',
    _gsect_name_desc    = (_DESCRIPTOR gsect_name)@DSC_SPEC,
    ;

define lconstant Set_channel(fname);
    lvars fname;
    ;;; set _sr_channel to be channel for fname opened in user mode -- this
    ;;; will be used to map the heap onto the file
    fname@V_BYTES -> _work_ufofab!FAB$L_FNA;        ;;; filename addr
    fname!V_LENGTH -> _work_ufofab!FAB$B_FNS;   ;;; filename length
    if _extern sys\$open(_work_ufofab) _bitst _1 then
        _work_ufofab!FAB$L_STV -> $-Sys$-Sr$- _sr_channel
    else
        Syserr_mishap(fname, 1, 'CAN\'T RE-OPEN SAVE FILE')
    endif
enddefine;


define lconstant Set_gsect_name();
    ;;; this puts the save/restore File IDent number as 12 hex chars into
    ;;; gsect_name(5) to gsect_name(16)
    lvars _fid = (_work_ufofab!FAB$L_NAM)@NAM$W_FID,    ;;; FID addr (6 bytes)
        _bptr = gsect_name@V_BYTES[_4], _blim = _bptr@(b)[_12], _c, _b;
    while _bptr <@(b) _blim do
        _fid!(b)++ -> (_b, _fid);
        (_b _bimask _16:F) -> _c;
        if _c _gr _9 then _c _add _:`7` else _c _add _:`0` endif
                                    -> _bptr!(b)++ -> _bptr;
        _shift(_b, _-4) -> _c;
        if _c _gr _9 then _c _add _:`7` else _c _add _:`0` endif
                                    -> _bptr!(b)++ -> _bptr
    endwhile;
    _:`1` -> gsect_name!V_BYTES[_17]
enddefine;

define lconstant Next_gsect_name();
    gsect_name!V_BYTES[_17] _add _1 -> gsect_name!V_BYTES[_17]
enddefine;

endsection;     /* $-Sys$-Io */


#_ELSEIF DEF UNIX

define lconstant Set_channel(fname);
    lvars fname;
    ;;; set _sr_channel to be file descriptor
    _sr_device!D_FILE_DESC -> _sr_channel
enddefine;

    #_IF DEF SYSTEM_V_SHMEM

    lconstant
        _IPC_CREAT      = _8:1000,      ;;; create entry if key doesn't exist
        _IPC_RMID       = _0,           ;;; remove identifier
        _SHM_RDONLY     = _8:10000,     ;;; attach read-only
        ;

    lvars _shmem_key;

    ;;; produce a (hopefully) unique longword key value for a file
    ;;; using its device and inode numbers
    define lconstant Set_shmem_key();
        lstackmem struct STATB _statb;
        _extern[NI] fstat(_sr_channel, _statb) -> ;
        _shift(_shift(_statb!ST_DEV, _16) _add _statb!ST_INO, _4)
                    _add _1 -> _shmem_key
    enddefine;


    #_ENDIF

#_ELSEIF DEF WIN32

define lconstant Set_channel(file);
    lvars file;
    ;;; set _sr_channel to the file handle of _sr_device
    _sr_device!D_CTRL_BLK!DCB_HANDLE -> _sr_channel;
enddefine;

#_ELSE_ERROR
#_ENDIF


define lconstant Update_nonpop_seg_used(saving);
    lvars saving, _lim, _seg = _seg_table, _flags, _base;
    while _seg <@(struct SEG) _seg_table_next_free do
        _seg!SEG_FLAGS -> _flags;
        if _flags _bitst _M_SEG_NON_POP
        and not(_flags _bitst (_M_SEG_CONSTANT _biset _M_SEG_NO_SAVE))
        then
            _seg!SEG_BASE_PTR -> _base;
            _base@(w){_seg!SEG_SIZE} -> _lim;
            if saving then
                ;;; this will stop somewhere!
                while _zero(_lim--!(w) -> _lim) do endwhile;
                _lim@(w)++ -> _lim;
                if _base <@(w) _lim then _lim else _base endif
                                                        -> _seg!SEG_FREE_PTR
            else
                ;;; restore unused part to zeroes
                ;;; (N.B. should try to zero-map pages here)
                _seg!SEG_FREE_PTR@(w.r->vpage) -> _base;
                _fill(_0, @@(w){_lim, _base}, _base)    ;;; fill with zeros
            endif
        endif;
        _seg@(struct SEG)++ -> _seg
    endwhile
enddefine;


    /*  Map or read the pages offset-size _poffs at page address _paddr,
        starting at (page aligned) _fileoffs on the restore file
        (channel/file descriptor in _sr_channel).
    */
define lconstant Map_or_read(_paddr, _poffs, _fileoffs, _const, _share);
    lvars   _baddr = _paddr@(w->b), _blen = ##(b){_poffs|w},
            _fileoffs, _const, _share, _res, _paddr, _poffs;

#_IF DEF VMS

    ;;; create/map section for segment
    lvars _flags, _gsdnam = _0;
    lstackmem struct MEMRANGE _mrp;
    _baddr              -> _mrp!MR_FIRST_ADDR;      ;;; start address
    _baddr--@(b)[_blen] -> _mrp!MR_LAST_ADDR;       ;;; last address
    if _const and _share then
        _gsect_name_desc -> _gsdnam;
        _:'SEC$M_GBL'
    elseif _const then
        _0
    else
        _:'SEC$M_CRF!SEC$M_WRT'     ;;; copy on ref, writeable
    endif -> _flags;

    repeat 2 times
        _extern sys\$crmpsc(
            /* inadr  */    _mrp,
            /* retadr */    ,
            /* acmode */    ,
            /* flags  */    _flags,
            /* gsdnam */    _gsdnam,
            /* ident  */    ,
            /* relpag */    ,
            /* chan   */    _sr_channel,
            /* pagcnt */    ##(vpagelet){_poffs},
            /* vbn    */    ##(vpagelet){_fileoffs} _add _1,
            /* prot   */    ,
            /* pfc    */    )   -> _res;

        quitif(_res _bitst _1 or _zero(_gsdnam));
        ;;; if failed on global sect, try private
        _0 ->> _flags -> _gsdnam
    endrepeat;

    unless _res _bitst _1 then
        Syserr_mishap(0, 'ERROR MAPPING SAVED IMAGE')
    endunless;

#_ELSEIF DEF UNIX
  #_IF DEF BSD_MMAP
    #_IF DEF AIX
    ;;; Stops AIX giving ENOMEM errors for stuff already mapped
    _extern munmap(_baddr, _blen) -> ;
    #_ENDIF

    ;;; use mmap
    lvars _prot = if _const then _M_PROT_NOWRITE else _M_PROT_ALL endif;

    _extern mmap(_baddr, _blen, _prot,  if _const and _share then
                                            _MAP_SHARED
                                        else
                                            _MAP_PRIVATE
                                        endif _biset _MAP_FIXED,
                    _sr_channel, _fileoffs) -> _res;

    if _res _eq _-1 then
        Syserr_mishap(_sr_device, 1, 'ERROR MAPPING SAVED IMAGE')
    elseif _res /== _baddr then
        mishap(_sr_device, 1, 'ERROR MAPPING SAVED IMAGE (mapped at wrong address)')
    endif;


  #_ELSE

    ;;; just read the area
    Seek(_fileoffs);
    if (_extern[NI] read(_sr_channel, _baddr, _blen) ->> _res) /== _blen then
        Readerr_mishap(_sr_device, _res, 'ERROR READING SAVED IMAGE')
    endif

  #_ENDIF
#_ELSEIF DEF WIN32

    ;;; just read for now, until file mapping sorted out
    Seek(_fileoffs);
    if (_extern pop_read(_sr_channel, _baddr, _blen) ->> _res) /== _blen then
        Readerr_mishap(_sr_device, _res, 'ERROR READING SAVED IMAGE')
    endif;

#_ELSE_ERROR
#_ENDIF
enddefine;      /* Map_or_read */


#_IF DEF AIX

section $-Sys;
    ;;; Dummy initialisation -- value set up by poplink
constant _data_seg_obj_start = _data_seg_obj_start;
endsection;

struct IOVEC
  { (byte)  IOV_BASE;
    int     IOV_LEN;
  };

    /*  This is needed in AIX because nonwriteable data is mixed up with
        writeable stuff.
    */
define Save_rest_data_seg(saving);
    lvars   _rec = _data_seg_obj_start@~POPBASE, _lim = _data_seg_end@~POPBASE,
            _lastblk = _data_seg_start@~POPBASE, _lastlim = _rec,
            _key, _Rec, _nbytes = _0;
    lconstant macro NUMIOV = 16;
    lstackmem struct IOVEC _iovec[NUMIOV];
    lvars _iovp = _iovec, _iovlim = _iovec@(struct IOVEC)[_:NUMIOV];

    define do_iov(_iovec, _iovlim, _nbytes, saving);
        lvars   _fd = _sr_device!D_FILE_DESC,
                _iovcnt = ##(struct IOVEC){_iovlim, _iovec};
        if saving then
            unless _extern writev(_fd, _iovec, _iovcnt) == _nbytes then
                Syserr_mishap(_sr_device, _pint(_nbytes), 2, 'ERROR WRITING SAVED IMAGE')
            endunless
        else
            unless _extern readv(_fd, _iovec, _iovcnt) == _nbytes then
                Readerr_mishap(_sr_device, _pint(_nbytes), 'ERROR READING SAVED IMAGE')
            endunless
        endif
    enddefine;

    while _rec <@(w) _lim do
        _rec!KEY -> _key;
        if _key == objmod_pad_key then
            if _rec!V_LENGTH == _-1 then
                ;;; length of this structure is 2 words and a pop
                ;;; structure follows it
                _rec@(struct POPREC1)++ -> _rec
            else
                ;;; V_LENGTH is a byte length -- get to next word after
                ;;; end of this struct
                _rec@V_WORDS[_rec!V_LENGTH | b.r]@~POPBASE -> _rec;
                ;;; then find next objmod_pad key (should be at start of
                ;;; next object module)
                repeat
                    if _rec >=@(w) _lim then
                        mishap(0, 'Save_rest_data_seg: RUN OFF END OF AREA')
                    endif;
                    quitif(_rec!KEY == objmod_pad_key);
                    _rec@(w)++ -> _rec
                endrepeat
            endif;
            nextloop
        endif;

        _rec -> _Rec;
        _rec@(w){fast_apply(_rec, _key!K_GET_SIZE)} -> _rec;
        ;;; nonwriteable_rawstructs encapsulate nonwriteable data
        ;;; and are not saved/restored
        nextif(_key == nonwriteable_rawstruct_key);

        ;;; save/restore this structure
        if _Rec /== _lastlim then
            _lastblk@POPBASE -> _iovp!IOV_BASE;
            ##(b){_lastlim, _lastblk | w} -> _iovp!IOV_LEN;
            _iovp!IOV_LEN _add _nbytes -> _nbytes;
            _iovp@(struct IOVEC)++ -> _iovp;
            if _iovp >=@(struct IOVEC) _iovlim then
                do_iov(_iovec, _iovp, _nbytes, saving);
                _iovec -> _iovp;
                _0 -> _nbytes
            endif;
            _Rec -> _lastblk
        endif;
        _rec -> _lastlim
    endwhile;

    _lastblk@POPBASE -> _iovp!IOV_BASE;
    ##(b){_lastlim, _lastblk | w} -> _iovp!IOV_LEN;
    do_iov(_iovec, _iovp@(struct IOVEC)++, _iovp!IOV_LEN _add _nbytes, saving)
enddefine;

#_ENDIF /* AIX */


define lconstant Save_rest_segs(saving, _magicnum);
    lvars   saving, isinstalled, _seg, _base, _size, _flags, _fileoffs,
            _const, _share, _res, _shrtab_offs, _magicnum, _sysend
        ;

    define lconstant Used_size(_seg);
        lvars _seg;
        @@(w){  if _seg!SEG_FLAGS _bitst _M_SEG_NON_POP then
                    _seg!SEG_FREE_PTR@(w.r->vpage)
                else
                    _seg!SEG_FREE_PTR@POPBASE@(w.r->vpage)@~POPBASE
                endif, _seg!SEG_BASE_PTR}
    enddefine;

#_IF DEF VMS
    ;;; set the first global section name
    Set_gsect_name();
#_ELSEIF DEF SYSTEM_V_SHMEM
    ;;; set the first shared memory key value
    Set_shmem_key();
#_ENDIF

    undef -> isinstalled;                   ;;; don't know to begin with
    _0 -> _shrtab_offs;                     ;;; offs in shareable sect table
    _magicnum == PSVS_MAGIC -> _share;      ;;; true if const is shared

    ;;; start the segments at the next virtual page aligned block
    @@(vpage)[_int(sysseek(_sr_device, 0, 1, true)) | b.r] -> _fileoffs;
    Seek(_fileoffs);
    _seg_table -> _seg;
    _system_end@POPBASE -> _sysend;

    while _seg <@(struct SEG) _seg_table_next_free do
        _seg!SEG_FLAGS -> _flags;               ;;; flags
        _seg!SEG_BASE_PTR -> _base;             ;;; seg base address
        unless _seg!SEG_FLAGS _bitst _M_SEG_NON_POP then
            _base@POPBASE -> _base
        endunless;

        if _flags _bitst _M_SEG_NO_SAVE then
#_IF DEF VMS
            unless saving then Delete_mem(_base, _seg!SEG_SIZE) endunless;
#_ENDIF
            _seg@(struct SEG)++ -> _seg;
            nextloop
        endif;

        Used_size(_seg) -> _size;               ;;; seg used size
        _seg@(struct SEG)++ -> _seg;            ;;; next seg
        _flags _bitst _M_SEG_CONSTANT -> _const;    ;;; true if constant
        nextif(_zero(_size) or (_const and _base <@(w) _sysend));   ;;; in current sys - dont save

        ;;; non-constant or outside current system
        ;;; save or restore it
        ;;; concatenate contiguous constant/non-constant areas
        while _seg <@(struct SEG) _seg_table_next_free
        and (_seg!SEG_FLAGS _bitst _M_SEG_CONSTANT) == _const
        and not(_seg!SEG_FLAGS _bitst _M_SEG_NO_SAVE)
        and _base@(w){_size} == if _seg!SEG_FLAGS _bitst _M_SEG_NON_POP then
                                    _seg!SEG_BASE_PTR
                                else
                                    _seg!SEG_BASE_PTR@POPBASE
                                endif
        do
            Used_size(_seg) _add _size -> _size;
            _seg@(struct SEG)++ -> _seg
        endwhile;

        if saving then

#_IF DEF VMS or DEF SYSTEM_V_SHMEM
            if _const and _share then
                ;;; record the potentally shareable section's position
                ;;; on the file for possible use by sys_install_image
                lstackmem struct SHR_SECT _shrsp;
                _fileoffs -> _shrsp!SHRS_FILE_OFFS; ;;; word offset in file
                _size     -> _shrsp!SHRS_SIZE;      ;;; size as word offset
                Seek(@@IMG_SHR_SECT_TABLE{_shrtab_offs});
                Write(_shrsp, @@(struct SHR_SECT)++);
                @@(struct SHR_SECT){_shrtab_offs}++ -> _shrtab_offs;
                Seek(_fileoffs)             ;;; back to previous position
            endif;
#_ENDIF
            Write(_base, _size)

        ;;; if layering, don't map non-constant areas
        elseunless _layering and not(_const) then

#_IF DEF VMS

            _0 -> _res;
            if _const and _share and isinstalled /== false then
                ;;; first try to map it as a system global section
                lstackmem struct MEMRANGE _mrp;
                _base               -> _mrp!MR_FIRST_ADDR;
                _base--@(b){_size}  -> _mrp!MR_LAST_ADDR;
                _extern sys\$mgblsc(
                        /* inadr  */    _mrp,
                        /* retadr */    ,
                        /* acmode */    ,
                        /* flags  */    _:'SEC$M_SYSGBL',
                        /* gsdnam */    _gsect_name_desc,
                        /* ident  */    ,
                        /* relpag */    ) -> _res;
                if isinstalled == undef then
                    ;;; first try for sysglobal, set isinstalled
                    _res _bitst _1 -> isinstalled
                elseunless _res _bitst _1 then
                    ;;; previous sect was sysglobal but not this one!
                    Syserr_mishap(0, 'ERROR MAPPING INSTALLED SAVED IMAGE')
                endif;
            endif;
            unless _res _bitst _1 then
                ;;; otherwise create/map section for seg
                Map_or_read(_base, _size, _fileoffs, _const, _share)
            endunless;
            if _const and _share then
                ;;; step the global section name
                Next_gsect_name()
            endif;

#_ELSEIF DEF BSD_MMAP or not(DEF SYSTEM_V_SHMEM)

            Map_or_read(_base, _size, _fileoffs, _const, _share);

#_ELSE

            ;;; shared mem
            _-1 -> _res;
            if _const and _share and isinstalled /== false then
                ;;; try to map it as a shared memory segment
                _extern[SE] shmget(_shmem_key, ##(b){_size|w}, _0) -> _res;
                if isinstalled == undef then
                    ;;; first try for shared memory, set isinstalled
                    _nonneg(_res) -> isinstalled
                elseif _neg(_res) then
                    ;;; previous sect was shared mem but not this one!
                    Syserr_mishap(0, 'ERROR MAPPING INSTALLED SAVED IMAGE')
                endif
            endif;
            if _nonneg(_res) then
                ;;; _res is a shared memory identifier - now attach to it
                lvars _baddr = _base@(w->b);
                if _extern shmat(_res, _baddr, _SHM_RDONLY) /== _baddr then
                    Syserr_mishap(0, 'ERROR ATTACHING TO INSTALLED SAVED IMAGE');
                endif;
                ;;; done -- step the shared memory key for a subsequent section
                _shmem_key _add _1 -> _shmem_key
            else
                ;;; no shared mem -- read the area
                Map_or_read(_base, _size, _fileoffs, _const, _share);
            endif

#_ENDIF

        endif;

        ;;; step file position
        _fileoffs _add _size -> _fileoffs
    endwhile;

    if saving then
        ;;; write word offset length of shareable sect table
        lstackmem int _ip;
        Seek(@@IMG_SHR_SECT_TAB_SIZE);
        _shrtab_offs -> _ip!(int);
        Write_word(_ip);
        Seek(_fileoffs)
    elseif _layering then
        ;;; no more read from the file in this case
        return
    endif;

#_IF DEF AIX
    unless saving then Seek(_fileoffs) endunless;
    Save_rest_data_seg(saving);
#_ELSE
    ;;; save/restore system data seg -- try and map to whole pages
    ;;; in the middle of it
    lvars _s = _data_seg_start, _e = _data_seg_end;
    _s@(w.r->vpage) -> _base;
    _e@(w.t->vpage) -> _sysend;
    if (@@(w){_sysend, _base} ->> _size) _sgr _0 then
        ;;; some whole pages in middle
        lvars _ssz = @@(w){_base, _s}, _esz = @@(w){_e, _sysend};
        if saving then
            Write(_base, _size);
            Write(_s, _ssz), Write(_sysend, _esz)
        else
            Map_or_read(_base, _size, _fileoffs, false, false);
            Seek(_fileoffs _add _size);
            Read(_s, _ssz), Read(_sysend, _esz)
        endif
    elseif saving then
        Write(_s, @@(w){_e, _s})
    else
        Seek(_fileoffs);
        Read(_s, @@(w){_e, _s})
    endif;
#_ENDIF

    ;;; leaves file positioned for rest of save data
enddefine;      /* Save_rest_segs */


;;; --- SAVING ------------------------------------------------------------

    /*  This procedure MUST be called via Save, never directly.
        Like Do_Restore, it must not have as dlocals
        any dynamic variables in general use.
    */
define lconstant Do_Save(file, shareable, _new_system, _sframe,
                                                            _retaddr_ptr);
    lvars   file, shareable, _magicnum, _sframe, _retaddr_ptr, _tmp;
    dlvars  _usrhi;
    dlocal  _disable, _srp, _new_system, _sr_device, _srlen;

    ;;; N.B. Checks that the system is OK to save are done in Save_checks
    ;;; below.

    ;;; create the save file with magicnum for shared/private
    if shareable then PSVS_MAGIC else PSVP_MAGIC endif -> _magicnum;
    Create(file, '.psv', _magicnum);

    Ensure_min_userstack();
    _DISABLE_ALL -> _disable;

    ;;; start the saved data at IMG_SAVED_DATA
    Seek(@@IMG_SAVED_DATA);

    _stklength() -> _usersize;
    if _new_system then
        ;;; cut free space in the open seg down to max 2:1e16 words
        _userlim@(w){@@(w)[_2:1e16] _add _usersize}@(w.r->vpage) -> _usrhi;
        if _userhi <@(vpage) _usrhi then _userhi -> _usrhi endif
    else
        _userhi -> _usrhi
    endif;

    Write_idval(ident _new_system);         ;;; false if not creating a system
    Write_idval(ident _usrhi);
    Write_idval(ident _usersize);           ;;; length of user stack
    Write_idval(ident _userlim);            ;;; user limit

    ;;; table of memory segments - add the open segment
    ;;; rounded to exact number of pages onto the end of the table
    Tab_open_seg();

    ;;; update the 'used' sizes of non-pop segments (i.e. size to last
    ;;; non-zero word) before writing the seg table
    Update_nonpop_seg_used(true);

    @@(struct SEG){_seg_table_next_free, _seg_table} -> _srlen;
    Write_idval(ident _srlen);              ;;; length of seg table
    Write(_seg_table, _srlen);              ;;; seg table

    ;;; write the segments, including system data seg
    Save_rest_segs(true, _magicnum);        ;;; true means saving

    ;;; remove the open seg from the end of the table
    Detab_open_seg();

    ;;; user stack
    Write(_userhi@(w)-{_usersize}, _usersize);

    ;;; nonpop vars to be saved
    Write_idval(ident _system_stack_base);
    appdata(sr_stkpt_nonpop_vars, Write_idval); ;;; pointers into the stack area
    appdata(sr_nonpop_vars, Write_idval);       ;;; others

    ;;; call stack
    Write_word(_retaddr_ptr);               ;;; save return address
    @@(csword){_call_stack_hi, _sframe} -> _srlen;
    Write_idval(ident _srlen);              ;;; length of call stack
    Write(if _sframe >@(w) _call_stack_hi then _call_stack_hi
             else _sframe
             endif, _srlen);                ;;; save call stack

    ;;; prolog area
    if testdef prolog_reset then
        weakref[prolog_reset] $-Sys$-Plog$-Area_save()
    endif;

    ;;; remember system image args current system was run from
    define lconstant save_image_args();
        lvars arg, l = fast_back(poparglist0);
        fast_repeat _sysimage_arg_count times
            destpair(l) -> (arg, l);
            Write_word(arg@V_LENGTH);
            Write(arg@V_WORDS, _BYTEVEC_DATA_SIZE(arg!V_LENGTH))
        endrepeat;
        Write_word(nullstring@V_LENGTH)
    enddefine;

    lstackmem full _srargs;
    sysseek(_sr_device, 0, 2, true) -> _srargs!(full);  ;;; remember eof pos
    save_image_args();

    ;;; if saving a system, set the new upper limit for the system
    ;;; and the new system version string
    if _new_system then
        _open_seg_base_ptr -> _system_end;
        _new_system -> pop_system_version
    endif;

    if testdef external_do_load then
        ;;; deal with external loading
        ;;; Extern$-Save returns addr in file or 0 if none
        weakref[external_do_load] $-Sys$-Extern$-Save(_sr_device, _new_system)
                                                                    -> _srp;
        ;;; record the symbol table offset
        Seek(@@IMG_SYMTAB_OFFS);
        Write_idval(ident _srp)
    endif;

    ;;; record system image args table offset
    Seek(@@IMG_IMAGE_ARGS_OFFS);
    Write_word(_srargs);

    sysclose(_sr_device);
    false                           ;;; say just saved
enddefine;

    /*  Checks required before calling Do_Save -- called right at the
        beginning of syssave and sys_lock_system.
    */
define Save_checks(file);
    lvars file, mess;
    if _call_stack_seg_hi /== _call_stack_hi or _in_external_control then
        mishap(0, 'CAN\'T SAVE SYSTEM WHILE INSIDE EXTERNAL PROCEDURE CALLS')
    endif;

    if pop_runtime and pop_runtime /== "undef" then
        if pop_runtime == "x" then
            {'X TOOLKIT SETUP DONE, X WILL NOT WORK IN SAVED IMAGE' 16:12}
        else
            {'RUN-TIME ACTIONS DONE, SAVED IMAGE MAY BE COMPROMISED' 16:12}
        endif -> mess;
        sys_raise_exception(file, 1, mess, `W`)
    endif;

    Ensure_open_seg_open()
enddefine;

    /*  Do_Save MUST be called thru this procedure, which calls it thru
        Reg_save_apply to save all register lvars.
        Called by syssave and sys_lock_system.
    */
define Save(_new_system) with_nargs 3;
    lvars _new_system;
    Reg_save_apply((), _new_system, Do_Save);   ;;; leave result on stack
    Reset_last_gc_data(_new_system);
    if () then
        ;;; restored

        ;;; read special params and do run-time actions if appropriate
        Do_runtime_startup();

        pop_after_restore();
        true
    else
        ;;; saved
        false
    endif
enddefine;

define syssave(file);
    lvars file;
    Save_checks(file);
    Sysgarbage(false, 'save');
    Save(file, false, false)        ;;; no sharing, not creating a system
enddefine;


;;; --- RESTORING -------------------------------------------------------

    ;;; Variables below are made dlocal to Do_Restore's call of
    ;;; Save_rest_segs, and thus remain unchanged after the restore.
lconstant macro NO_RESTORE_VARS = [
    weakref[popdevin]  dev_in,
    weakref[popdevout] dev_out,
    weakref[popdeverr] dev_err,
    weakref[poprawdevin]  raw_dev_in,
    weakref[poprawdevout] raw_dev_out,
    weakref poppid,
    weakref popusername,
    weakref popheader,
    weakref popgctime,
    weakref sys_encoding,
    weakref default_device_encoding,
    weakref $-Sys$-Extern$-encoding_funcs_loaded,
    popdirectory,
    popversion,
    poparglist0,
    poparg0,
    poparglist,
    popenvlist,
];

lvars
    _callstack_reloc;

define Restore_cs_ptrs(id_vec);
    lvars id_vec;
    appdata(id_vec,
                procedure(_ptr_id);
                    lvars _ptr_id;
                    Read_idval(_ptr_id);
                    _ptr_id!ID_VALOF@(w){_callstack_reloc} -> _ptr_id!ID_VALOF
                endprocedure)
enddefine;

    /*  The file argument may be a boolean, in which case the file is
        next on the stack.
    */
define lconstant Do_Restore(file, _layering, _sframe, _retaddr_ptr);
    lvars   file, fname, _sframe, _retaddr_ptr, _sysend, _magicnum;
    dlocal  _disable, _srp, _layering, _sr_device, _srlen, _sr_channel;

    ;;; N.B. this procedure must not have any other perm variables as dlocals
    ;;; unless they variables not used anywhere else (except perhaps
    ;;; in syssave or the garbage collector). This is because dlocal'ing a
    ;;; variable here will cause its saved image value to be overwritten.


    define lconstant Normalise_mem();
        ;;; if restoring a system, set the new upper limit for the system
        ;;; and the new system version string
        if _new_system then
#_IF DEF CACHEFLUSH
            ;;; flush restored data from the I-cache
            CACHEFLUSH(_system_end, @@(b){_open_seg_base_ptr, _system_end});
#_ENDIF
            _new_system -> pop_system_version;
            _open_seg_base_ptr ->> _system_end;         ;;; new system end
                                -> _open_seg_free_ptr;  ;;; clear the open seg
            false -> _prohibit_restore
        endif;

        ;;; make sure freelists are cleared, as they might reference garbage
        ;;; (not free blocks)
        Clear_freelists(false);
#_IF DEF UNIX
        ;;; same with the current directory (will be recomputed next time)
        false -> $-Sys$-Dir$-current;
#_ENDIF
    enddefine;


    Ensure_open_seg_open();
    if _prohibit_restore then
        mishap(0, 'RESTORE PROHIBITED BY EXTERNAL LOAD/MEMORY FACTORS')
    endif;

    ;;; Open the save file (the file argument may be a boolean or an ident,
    ;;; in which case the file is next on the stack).
    if Open(file, '.psv', #_< [^PSVP_MAGIC ^PSVS_MAGIC] >_#) ->> fname then
        -> (_sysend, _magicnum)
    else
        return(if _layering then false endif)
    endif;
    Set_channel(fname);     ;;; sets _sr_channel
    Ensure_min_userstack();

    ;;; saved data starts at IMG_SAVED_DATA
    Seek(@@IMG_SAVED_DATA);

    Read_idval(ident _new_system);      ;;; false if not a system image
    if _new_system then
        if isident(file) then
            ;;; being called from Init_restore -- count system images restored
            _sysimage_arg_count fi_+ 1 -> _sysimage_arg_count
        endif
    elseif _layering then
        mishap(fname, 1, 'NOT A SYSTEM SAVED IMAGE')
    endif;

    ;;; If restore requires expanded memory, check it's available
    Read_idval(ident _nuserhi);         ;;; new userhi
    if _nuserhi >@(w) _userhi and not(Set_userhi_to(_nuserhi, true)) then
        mishap(0, 'INSUFFICIENT MEMORY TO RESTORE SAVED IMAGE');
        interrupt()
    endif;

    ;;; committed
    _DISABLE_ALL -> _disable;
    sys_exception_handler -> pop_exception_handler;
    Last_chance_exception_final -> pop_exception_final;
    sysexit -> interrupt;
    Close_filetab_files(true);          ;;; close all files, clear table

    if testdef external_do_load then
        ;;; clean up external loads (if possible)
        weakref[external_do_load] $-Sys$-Extern$-Pre_restore();
    endif;

    ;;; restore memory configuration outside system boundary
#_IF DEF VMS
    ;;; deal with possibly different _system_end (_sysend)
    lvars _diff = @@(vpage){_sysend, _system_end};
    if     _diff _sgr _0 then
        Delete_mem(_system_end@POPBASE, _diff)
    elseif _diff _slt _0 then
        Create_mem(_sysend@POPBASE, _negate(_diff))
    endif;
    _sysend -> _system_end;
    ;;; set all pages writeable from _system_end to new _userhi
    Set_mem_prot(_system_end@POPBASE, _nuserhi, _M_PROT_ALL) -> ;
#_ENDIF

    Read_idval(ident _usersize);            ;;; new length of user stack
    _nuserhi@(w)-{_usersize} -> _user_sp(); ;;; careful ...!
    if _nuserhi <@(w) _userhi then
        Set_userhi_to(_nuserhi, true) ->    ;;; contract mem
    endif;
    Read_idval(ident _userlim);             ;;; new userlim

    ;;; table of memory segments -- open seg is on the end
    Read_idval(ident _srlen);               ;;; size of seg table
    Read(_seg_table, _srlen);               ;;; the table
    _seg_table@(struct SEG){_srlen} -> _seg_table_next_free;

    ;;; Read or map onto segments, constant ones only if layering.
    ;;; If not layering, includes system data seg (which overwrites
    ;;; pop_exception_final and interrupt etc). Save_rest_segs is called with
    ;;; NO_RESTORE_VARS dlocal to prevent these being restored from the saved
    ;;; image.
    procedure;
        dlocal NO_RESTORE_VARS;
        Save_rest_segs()
    endprocedure(false, _magicnum);     ;;; false means restoring

    ;;; remove the open seg from the end of the table
    Detab_open_seg();

    ;;; if layering images, return at this point
    if _layering then
        ;;; put all current segs outside the heap
        Lock_heap_segs();
        Normalise_mem();
        return(true)
    endif;

    ;;; else it's a full restore
    lvars   save_hand = pop_exception_handler,
            save_fhand = pop_exception_final,
            save_intr = interrupt;
    sys_exception_handler -> pop_exception_handler;
    Last_chance_exception_final -> pop_exception_final;
    sysexit -> interrupt;

    ;;; Restore zero trailing parts of non-pop segments
    Update_nonpop_seg_used(false);

    ;;; user stack
    Read(_userhi@(w)-{_usersize}, _usersize);

    ;;; Restore special non-pop identifiers.
    ;;; Since the system stack end may have moved, we now have to relocate
    ;;; all addresses referring to system stack locations (this implies that
    ;;; all the prolog area and the callstack must be completely relocatable
    ;;; -- they are anyway for other reasons, except for SPARC frame pointers).

    ;;; Get old _system_stack_base
    Read_idval(ident _srp);

    ;;; The difference between _system_stack_base and _srp
    ;;; is the amount it moved.
    @@(w){_system_stack_base, _srp} -> _callstack_reloc;

    ;;; restore and relocate other callstack pointers using that value
    Restore_cs_ptrs(sr_stkpt_nonpop_vars);
    _call_stack_hi -> _call_stack_seg_hi;

    ;;; Then restore the other special non-pop identifiers
    appdata(sr_nonpop_vars, Read_idval);

    ;;; call stack -- _call_stack_hi is already restored
    Read_word(_retaddr_ptr);                    ;;; restore return address
    Read_idval(ident _srlen);                   ;;; old len of call stack
    _call_stack_hi@(csword)-{_srlen} -> _srp;   ;;; new pos of stack frame
    ;;; move stack frames
    unless Move_callstack(##(csword){_srp, _sframe}, _sframe, false) then
        mishap(0, 'INSUFFICIENT MEMORY TO RESTORE SAVED IMAGE (callstack)')
    endunless;
    Read(if _srp >@(w) _call_stack_hi then _call_stack_hi
             else _srp
             endif, _srlen);                    ;;; restore call stack

#_IF DEF SPARC
    ;;; correct frame pointers in stack frames
    _srp -> _sframe;                            ;;; first frame read in
    while _sframe <@(csword) _call_stack_hi do
        _sframe!SF_FP@(w){_callstack_reloc} ->> _sframe!SF_FP -> _sframe
    endwhile;
#_ENDIF

    ;;; prolog area
    if testdef prolog_reset then
        weakref[prolog_reset] $-Sys$-Plog$-Area_restore()
    endif;

    ;;; kill off old files
    define lconstant kill_file();
        lvars dev = ()!(w);
        $-Sys$-Io$-Kill_device(dev)
    enddefine;

    $-Sys$-Gc$-App_range(_file_tab,
                    @@(w){_file_tab_next_free, _file_tab}, kill_file);
    $-Sys$-Gc$-App_range(_file_tab_close_ptr,
                    @@(w){_file_tab_limit, _file_tab_close_ptr}, kill_file);
    _file_tab -> _file_tab_next_free;
    _file_tab_limit -> _file_tab_close_ptr;

    Normalise_mem();

#_IF DEF CACHEFLUSH
    ;;; flush restored data from the I-cache
    CACHEFLUSH(_system_end, @@(b){_open_seg_free_ptr, _system_end});
#_ENDIF

    Process_percent_args();         ;;; set variables from %args

#_IF DEF VMS and DEF SHARED_LIBRARIES
    if testdef external_do_load then
        ;;; activate shareable images at this point since when _new_system
        ;;; is true the open seg is empty and can be temporarily deleted and
        ;;; recreated after the shrims get mapped (avoiding creating an
        ;;; unnecessary segment)
        weakref[external_do_load] $-Sys$-Extern$-Reactivate_shrims(_new_system)
    endif;
#_ENDIF

    if _new_system then
        ;;; allocate an initial fixed seg (must be done while open seg
        ;;; is empty)
        Initial_fixed_seg()
    endif;

    if testdef external_do_load then
        ;;; deal with external loads
        ;;; get the symbol table pointer for Extern$-Restore to use
        Seek(@@IMG_SYMTAB_OFFS);
        Read_idval(ident _srp);
        weakref[external_do_load]
                $-Sys$-Extern$-Restore(_srp, _sr_device, _new_system)
    endif;

    sysclose(_sr_device);

#_IF DEF UNIX
    sys_reset_signal();     ;;; restore signals to correct state
#_ENDIF

    false -> poplogfile;
    save_hand -> pop_exception_handler;
    save_fhand -> pop_exception_final;
    save_intr -> interrupt;

    true                    ;;; true means just restored
enddefine;

    ;;; Do_Restore MUST be called thru this procedure.
    ;;; Called by sysrestore and Init_restore
define lconstant Restore = Reg_save_apply(% Do_Restore %) enddefine;

define sysrestore() with_nargs 1;
    Restore(false)                  ;;; not layering images
enddefine;


;;; --- RESTORING SAVED IMAGES ON STARTUP --------------------------------

    /*  Try to restore saved images from poparglist args, using
        dir_list as the directories to look in, and default_extn
        for args that don't have one.
    */
define Init_restore(dir_list, default_extn);
    lvars   arg, dir_list, default_extn;
    dlvars  save_pal, sysargs, _layer, _flagc = OLD_PSV_CHAR, _next_flagc;
    dlocal  interrupt = sysexit;

    define lconstant next_is_restore_arg();
        lvars arg;
        returnif(poparglist == []
                 or not(isstring(fast_front(poparglist) ->> arg))
                 or datalength(arg) == 0
                ) (false, _flagc);
        if dup(fast_subscrs(1, arg)) == `+` then
            dup()
        else
            () == _flagc, _flagc
        endif
    enddefine;

    define lconstant try_restore() -> ok;
        lvars ok, pal;
        _sysimage_arg_count -> sysargs;
        Restore((), ident sysargs, _layer);
        unless _layer then false endunless -> ok;
        if ok then
            _next_flagc -> _flagc;
            return
        endif;
        returnunless(ispair(sysargs));

        ;;; splice the preceding images before the current in poparglist
        save_pal -> pal;
        fast_destpair(pal), fast_destpair(sysargs)
            -> (fast_front(sysargs), fast_back(sysargs),
                fast_front(pal), fast_back(pal));
        pal -> poparglist;
        repeat
            _flagc -> fast_subscrs(1,fast_front(pal));
            quitif(fast_back(pal) == []);
            fast_back(pal) -> pal
        endrepeat;
        sysargs -> fast_back(pal);
        true -> ok
    enddefine;

    returnunless(next_is_restore_arg() -> _flagc);

    repeat
        poparglist -> save_pal;
        allbutfirst(1, fast_destpair(poparglist) -> poparglist) -> arg;
        next_is_restore_arg() -> (_layer, _next_flagc);
        Init_arg_search(arg, dir_list, default_extn, 'SAVED IMAGE NOT FOUND',
                                        try_restore)
    endrepeat
enddefine;      /* Init_restore */


;;; --- INSTALLING AND DE-INSTALLING SAVED IMAGES ------------------------

#_IF DEF VMS or DEF SYSTEM_V_SHMEM


define lconstant Warning(file, mess);
    lvars file, mess;
    printf(file, mess, ';;; WARNING - %p (%M)\n;;; INVOLVING:  %p\n')
enddefine;


#_IF DEF VMS

define lconstant Delete_sects(file, nsects);
    lvars nsects, file, _break, _res;
    lstackmem struct MEMRANGE _mrp;

    Set_gsect_name();   ;;; set the first global section name

    ;;; try mapping 1 page of sections at end of P0
    Get_mem_break() -> _break;

    repeat nsects times
        ;;; see if perm global section exists
        _break                   -> _mrp!MR_FIRST_ADDR;
        _break@(vpage)[_1]--@(b) -> _mrp!MR_LAST_ADDR;
        _extern sys\$mgblsc(
                        /* inadr  */    _mrp,
                        /* retadr */    _mrp,
                        /* acmode */    ,
                        /* flags  */    _:'SEC$M_SYSGBL',
                        /* gsdnam */    _gsect_name_desc,
                        /* ident  */    ,
                        /* relpag */    ) -> _res;
        unless _res == _:'SS$_NOSUCHSEC' then
            _extern sys\$deltva(_mrp, , ) -> ;
            ;;; deleting existing permanent system global section
            unless _extern sys\$dgblsc(
                    /* flags  */    _:'SEC$M_SYSGBL',
                    /* gsdnam */    _gsect_name_desc,
                    /* ident  */    )
            _bitst _1 then
                Syserr_mishap(file, 1, 'CAN\'T DE-INSTALL SAVED IMAGE')
            endunless;
        endunless;
        ;;; step the global section name
        Next_gsect_name()
    endrepeat
enddefine;

define lconstant Create_sects(file, nsects);
    lvars nsects, created, file;
    lstackmem struct SHR_SECT _shrsp;

    Set_gsect_name();   ;;; set the first global section name
    ;;; table of shared sections is at offset IMG_SHR_SECT_TABLE
    ;;; each entry gives word offset within file and word offset length
    Seek(@@IMG_SHR_SECT_TABLE);
    0 -> created;
    repeat nsects times
        Read(_shrsp, @@(struct SHR_SECT)++);    ;;; file offset and offs length
        ;;; create new permanent system global section from this area
        unless _extern sys\$crmpsc(
                /* inadr  */    ,
                /* retadr */    ,
                /* acmode */    ,
                /* flags  */    _:'SEC$M_GBL!SEC$M_SYSGBL!SEC$M_PERM',
                /* gsdnam */    _gsect_name_desc,
                /* ident  */    ,
                /* relpag */    ,
                /* chan   */    _sr_channel,
                /* pagcnt */    ##(vpagelet){ _shrsp!SHRS_SIZE },
                /* vbn    */    ##(vpagelet){ _shrsp!SHRS_FILE_OFFS } _add _1,
                /* prot   */    ,
                /* pfc    */    )
        _bitst _1 then
            Warning(file, 'CAN\'T INSTALL SAVED IMAGE');
            ;;; delete any sects created thus far
            Delete_sects(file, created);
            return
        endunless;
        created fi_+ 1 -> created;
        ;;; step the global section name
        Next_gsect_name()
    endrepeat
enddefine;


#_ELSEIF DEF SYSTEM_V_SHMEM

define lconstant Delete_sects(file, nsects);
    lvars nsects, file, _shmid;
    lstackmem struct SHR_SECT _shrsp;

    Set_shmem_key();        ;;; set the first shared memory key value
    ;;; table of shared sections is at offset IMG_SHR_SECT_TABLE
    ;;; each entry gives word offset within file and word offset length
    Seek(@@IMG_SHR_SECT_TABLE);
    repeat nsects times
        Read(_shrsp, @@(struct SHR_SECT)++);    ;;; file offset and offs length
        ;;; deleting existing shared memory segments
        _extern[SE] shmget(_shmem_key, ##(b){_shrsp!SHRS_SIZE | w},
                           _0) -> _shmid;
        if _neg(_shmid)
        or _neg(_extern[SE] shmctl(_shmid, _IPC_RMID, _0)) then
            Warning(file, 'CAN\'T DE-INSTALL SAVED IMAGE');
            return
        endif;
        ;;; step the shared memory key
        _shmem_key _add _1 -> _shmem_key;
    endrepeat;
enddefine;

define lconstant Create_sects(file, nsects);
    lvars nsects, created, file, _taboffs, _shmid, _baddr, _blen;
    lstackmem struct SHR_SECT _shrsp;

    Set_shmem_key();        ;;; set the first shared memory key value
    ;;; table of shared sections is at offset IMG_SHR_SECT_TABLE
    ;;; each entry gives word offset within file and word offset length
    _0 -> _taboffs;
    0 -> created;
    repeat nsects times

        ;;; get next table entry
        Seek(@@IMG_SHR_SECT_TABLE{_taboffs});
        Read(_shrsp, @@(struct SHR_SECT)++);    ;;; file offset and size
        @@(struct SHR_SECT){_taboffs}++ -> _taboffs;
        Seek(_shrsp!SHRS_FILE_OFFS);                ;;; start of area in file
        ##(b){_shrsp!SHRS_SIZE | w} -> _blen;       ;;; its length in bytes

        ;;; create new shared memory segment from this area
        ;;; (644 mode is read/write by owner and read by all)
        _extern[SE] shmget(_shmem_key, _blen,
                           _IPC_CREAT _biset _8:644) -> _shmid;
        if _neg(_shmid)
        or (_extern shmat(_shmid, _0, _0) ->> _baddr) == _-1
        or _extern[NI] read(_sr_channel, _baddr, _blen) /== _blen
        then
            _extern shmdt(_baddr) -> ;
            _extern shmctl(_shmid, _IPC_RMID, _0) -> ;  ;;; in case created
            Warning(file, 'CAN\'T INSTALL SAVED IMAGE');
            ;;; delete any sects created thus far
            Delete_sects(file, created);
            return
        endif;

        ;;; created, attached and read in to
        ;;; now detach from it
        _extern shmdt(_baddr) -> ;
        created fi_+ 1 -> created;

        ;;; step the shared memory key
        _shmem_key _add _1 -> _shmem_key;
    endrepeat;
enddefine;


#_ENDIF

constant procedure $-consdescriptor;

define sys_install_image(file, create);
    lvars file, nsects, create;
    dlvars _magicnum;
    dlocal _sr_device, _sr_channel;
    lstackmem int _ip;

    unless sysopen(file, 0, true, `F`) ->> _sr_device then
        if create then
            Syserr_mishap(file, 1, 'CAN\'T OPEN SAVED IMAGE')
        endif;
        return
    endunless;

    Read_idval(ident _magicnum);                ;;; read magic number
    unless issimple(_magicnum) and _magicnum == PSVS_MAGIC then
        sysclose(_sr_device);
        unless _magicnum == PSVP_MAGIC then
            mishap(file, 1, 'FILE IS NOT A POPLOG SAVED IMAGE')
        elseif create then
            printf(file, 'SAVED IMAGE NOT SHAREABLE - NOT INSTALLED',
                                ';;; WARNING - %p\n;;; INVOLVING:  %p\n')
        endunless;
        return
    endunless;

    Set_channel(sysfileok(file));       ;;; sets _sr_channel
    ;;; word offset size of shared sect table is at offset IMG_SHR_SECT_TAB_SIZE
    Seek(@@IMG_SHR_SECT_TAB_SIZE);
    Read_word(_ip);
    _pint( ##(struct SHR_SECT){_ip!(int)} ) -> nsects;
    if create then
        ;;; installing image
        Create_sects(file, nsects)
    else
        ;;; de-installing image
        Delete_sects(file, nsects)
    endif;
    sysclose(_sr_device);
enddefine;


#_ENDIF

endsection;     /* $-Sys$-Sr */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  2 1998
        Added AIX Save_rest_data_seg.
--- John Gibson, Feb 28 1997
        Added Extern$-encoding_funcs_loaded to NO_RESTORE_VARS.
--- John Gibson, Aug 29 1996
        Changed Do_Save to record all system image args that were used to run
        the system; then changed Init_restore to recover those args, so
        only the last of a sequence of images to restore need be specified.
--- John Gibson, Mar 29 1996
        Made Do_Save cut down the open seg size stored on a system saved image
        so it doesn't start off with too large an amount of free space.
--- John Gibson, Feb  6 1996
        Replaced old pr*mishap with pop_exception_final etc.
        Now calls sys_raise_exception for runtime-actions-done warning.
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Nov 16 1994
        Made variables previously local to Do_Restore be local to a procedure
        wrapped around the call of Save_rest_segs (so variables like
        poparglist have their correct values before leaving Do_Restore)
--- John Gibson, Nov 15 1994
        Removed call of Extern$-Post*_restore (not necessary, can be done at
        the end of Extern$-Restore). Also made Extern$-Pre_restore be called
        always
--- Robert John Duncan, Sep  5 1994
        Added code for Win32
--- John Gibson, Aug 15 1994
        Changes to _file_tab layout
--- John Gibson, Jun  2 1994
        Replaced all uses of _d*ouble with lstackmems
--- John Gibson, May 10 1994
        Added [NI] flag to _extern calls needing it
--- John Gibson, May  5 1994
        Replaced _extern pop_r*ead with _extern read
--- Robert John Duncan, Jan 26 1994
        Removed Symmetry code (now defunct)
--- Robert John Duncan, Aug  7 1992
        More changes for SunOS dynamic linking (now assumed for SunOS >= 5)
--- Robert John Duncan, Jun 16 1992
        System call -read- replaced by -pop_r*ead- from "c_core.c"
--- Simon Nichols, Mar  2 1992
        Changes to support SunOS dynamic linking (temporarily flagged by
        SU*NOS_DYNAMIC).
--- John Gibson, Jan  9 1992
        Moved checks done at the beginning of -Do_Save- into a separate
        procedure -Save_checks- (that can be called by -sys_lock_system-
        before it does anything).
--- John Gibson, Dec 11 1991
        Added warning messages in Do_Save when pop_runtime is true.
--- John Gibson, Dec  9 1991
        Change to VMS -Delete_sects- so it doesn't complain if sects
        are not installed
--- John Gibson, May 15 1991
        Added call of -Do_runtime_startup- in -Save-
--- John Gibson, Apr 15 1991
        Changes to -Do_Restore- for VMS
--- Robert John Duncan, Feb 11 1991
        Added cache flush
--- John Gibson, Dec  5 1990
        Changes to VMS descriptors
--- John Gibson, Nov 18 1990
        Changed -Save_rest_segs- so that whole-page part of system data
        segment is mapped (if possible) rather than read (speeds up
        restoring).
--- John Gibson, Oct 10 1990
        VMS _extern changed to return proper system call result (thus test
        for success is now result _bitst _1).
--- John Gibson, Sep  1 1990
        Moved in -Init_restore- from init_args.p (i.e. from old
        init_restore.p)
--- Simon Nichols, Mar 30 1990
        Changed -Do_restore- to check each device in -_file_tab- to ensure
        that if it is a pw*m_id, <false> is assigned to its PW*M_IDCHAR field,
        instead of applying -Kill_device- to it.
--- John Gibson, Feb 27 1990
        Added extra arg to call of -Sysgarbage-.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Nov  9 1989
        Changes to prohibit saving during external callback
--- John Gibson, Nov  9 1989
        Removed p*opinicom.
--- John Gibson, Sep  2 1989
        Uses -Move_callstack-
--- John Gibson, Aug 28 1989
        Removed reference to s*td_in_tty
--- John Gibson, Aug 10 1989
        Code for restoring layered saved images on startup moved to
        init_restore.p
--- John Gibson, Jul 20 1989
        Made -pop_after_restore- be called in -Save- after restoring
        rather than by -Do_Restore- (so it's called outside of
        -Reg_save_apply-).
--- Roger Evans, Mar 20 1989
        Added poparg0
--- John Gibson, Feb  5 1989
        Fixed bug in -Layer_restore- (if no file could be opened was
        returning true instead of false because of missing parentheses
        round Restore(...) ->> loaded ).
--- John Gibson, Feb  4 1989
        Got rid of Pw*m_nr (poplink can now set up defaults for weak
        active vars)
--- John Gibson, Nov 22 1988
        In VMS, -Save_rest_segs- was attempting to put out a warning message
        when it failed to make a constant image segment group-shareable
        (having to default it to private). Unfortunately, the warning was
        trying to use -sysio*message-, which was trying to create a string
        (which is not an allowable thing to do at that point); consequently,
        access violations were caused, and the system fell over.
            (This is why many people were having problems with the Clisp
        saved image; they didn't have enough space in the VMS global page
        table to make the constant part a group-shareable global section.)
            Have therefore removed the warning message code (so that it
        just defaults to non-shared silently).
--- Rob Duncan, Oct 10 1988
        More Symmetry hacks
--- Rob Duncan, Oct  6 1988
        Added code for Symmetry mmap (in -Do_Save- & -Save_rest_segs-)
--- John Gibson, Aug 11 1988
        Added use of -mmap- for mapping saved images (available at last!)
--- John Gibson, Jul 28 1988
        Further changes for SPARC.
--- John Gibson, Jul  5 1988
        Changes to accomodate new -Reg_save_apply-.
--- John Gibson, Mar 10 1988
        Renamed sr_sys.p (previously saverest.p), sectionised
--- John Gibson, Mar  4 1988
        Prolog stuff into plog_area.p
--- John Gibson, Feb 25 1988
        Garbage collector routines into section Sys$-Gc
--- John Gibson, Feb 16 1988
        Weakref'ed external load references
--- John Gibson, Feb 15 1988
        Weakref'ed pw*m stuff
--- Roger Evans, Jan 20 1988
        Added call to sys_reset_signal when restoring (for UNIX systems
        only), anticipating the arrival of new signal handling
--- John Gibson, Jan 17 1988
        Different data sections for words and identifiers generated by poplink
        now replaced by area from _data_seg_start to _data_seg_end
 */
