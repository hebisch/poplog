/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.unix/src/unixextern.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; -------------- ROUTINES FOR EXTERNAL PROCEDURES (UNIX) ----------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'memseg.ph'
#_INCLUDE 'unixdefs.ph'
#_INCLUDE 'external.ph'

constant
        procedure (sysseek, sysdelete, sysobey, applist, maplist, sysfileok,
        sys_fname_name, sys_fname_extn, sys_file_stat),
        active (popmemlim)
    ;

vars
        poppid, pop_status
    ;

section $-Sys;

constant
        procedure (Open_seg_shift_gap_base, Do_open_seg_shift_gc,
        Cons_extern_ptr)
    ;

endsection;

section $-Sys$-Extern;

constant
        procedure (Shlib_open, Shlib_close, Shlib_findsym, Name_translate,
        Get_symbol_ptr, Readb, Writeb)
    ;

vars    _image_handle   ;;; for SunOS
    ;

endsection;


;;; -----------------------------------------------------------------------

section $-Sys$-Extern => Extern_make_base;


;;; --- UNIX A.OUT FILE DEFINITIONS ---------------------------------------

#_IF not(DEF SHARED_LIBRARIES)

;;; --- COMMON OBJECT FILE FORMAT (SYSTEM V) ------------------------------

#_IF DEF COFF

    /* file header */
struct A_OUT_HDR
{
    short   AHF_MAGIC,      ;;; magic number
            AHF_NSCNS;      ;;; number of sections
    long    AHF_TIMDAT,     ;;; time and date stamp
            AHF_SYMPTR,     ;;; file pointer to symbol table
            AHF_NSYMS;      ;;; number of symbol table entries
    short   AHF_OPTHDR,     ;;; size of optional header
            AHF_FLAGS;      ;;; flags
};

    /* UNIX system header */
struct A_OUT_UNIX_HDR
{
    short   AH_MAGIC,       ;;; magic number
            AH_VSTAMP;      ;;; version stamp
    long    AH_TSIZE,       ;;; size of text segment
            AH_DSIZE,       ;;; size of data segment
            AH_BSIZE,       ;;; size of bss segment
            AH_ENTRY,       ;;; entry point
            AH_TEXT_START,  ;;; base address of text segment
            AH_DATA_START;  ;;; base address of data segment
#_IF DEF MIPS
    long    AH_BSS_START,   ;;; base address of bss segment
            AH_GPRMASK,     ;;; general purpose register mask
            AH_CPRMASK[4],  ;;; co-processor register masks
            AH_GP_VALUE;    ;;; gp value
#_ENDIF
};

    /* section header */
struct A_OUT_SECT_HDR
{
    byte    ASC_NAME[8];    ;;; section name
    long    ASC_PADDR,      ;;; physical address
            ASC_VADDR,      ;;; virtual address
            ASC_SIZE,       ;;; section size
            ASC_SCNPTR,     ;;; file pointer to raw data
            ASC_RELPTR,     ;;; file pointer to relocation
            ASC_LNNOPTR;    ;;; file pointer to line numbers
    short   ASC_NRELOC,     ;;; number of relocation entries
            ASC_NLNNO;      ;;; number of line number entries
    long    ASC_FLAGS;      ;;; flags
};

#_IF DEF MIPS

    /* Symbolic Header */
struct SYM_HDR
{
    short   SH_MAGIC,       ;;; magic number
            SH_VSTAMP;      ;;; version stamp
    long    SH_DUMMY1[15],
            SH_ISSEXTMAX,   ;;; max index into external strings
            SH_SSEXTOFFS,   ;;; offset to start of external strings
            SH_DUMMY2[4],
            SH_IEXTMAX,     ;;; max index into external symbols
            SH_EXTOFFS;     ;;; offset to start of external symbol entries
};

    /* (external) symbol table entry */
struct SYMBOL
{
    short   SYM_RESERVED1,  ;;; reserved for future use
            SYM_IFD;        ;;; where the iss and index fields point into
    long    SYM_ISS,        ;;; index into string space of names
            SYM_VALUE;      ;;; symbol value
    6       SYM_ST;         ;;; symbol type
    5       SYM_SC;         ;;; storage class
    1       SYM_RESERVED2;  ;;; reserved for future use
    20      SYM_INDEX;      ;;; index into sym/aux table
};

lconstant
    _SYM_BYTES      = ##(b)[_1|struct SYMBOL],
    _UNIX_HDR_BYTES = ##(b)[_1|struct A_OUT_UNIX_HDR],
    _SECT_HDR_BYTES = ##(b)[_1|struct A_OUT_SECT_HDR],
    _SYM_HDR_BYTES  = ##(b)[_1|struct SYM_HDR],

    _OMAGIC         = _8:407,
    _SC_ABS         = _5,
    _IFD_NIL        = _-1,
    _INDEX_NIL      = _16:FFFFF,
;

#_ELSE

    /* symbol table entry */
struct SYMBOL
{
    {
      byte  SYM_NAME[8];    ;;; symbol name if not in string table
    | long  SYM_ZEROES,     ;;; zero if in string table
            SYM_OFFSET;     ;;; offset into string table
    }
    long    SYM_VALUE;      ;;; symbol value
    short   SYM_SCNUM,      ;;; section number
            SYM_TYPE;       ;;; type and derived class
    byte    SYM_SCLASS,     ;;; storage class
            SYM_NUMAUX,     ;;; number of auxilary entries
            SYMBOL_END[0];  ;;; for getting correct size of this struct in
                            ;;; bytes -- ##(b)[_1|struct SYMBOL] rounds to an
                            ;;; exact number of longs = 20 bytes not 18
};

lconstant
    _SYM_BYTES      = ##SYMBOL_END,
    _UNIX_HDR_BYTES = ##(b)[_1|struct A_OUT_UNIX_HDR],
    _SECT_HDR_BYTES = ##(b)[_1|struct A_OUT_SECT_HDR],

    _AHF_RELFLG     = _8:00001,
    _AHF_LNNO       = _8:00004,
    _AHF_LSYMS      = _8:00010,
    _N_ABS          = _-1,
    _C_EXT          = _2,
    _STYP_TEXT      = _16:20,
    _STYP_DATA      = _16:40,
    _STYP_BSS       = _16:80,
;


normal_compile

define macro _N_SYMOFF x;
    lvars x;
    [( ^x!AHF_SYMPTR )].dl
enddefine;

define macro _N_STROFF x;
    lvars x;
    [( _N_SYMOFF ^x _add ^x!AHF_NSYMS _mult _SYM_BYTES )].dl
enddefine;

end_normal_compile;

#_ENDIF


;;; --- HP-UX ------------------------------------------------------------

#_ELSEIF DEF HPUX       ;;; must come before BERKELEY

    /* file header */
struct A_OUT_HDR
  { short   AH_SYSID,       ;;; system id
            AH_MAGIC,       ;;; magic number
            AH_STAMP,       ;;; version stamp
            AH_HIGHWATER;   ;;; shlib highwater mark
    long    AH_MISCINFO,    ;;; miscellaneous info
            AH_TEXT,        ;;; size of text segment
            AH_DATA,        ;;; size of data segment
            AH_BSS,         ;;; size of bss segment
            AH_TRSIZE,      ;;; size of text relocation
            AH_DRSIZE,      ;;; size of data relocation
            AH_PASINT,      ;;; size of pascal interface text
            AH_SYMS,        ;;; size of symbol table
            AH_SPARED,
            AH_ENTRY,       ;;; entry point
            AH_SPARES,
            AH_SUPSYM,      ;;; supplementary symtab size
            AH_DRELOCS,     ;;; nonpic relocations
            AH_EXTENSION;   ;;; file offset of extension
  };

struct SYMBOL
  { long    SYM_VALUE;
    byte    SYM_TYPE,       ;;; symbol type
            SYM_LENGTH;     ;;; symbol length
    short   SYM_ALMOD,
            SYM_UNUSED;
    byte    SYM_CHARS[];
  };

lconstant
    _SYM_BYTES      = _256, ;;; max length of SYMBOL

    _DEMAND_MAGIC   = _8:413,
    _RELOC_MAGIC    = _8:406,
    _EXEC_PAGESIZE  = _4096,
    _N_TYPE         = _8:37,
    _N_TEXT         = _8:2,
    _N_DATA         = _8:3,
    _N_BSS          = _8:4,
    _N_ABS          = _8:1,

    ;

normal_compile

define macro _N_TXTOFF x;
    lvars x;
    [(if ^x!AH_MAGIC == _DEMAND_MAGIC then Exec_align(_HDR_BYTES)
        else _HDR_BYTES
        endif
    )].dl
enddefine;

define macro _N_SYMOFF x;
    lvars x;
    [(  if ^x!AH_MAGIC == _DEMAND_MAGIC then
            Exec_align(_HDR_BYTES) _add
            Exec_align(^x!AH_TEXT) _add
            Exec_align(^x!AH_DATA) _add
            ^x!AH_PASINT
        else
            _HDR_BYTES _add
            ^x!AH_TEXT _add
            ^x!AH_DATA _add
            ^x!AH_PASINT
        endif
    )].dl
enddefine;

end_normal_compile;


;;; --- BERKELEY ----------------------------------------------------------

#_ELSEIF DEF BERKELEY

struct A_OUT_HDR
  {
#_IF DEFV SUNOS >= 4.0
    1       AH_DYNAMIC;     ;;; has a __DYNAMIC
    7       AH_TOOLVERSION; ;;; version of toolset used to create the file
    byte    AH_MACHTYPE;    ;;; machine type
    short   AH_MAGIC;       ;;; magic number
#_ELSEIF DEFV SUNOS >= 3.0
    short   AH_MACHTYPE,    ;;; machine type
            AH_MAGIC;       ;;; magic number
#_ELSE
    long    AH_MAGIC;       ;;; magic number
#_ENDIF
    long    AH_TEXT,        ;;; size of text segment
            AH_DATA,        ;;; size of data segment
            AH_BSS,         ;;; size of bss segment
            AH_SYMS,        ;;; size of symbol table
            AH_ENTRY,       ;;; entry point
            AH_TRSIZE,      ;;; size of text relocation
            AH_DRSIZE;      ;;; size of data relocation
  };

struct SYMBOL
  { long    SYM_STRX;       ;;; index into string table
    byte    SYM_TYPE,       ;;; type flags
            SYM_OTHER;
    short   SYM_DESC;
    long    SYM_VALUE;      ;;; symbol value
  };

lconstant
    _NMAGIC     = _8:410,
    _ZMAGIC     = _8:413,
    ;

#_IF DEF LINUX
lconstant
    _MAGIC_MASK = _16:FFFF,
    _QMAGIC     = _8:314;
#_ENDIF

lconstant
    _SYM_BYTES  = ##(b)[_1|struct SYMBOL],

    _N_UNDF     = _16:0,        ;;; undefined
    _N_ABS      = _16:2,        ;;; absolute
    _N_TEXT     = _16:4,        ;;; text
    _N_DATA     = _16:6,        ;;; data
    _N_BSS      = _16:8,        ;;; bss
    _N_COMM     = _16:12,       ;;; common (internal to ld)
    _N_FN       = _16:1F,       ;;; file name symbol

    _N_EXT      = _16:01,       ;;; external bit
    _N_TYPE     = _16:1E,       ;;; mask for type bits
    ;


normal_compile

define macro _N_TXTOFF x;
    lvars x;
#_IF DEFV SUNOS >= 3.0
    ;;; cm 14-2-86
    ;;; different ZMAGIC text section - starts at 0 and includes header
    [(  if ^x!AH_MAGIC == _ZMAGIC then _0
        else _HDR_BYTES
        endif
    )].dl
#_ELSEIF DEF LINUX
    [(  if (^x!AH_MAGIC _bimask _MAGIC_MASK) == _ZMAGIC then _1024
        elseif (^x!AH_MAGIC _bimask _MAGIC_MASK) == _QMAGIC then _0
        else _HDR_BYTES
        endif
    )].dl
#_ELSE
    [(  if ^x!AH_MAGIC == _ZMAGIC then ##(b)[_1|vpage]
        else _HDR_BYTES
        endif
    )].dl
#_ENDIF
enddefine;

define macro _N_SYMOFF x;
    lvars x;
    [( _N_TXTOFF ^x _add ^x!AH_TEXT _add ^x!AH_DATA _add ^x!AH_TRSIZE
        _add ^x!AH_DRSIZE
    )].dl
enddefine;

define macro _N_STROFF x;
    lvars x;
    [( _N_SYMOFF ^x _add ^x!AH_SYMS )].dl
enddefine;

end_normal_compile;


#_ENDIF


lconstant
    ;;; size of structs in bytes
    _HDR_BYTES  = ##(b)[_1|struct A_OUT_HDR],

    ;;; working buffers
    _header_buf = (writeable inits(_pint(_HDR_BYTES)))@V_WORDS,
    _sym_buf    = (writeable inits(_pint(_SYM_BYTES)))@V_WORDS,
    ;


#_IF DEF COFF
lconstant
    _sect_header_buf = (writeable inits(_pint(_SECT_HDR_BYTES)))@V_WORDS,
    _unix_header_buf = (writeable inits(_pint(_UNIX_HDR_BYTES)))@V_WORDS,
    ;
#_ENDIF

#_IF DEF MIPS
lconstant
    _sym_header_buf  = (writeable inits(_pint(_SYM_HDR_BYTES)))@V_WORDS,
    ;
#_ENDIF

#_ENDIF ;;; not(DEF SHARED_LIBRARIES)


;;;-------------------------------------------------------------------------

    ;;; This (exported) procedure takes the POPLOG image and produces
    ;;; a new object file in which all symbols commencing with one of
    ;;; the list del_prefixes have been removed. This is used to get
    ;;; rid of POP's symbols which are not needed for external linking

#_IF DEF SHARED_LIBRARIES

    ;;; Dummy version which creates empty output file
define Extern_make_base(infile, outfile, del_prefixes);
    lvars infile, outfile, del_prefixes;
    sysclose(syscreate(outfile, 1, true));
enddefine;

#_ELSEIF DEF MIPS   ;;; must come before COFF

define Extern_make_base(infile, outfile, del_prefixes);
    lvars   infile, outfile, del_prefixes, prefix, tmp,
            _old_index, _new_index, _lim, _len, _hb, _sym, _old_strtab,
            _new_strtab, _stsize, _symoffs, _nsymbols, _ndeleted;
    dlocal  _disable = _DISABLE_INTERRUPTS;

    ;;; set popmemlim to 8MB - should be large enough for most symbol
    ;;; tables.
    dlocal popmemlim = 2e6;

    ;;; if 3rd arg is true, means delete all the pop symbols
    ;;; (POP_SYMBOL_PREFIXES is defined in asmout.p)
    if del_prefixes == true then
#_IF DEF POP_SYMBOL_PREFIXES
        POP_SYMBOL_PREFIXES -> del_prefixes;
#_ELSE
        [] -> del_prefixes;
#_ENDIF
    endif;
    del_prefixes <> ['_end\^@' 'end\^@' 'etext\^@' 'edata\^@'] -> del_prefixes;

    sysopen(infile, 0, true, `N`) -> infile;
    syscreate(outfile, 1, true) -> outfile;

    ;;; read input file header
    _header_buf -> _hb;
    Readb(infile, _hb, _HDR_BYTES);
    ;;; save offset to the symbolic header
    _hb!AHF_SYMPTR -> _symoffs;
    ;;; output file has no sections and zero flags
    _0 ->> _hb!AHF_NSCNS -> _hb!AHF_FLAGS;
    ;;; symbolic header follows file header and optional header
    _HDR_BYTES _add _UNIX_HDR_BYTES -> _hb!AHF_SYMPTR;
    ;;; write file header
    Writeb(outfile, _hb, _HDR_BYTES);

    ;;; read unix header
    _unix_header_buf -> _hb;
    Readb(infile, _hb, _UNIX_HDR_BYTES);
    ;;; zero header, saving old version stamp and gp value on stack
    _hb!AH_VSTAMP, _hb!AH_GP_VALUE;
    _bfill(_0, _UNIX_HDR_BYTES, _hb@(w->b));
    ;;; restore version stamp and gp value to header
    () -> _hb!AH_GP_VALUE, () -> _hb!AH_VSTAMP;
    ;;; set magic number to be impure format
    _OMAGIC -> _hb!AH_MAGIC;
    ;;; write unix header
    ;;; (this contains no useful information, but the loader complains if it
    ;;; is missing)
    Writeb(outfile, _hb, _UNIX_HDR_BYTES);

    ;;; read symbolic header
    _sym_header_buf -> _hb;
    sysseek(infile, _pint(_symoffs), 0);
    Readb(infile, _hb, _SYM_HDR_BYTES);
    ;;; save original number of symbols
    _hb!SH_IEXTMAX -> _nsymbols;

    ;;; create strings to hold old and new symbol string tables
    _hb!SH_ISSEXTMAX -> _stsize;
    inits(_pint(_stsize) fi_+ 16) -> tmp;
    copy(tmp)@V_BYTES[_0] -> _old_strtab;
    tmp@V_BYTES[_0] -> _new_strtab;

    ;;; read in whole string table
    sysseek(infile, _pint(_hb!SH_SSEXTOFFS), 0);
    Readb(infile, _old_strtab@(b->w), _stsize);
    ;;; seek to start of symbol table
    sysseek(infile, _pint(_hb!SH_EXTOFFS), 0);

    ;;; copy strings, discarding any beginning with del_prefixes
    _old_strtab -> _old_index;
    _new_strtab -> _new_index;
    _old_strtab@(b)[_stsize] -> _lim;
    _0 -> _ndeleted;
    while _old_index <@(b) _lim do
        @@(b){_locc(_old_index, @@(b)[_256], _0)}++ -> _len;
        for prefix in del_prefixes do
            if _bcmp(@@(b)[prefix!V_LENGTH], _old_index, prefix@V_BYTES[_0])
            then
                ;;; string (and associated symbol) can be removed
                _0 -> _old_index!(b);
                _ndeleted _add _1 -> _ndeleted;
                quitloop;
            endif;
        endfor;
        ;;; copy string into new string table if necessary
        if _nonzero(_old_index!(b)) then
            _bmove(_len, _old_index, _new_index) -> _new_index;
        endif;
        _old_index@(b)[_len] -> _old_index;
    endwhile;

    ;;; update symbolic header:
    ;;; size of new string table
    @@(w){_new_index, _new_strtab | b.r} -> _hb!SH_ISSEXTMAX;
    ;;; string table comes after file header, unix header and symbolic header
    _HDR_BYTES _add _UNIX_HDR_BYTES _add _SYM_HDR_BYTES -> _hb!SH_SSEXTOFFS;
    ;;; number of symbols
    _nsymbols _sub _ndeleted -> _hb!SH_IEXTMAX;
    ;;; symbol table comes after string table
    _hb!SH_SSEXTOFFS _add _hb!SH_ISSEXTMAX -> _hb!SH_EXTOFFS;
    ;;; zero dummy fields
    _fill(_0, ##(b)[_15|w], _hb@SH_DUMMY1);
    _fill(_0, ##(b)[_4|w], _hb@SH_DUMMY2);
    ;;; write the symbolic header
    Writeb(outfile, _hb, _SYM_HDR_BYTES);

    ;;; write the new string table
    Writeb(outfile, _new_strtab, _hb!SH_ISSEXTMAX);

    ;;; copy those symbols whose strings we previously copied
    _sym_buf -> _sym;
    _new_strtab -> _new_index;
    fast_repeat _pint(_nsymbols) times
        Readb(infile, _sym, _SYM_BYTES);
        if _nonzero(_old_strtab!(b)[_sym!SYM_ISS]) then
            ;;; update symbol:
            ;;; no file descriptor
            _IFD_NIL -> _sym!SYM_IFD;
            ;;; no auxiliary information
            _INDEX_NIL -> _sym!SYM_INDEX;
            ;;; make symbol absolute
            _SC_ABS -> _sym!SYM_SC;
            ;;; offset into new string table
            @@(b){_new_index, _new_strtab} -> _sym!SYM_ISS;
            ;;; write the symbol
            Writeb(outfile, _sym, _SYM_BYTES);
            ;;; step string index onto next string
            _new_index@(b){_locc(_new_index, @@(b)[_256], _0)}++
                -> _new_index;
        endif;
    endrepeat;

    sysclose(infile);
    sysclose(outfile);
enddefine;

#_ELSEIF DEF COFF

define Extern_make_base(infile, outfile, del_prefixes);
    lvars infile, outfile, del_prefixes, prefix, tmp, _hb,
        _sym, _sb, _old_strtab, _new_strtab, _new_index, _symtabsize,
        _newnumsyms, _str, _n, _newstrtabsize
        ;
    dlvars _stsize;
    dlocal _disable = _DISABLE_INTERRUPTS;

    ;;; set popmemlim to 8MB - should be large enough for most symbol
    ;;; tables.
    dlocal popmemlim = 2e6;

    ;;; if 3rd arg is true, means delete all the pop symbols
    ;;; (POP_SYMBOL_PREFIXES is defined in asmout.p)
    if del_prefixes == true then
#_IF DEF POP_SYMBOL_PREFIXES
        POP_SYMBOL_PREFIXES -> del_prefixes
#_ELSE
        [] -> del_prefixes
#_ENDIF
    endif;
    del_prefixes <> ['_end\^@' '_etext\^@' '_edata\^@'] -> del_prefixes;

    sysopen(infile, 0, true, `N`) -> infile;
    syscreate(outfile, 1, true) -> outfile;

    ;;; read input file header
    _header_buf -> _hb;
    Readb(infile, _hb, _HDR_BYTES);
    ;;; write initial header on output (will be re-written at end)
    Writeb(outfile, _hb, _HDR_BYTES);

    ;;; copy across the bss section header, so there's
    ;;; at least one section header on the file
    sysseek(infile, _pint(_hb!AHF_OPTHDR), 1);  ;;; skip unix header
    _sect_header_buf -> _sb;
    repeat _pint(_hb!AHF_NSCNS) times
        Readb(infile, _sb, _SECT_HDR_BYTES);
        if _sb!ASC_FLAGS _bitst _STYP_BSS then
            Writeb(outfile, _sb, _SECT_HDR_BYTES);
        endif;
    endrepeat;
    ;;; update file header values
    _1 -> _hb!AHF_NSCNS;        ;;; 1 section header
    _0 ->> _hb!AHF_OPTHDR -> _hb!AHF_FLAGS; ;;; no opt header and no flags

    ;;; read in whole string table
    sysseek(infile, _pint(_N_STROFF _hb), 0);   ;;; string table start
    Readb(infile, ident _stsize, ##(b)[_1|w]);  ;;; get str table size

    ;;; create strings to hold old and new symbol string tables
    inits(_pint(_stsize) fi_+ 16) -> tmp;
    copy(tmp)@V_BYTES[_0] -> _old_strtab;
    tmp@V_BYTES[_0] -> _new_strtab;
    sysseek(infile, _pint(##(b)[_-1|w]), 1);    ;;; back to strtab start
    Readb(infile, _old_strtab@(b->w), _stsize);     ;;; read in string table
    _new_strtab@(b)[_1|w] -> _new_index;        ;;; start index in new tab

    ;;; get to symbol table on infile
    sysseek(infile, _pint(_N_SYMOFF _hb), 0);
    _hb!AHF_NSYMS _mult _SYM_BYTES -> _symtabsize;  ;;; symbol table size
    _0 -> _newnumsyms;                      ;;; 0 for new number of symbols
    _sym_buf -> _sym;

    ;;; current output position is start of symbol table
    _int(sysseek(outfile, 0, 1, true)) -> _hb!AHF_SYMPTR;

    ;;; read symbols and discard any beginning with del_prefixes
    until _zero(_symtabsize) do
        Readb(infile, _sym, _SYM_BYTES);    ;;; read next symbol entry
        _symtabsize _sub _SYM_BYTES -> _symtabsize;
        ;;; if has auxilary entries, discard it
        if _nonzero(_sym!SYM_NUMAUX) then
            for _sym!SYM_NUMAUX -> _n step _n _sub _1 -> _n
            till _zero(_n) do
                Readb(infile, _sym, _SYM_BYTES);
                _symtabsize _sub _SYM_BYTES -> _symtabsize;
            endfor;
            nextloop
        endif;
        ;;; also discard if not an external symbol
        unless _sym!SYM_SCLASS == _C_EXT then
            nextloop
        endunless;
        ;;; name is either in SYM_NAME or in string table
        if _zero(_sym!SYM_ZEROES) then
            ;;; in string table
            _old_strtab@(b)[_sym!SYM_OFFSET] -> _str;
        else
            ;;; in SYM_NAME - make null-terminated temp copy in new string table
            _new_index -> _str;
            _0 -> _bmove(@@(b)[_8], _sym@SYM_NAME[_0], _str)!(b);
        endif;

        for prefix in del_prefixes do
            if _bcmp(@@(b)[prefix!V_LENGTH], _str, prefix@V_BYTES[_0]) then
                ;;; symbol can be removed
                nextloop(2)
            endif;
        endfor;

        ;;; copy string into new string table if necessary
        if _zero(_sym!SYM_ZEROES) then
            ##(b){_new_index, _new_strtab} -> _sym!SYM_OFFSET;
            _bmove(@@(b){_locc(_str, @@(b)[_256], _0)}++, _str, _new_index)
                                -> _new_index;
        endif;

        ;;; make all text, data and bss symbols absolute
        if _sym!SYM_SCNUM _sgr _0 then
            _N_ABS -> _sym!SYM_SCNUM        ;;; == -1
        endif;

        ;;; write symbol to output file
        Writeb(outfile, _sym_buf, _SYM_BYTES);
        _newnumsyms _add _1 -> _newnumsyms;
    enduntil;

    ;;; write output string table
    ##(b){_new_index, _new_strtab} -> _newstrtabsize;   ;;; len of string table
    tmp@V_WORDS[_0] -> _new_strtab;
    _newstrtabsize -> _new_strtab!(w);
    Writeb(outfile, _new_strtab, _newstrtabsize);


    ;;; write updated header to output
    sysseek(outfile, 0, 0);
    _newnumsyms -> _hb!AHF_NSYMS;               ;;; new no of symbols
    Writeb(outfile, _hb, _HDR_BYTES);

    sysclose(infile);
    sysclose(outfile);
enddefine;


#_ELSEIF DEF HPUX       ;;; must come before BERKELEY

define lconstant Exec_align(_nbytes);
    lvars _nbytes;
    ##(b){@@(vpage)[_nbytes|b.r] | vpage}
enddefine;

define Extern_make_base(infile, outfile, del_prefixes);
    lvars infile, outfile, del_prefixes, prefix, tmp, _hb, _type,
        _sym, _sb, _old_strtab, _new_strtab, _new_index, _symsize, _symtabsize,
        _newsymtabsize, _n, _newstrtabsize
        ;
    dlocal _disable = _DISABLE_INTERRUPTS;

    ;;; set popmemlim to 8MB - should be large enough for most symbol
    ;;; tables.
    dlocal popmemlim = 2e6;

    ;;; if 3rd arg is true, means delete all the pop symbols
    ;;; (POP_SYMBOL_PREFIXES is defined in asmout.p)
    if del_prefixes == true then
#_IF DEF POP_SYMBOL_PREFIXES
        POP_SYMBOL_PREFIXES -> del_prefixes
#_ELSE
        [] -> del_prefixes
#_ENDIF
    endif;

    ;;; bcmp takes two byte regions of same length - since we do not bother
    ;;; to insert nulls at the end of our symbols we should not have nulls
    ;;; at the end of these
    del_prefixes <> ['_end' '_etext' '_edata' '__cleanup'
#_IF DEFV HPUX >= 7.0
                     '__end' '__etext' '__edata'
#_ENDIF
                    ] -> del_prefixes;

    sysopen(infile, 0, true, `N`) -> infile;
    syscreate(outfile, 1, true) -> outfile;

    ;;; read input file header
    _header_buf -> _hb;
    Readb(infile, _hb, _HDR_BYTES);
    ;;; write initial header on output (will be re-written at end)
    Writeb(outfile, _hb, _HDR_BYTES);

    ;;; get to symbol table on infile
    sysseek(infile, _pint(_N_SYMOFF _hb), 0);

    _hb!AH_SYMS -> _symtabsize;     ;;; symbol table size
    _0 -> _newsymtabsize;           ;;; 0 for new number of symbols
    _sym_buf -> _sym;

    ;;; read symbols and discard any beginning with del_prefixes
    until _zero(_symtabsize) do
        Readb(infile, _sym, ##SYM_CHARS);       ;;; read next symbol header
        Readb(infile, _sym@SYM_CHARS, _sym!SYM_LENGTH); ;;; read sym chars
        _sym!SYM_LENGTH _add ##SYM_CHARS -> _symsize;   ;;; total size of sym
        _symtabsize _sub _symsize -> _symtabsize;

        for prefix in del_prefixes do
            if _bcmp(@@(b)[prefix!V_LENGTH], _sym@SYM_CHARS, prefix@V_BYTES)
            then
                ;;; symbol can be removed
                nextloop(2)
            endif
        endfor;

        ;;; make all text, data and bss symbols absolute
        _sym!SYM_TYPE _bimask _N_TYPE -> _type;
        if _type == _N_TEXT or _type == _N_DATA or _type == _N_BSS then
            _sym!SYM_TYPE _biclear _type _biset _N_ABS -> _sym!SYM_TYPE
        endif;

        ;;; write symbol to output file
        Writeb(outfile, _sym, _symsize);
        _newsymtabsize _add _symsize -> _newsymtabsize
    enduntil;

    ;;; write updated header to output
    sysseek(outfile, 0, 0);
    _newsymtabsize -> _hb!AH_SYMS;      ;;; new symbol table size
    _RELOC_MAGIC -> _hb!AH_MAGIC;       ;;; if this not here
                                        ;;; bad magic number from ld
    ;;; only symbol table in this file so zero text,data etc
    _0 ->> _hb!AH_TEXT ->> _hb!AH_DATA ->> _hb!AH_BSS
            ->> _hb!AH_TRSIZE ->> _hb!AH_DRSIZE ->> _hb!AH_PASINT
            ->> _hb!AH_SUPSYM ->> _hb!AH_DRELOCS -> _hb!AH_EXTENSION;
    Writeb(outfile, _hb, _HDR_BYTES);

    sysclose(infile);
    sysclose(outfile);
enddefine;


#_ELSEIF DEF BERKELEY

define Extern_make_base(infile, outfile, del_prefixes);
    lvars infile, outfile, del_prefixes, prefix, tmp, _hb, _type,
        _old_strtab, _new_strtab, _new_index, _symtabsize,
        _newsymtabsize, _str, _newstrtabsize
        ;
    dlvars _stsize;
    dlocal _disable = _DISABLE_INTERRUPTS;

    ;;; set popmemlim to 8MB - should be large enough for most symbol
    ;;; tables.
    dlocal popmemlim = 2e6;

    ;;; if 3rd arg is true, means delete all the pop symbols
    ;;; (POP_SYMBOL_PREFIXES is defined in asmout.p)
    if del_prefixes == true then
#_IF DEF POP_SYMBOL_PREFIXES
        POP_SYMBOL_PREFIXES -> del_prefixes
#_ELSE
        [] -> del_prefixes
#_ENDIF
    endif;
    del_prefixes <> ['_end\^@' '_etext\^@' '_edata\^@'] -> del_prefixes;

    sysopen(infile, 0, true, `N`) -> infile;
    syscreate(outfile, 1, true) -> outfile;

    ;;; read input header
    _header_buf -> _hb;
    Readb(infile, _hb, _HDR_BYTES);
    ;;; read in whole string table
    sysseek(infile, _pint(_N_STROFF _hb), 0);   ;;; string table start
    Readb(infile, ident _stsize, ##(b)[_1|w]);  ;;; get str table size

    ;;; create strings to hold old and new symbol string tables
    inits(_pint(_stsize)) -> tmp;
    copy(tmp)@V_BYTES[_0] -> _old_strtab;
    tmp@V_BYTES[_0] -> _new_strtab;
    sysseek(infile, _pint(##(b)[_-1|w]), 1);    ;;; back to strtab start
    Readb(infile, _old_strtab@(b->w), _stsize); ;;; read in string table
    _new_strtab@(b)[_1|w] -> _new_index;        ;;; start index in new tab

    ;;; get to symbol table on infile
    sysseek(infile, _pint(_N_SYMOFF _hb), 0);
    ;;; write initial header on output (will be re-written at end)
    Writeb(outfile, _hb, _HDR_BYTES);
    _hb!AH_SYMS -> _symtabsize;             ;;; symbol table size
    _0 -> _newsymtabsize;                   ;;; 0 for new size

    ;;; read symbols and discard any beginning with del_prefixes
    until _zero(_symtabsize) do
        Readb(infile, _sym_buf, _SYM_BYTES);    ;;; read next symbol entry
        _symtabsize _sub _SYM_BYTES -> _symtabsize;

        ;;; discard local symbols
        nextunless(_sym_buf!SYM_TYPE _bitst _N_EXT);

        _old_strtab@(b)[_sym_buf!SYM_STRX] -> _str;
        for prefix in del_prefixes do
            if _bcmp(@@(b)[prefix!V_LENGTH], _str, prefix@V_BYTES[_0]) then
                ;;; symbol can be removed
                nextloop(2)
            endif
        endfor;

        ;;; make all text, data and bss symbols absolute
        _sym_buf!SYM_TYPE _bimask _N_TYPE -> _type;
        if _type == _N_TEXT or _type == _N_DATA or _type == _N_BSS then
            _sym_buf!SYM_TYPE _biclear _type _biset _N_ABS -> _sym_buf!SYM_TYPE;
        endif;

        ;;; copy symbol string into new string table
        ##(b){_new_index, _new_strtab} -> _sym_buf!SYM_STRX;
        _bmove(@@(b){_locc(_str, @@(b)[_256], _0)}++, _str, _new_index)
                                -> _new_index;

        ;;; write symbol to output file
        Writeb(outfile, _sym_buf, _SYM_BYTES);
        _newsymtabsize _add _SYM_BYTES -> _newsymtabsize;
    enduntil;

    ;;; write output string table
    ##(b){_new_index, _new_strtab} -> _newstrtabsize;   ;;; len of string table
    tmp@V_WORDS[_0] -> _new_strtab;
    _newstrtabsize -> _new_strtab!(w);
    Writeb(outfile, _new_strtab, _newstrtabsize);

    ;;; write updated header to output
    sysseek(outfile, 0, 0);
    ;;; write header with no text, data etc
    _newsymtabsize -> _hb!AH_SYMS;          ;;; new symbol table size
    _NMAGIC -> _hb!AH_MAGIC;
    _0 ->> _hb!AH_TEXT ->> _hb!AH_DATA ->> _hb!AH_BSS
            ->> _hb!AH_TRSIZE -> _hb!AH_DRSIZE;
    Writeb(outfile, _hb, _HDR_BYTES);

    sysclose(infile);
    sysclose(outfile);
enddefine;


#_ELSE

define Extern_make_base();
    mishap(0, 'EXTERNAL PROCEDURES NOT IMPLEMENTED')
enddefine;


#_ENDIF


    ;;; copy a symbol table object file onto the end of a save file,
define Save_symtab(symfile, save_dev);
    lvars save_dev, symfile;
#_IF DEF SHARED_LIBRARIES
    mishap(0, 'SYSTEM ERROR IN Save_symtab');
#_ELSE
    lvars buf _n;
    sysopen(symfile, 0, true, `N`) -> symfile;
    ;;; do copy
    inits(512) -> buf;
    until (sysread(symfile, buf, 512) ->> _n) == 0 do
        syswrite(save_dev, buf, _n)
    enduntil;
    sysclose(symfile);
#_ENDIF
enddefine;

    ;;; do the reverse, i.e. copy the object file/symbol table on
    ;;; the end of a save file onto a separate file
define Restore_symtab(symfile, save_dev);
    lvars symfile, save_dev;
#_IF DEF SHARED_LIBRARIES
    mishap(0, 'SYSTEM ERROR IN Restore_symtab');
#_ELSE
    lvars buf, _n;
    syscreate(symfile, 1, true) -> symfile;
    inits(512) -> buf;
    until (sysread(save_dev, buf, 512) ->> _n) == 0 do
        syswrite(symfile, buf, _n)
    enduntil;
    sysclose(symfile);
#_ENDIF
enddefine;

define Get_link_base() -> link_file;
    lvars link_file;
#_IF DEF SHARED_LIBRARIES
    false -> link_file;
#_ELSE
    unless systranslate('popexlinkbase') ->> link_file then
        mishap(0, 'EXTERNAL_LOAD: ENVIRON VAR popexlinkbase IS UNDEFINED')
    endunless
#_ENDIF
enddefine;

define Temp_name();
    (systranslate('TMPDIR') or '/tmp') dir_>< ('PXT' sys_>< poppid sys_>< '.')
enddefine;

define lconstant Process_objfiles(objfiles, do_sfok);
    lvars name, objfiles, do_sfok;
    [%  for name in objfiles do
            if isword(name) then
                name!W_STRING -> name;
                unless datalength(name)/==0 and subscrs(1,name)==`-` then
                    name <> '.o' -> name
                endunless
            endif;
            if do_sfok then sysfileok(name) else name endif
        endfor
    %]
enddefine;

define lconstant Convert_symbols(symbol_list, fprintf);
    lvars spec, symname, lang, symbol_list, fprintf;
    fast_for spec in symbol_list do
        ;;; spec is a vector where spec(1) is the symbol name as a word or
        ;;; string, and spec(2) is a language name string (or a pair
        ;;; conspair(lang-name, type), where type is used only by Popc).
        ;;; If symbol name is a string, take it as is, otherwise do standard
        ;;; O/S conversion on it, taking language name into account.
        Name_translate(spec(1),
                        ispair(spec(2)->>lang) and fast_front(lang) or lang,
                        false) ->> symname -> spec(1);
        if fprintf then
            ;;; output to assembler file
            fprintf(symname, ASM_WORD_STR, '%p%p\n')
        endif;
    endfor
enddefine;

#_IF DEF SHARED_LIBRARIES

;;; == USING SHARED LIBRARIES =============================================


lconstant
    ERROR = 'Error',
    WARNING = 'Warning',
;

define lconstant report(kind, msg, args);
    lvars kind, msg, args;
    dlocal cucharout = cucharerr;
    printf(';;; %S - %s\n', [^kind ^msg ^^args]);
enddefine;

define:inline lconstant DEBUG_PRINTF(s, l);
#_IF DEF DEBUG
    report('Debug', s, l);
#_ENDIF
enddefine;

lvars
    link_stack = [],
        ;;; stack of items describing each external link load
;

lconstant macro (
    ;;; vector subscripts for items in the link stack
    LINK_HANDLES    = 1,
    LINK_ARGS       = 2,
    LINK_FILE       = 2,
    LINK_OBJFILES   = 3,
    LINK_SYMNAMES   = 4,
    LINK_SYMVALUES  = 5,
    ;;; length of link item
    LINK_LENGTH     = 5,
);

    /* return a list of directories to search for shared libraries
    */
define lconstant shlib_searchlist() -> (extras, searchlist);
    lvars (extras, searchlist) = ([], []);

        ;;; break up a colon-separated list of strings
    define lconstant path_searchlist(path);
        lvars path;
        lvars len = datalength(path);
        lvars i = 0, j;
        [%  while i fi_<= len do
                i fi_+ 1 -> j;
                unless locchar(`:`, j, path) ->> i then
                    len fi_+ 1 -> i;
                endunless;
                substring(j, i fi_- j, path);
            endwhile;
        %];
    enddefine;

#_IF DEF HPUX
    lconstant default_searchlist = ['/lib/' '/usr/lib/'];
    default_searchlist -> searchlist;
    lvars path;
    if systranslate('LPATH') ->> path then
        ;;; environment variable LPATH overrides the default
        path_searchlist(path) -> searchlist;
    endif;
    if systranslate('SHLIB_PATH') ->> path then
        ;;; extra directories for dynamic linking
        path_searchlist(path) nc_<> searchlist -> searchlist;
    endif;
#_ELSEIF DEF AIX
    lconstant default_searchlist = ['/usr/lib/' '/lib'];
    default_searchlist -> searchlist;
    lvars path;
    if systranslate('LIBPATH') ->> path then
        ;;; LIBPATH adds to the defaults
        path_searchlist(path) nc_<> searchlist -> searchlist;
    endif;
#_ELSE
  #_IF DEF OSF1
    lconstant default_searchlist = ['/usr/shlib/'];
  #_ELSE
    lconstant default_searchlist = [''];
  #_ENDIF
    default_searchlist -> searchlist;
    ;;; environment variable LD_LIBRARY_PATH adds to the defaults
    lvars path, i;
    if systranslate('LD_LIBRARY_PATH') ->> path then
        if locchar(`;`, 1, path) ->> i then
            ;;; anything before the `;` should be searched first
            path_searchlist(substring(1, i-1, path)) -> extras;
            substring(i+1, datalength(path)-i, path) -> path;
        endif;
        path_searchlist(path) nc_<> searchlist -> searchlist;
    endif;
#_ENDIF
enddefine;

    /*  convert linker arguments to a list of shared libraries to be
        loaded and add these to the link_item; the dummy argument is
        for compatibility with the other interface defined below
    */
define lconstant process_link_args(args, dummy, link_item);
    lvars args, dummy, link_item, modname;

    define lconstant exists = sys_file_stat(%{}%) enddefine;

    define lconstant find_library(nam, searchlist) -> full_name;
        lvars nam, searchlist, full_name;
        lvars dir, name = false, aname = false;
        for dir in searchlist do
            unless name then nam <> SHLIB_EXTN -> name endunless;
            if (dir = '') then return(name ->> full_name) ; endif;
            returnif(exists(dir dir_>< name ->> full_name));
#_IF DEF AIX
            ;;; AIX allows shared object modules in .a archives, so
            ;;; try .a afterwards
            unless aname then nam <> '.a' -> aname endunless;
            returnif(exists(dir dir_>< aname ->> full_name));
#_ENDIF
        endfor;
        false -> full_name;
    enddefine;

    define lconstant strip_modname(arg);
        lvars arg, b;
#_IF DEF AIX
        if (locchar(`(`, 1, arg) ->> b) and isendstring(')', arg) then
            ;;; assume arg ends with module name (...)
            b fi_- 1 -> b;
            return( substring(1, b, arg), allbutfirst(b, arg) )
        endif;
#_ENDIF
        arg, nullstring
    enddefine;

    lvars (search_first, search_last) = shlib_searchlist();
    '$popexternlib' :: search_last -> search_last;
    [%  until args == [] do
            lvars arg;
            fast_destpair(args) -> (arg, args);
            if isword(arg) then
                copy(arg!W_STRING) -> arg;
                unless isstartstring('-', arg) then
                    arg <> SHLIB_EXTN -> arg;
                endunless;
            endif;
            if isstartstring('-l', arg) then
                lvars lib_name;
                strip_modname(allbutfirst(2, arg)) -> (arg, modname);
                'lib' <> arg -> arg;
                if find_library(arg, search_first)
                or find_library(arg, search_last) ->> lib_name
                then
                    ;;; add to list
#_IF DEF AIX
                    if isendstring('.a', lib_name) then
                        lib_name <> modname -> lib_name
                    endif;
#_ENDIF
                    lib_name;
                else
                    report(WARNING, 'can\'t open %S %S', [^SHLIB_NAME ^arg]);
                endif;
            elseif isstartstring('-L', arg)
#_IF DEFV SYSTEM_V >= 4.0 or DEF LINUX_ELF
            ;;; allow -R as well
            or isstartstring('-R', arg)
#_ENDIF
            then
                lvars i;
                if (skipchar(`\s`, 3, arg) ->> i) then
                    allbutfirst(i-1, arg) -> arg;
                    search_first nc_<> [^arg] -> search_first;
                elseif args == [] then
                    report(WARNING, 'no directory specified for link option %S',
                        [^arg]);
                else
                    fast_destpair(args) -> (arg, args);
                    search_first nc_<> [^arg] -> search_first;
                endif;
            elseif isstartstring('-', arg) then
                report(WARNING, 'ignoring link option %S', [^arg]);
            elseif sys_fname_extn(arg) = '.o' then
                report(WARNING, 'ignoring relocatable object file %S', [^arg]);
            elseif exists(strip_modname(arg) -> modname ->> arg) then
                ;;; assume it's a shared library and add it to the list
                if locchar(`/`, 1, arg) then
                    arg;
                else
                    ;;; SVR4 linker won't look in the current directory
                    ;;; by default, so make sure the file has an
                    ;;; explicit directory
                    './' dir_>< arg;
                endif <> modname;
            else
                report(WARNING, 'can\'t open %S %S (%M)', [^SHLIB_NAME ^arg]);
            endif;
        enduntil;
    %] -> link_item(LINK_ARGS);
enddefine;

    /*  open a number of shared libraries, returning a vector of handles
    */
define lconstant open_objfiles(objfiles) -> handles;
    lvars objfiles, handles;
    {%  lvars objfile, handle, msg;
        fast_for objfile in objfiles do
            ;;; map in the shared object
            if Shlib_open(objfile, false) ->> handle then
                DEBUG_PRINTF('loaded %S (handle = 16:%x)', [% objfile,
                    handle!XP_PTR %]);
                handle;
            endif;
        endfor;
    %} -> handles;
enddefine;

    /*  close some shared libraries, given a vector of handles
    */
define lconstant close_objfiles(handles);
    lvars handles;
    appdata(handles, Shlib_close);
enddefine;

    /*  retrieve the values of symbols, searching in all the shared libraries
        opened so far; the value of each symbol is filled in to a given
        external pointer
    */
define lconstant get_symbols(handles, names, values) -> success;
    lvars handles, names, values, success = true;
    lvars name, value, msg;

    define lconstant try_previous(try_handle_p);
        lvars item, try_handle_p;
        for item in link_stack do
            appdata(item(LINK_HANDLES), try_handle_p);
        endfor;
    enddefine;

    fast_for name, value in names, values do
        if Shlib_findsym(handles, try_previous, name, value) then
            DEBUG_PRINTF('found symbol %S (value = 16:%x)', [% name,
                value!XP_PTR %]);
        else
            false -> success;
        endif;
    endfor;
enddefine;

    /* open the shared libraries and load the symbols for a link_item
    */
define lconstant Link_load(link_item);
    lvars link_item;

    ;;; transform each file name with sysfileok, and delete any object files
    ;;; opened in a previous external load
    lvars objfile, objfiles;
    [%  for objfile in link_item(LINK_ARGS) do
            sysfileok(objfile) -> objfile;
            lvars item;
            for item in link_stack do
                nextif(lmember_=(objfile, item(LINK_OBJFILES)))(2);
            endfor;
            objfile;
        endfor;
    %] -> objfiles;
    objfiles -> link_item(LINK_OBJFILES);

    ;;; Open the shared objects and get the symbol values
    lvars handles = open_objfiles(objfiles);
    handles -> link_item(LINK_HANDLES);
    unless get_symbols(handles, link_item(LINK_SYMNAMES), link_item(LINK_SYMVALUES))
    then
        close_objfiles(handles);
        mishap(0, 'ERRORS ACCESSING EXTERNAL SYMBOLS (see above)');
    endunless;
enddefine;

    /*  redo all external loads required by a saved image.
    */
define Redo_link_loads(dummy);
    lvars dummy, link_item, link_items = link_stack;
    dlocal pr = sys_syspr, pop_pr_quotes = false;
    [] -> link_stack;
    _0 -> _image_handle;    ;;; for SunOS
    repeat #| dl(link_items) |# times
        -> link_item;
        Link_load(link_item);
        link_item :: link_stack -> link_stack;
    endrepeat;
    false;  ;;; dummy
enddefine;

    /*  remove the shared objects loaded by the last call to Do_link_load.
    */
define Undo_link_load();
    lvars link_item;
    dest(link_stack) -> (link_item, link_stack);
    close_objfiles(link_item(LINK_HANDLES));
enddefine;

    /*  Do a single user external load.
        The symbol specs are updated with external pointer values.
    */
define Do_link_load(objfiles, symspecs, dummy, linkfile);
    lvars objfiles, symspecs, dummy, linkfile;
    lvars link_item = writeable initv(LINK_LENGTH);
    lvars symspec, symvalue;

    process_link_args(objfiles, linkfile, link_item);

    ;;; Convert symbol names
    Convert_symbols(symspecs, false);
    [%  fast_for symspec in symspecs do
            symspec(1);
        endfor;
    %] -> link_item(LINK_SYMNAMES);

    ;;; Create external pointer for each symbol value
    [%  fast_for symspec in symspecs do
            Get_symbol_ptr(symspec)     ;;; assigns ptr into symspec as well
        endfor;
    %] -> link_item(LINK_SYMVALUES);

    Link_load(link_item);

    ;;; Add external load record to the history list
    link_item :: link_stack -> link_stack;
enddefine;

    /*  Remove all shared objects mapped in to memory by Do_link_load
    */
define Undo_link_loads();
    until link_stack == [] do
        Undo_link_load();
    enduntil;
enddefine;

#_ELSE

;;; == NOT USING SHARED LIBRARIES =========================================

    /*  Do the guts of an external load.
        link user's object files with current base symbol table
        produce new base file and image containing program/data
        read program/data into memory and set up layer table
        put external pointers in each symbol spec
    */
define Do_link_load(objfiles, symbol_list, old_link_base, new_link_base);
    lvars   dev, expdr, spec, symbol_list, tmpname, asource, aobj, objfiles,
            new_link_file, old_link_base, new_link_base, x, fail,
            procout, ld_commands,
            _imstart, _datastart, _textsize, _datasize, _bsssize
        ;

    dlvars  procedure out;

    define lconstant fprintf();
        dlocal cucharout = out;
        printf()
    enddefine;

    define lconstant hex_string();
        dlocal pop_pr_radix = 16, pop_pr_quotes = false;
        () >< ''
    enddefine;

#_IF DEF LINUX
    define lconstant ignore_ld_messages(file);
    lvars file, charrep = discin(file), ch, line;
    lconstant text_start = 'text_start', data_start = 'data_start';

        false -> fail;

        while (charrep()->>ch) /== termin do
            consstring(#| ch;
                while (charrep()->>ch) /== termin and
                    ch /== `\n` do
                    ch;
                endwhile |#) -> line;
            unless isstartstring(text_start, line) or
                isstartstring(data_start, line) then
                true -> fail;
            endunless;
        endwhile;
    enddefine;
#_ENDIF

    ;;; process object file names
    Process_objfiles(objfiles, true) -> objfiles;

    ;;; construct assembler file for array of symbol values -- address of this
    ;;; is returned as result
    Temp_name() -> tmpname;
    tmpname <> 's' -> asource;
    tmpname <> 'o' -> aobj;
    discout(asource) -> out;
    ;;; ASM_TEXT_STR and ASM_WORD_STR are macros defined in syscomp/asmout.p
    fprintf(ASM_TEXT_STR, '%p\n');

#_IF DEF LINUX
    ;;; Linux linker complains if there are no symbols in the object file
    ;;; so if none are requested, include one we know will be there.
    ;;;
    if symbol_list == [] then
        fprintf(ASM_WORD_STR, '%p _end\n')
    else
        Convert_symbols(symbol_list, fprintf)
    endif;
#_ELSE
    Convert_symbols(symbol_list, fprintf);
#_ENDIF
    out(termin);

    ;;; start address for image
    Open_seg_shift_gap_base()@POPBASE -> _imstart;

    tmpname <> 'lnk' -> new_link_file;
    tmpname <> 'out' -> procout;


#_IF DEF BERKELEY

    lvars
        as_cmd  = '/bin/as',
        ld_cmd  = '/bin/ld',
        flags   = ['-n'],
        Tflag   = '-T',
        libc    = '-lc',
    ;

  #_IF DEF IRIX or DEF LINUX
        '/usr/bin/as' -> as_cmd;
        '/usr/bin/ld' -> ld_cmd;
  #_ENDIF

  #_IF DEF HPUX
        '-R' -> Tflag;
    #_IF DEFV HPUX >= 8.0
        ['-a archive' ^^flags] -> flags;
    #_ENDIF
  #_ENDIF

  #_IF DEF LINUX
        '-Ttext' -> Tflag;
        ['-static' ^^flags] -> flags;
  #_ENDIF

  #_IF DEF MIPS
        ['-N'] -> flags;
        '-lc_G0' -> libc;
  #_ENDIF

    sysobey('/bin/sh', [%'sh', '-c',
        cons_with consstring {% for x in
            [ '('
            ;;; assemble to get object file with symbol values
            ^as_cmd '-o' ^aobj ^asource ';'
            ;;; run ld to link symbol value object and user object files
            ;;; with the file linked at the last load
            ^ld_cmd ^^flags
                        ^Tflag % hex_string(_pint(_imstart@(w->b))) %
                        '-o' ^new_link_file ^old_link_base ^aobj
                        '-L$popexternlib' ^^objfiles
                        ^libc
            ') >' ^procout '2>&1'
            ]
        do explode(x), `\s` endfor%}
    %],
    #_IF DEF LINUX
        true
    #_ELSE
        false
    #_ENDIF
    );      ;;; false arg says no terminal output

#_ELSEIF DEF SYSTEM_V

    ;;; create the ld command file
    tmpname <> 'ld' -> ld_commands;
    discout(ld_commands) -> out;
    fprintf(_imstart@(w->b), 'MEMORY { valid : o=0x%x,len=0x10000000 }\n');
    fprintf(_pint(##(b)[_1|vpage]),
            'SECTIONS \
            {   .text : {} \
                GROUP ALIGN(%p) : { .data : {}   .bss  : {}  } \
            }\n');
    out(termin);

    sysobey('/bin/sh', [%'sh', '-c',
        cons_with consstring {% for x in
            [ '('
            ;;; assemble to get object file with symbol values
            '/bin/as' '-o' ^aobj ^asource ';'
            ;;; run ld to link symbol value object and user object files
            ;;; with the file linked at the last load, supplying
            ;;; commands file to specify where stuff is linked
            '/bin/ld' '-n' '-x' '-o' ^new_link_file
                    ^ld_commands ^old_link_base ^aobj
                    '-L$popexternlib' ^^objfiles
                    '-lc'
            ') >' ^procout '2>&1'
            ]
        do explode(x), `\s` endfor%}
    %], false);     ;;; false arg says no terminal output

    sysdelete(ld_commands) -> ;

#_ELSE_ERROR
#_ENDIF

    sysdelete(asource) -> ;
    sysdelete(aobj) -> ;
    false -> fail;
    discin(procout) -> out;
    until (out() ->> x) == termin do
        cucharout(x);
        true -> fail;
    enduntil;
#_IF DEF LINUX
    ;;; some options to the Linux linker always seem to generate output
    if fail then
        ignore_ld_messages(procout);
    endif;
#_ENDIF
    sysdelete(procout) -> ;
    if fail then
        sysdelete(new_link_file) -> ;
        mishap(0, 'ERRORS IN EXTERNAL LOAD LINK (see above)')
    endif;

    ;;; create the base file for a subsequent link
    Extern_make_base(new_link_file, new_link_base, []);

    ;;; read image header to find size
    sysopen(new_link_file, 0, true, `N`) -> dev;

#_IF DEF COFF

    ;;; ignore file header
    Readb(dev, _header_buf, _HDR_BYTES);
    Readb(dev, _unix_header_buf, _UNIX_HDR_BYTES);
#_IF DEF MIPS
    @@(w)[_unix_header_buf!AH_TSIZE | b.r] -> _textsize;
#_ELSE
    @@(vpage)[_unix_header_buf!AH_TSIZE | b.r] -> _textsize;
#_ENDIF
    @@(w)[_unix_header_buf!AH_DSIZE | b] -> _datasize;
    @@(w)[_unix_header_buf!AH_BSIZE | b] -> _bsssize;

#_ELSEIF DEF BERKELEY

    Readb(dev, _header_buf, _HDR_BYTES);
    @@(w)[_header_buf!AH_TEXT | b] -> _textsize;
    @@(w)[_header_buf!AH_DATA | b] -> _datasize;
    @@(w)[_header_buf!AH_BSS | b] -> _bsssize;

#_ELSE_ERROR
#_ENDIF

    _imstart@(w){_textsize} -> _datastart;

#_IF DEF BERKELEY and not(DEF MIPS)
    ;;; round _datastart to segment boundary (segment size defaults to
    ;;; page size if not otherwise specified in syscomp/sysdefs.p).
    _datastart@(w.r->segment) -> _datastart;
#_ENDIF

    ;;; check gap base hasn't changed (could have if more space had been
    ;;; allocated for fixed structs, i.e. we shouldn't use them after getting
    ;;; _imstart above).
    if Open_seg_shift_gap_base()@POPBASE /== _imstart then
        mishap(0, 'SYSTEM ERROR IN EXTERNAL LOAD')
    endif;
    ;;; shift up the open segment to make room
    unless Do_open_seg_shift_gc(@@(vpage){ @@(w){_datastart, _imstart}
                                _add _datasize _add _bsssize | w.r}, false)
    then
        ;;; can't do a copying gc
        mishap(0, 'INSUFFICIENT MEMORY TO MAKE SPACE FOR EXTERNAL LOAD')
    endunless;

    ;;; add segments for load
    Add_nonpop_seg_entry(_imstart, _textsize, _M_SEG_CONSTANT);
    Add_nonpop_seg_entry(_datastart, @@(w){_open_seg_base_ptr@POPBASE,
                                                        _datastart}, _0);

    ;;; get to start of image and read it in

#_IF DEF COFF
    lvars _textptr, _dataptr, _tsize, _dsize, _sb;
    ;;; section header for text
    _sect_header_buf -> _sb;
    Readb(dev, _sb, _SECT_HDR_BYTES);
    _sb!ASC_SCNPTR -> _textptr;
    _sb!ASC_SIZE -> _tsize;
    ;;; and for data
    unless _zero(_datasize) then
        Readb(dev, _sb, _SECT_HDR_BYTES);
        _sb!ASC_SCNPTR -> _dataptr;
        _sb!ASC_SIZE -> _dsize;
    endunless;
    ;;; read text
    sysseek(dev, _pint(_textptr), 0);
    Readb(dev, _imstart, _tsize);
    ;;; read data
    unless _zero(_datasize) then
        unless _sb!ASC_VADDR == _datastart@(w->b) then
            mishap(0, 'EXTERNAL_LOAD: SYSTEM ERROR READING IMAGE')
        endunless;
        sysseek(dev, _pint(_dataptr), 0);
        Readb(dev, _datastart, _datasize);  ;;; was _dsize
    endunless;

#_ELSEIF DEF BERKELEY
    sysseek(dev, _pint(_N_TXTOFF _header_buf), 0);
    Readb(dev, _imstart, _header_buf!AH_TEXT);
    Readb(dev, _datastart, _header_buf!AH_DATA);

#_ELSE_ERROR
#_ENDIF

    ;;; zero bss
    _fill(_0, _bsssize, _datastart@(w){_datasize});

    sysclose(dev);
    sysdelete(new_link_file) -> ;

    ;;; Put external pointers with symbol values in spec(3) for each symbol
    fast_for spec in symbol_list do
        Get_symbol_ptr(spec) -> x;      ;;; assigns ptr into spec as well
        _imstart!(w)++ -> (x!XP_PTR, _imstart)
    endfor
enddefine;

#_ENDIF ;;; DEF SHARED_LIBRARIES


endsection;     /* $-Sys$-Extern */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  9 1999
        Undid last change
--- Robert Duncan, Apr  8 1999
        Added Linux -rpath option to process_link_args
--- Robert Duncan, Aug  3 1998
        Added LIBPATH to AIX library search list
--- John Gibson, Jul 30 1998
        Added more AIX code in process_link_args
--- John Gibson, Jun 16 1998
        Fixed bug in Convert_symbols (not passing language name correctly)
--- John Gibson, May 13 1998
        Changes for AIX
--- Robert Duncan, Dec  3 1997
        Added SHLIB_PATH to HP-UX library search list
--- John Gibson, Feb 27 1997
        Shared library stuff to extern_symbols.p and external.ph.
--- Robert Duncan, Sep  6 1996
        Fixed NCR version of shlib_open to include GLOBAL mode flag
--- John Gibson, Aug 16 1996
        Changed Convert_symbols to allow for spec(2) being a pair with the
        symbol type in the back.
        Removed Do_p*opc_load.
--- Robert Duncan, Aug  9 1996
        Added cases for NCR UNIX (SVR4). Removed old shared-library code
        which isn't used any more.
--- John Gibson, May 28 1996
        Changed to use Get_symbol_ptr to get the external pointer for each
        symbol.
--- Robert Duncan, Apr 25 1996
        Added cases for Linux ELF (shared libraries). Changed debugging
        macro to plain DEBUG.
--- John Gibson, Apr 12 1996
        Replaced use of sysio*message with %M in printf
--- John Gibson, Dec  4 1995
        Uses lmember_= instead of mem*ber
--- Julian Clinton, Oct 12 1995
        Made Linux use -Ttext flag for linker text option.
--- Integral Solutions Ltd, Aug 31 1995 (Julian Clinton)
        Added check for empty symbol list in Do_link_load for Linux.
        Corrected #_ELSE ERROR to #_ELSE_ERROR
--- John Gibson, Mar  4 1995
        Added DEF OSF1 in SHARED_LIBRARIES code
--- Robert John Duncan, Mar  3 1995
        Removed reference to RIS*COS
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Changes for Linux.
--- Robert John Duncan, May 27 1994
        Enabled use of shared libraries for IRIX 5
--- Robert John Duncan, May 23 1994
        Changed library search under Solaris to look in the image before
        any dynamically-loaded shared objects.
--- John Gibson, Apr  8 1994
        Changed Temp_name to use $TMPDIR if defined
--- Robert John Duncan, Jan 26 1994
        Deleted Symmetry, Sun386 and Orion code (all now defunct)
--- Robert John Duncan, Jan 12 1994
        For SunOS 5.3 (Solaris 2.3), merged shared-library case with that
        for HP-UX; the previous (SVR4) code is kept for now just in case.
--- John Gibson, Nov 26 1993
        Changed Convert_symbols to convert all symbols to lowercase for
        FORTRAN
--- John Gibson, Nov 15 1993
        Added -L$popexternlib to link commands
--- John Gibson, Nov 13 1993
        Removed $pop*externlib/libpop.? from link commands (everything
        now linked in to basepop11)
--- Simon Nichols, Aug  2 1993
        Locally assigned "safe" values to pr and pop_pr_quotes in
        Redo_link_loads. This was prompted by a problem in restoring saved
        images built on top of clisp.
--- John Gibson, Jul 22 1993
        libpop.olb -> libpop.a for non-shared cases
--- Simon Nichols, Jun  2 1993
        Added a dummy exload to ensure that the dynamic linker interface
        library is pulled in where necessary.
--- Simon Nichols, May 11 1993
        Lots more additions to support dynamic linking under HP-UX 8.0.
        This has been merged with SVR4/SunOS 5 code where possible (which
        has consequently been rewritten to a large extent).
        Dynamic linking code is now flagged just by DEF SHARED_LIBRARIES.
--- Simon Nichols, May  5 1993
        Fixed silly bug in Redo_link_loads (which by a fluke happened to
        work).
--- Simon Nichols, Mar  4 1993
        SunOS 5: made external load records writeable
--- Simon Nichols, Jan 27 1993
        Numerous (conditional) additions to support dynamic linking under
        SunOS 5, all flagged by: DEFV SUNOS >= 5.0 or DEF SHARED_LIBRARIES.
--- Simon Nichols, Jun  9 1992
        Changed -Do_link_load- to add a trailing underscore to FORTRAN
        symbol names even on COFF systems (see bugreport ISL-fr.4454).
--- Robert John Duncan, Dec  2 1991
        Added '_end' to -del_prefixes- list for MIPS for the benefit of
        SGI IRIX 4.0
--- Simon Nichols, Nov 11 1991
        Changes for HP-UX 8.0:
        -- zeroed fields in A_OUT_HDR which used to be spare;
        -- added '-a archive' to link options in -Do_link_load- to get
           non-shared libraries.
--- Robert John Duncan, Jun 24 1991
        Changed assemble & link command names for SG IRIX.
--- Jonathan Meyer, Jun 24 1991
        Added dlocal for popmemlim to all of the Extern_make_base procs.
--- Jonathan Meyer, Jun 21 1991
        Added dlocal for popmemlim to 2e6 (2 million words = 8MB),
        since Poplog linked with Motif/OpenLook often has more symbols
        than the default popmemlim can cope with.
--- John Gibson, Jun 20 1991
        Added extra false args to sysobey calls.
--- John Gibson, Apr 27 1991
        -Do_link_load- now fills in external pointers in symbol specs
--- John Gibson, Feb 16 1991
        Added check that Open_seg_shift_gap_base hasn't changed before
        shifting up mem
--- John Gibson, Jul  7 1990
        Changed -Do_link_load- for new format entries in symbol_list.
--- Simon Nichols, May 29 1990
        Numerous additions for MIPS
--- John Gibson, May 28 1990
        Changed to use new arg to -sysopen-
--- John Gibson, May 20 1990
        Added $pop*externlib/libpop.olb to link commands
--- Ian Rogers, Mar 21 1990
        Added '__end' '__etext' '__edata' to -del_prefixes- in the HPUX
        version of -Extern_make_base-. Fixes a bug in 7.0
--- Ian Rogers, Mar  1 1990
        -Do_link_load- now passes the object-file list through
        -sysfileok-. Cf. FR 4299 & bugreport davidwh@tsuna.2
--- John Gibson, Feb 19 1990
        Changes to mechanism for using copying gc to shift up open seg
--- John Gibson, Dec  1 1989
        Changes for new pop pointers
--- John Gibson, Aug 22 1989
        Bobcat now tested for as HPUX, and has BERKELEY set as well.
--- John Gibson, Jul 20 1989
        Disabled interrupts during -Extern_make_base- (as they can lead
        to nonpop pointers becoming invalid when interrupts serviced e.g.
        in -null- generated by for ... in ...)
--- John Gibson, Apr 30 1989
        Put into section $-Sys$-Extern.
--- Rob Duncan, Apr  4 1989
        Replaced SUN_RELEASE with SUNOS
--- Rob Duncan, Feb 17 1989
        Replaced most occurrences of #_IF DEF SYSTEM_V with #_IF DEF COFF
        and rearranged some of the tests to ensure that COFF is tested
        before BERKELEY; this is to support the Sun 386 which is a
        BSD system, but with System V-type COFF object files. Added some
        extra #_IF DEF SUN386 tests for special cases.
--- John Gibson, Feb  8 1989
        Removed fix for SunOS 4.0 of using special $popsrc/libc.olb
        (since normal libc.a is fixed in OS 4.0.1)
--- Robert Duncan, Aug 30 1988
        Added #_IF DEFs for Sequent Symmetry
--- John Gibson, Aug 18 1988
        Changes for SPARC and Sun OS4.
--- Richard Bignell, Mar 21 1988
        Changed Temp_name, removing local assignment to
        pop_pr_quotes and changed to use sys_>< rather than ><. Change due
        to problems calling -external_load- from clisp.
--- Robert Smith, Mar 17 1988
        Added #_IF DEFs for ORION
 */
