/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/external.ph
 > Purpose:         Common definitions for external routines
 > Author:          Roger Evans, Apr 28 1988 (see revisions)
 > Documentation:   REF external
 > Related Files:   aextern.p extern_block.p
 */

    ;;; temporary to get next include to work from any dir
define lconstant macro #_INCLUDE_REL relname;
    lvars relname;
    "'#_INCLUDE'", sys_fname_path(popfilename) dir_>< relname
enddefine;


    /* This defines the flag bits in Sys$- _external_flags
    */
#_INCLUDE_REL '../lib/include/external_flags.ph'


    /* external function closure
    */

#_IF DEF MIPS
lconstant macro EFC_CODE_SIZE = 20;
#_ELSE
lconstant macro EFC_CODE_SIZE = 16;
#_ENDIF

struct EXFUNC_CLOS
  { full    EFC_FUNC,       ;;; exptr to base function
            KEY;
>-> byte    EFC_CODE[EFC_CODE_SIZE];
                            ;;; code (a subroutine call, or whatever)
    full    EFC_ARG;        ;;; arg to assign before calling func
    (word)  EFC_ARG_DEST;   ;;; ptr to arg destination
  };

    /*  Result structure used by _call_external
    */
struct EXTERN_RESULT
  { word    RAW_SIZE;       ;;; size as word offset
    full    KEY;
>-> dfloat  EXRES_FLOAT;
    <word>  EXRES_WORD;
  };


    /* Shared libraries
    */
#_IF DEF SHARED_LIBRARIES or DEF VMS
    ;;; (N.B. VAX VMS supports shareable images even though
    ;;; SHARED_LIBRARIES is false)

#_IF DEF WINDOWS

lconstant
    SHLIB_EXTN = '.dll',            ;;; extension for shared libraries
    SHLIB_NAME = 'DLL',             ;;; what to call a shared library
;

#_ELSEIF DEF VMS

lconstant
    SHLIB_EXTN = '.exe',
    SHLIB_NAME = 'shareable image',
;

#_ELSEIF DEF HPUX

lconstant
    DL_LIB     = '-ldld',           ;;; interface to the dynamic linker
    SHLIB_EXTN = '.sl',             ;;; extension for shared libraries
    SHLIB_NAME = 'shared library',  ;;; what to call a shared library
;

#_ELSEIF DEFV SYSTEM_V >= 4.0 or DEF OSF1 or DEF LINUX_ELF or DEF AIX

lconstant
    DL_LIB     = '-ldl',
    SHLIB_EXTN = '.so',
    SHLIB_NAME = 'shared object',
;

#_ELSE_ERROR
#_ENDIF
#_ENDIF     ;;; DEF SHARED_LIBRARIES or DEF VMS


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 20 1998
        Added AIX case for SHLIB_EXTN
--- John Gibson, Feb 27 1997
        Added shared library defs
--- John Gibson, Mar 16 1995
        EFC_CODE now a byte field of size EFC_CODE_SIZE
--- John Gibson, Feb 17 1995
        Added EXTERN_RESULT struct
--- Robert John Duncan, Mar 15 1994
        Increased size of code for MIPS external function closures
--- John Gibson, Dec 11 1992
        Moved perm declarations to declare.ph
--- John Gibson, Jul  8 1991
        Made PEF flags pop rather than nonpop and moved them to
        lib/include/external_flags.ph
--- John Gibson, Jan 19 1991
        Added PEF_ASYNC_CALLBACK and replaced PEF_D*OING_USER_EXTERN
        with PEF_UNUSED
--- John Gibson, Nov 13 1990
        Removed redundant X stuff. Added _PEF_ flags, etc.
--- John Gibson, Sep  5 1990
        Added struct EXFUNC_CLOSURE
--- John Gibson, Jan  7 1990
        Changes for new pointers
 */
