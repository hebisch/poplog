/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/sr.ph
 > Purpose:
 > Author:          John Gibson, Aug 23 1996
 */

;;; ---------------- DEFINITIONS FOR SAVE/RESTORE ----------------------------


lconstant macro (
    VERSION_LEN     = 256,          ;;; max length of version string

    ;;; magic numbers for .psv files
    PSVP_MAGIC      = 36:PSVFP,     ;;; private
    PSVS_MAGIC      = 36:PSVFS,     ;;; shared
    ;;; magic number for .psi files
    PSI_MAGIC       = 36:PSIFP,
    );

#_IF DEF UNIX
lconstant macro OLD_PSV_CHAR = `-`;
#_ELSEIF DEF VMS
lconstant macro OLD_PSV_CHAR = `/`;
#_ELSE
;;; there is no ``old'' char: make it same as the new
lconstant macro OLD_PSV_CHAR = `+`;
#_ENDIF


    /*  Entry in IMG_SHR_SECT_TABLE
    */
struct SHR_SECT
  { int SHRS_FILE_OFFS,     ;;; word offset within file
        SHRS_SIZE;          ;;; word offset size
  };

    /*  Format of saved image header
    */

    ;;; Part written by Create/read by Open (see sr_util.p)
struct IMAGE_INIT_HEADER
  { full    IMG_MAGIC;                  ;;; popint magic number
    (vpage) IMG_SYSTEM_END;             ;;; saved value of _system_end
    word    IMG_VERSION_STRING_LEN;     ;;; sysint length of version string
    byte    IMG_VERSION_STRING[VERSION_LEN];    ;;; version string
    full    IMG_IMAGE_ARGS_OFFS;        ;;; popint offset of previous image args
    (vpage) IMG_INITIAL_MEM_END;        ;;; saved value of _initial_mem_end
  };

struct IMAGE_HEADER
  { struct IMAGE_INIT_HEADER
            IMG_INIT_HEADER;            ;;; initial header
    full    IMG_SYMTAB_OFFS;            ;;; popint offset of symbol table,
                                        ;;; or 0 if none
    int     IMG_SHR_SECT_TAB_SIZE;      ;;; offset size of shareable
                                        ;;; sections table
    struct SHR_SECT
            IMG_SHR_SECT_TABLE[32];     ;;; shareable sections table

    word    IMG_SAVED_DATA[];           ;;; rest of saved data -- see code
  };


section $-Sys$-Sr;

constant
        procedure (Open, Create, Seek, Write, Write_word, Write_idval,
        Read, Read_word, Read_idval)
    ;

vars
        _sr_device, _sr_channel
    ;

endsection;
