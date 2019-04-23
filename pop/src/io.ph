/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/io.ph
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;------------------ FORMAT OF DEVICES, IO BUFFERS ------------------------

#_TERMIN_IF DEF IO_INCLUDED

#_IF WORD_BITS == 64

deftype file_desc_t = int;

#_ELSE

deftype file_desc_t = short;

#_ENDIF

struct DEVICE
  {
    short   D_FLAGS;
    file_desc_t     D_FILE_DESC;
    full    KEY,
>->         D_CTRL_BLK,
            D_UNIT_N,
            D_UNIT_P,
            D_OPEN_NAME,
            D_FULL_NAME,
            D_IN_BUFFER,
            D_OUT_BUFFER,
            D_READ,
            D_WRITE,
            D_SEEK,
            D_FLUSH,
            D_CLOSE,
            D_CLEAR_INPUT,
            D_TEST_INPUT,
            D_ENCODING_ID,
            D_ENCODING_OWN_ID;
  };


lconstant macro (
    ;;; device flag bits in D_FLAGS
    _M_D_CLOSED             = _2:1e0,   ;;; device is closed
    _M_D_TERMINAL           = _2:1e1,   ;;; is a (real) terminal
    _M_D_TERM_PROMPT        = _2:1e2,   ;;; prompt for terminal reads
    _M_D_INTERACTIVE        = _2:1e3,   ;;; interactive (term or pipe/mailbox)
    _M_D_UNUSED             = _2:1e4,   ;;; currently unused
    ;;; N.B. next two are replicated in syscomp/genstruct.p
    _M_D_USER_DEV           = _2:1e5,   ;;; is a user device
    _M_D_LOGICAL_TERM       = _2:1e6,   ;;; is a (logical) terminal

    ;;; I/O Sets
    RD_SET      = 0,
    WR_SET      = 1,
    EX_SET      = 2,
    );


#_IF DEF VMS

    ;;; status return buffer used by VMS I/O system calls
struct IO_STAT_BUF
  { short   IOSB_STATUS,
            IOSB_COUNT,
            IOSB_DUMMY,
            IOSB_TERMIN_LEN;
  };

#_ENDIF


struct IO_BUFFER         ;;; (a string)
  { word    V_LENGTH;
    full    KEY;
>-> int     BUF_SIZE,
            BUF_POSITION,
            BUF_COUNT,
            BUF_BLK_NUM,
            BUF_FLAGS,
            BUF_ENCODE_STATE;
#_IF DEF VMS
    struct IO_STAT_BUF
            BUF_IOSB;
#_ENDIF
    byte    BUF_START[];
  };

lconstant macro (
    ;;; buffer flag bits in BUF_FLAGS
    _M_BUF_FIXED_ADDR       = _2:1e0,   ;;; buffer string is fixed-address
    _M_BUF_MODIFIED         = _2:1e1,   ;;; buffer contents modified
    );

lconstant IO_INCLUDED = true;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  4 1997
        Replaced D_ENCODING by D_ENCODING_ID and D_ENCODING_OWN_ID
--- John Gibson, Feb 25 1997
        Added coding function stuff.
--- John Gibson, Feb 18 1997
        Added new device field D_ENCODING, and new buffer field
        BUF_ENCODE_STATE.
--- John Gibson, Jul  8 1994
        Added RD_SET etc
--- John Gibson, Dec  5 1992
        Added VMS IO_STAT_BUF definition
--- John Gibson, Feb  3 1991
        Added buffer flags
--- John Gibson, Jan 22 1991
        Removed async I/O defs
--- John Gibson, Dec 12 1990
        Added D_UNIT_N and D_UNIT_P fields in device
--- John Gibson, Oct 26 1990
        Removed ved device flag, added _M_D_LOGICAL_TERM
--- John Gibson, Oct 24 1990
        Added new fields to struct DEVICE
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Aug 24 1990
        Added _M_D_RESTORE_ASYNC
--- Roger Evans, Jun 25 1990
        changed SIM_ASYNC_IO to rule out ALL HPUX systems
--- John Gibson, Jan  7 1990
        Changes for new pointers
--- Roger Evans
        added async io support
--- John Gibson, Feb 21 1989
        Got rid of _M_D_GOT_EOL (_M_D_TERM_PROMPT used instead)
--- John Gibson, Feb 19 1989
        Moved out of syscomp/symdefs.p
 */
