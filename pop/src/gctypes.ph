/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/gctypes.ph
 > Purpose:         Garbage collector types of structures
 > Author:          John Gibson, Feb 12 1998
 > Documentation:
 > Related Files:
 */


lconstant macro (
    GCTYPE_NONE             =  0,
    GCTYPE_PROCEDURE        =  1,
    GCTYPE_FULLREC1         =  2,
    GCTYPE_FULLREC2         =  3,
    GCTYPE_FULLREC          =  4,
    GCTYPE_BYTEVEC          =  5,
    GCTYPE_VECTOR           =  6,
    GCTYPE_DDECIMAL         =  7,
    GCTYPE_FULL2ND_REC2     =  8,
    GCTYPE_WORD             =  9,
    GCTYPE_KEY              = 10,
    GCTYPE_NFULLREC         = 11,
    GCTYPE_PROCESS          = 12,
    GCTYPE_USERNFREC        = 13,
    GCTYPE_USERNFVEC        = 14,
    GCTYPE_USERREC          = 15,
    GCTYPE_USERVEC          = 16,
    GCTYPE_DESCRIPTOR       = 17,      ;;; (VMS only)
    GCTYPE_PROPERTY         = 18,
    GCTYPE_PROPENT_PERM     = 19,
    GCTYPE_PROPENT_TMPARG   = 20,
    GCTYPE_PROPENT_TMPVAL   = 21,
    GCTYPE_PROPENT_TMPBOTH  = 22,
    GCTYPE_PROPENT_TMPCLR   = 23,
    GCTYPE_PROPENT_DESTROY  = 24,

    ;;; For special short keys
    GCTYPE_DESTROY_MARK     = 25,
    GCTYPE_STACK            = 26,      ;;; for process userstack stack
    GCTYPE_PROC_STATE       = 27,      ;;; process state structure
    GCTYPE_PLOG_PROC_STATE  = 28,      ;;; prolog process state record
    GCTYPE_CONST_PLOGVAR    = 29,
    GCTYPE_DOUBLE_PAD       = 30,
    GCTYPE_OBJMOD_PAD       = 31,
    GCTYPE_FREE_BLOCK       = 32,
    GCTYPE_RAWSTRUCT        = 33,
);
