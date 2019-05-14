/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/include/vedfile_struct.ph
 > Purpose:         Macros for ved file structure subscripts
 > Author:          John Gibson, Jul  9 1991 (see revisions)
 */

#_TERMIN_IF DEF VEDFILE_STRUCT_INCLUDED

section;

    /*  A Ved file structure is a standard full vector. This defines
        macros for subscripting the fields.
    */

iconstant macro (
    VF_NAME             = 1,        ;;; vedcurrent
    VF_DIRECTORY        = 2,        ;;; veddirectory
    VF_BUFFER           = 3,        ;;; vedbuffer
    VF_LINE             = 4,        ;;; vedline
    VF_LINEOFFSET       = 5,        ;;; vedlineoffset
    VF_COLUMNOFFSET     = 6,        ;;; vedcolumnoffset
    VF_COLUMN           = 7,        ;;; vedcolumn
    VF_STATIC           = 8,        ;;; vedstatic
    VF_BREAK            = 9,        ;;; vedbreak
    VF_CHANGED          = 10,       ;;; vedchanged
    VF_WRITEABLE        = 11,       ;;; vedwriteable
    VF_NEEDSCOMPILING   = 12,       ;;; vedneedscompiling
    VF_COMPILEABLE      = 13,       ;;; vedcompileable
    VF_SUBSYSTEM        = 14,       ;;; subsystem
    VF_POSITIONSTACK    = 15,       ;;; vedpositionstack
    VF_MARKLO           = 16,       ;;; vvedmarklo
    VF_MARKHI           = 17,       ;;; vvedmarkhi
    VF_MARKPROPS        = 18,       ;;; vvedmarkprops
    VF_MARKSTACK        = 19,       ;;; marked_range_stack (internal)
    VF_BUFFERSIZE       = 20,       ;;; vvedbuffersize
    VF_LINESIZE         = 21,       ;;; vvedlinesize
    VF_NAMESTRING       = 22,       ;;; vednamestring
    VF_LEFTMARGIN       = 23,       ;;; vedleftmargin
    VF_LINEMAX          = 24,       ;;; vedlinemax
    VF_NOTABS           = 25,       ;;; vednotabs
    VF_INDENTSTEP       = 26,       ;;; vedindentstep
    VF_WINDOWLENGTH     = 27,       ;;; vedwindowlength
    VF_SCREENOFFSET     = 28,       ;;; vedscreenoffset
    VF_FILEPROPS        = 29,       ;;; vedfileprops
    VF_PROC_WAIT        = 30,       ;;; vedprocwait
    VF_WASONSTATUS      = 31,       ;;; vedwasonstatus
    VF_STATUSLINE       = 32,       ;;; vedstatusline
    VF_WINDOW           = 33,       ;;; wvedwindow
    VF_PATHNAME         = 34,       ;;; vedpathname
    VF_ON_STATUS        = 35,       ;;; ved_on_status
    VF_OTHER_LOCALS     = 36,       ;;; other_locals_assoc (internal)
    VF_PROMPT_CHAR      = 37,       ;;; vvedpromptchar

    VF_VFLENGTH         = 37,
    );


iconstant VEDFILE_STRUCT_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 12 1996
        Added new field VF_PROMPT_CHAR for vvedpromptchar
--- John Gibson, Apr 20 1993
        Added new field VF_OTHER_LOCALS to support other local vars
--- John Gibson, Jan 12 1993
        VF_COM*PILER -> VF_SUBSYSTEM
--- John Gibson, Dec 20 1991
        uses iconstant
--- Jonathan Meyer, Sep 11 1991 Fixed assignment to proglist at top of file
 */
