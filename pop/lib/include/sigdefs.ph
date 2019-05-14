/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lib/include/sigdefs.ph
 > Purpose:         Defining signal numbers
 > Author:          Roger Evans, Sep 21 1988 (see revisions)
 > Documentation:   REF SIGNALS
 */

#_TERMIN_IF DEF SIGDEFS_INCLUDED

include sysdefs.ph;

section;


#_IF DEF LINUX

;;; Because Linux differs earlier than other Unix signal numbers,
;;; just dump the whole lot
iconstant macro (
    SIG_HUP     = 1,
    SIG_INT     = 2,
    SIG_QUIT    = 3,
    SIG_ILL     = 4,
    SIG_TRAP    = 5,
    SIG_IOT     = 6,
    SIG_UNUSED  = 7,
    SIG_EMT     = 7,
    SIG_BUS     = 7,    ;;; Linux sets this for compatability only
    SIG_FPE     = 8,
    SIG_KILL    = 9,
    SIG_USR1    = 10,
    SIG_SEGV    = 11,
    SIG_USR2    = 12,
    SIG_PIPE    = 13,
    SIG_ALRM    = 14,
    SIG_TERM    = 15,
    SIG_STKFLT  = 16,
    SIG_CHLD    = 17,
    SIG_CONT    = 18,
    SIG_STOP    = 19,
    SIG_TSTP    = 20,
    SIG_TTIN    = 21,
    SIG_TTOU    = 22,
    SIG_IO      = 23,
    SIG_POLL    = SIG_IO,
    SIG_URG     = SIG_IO,
    SIG_XCPU    = 24,
    SIG_XFSZ    = 25,
    SIG_VTALRM  = 26,
    SIG_PROF    = 27,
    SIG_WINCH   = 28,
    SIG_PWR     = 30,
    MAXSIG      = 30,
);

#_ELSEIF DEF UNIX

iconstant macro (
    SIG_HUP     = 1,
    SIG_INT     = 2,
    SIG_QUIT    = 3,
    SIG_ILL     = 4,
    SIG_TRAP    = 5,
    SIG_ABRT    = 6,    SIG_IOT = 6,
    SIG_EMT     = 7,
    SIG_FPE     = 8,
    SIG_KILL    = 9,
    SIG_BUS     = 10,
    SIG_SEGV    = 11,
    SIG_SYS     = 12,
    SIG_PIPE    = 13,
    SIG_ALRM    = 14,
    SIG_TERM    = 15,
);

  #_IF DEF HPUX
    iconstant macro (
        SIG_USR1    = 16,
        SIG_USR2    = 17,
        SIG_CHLD    = 18,
        SIG_PWR     = 19,
        SIG_VTALRM  = 20,
        SIG_PROF    = 21,
        SIG_IO      = 22,
        SIG_WINCH   = 23,
        SIG_STOP    = 24,
        SIG_TSTP    = 25,
        SIG_CONT    = 26,
        SIG_TTIN    = 27,
        SIG_TTOU    = 28,
        SIG_URG     = 29,
        SIG_LOST    = 30,
        SIG_RESERVED= 31,
        SIG_DIL     = 32,
        MAXSIG      = 32,
    );

  #_ELSEIF DEF ATT386
    iconstant macro (
        SIG_USR1    = 16,
        SIG_USR2    = 17,
        SIG_CHLD    = 18,
        SIG_PWR     = 19,
        SIG_WINCH   = 20,
        SIG_PHONE   = 21,
        SIG_POLL    = 22,
        SIG_IO      = SIG_POLL,
        SIG_STOP    = 23,
        SIG_TSTP    = 24,
        SIG_CONT    = 25,
        SIG_TTIN    = 26,
        SIG_TTOU    = 27,
        SIG_VTALRM  = 28,
        SIG_PROF    = 29,
        MAXSIG      = 32,
    );

  #_ELSEIF DEFV IRIX < 5.0
    iconstant macro (
        SIG_USR1    = 16,
        SIG_USR2    = 17,
        SIG_CHLD    = 18,
        SIG_PWR     = 19,
        SIG_STOP    = 20,
        SIG_TSTP    = 21,
        SIG_POLL    = 22,
        SIG_IO      = 23,
        SIG_URG     = 24,
        SIG_WINCH   = 25,
        SIG_VTALRM  = 26,
        SIG_PROF    = 27,
        SIG_CONT    = 28,
        SIG_TTIN    = 29,
        SIG_TTOU    = 30,
        SIG_XCPU    = 31,
        SIG_XFSZ    = 32,
        MAXSIG      = 32,
    );

  #_ELSEIF DEF BERKELEY
    iconstant macro (
        SIG_URG     = 16,
        SIG_STOP    = 17,
        SIG_TSTP    = 18,
        SIG_CONT    = 19,
        SIG_CHLD    = 20,
        SIG_TTIN    = 21,
        SIG_TTOU    = 22,
        SIG_IO      = 23,
        SIG_XCPU    = 24,
        SIG_XFSZ    = 25,
    );

    #_IF not(DEF AIX)
        iconstant macro (
            SIG_VTALRM  = 26,
            SIG_PROF    = 27,
        );
    #_ENDIF

    #_IF DEF SUNOS or DEF OSF1 or DEF DYNIX or DEF ULTRIX or DEF AIX or DEF ALPHA_LINUX
        iconstant macro (
            SIG_WINCH   = 28,
            SIG_LOST    = 29,   SIG_INFO    = 29,
            SIG_USR1    = 30,
            SIG_USR2    = 31,
        );

        #_IF DEF AIX
            iconstant macro (
                SIG_PROF    = 32,
                SIG_DANGER  = 33,
                SIG_VTALRM  = 34,
                MAXSIG      = 34,
            );
        #_ELSE
            iconstant macro MAXSIG = 31;
        #_ENDIF

    #_ELSE
        iconstant macro MAXSIG = 27;

    #_ENDIF /* SUNOS/DYNIX/ULTRIX/OSF/ALPHA_LINUX */

  #_ELSEIF DEF SYSTEM_V
    iconstant macro (
        SIG_USR1    = 16,
        SIG_USR2    = 17,
        SIG_CHLD    = 18,
        SIG_PWR     = 19,
        SIG_WINCH   = 20,
        SIG_POLL    = 22,
        SIG_IO      = SIG_POLL, ;;; Apparently these are the same
        MAXSIG      = 32,
    );

    #_IF DEFV SYSTEM_V >= 4.0
        iconstant macro (
            SIG_URG     = 21,
            SIG_STOP    = 23,
            SIG_TSTP    = 24,
            SIG_CONT    = 25,
            SIG_TTIN    = 26,
            SIG_TTOU    = 27,
            SIG_VTALRM  = 28,
            SIG_PROF    = 29,
            SIG_XCPU    = 30,
            SIG_XFSZ    = 31,
        );

    #_ENDIF /* SYSTEM_V >= 4.0 */

  #_ELSE    ;;; not BERKELEY or SYSTEM_V
    iconstant macro MAXSIG = 15;

  #_ENDIF /* BERKELEY or SYSTEM_V */


#_ELSEIF DEF VMS
    iconstant macro (
        SIG_INT     = 1,    ;;; Ctrl-C
        SIG_ALRM    = 2,    ;;; timer
        SIG_CHLD    = 3,    ;;; spawned child died
        SIG_IO      = 4,    ;;; input available
        MAXSIG      = 4,
    );


#_ELSEIF DEF WIN32

    /*  These indicate the occurrence of various Win32 events
    */
iconstant macro (
    SIG_HUP     = 1,    ;;; Not used
    SIG_INT     = 2,    ;;; Console CTRL_C
    SIG_QUIT    = 3,    ;;; Console CTRL_BREAK
    SIG_ILL     = 4,    ;;; Illegal instruction
    SIG_EXN     = 5,    ;;; Other exception
    SIG_CHLD    = 6,    ;;; Child process terminated
    SIG_ALRM    = 7,    ;;; Timer expired
    SIG_FPE     = 8,    ;;; FP error
    SIG_BUS     = 10,   ;;; Data alignment error
    SIG_SEGV    = 11,   ;;; Access violation
    SIG_TERM    = 15,   ;;; Console CTRL_CLOSE, LOGOFF or SHUTDOWN
    MAXSIG      = 15,
);

#_ELSE_ERROR
#_ENDIF

;;; aliases for signal names
#_IF DEF SIG_CHLD
    iconstant macro (
        SIG_CHILD   = SIG_CHLD,
        SIG_CLD     = SIG_CHLD,
    );
#_ENDIF

#_IF DEF SIG_WINCH
    iconstant macro (
        SIG_WINDOW = SIG_WINCH,
    );
#_ENDIF


iconstant SIGDEFS_INCLUDED = true;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, May  8 1998
        Added AIX changes
--- Robert Duncan, Mar 28 1996
        Win32 now sends SIG_TERM when console window is closed
--- Robert John Duncan, Mar  3 1995
        Removed definitions for RIS*COS
--- John Gibson, Mar  1 1995
        Slight changes for OSF1, and tidied up
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Added options for Linux and ATT386 (SCO).
--- Robert John Duncan, Sep  5 1994
        Added definitions for Win32.
--- Robert John Duncan, Mar 21 1994
        Changed for IRIX 5+ (SVR4)
--- John Gibson, Oct 29 1992
        Added #_TERMIN_IF
--- Robert John Duncan, Jul 22 1992
        Added extra signal numbers for SVR4
--- Jonathan Meyer, Aug  2 1991
        Added 'include sysdefs'
--- Robert John Duncan, Jun 21 1991
        Added definitions for SG IRIX
--- John Gibson, Nov 22 1990
        Added SIG_IO for VMS
--- Jonathan Meyer, Oct 23 1990
        Added SIG_IO = SIG_POLL for SYSTEM_V (apparently the same)
--- Jonathan Meyer, Jul 10 1990
        Added definitions for SYSTEM_V (signals 16 to 32).
--- Simon Nichols, Jun 15 1990
        Added definitions for RISC/os (signals 16 to 31).
--- Rob Duncan, Mar  6 1990
        Added extra signals for ULTRIX
--- John Gibson, Aug 24 1989
        Added signals for HPUX 6.5
--- John Gibson, Jun  5 1989
        Made all macro defs INCLUDE_constants (lconstant when #_INCLUDEd,
        global constant otherwise).
 */
