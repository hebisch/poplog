/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.unix/src/unix_tty.ph
 > Purpose:         Terminal IOCTL codes and parameters
 > Author:          John Gibson, Aug 22 1989 (see revisions)
 */


;;; -- TERMINAL INTERFACES ------------------------------------------------

#_IF DEF POSIX1

lconstant macro USE_TERMIOS = true;

#_ELSEIF DEFV SYSTEM_V < 4.0

lconstant macro USE_TERMIO = true;

#_ELSEIF DEF BERKELEY

lconstant macro USE_BSD_TTY = true;

#_ELSE_ERROR
#_ENDIF


;;; -- TERMIO(S) ----------------------------------------------------------

    /* Alternative structure definitions
    */

#_IF DEF USE_TERMIOS

    /*  See <sys/termios.h> or <termios.h>
    */

  #_IF DEFV SYSTEM_V >= 4.0 or DEF ULTRIX or DEFV LINUX < 2.0

deftype tcflag_t = long;
lconstant macro NCCS = 19;

  #_ELSEIF DEF SUNOS

deftype tcflag_t = long;
lconstant macro NCCS = 17;

  #_ELSEIF DEF HPUX or DEF AIX

deftype tcflag_t = int;
lconstant macro NCCS = 16;

  #_ELSEIF DEF IRIX

deftype tcflag_t = short;
lconstant macro NCCS = 31;

  #_ELSEIF DEF OSF1

deftype tcflag_t = int;
lconstant macro NCCS = 20;

  #_ELSEIF DEFV LINUX >= 2.0

deftype tcflag_t = int;
lconstant macro NCCS = 32;

  #_ELSE_ERROR
  #_ENDIF

struct TERMIOS
  { tcflag_t    TMIO_IFLAG,     ;;; input modes
                TMIO_OFLAG,     ;;; output modes
                TMIO_CFLAG,     ;;; control modes
                TMIO_LFLAG;     ;;; local modes

  #_IF DEFV SUNOS < 5.0 or DEF IRIX or DEF LINUX
    byte        TMIO_LINE;      ;;; line discipline
  #_ELSEIF DEF HPUX
    long        TMIO_RESERVED;  ;;; reserved for future use
  #_ENDIF

    byte        TMIO_CC[NCCS];  ;;; control characters

  #_IF DEF ULTRIX or DEF ALPHA_LINUX
        byte            TMIO_LINE;              ;;; line discipline
  #_ENDIF
  #_IF DEF OSF1 or DEFV LINUX >= 2.0
    int         TMIO_ISPEED,    ;;; input speed
                TMIO_OSPEED;    ;;; output speed
  #_ENDIF
  };


    ;;; Argument to tcsetattr()

#_IF DEFV SYSTEM_V >= 4.0
lconstant macro (
    _TCSANOW        = _int((`T`<<8)||14),
    _TCSADRAIN      = _int((`T`<<8)||15),
    _TCSAFLUSH      = _int((`T`<<8)||16),
);
#_ELSEIF DEF ULTRIX
lconstant macro (
    _TCSANOW        = _IOW(`t`, 84, SIZEOF(struct TERMIOS)),
    _TCSADRAIN      = _IOW(`t`, 83, SIZEOF(struct TERMIOS)),
    _TCSAFLUSH      = _IOW(`t`, 82, SIZEOF(struct TERMIOS)),
);
#_ELSE
lconstant macro (
    _TCSANOW        = _0,
    _TCSADRAIN      = _1,
    _TCSAFLUSH      = _2,
);
#_ENDIF


    ;;; Argument to tcflush()
lconstant macro (
    _TCIFLUSH       = _0,
    _TCOFLUSH       = _1,
    _TCIOFLUSH      = _2,
);

    ;;; Poplog codes
lconstant macro (
    _TSETPARM_N     = _TCSANOW,
    _TSETPARM_W     = _TCSADRAIN,
    _TSETPARM_P     = _TCSAFLUSH,
);

#_ELSEIF DEF USE_TERMIO

    /*  See <sys/termio.h>
    */

lconstant macro NCC = 8;

struct TERMIO
  { short   TMIO_IFLAG,
            TMIO_OFLAG,
            TMIO_CFLAG,
            TMIO_LFLAG;
    byte    TMIO_LINE,
            TMIO_CC[NCC];
  };

    ;;; Ioctls etc.

lconstant macro (
    _TCGETA         = _IOR(`T`, 1, SIZEOF(struct TERMIO)),
    _TCSETA         = _IOW(`T`, 2, SIZEOF(struct TERMIO)),
    _TCSETAW        = _IOW(`T`, 3, SIZEOF(struct TERMIO)),
    _TCSETAF        = _IOW(`T`, 4, SIZEOF(struct TERMIO)),
    _TCSBRK         = _IO(`T`, 5),
    _TCXONC         = _IO(`T`, 6),
    _TCFLSH         = _IO(`T`, 7),
    );

    ;;; Poplog codes

lconstant macro (
    _TGETPARM       = _TCGETA,
    _TSETPARM_N     = _TCSETA,
    _TSETPARM_W     = _TCSETAW,
    _TSETPARM_P     = _TCSETAF,
);

#_ENDIF


    /* Common macro definitions
    */
#_IF DEF USE_TERMIOS or DEF USE_TERMIO

    ;;; Control character indexes in TMIO_CC

  #_IF DEF OSF1 or DEF ALPHA_LINUX

    lconstant macro (
        _POSIX_VDISABLE = _16:FF,       ;;; value to disable a control char

        _CC_VEOF        = _0,
        _CC_VEOL        = _1,
        _CC_VEOL2       = _2,
        _CC_VERASE      = _3,
        _CC_VWERASE     = _4,
        _CC_VKILL       = _5,
        _CC_VREPRINT    = _6,
        _CC_VSWTCH      = _7,
        _CC_VINTR       = _8,
        _CC_VQUIT       = _9,
        _CC_VSUSP       = _10,
        _CC_VDSUSP      = _11,
        _CC_VSTART      = _12,
        _CC_VSTOP       = _13,
        _CC_VLNEXT      = _14,
        _CC_VDISCARD    = _15,
        _CC_VMIN        = _16,
        _CC_VTIME       = _17,
        _CC_VSTATUS     = _18,
    );

  #_ELSEIF DEF X86_LINUX and DEFV LINUX >= 2.0

    lconstant macro (
        _POSIX_VDISABLE = _0,       ;;; value to disable a control char

        _CC_VINTR       = _0,
        _CC_VQUIT       = _1,
        _CC_VERASE      = _2,
        _CC_VKILL       = _3,
        _CC_VEOF        = _4,
        _CC_VTIME       = _5,
        _CC_VMIN        = _6,
        _CC_VSWTCH      = _7,
        _CC_VSTART      = _8,
        _CC_VSTOP       = _9,
        _CC_VSUSP       = _10,
        _CC_VEOL        = _11,
        _CC_VREPRINT    = _12,
        _CC_VDISCARD    = _13,
        _CC_VWERASE     = _14,
        _CC_VLNEXT      = _15,
        _CC_VEOL2       = _16,
    );

  #_ELSE

    lconstant macro (
        _POSIX_VDISABLE = _0,       ;;; value to disable a control char

        _CC_VINTR       = _0,
        _CC_VQUIT       = _1,
        _CC_VERASE      = _2,
        _CC_VKILL       = _3,
        _CC_VEOF        = _4,
        _CC_VEOL        = _5,
        _CC_VEOL2       = _6,
    );

    #_IF DEF HPUX

    lconstant macro (
        _CC_VSWTCH      = _7,
        _CC_VMIN        = _11,
        _CC_VTIME       = _12,
        _CC_VSUSP       = _13,
        _CC_VSTART      = _14,
        _CC_VSTOP       = _15,
    );

    #_ELSEIF DEF ULTRIX

    lconstant macro (
        _CC_VSWTCH      = _7,
        _CC_VMIN        = _8,
        _CC_VTIME       = _9,
        _CC_VSTART      = _10,
        _CC_VSTOP       = _11,
        _CC_VSUSP       = _12,
        _CC_VDSUSP      = _13,
        _CC_VREPRINT    = _14,
        _CC_VDISCARD    = _15,
        _CC_VWERASE     = _16,
        _CC_VLNEXT      = _17,
    );

    #_ELSEIF DEF AIX

    lconstant macro (
        _CC_VMIN        = _4,
        _CC_VTIME       = _5,
        _CC_VSTART      = _7,
        _CC_VSTOP       = _8,
        _CC_VSUSP       = _9,
        _CC_VDSUSP      = _10,
        _CC_VREPRINT    = _11,
        _CC_VDISCARD    = _12,
        _CC_VWERASE     = _13,
        _CC_VLNEXT      = _14,
    );

    #_ELSE

    lconstant macro (
        _CC_VSWTCH      = _7,
        _CC_VMIN        = _CC_VEOF,
        _CC_VTIME       = _CC_VEOL,
        _CC_VSTART      = _8,
        _CC_VSTOP       = _9,
        _CC_VSUSP       = _10,
        _CC_VDSUSP      = _11,
        _CC_VREPRINT    = _12,
        _CC_VDISCARD    = _13,
        _CC_VWERASE     = _14,
        _CC_VLNEXT      = _15,
    );

    #_ENDIF

  #_ENDIF


    ;;; Input modes

lconstant macro (
    _TTY_IGNBRK     = _16:00000001,
    _TTY_BRKINT     = _16:00000002,
    _TTY_IGNPAR     = _16:00000004,
    _TTY_PARMRK     = _16:00000008,
    _TTY_INPCK      = _16:00000010,
    _TTY_ISTRIP     = _16:00000020,
    _TTY_INLCR      = _16:00000040,
    _TTY_IGNCR      = _16:00000080,
    _TTY_ICRNL      = _16:00000100,
);

  #_IF DEF OSF1 or DEF ALPHA_LINUX

lconstant macro (
    _TTY_IXON       = _16:00000200,
    _TTY_IXOFF      = _16:00000400,
    _TTY_IXANY      = _16:00000800,
    _TTY_IUCLC      = _16:00001000,
);

  #_ELSEIF DEF AIX

lconstant macro (
    _TTY_IXON       = _16:00000200,
    _TTY_IXOFF      = _16:00000400,
    _TTY_IUCLC      = _16:00000800,
    _TTY_IXANY      = _16:00001000,
);

  #_ELSE

lconstant macro (
    _TTY_IUCLC      = _16:00000200,
    _TTY_IXON       = _16:00000400,
    _TTY_IXANY      = _16:00000800,
    _TTY_IXOFF      = _16:00001000,
);

  #_ENDIF


    ;;; Output modes

  #_IF DEF OSF1 or DEF ALPHA_LINUX

lconstant macro (
    _TTY_OPOST      = _16:00000001,
    _TTY_ONLCR      = _16:00000002,
    _TTY_OLCUC      = _16:00000004,
    _TTY_OCRNL      = _16:00000008,
    _TTY_ONOCR      = _16:00000010,
    _TTY_ONLRET     = _16:00000020,
    _TTY_OFILL      = _16:00000040,
    _TTY_OFDEL      = _16:00000080,

    _TTY_NLDLY      = _16:00000300,
        _TTY_NL0    = _16:00000000,
        _TTY_NL1    = _16:00000100,
        _TTY_NL2    = _16:00000200,
        _TTY_NL3    = _16:00000300,
    _TTY_TABDLY     = _16:00000C00,
        _TTY_TAB0   = _16:00000000,
        _TTY_TAB1   = _16:00000400,
        _TTY_TAB2   = _16:00000800,
        _TTY_TAB3   = _16:00000C00,
    _TTY_CRDLY      = _16:00003000,
        _TTY_CR0    = _16:00000000,
        _TTY_CR1    = _16:00001000,
        _TTY_CR2    = _16:00002000,
        _TTY_CR3    = _16:00003000,
    _TTY_FFDLY      = _16:00004000,
        _TTY_FF0    = _16:00000000,
        _TTY_FF1    = _16:00004000,
    _TTY_BSDLY      = _16:00008000,
        _TTY_BS0    = _16:00000000,
        _TTY_BS1    = _16:00008000,
    _TTY_VTDLY      = _16:00010000,
        _TTY_VT0    = _16:00000000,
        _TTY_VT1    = _16:00010000,
);


  #_ELSE

    lconstant macro (
    _TTY_OPOST      = _16:00000001,
    _TTY_OLCUC      = _16:00000002,
    _TTY_ONLCR      = _16:00000004,
    _TTY_OCRNL      = _16:00000008,
    _TTY_ONOCR      = _16:00000010,
    _TTY_ONLRET     = _16:00000020,
    _TTY_OFILL      = _16:00000040,
    _TTY_OFDEL      = _16:00000080,
);

    #_IF DEF ULTRIX

    lconstant macro (
        _TTY_NLDLY      = _16:00000100,
            _TTY_NL0    = _16:00000000,
            _TTY_NL1    = _16:00000100,
        _TTY_CRDLY      = _16:00003000,
            _TTY_CR0    = _16:00000000,
            _TTY_CR1    = _16:00001000,
            _TTY_CR2    = _16:00002000,
            _TTY_CR3    = _16:00003000,
        _TTY_TABDLY     = _16:00000C00,
            _TTY_TAB0   = _16:00000000,
            _TTY_TAB1   = _16:00000400,
            _TTY_TAB2   = _16:00000800,
            _TTY_TAB3   = _16:00000C00,
        _TTY_BSDLY      = _16:00008000,
            _TTY_BS0    = _16:00000000,
            _TTY_BS1    = _16:00008000,
        _TTY_VTDLY      = _16:00000200,
            _TTY_VT0    = _16:00000000,
            _TTY_VT1    = _16:00000200,
        _TTY_FFDLY      = _16:00004000,
            _TTY_FF0    = _16:00000000,
            _TTY_FF1    = _16:00004000,
    );

    #_ELSEIF DEF AIX

    lconstant macro (
        _TTY_NLDLY      = _16:00004000,
            _TTY_NL0    = _16:00000000,
            _TTY_NL1    = _16:00004000,
        _TTY_CRDLY      = _16:00000300,
            _TTY_CR0    = _16:00000000,
            _TTY_CR1    = _16:00000100,
            _TTY_CR2    = _16:00000200,
            _TTY_CR3    = _16:00000300,
        _TTY_TABDLY     = _16:00000C00,
            _TTY_TAB0   = _16:00000000,
            _TTY_TAB1   = _16:00000400,
            _TTY_TAB2   = _16:00000800,
            _TTY_TAB3   = _16:00000C00,
        _TTY_BSDLY      = _16:00001000,
            _TTY_BS0    = _16:00000000,
            _TTY_BS1    = _16:00001000,
        _TTY_VTDLY      = _16:00008000,
            _TTY_VT0    = _16:00000000,
            _TTY_VT1    = _16:00008000,
        _TTY_FFDLY      = _16:00002000,
            _TTY_FF0    = _16:00000000,
            _TTY_FF1    = _16:00002000,
    );

    #_ELSE

    lconstant macro (
        _TTY_NLDLY      = _16:00000100,
            _TTY_NL0    = _16:00000000,
            _TTY_NL1    = _16:00000100,
        _TTY_CRDLY      = _16:00000600,
            _TTY_CR0    = _16:00000000,
            _TTY_CR1    = _16:00000200,
            _TTY_CR2    = _16:00000400,
            _TTY_CR3    = _16:00000600,
        _TTY_TABDLY     = _16:00001800,
            _TTY_TAB0   = _16:00000000,
            _TTY_TAB1   = _16:00000800,
            _TTY_TAB2   = _16:00001000,
            _TTY_TAB3   = _16:00001800,
        _TTY_BSDLY      = _16:00002000,
            _TTY_BS0    = _16:00000000,
            _TTY_BS1    = _16:00002000,
        _TTY_VTDLY      = _16:00004000,
            _TTY_VT0    = _16:00000000,
            _TTY_VT1    = _16:00004000,
        _TTY_FFDLY      = _16:00008000,
            _TTY_FF0    = _16:00000000,
            _TTY_FF1    = _16:00008000,
    );

    #_ENDIF

  #_ENDIF


    ;;; Control modes

  #_IF DEF OSF1

lconstant macro (
        _TTY_B0     = _0,
        _TTY_B50    = _50,
        _TTY_B75    = _75,
        _TTY_B110   = _110,
        _TTY_B134   = _134,
        _TTY_B150   = _150,
        _TTY_B200   = _200,
        _TTY_B300   = _300,
        _TTY_B600   = _600,
        _TTY_B900   = _900,
        _TTY_B1200  = _1200,
        _TTY_B1800  = _1800,
        _TTY_B2400  = _2400,
        _TTY_B3600  = _3600,
        _TTY_B4800  = _4800,
        _TTY_B7200  = _7200,
        _TTY_B9600  = _9600,
        _TTY_B19200 = _19200,
        _TTY_B38400 = _38400,
        _TTY_EXTA   = _19200,
        _TTY_EXTB   = _38400,

    _TTY_CSIZE      = _16:00000300,
        _TTY_CS5    = _16:00000000,
        _TTY_CS6    = _16:00000100,
        _TTY_CS7    = _16:00000200,
        _TTY_CS8    = _16:00000300,
    _TTY_CSTOPB     = _16:00000400,
    _TTY_CREAD      = _16:00000800,
    _TTY_PARENB     = _16:00001000,
    _TTY_PARODD     = _16:00002000,
    _TTY_HUPCL      = _16:00004000,
    _TTY_CLOCAL     = _16:00008000,
);

  #_ELSEIF DEF HPUX

lconstant macro (
    _TTY_CBAUD      = _16:0000001F,
        _TTY_B0     = _16:00000000,
        _TTY_B50    = _16:00000001,
        _TTY_B75    = _16:00000002,
        _TTY_B110   = _16:00000003,
        _TTY_B134   = _16:00000004,
        _TTY_B150   = _16:00000005,
        _TTY_B200   = _16:00000006,
        _TTY_B300   = _16:00000007,
        _TTY_B600   = _16:00000008,
        _TTY_B900   = _16:00000009,
        _TTY_B1200  = _16:0000000A,
        _TTY_B1800  = _16:0000000B,
        _TTY_B2400  = _16:0000000C,
        _TTY_B3600  = _16:0000000D,
        _TTY_B4800  = _16:0000000E,
        _TTY_B7200  = _16:0000000F,
        _TTY_B9600  = _16:00000010,
        _TTY_B19200 = _16:00000011,
        _TTY_B38400 = _16:00000012,
        _TTY_EXTA   = _16:0000001E,
        _TTY_EXTB   = _16:0000001F,
    _TTY_CSIZE      = _16:00000060,
        _TTY_CS5    = _16:00000000,
        _TTY_CS6    = _16:00000020,
        _TTY_CS7    = _16:00000040,
        _TTY_CS8    = _16:00000060,
    _TTY_CSTOPB     = _16:00000080,
    _TTY_CREAD      = _16:00000100,
    _TTY_PARENB     = _16:00000200,
    _TTY_PARODD     = _16:00000400,
    _TTY_HUPCL      = _16:00000800,
    _TTY_CLOCAL     = _16:00001000,
);

  #_ELSEIF DEF ALPHA_LINUX
lconstant macro (
        _TTY_CBAUD              = _16:0000001F,
                _TTY_B0         = _16:00000000,
                _TTY_B50        = _16:00000001,
                _TTY_B75        = _16:00000002,
                _TTY_B110       = _16:00000003,
                _TTY_B134       = _16:00000004,
                _TTY_B150       = _16:00000005,
                _TTY_B200       = _16:00000006,
                _TTY_B300       = _16:00000007,
                _TTY_B600       = _16:00000008,
                _TTY_B1200      = _16:00000009,
                _TTY_B1800      = _16:0000000A,
                _TTY_B2400      = _16:0000000B,
                _TTY_B4800      = _16:0000000C,
                _TTY_B9600      = _16:0000000D,
                _TTY_B19200     = _16:0000000E,
                _TTY_B38400     = _16:0000000F,
                _TTY_EXTA       = _TTY_B19200,
                _TTY_EXTB       = _TTY_B38400,
    _TTY_CBAUDEX        = _16:00000000,
        _TTY_B57600 = _16:00000010,
        _TTY_B115200    = _16:00000011,
        _TTY_B230400    = _16:00000012,
        _TTY_B460800    = _16:00000013,

        _TTY_CSIZE              = _16:00000300,
                _TTY_CS5        = _16:00000000,
                _TTY_CS6        = _16:00000100,
                _TTY_CS7        = _16:00000200,
                _TTY_CS8        = _16:00000300,
        _TTY_CSTOPB             = _16:00000400,
        _TTY_CREAD              = _16:00000800,
        _TTY_PARENB             = _16:00001000,
        _TTY_PARODD             = _16:00002000,
        _TTY_HUPCL              = _16:00004000,
        _TTY_CLOCAL             = _16:00008000,
);

  #_ELSE

lconstant macro (
    _TTY_CBAUD      = _16:0000000F,
        _TTY_B0     = _16:00000000,
        _TTY_B50    = _16:00000001,
        _TTY_B75    = _16:00000002,
        _TTY_B110   = _16:00000003,
        _TTY_B134   = _16:00000004,
        _TTY_B150   = _16:00000005,
        _TTY_B200   = _16:00000006,
        _TTY_B300   = _16:00000007,
        _TTY_B600   = _16:00000008,
        _TTY_B1200  = _16:00000009,
        _TTY_B1800  = _16:0000000A,
        _TTY_B2400  = _16:0000000B,
        _TTY_B4800  = _16:0000000C,
        _TTY_B9600  = _16:0000000D,
        _TTY_B19200 = _16:0000000E,
        _TTY_B38400 = _16:0000000F,
        _TTY_EXTA   = _16:0000000E,
        _TTY_EXTB   = _16:0000000F,
    _TTY_CSIZE      = _16:00000030,
        _TTY_CS5    = _16:00000000,
        _TTY_CS6    = _16:00000010,
        _TTY_CS7    = _16:00000020,
        _TTY_CS8    = _16:00000030,
    _TTY_CSTOPB     = _16:00000040,
    _TTY_CREAD      = _16:00000080,
    _TTY_PARENB     = _16:00000100,
    _TTY_PARODD     = _16:00000200,
    _TTY_HUPCL      = _16:00000400,
    _TTY_CLOCAL     = _16:00000800,
);

  #_ENDIF

    ;;; Local modes

  #_IF DEF OSF1 or DEF ALPHA_LINUX

lconstant macro (
    _TTY_ECHOKE     = _16:00000001,
    _TTY_ECHOE      = _16:00000002,
    _TTY_ECHOK      = _16:00000004,
    _TTY_ECHO       = _16:00000008,
    _TTY_ECHONL     = _16:00000010,
    _TTY_ECHOPRT    = _16:00000020,
    _TTY_ECHOCTL    = _16:00000040,
    _TTY_ISIG       = _16:00000080,
    _TTY_ICANON     = _16:00000100,
    _TTY_IEXTEN     = _16:00000400,
    _TTY_XCASE      = _16:00004000,
    _TTY_NOFLSH     = _16:80000000,
);

  #_ELSE

lconstant macro (
    _TTY_ISIG       = _16:00000001,
    _TTY_ICANON     = _16:00000002,
    _TTY_XCASE      = _16:00000004,
    _TTY_ECHO       = _16:00000008,
    _TTY_ECHOE      = _16:00000010,
    _TTY_ECHOK      = _16:00000020,
    _TTY_ECHONL     = _16:00000040,
    _TTY_NOFLSH     = _16:00000080,
);

  #_ENDIF


;;; -- BSD_TTY ------------------------------------------------------------

#_ELSEIF DEF USE_BSD_TTY

    /*  Old Berkeley tty structures
    */

struct SGTTYB
  { byte    SGT_ISPEED,
            SGT_OSPEED,
            SGT_ERASE,
            SGT_KILL;
    short   SGT_FLAGS;
  };

struct TCHARS
  { byte    TCH_INTRC,
            TCH_QUITC,
            TCH_STARTC,
            TCH_STOPC,
            TCH_EOFC,
            TCH_BRKC;
  };

struct LTCHARS
  { byte    LTCH_SUSPC,
            LTCH_DSTOPC,
            LTCH_RPRNTC,
            LTCH_FLUSHC,
            LTCH_WERASEC,
            LTCH_LNEXTC;
  };

lconstant macro (

    ;;; Old ioctls

    _TIOCFLUSH  = _IOW(`t`, 16, SIZEOF(int)),
    _TIOCSETP   = _IOW(`t`,  9, SIZEOF(struct SGTTYB)),
    _TIOCSETN   = _IOW(`t`, 10, SIZEOF(struct SGTTYB)),
    _TIOCGETP   = _IOR(`t`,  8, SIZEOF(struct SGTTYB)),
    _TIOCSETC   = _IOW(`t`, 17, SIZEOF(struct TCHARS)),
    _TIOCGETC   = _IOR(`t`, 18, SIZEOF(struct TCHARS)),
    _TIOCGLTC   = _IOR(`t`, 116, SIZEOF(struct LTCHARS)),
    _TIOCSLTC   = _IOW(`t`, 117, SIZEOF(struct LTCHARS)),
    _TIOCSPGRP  = _IOW(`t`, 118, SIZEOF(int)),
    _TIOCGPGRP  = _IOR(`t`, 119, SIZEOF(int)),
    _TIOCLGET   = _IOR(`t`, 124, SIZEOF(int)),
    _TIOCLSET   = _IOW(`t`, 125, SIZEOF(int)),
    _TIOCLBIC   = _IOW(`t`, 126, SIZEOF(int)),
    _TIOCLBIS   = _IOW(`t`, 127, SIZEOF(int)),

    ;;; Poplog codes

    _TGETPARM   = _TIOCGETP,
    _TSETPARM_P = _TIOCSETP,
    _TSETPARM_W = _TIOCSETP,    ;;; NB: Purges input, unlike termio(s)
    _TSETPARM_N = _TIOCSETN,

    ;;; Old modes

    _TTY_CRMOD      = _16:00000010,
    _TTY_ECHO       = _16:00000008,
    _TTY_CBREAK     = _16:00000002,
    _TTY_RAW        = _16:00000020,
    _TTY_XTABS      = _16:00000C00,
    _TTY_VTDELAY    = _16:00004000,
    _TTY_BSDELAY    = _16:00008000,
    );

#_ENDIF


;;; -- POPLOG TTY STRUCTURES ----------------------------------------------


struct TTY_PARAMS       ;;; (a string)
  { word    V_LENGTH;
    full    KEY;
>->
#_IF DEF USE_TERMIOS
    struct TERMIOS
            TTP_SGTTYB;     ;;; termios structure
#_ELSEIF DEF USE_TERMIO
    struct TERMIO
            TTP_SGTTYB;     ;;; termio structure
#_ELSE  ;;; USE_BSD_TTY
    struct SGTTYB
            TTP_SGTTYB;     ;;; sgttyb structure
    struct TCHARS
            TTP_TCHARS;     ;;; tchars structure
    int     TTP_LMWORD;     ;;; berkeley local mode
    struct LTCHARS
            TTP_LTCHARS;    ;;; berkeley local special chars
#_ENDIF
  };

    /*  Structure in dev!D_CTRL_BLK
    */
struct TTY_CTRL_BLK     ;;; (a vector)
  { word    V_LENGTH;
    full    KEY,
>->         TCB_PARAMS,         ;;; this device's params string
            TCB_PMODE,          ;;; TIOCSETP or TIOCSETN as a pop integer
            TCB_ARG3,           ;;; arg3 device opened with
            TCB_PARAMS_SETUP;   ;;; params string has been set up
  };

    /*  Values in dev!D_UNIT_P!UNT_TTPARM_STATUS
    */
lconstant macro (
    ATT_PARM_INVALID    = 0,
    ATT_PARM_VALID      = 1,
    ATT_PARM_CHANGED    = 2,
    );



/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jul  6 1999
        Revised definitions for Linux 2.x
--- John Gibson, Mar  1 1995
        Added OSF1 stuff and changed test for using TERMIOS be to POSIX1
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Added cases for Linux.
--- John Gibson, Feb 15 1994
        Added some more control character (CC_) macros for USE_TERMIOS
--- Robert John Duncan, Jun  7 1993
        Enabled termios for SVR4
--- Robert John Duncan, Jul 22 1992
        Fixed termios parameters for SunOS 5.0
--- Robert John Duncan, Jun  2 1992
        Enabled termios for IRIX
--- Robert John Duncan, Jun  1 1992
        Reorganised to support termios and enabled it for SunOS and HP-UX
--- Robert John Duncan, Jun 21 1991
        Added definitions for SG IRIX
--- John Gibson, Jan  3 1991
        SIZEOF macro now requires parentheses
--- John Gibson, Dec 12 1990
        Removed A_TTY struct (replaced by DEVUNIT_P struct in unixdefs.ph)
--- John Gibson, Jan  7 1990
        Changes for new pointers.
--- John Gibson, Aug 28 1989
        Added extra fields to TCB/TTY structs
--- John Gibson, Aug 22 1989
        Split off from unixdefs.ph. Reorganised so that Bobcat system
        can use Berkeley job control tty params (Bobcat now has BERKELEY
        set but not SYSTEM_V).
 */
