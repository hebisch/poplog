/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.unix/lib/include/unix_ioctl.ph
 > Purpose:         Unix ioctl definitions
 > Author:          John Gibson, Oct 11 1992
 > Documentation:   HELP *IOCTL
 */

#_TERMIN_IF DEF UNIX_IOCTL_INCLUDED

section;

/* Ioctl's have the command encoded in the lower word, and the size of any
   in or out parameters in the upper word.  The high 2 bits of the upper word
   are used to encode the in/out status of the parameter.
   Parameters are restricted to 128 bytes.
 */

define iconstant ioctl_IO(x,y);
    lvars x, y;
    16:20000000 + (x<<8) + y
enddefine;

define iconstant ioctl_IOW(x,y,s);
    lvars x, y, s;
    16:80000000 + (s<<16) + (x<<8) + y
enddefine;

define iconstant ioctl_IOR(x,y,s);
    lvars x, y, s;
    16:40000000 + (s<<16) + (x<<8) + y
enddefine;


iconstant macro (

    LCRTBS  = 8:000001,     /* do backspacing for crt */
    LPRTERA = 8:000002,     /* \ ... / erase */
    LCRTERA = 8:000004,     /* " \b " to wipe out char */
    LTILDE  = 8:000010,     /* hazeltine tilde kludge */
    LMDMBUF = 8:000020,     /* start/stop output on carrier intr */
    LLITOUT = 8:000040,     /* literal output */
    LTOSTOP = 8:000100,     /* SIGSTOP on background output */
    LFLUSHO = 8:000200,     /* flush output to terminal */
    LNOHANG = 8:000400,     /* no SIGHUP on carrier drop */
    LCRTKIL = 8:002000,     /* kill line with " \b " */
    LCTLECH = 8:010000,     /* echo control chars as ^X */
    LPENDIN = 8:020000,     /* tp->t_rawq needs reread */
    LDECCTQ = 8:040000,     /* only ^Q starts after ^S */
    LNOFLSH = 8:100000,     /* no output flush on signal */

    TIOCLBIS    = ioctl_IOW(`t`, 127, 4),   /* bis local mode bits */
    TIOCLBIC    = ioctl_IOW(`t`, 126, 4),   /* bic local mode bits */
    TIOCLSET    = ioctl_IOW(`t`, 125, 4),   /* set entire local mode word */
    TIOCLGET    = ioctl_IOR(`t`, 124, 4),   /* get local modes */

    TIOCSLTC    = ioctl_IOW(`t`,117,6), /* set local special chars */
    TIOCGLTC    = ioctl_IOR(`t`,116,6), /* get local special chars */


    ;;; baud rates for sg_ispeed and sg_ospeed fields
    B0      = 0,
    B50     = 1,
    B75     = 2,
    B110    = 3,
    B134    = 4,
    B150    = 5,
    B200    = 6,
    B300    = 7,
    B600    = 8,
    B1200   = 9,
    B1800   = 10,
    B2400   = 11,
    B4800   = 12,
    B9600   = 13,
    EXTA    = 14,
    EXTB    = 15,


    ;;; sg_flags field specifiers

    TANDEM  = 8:00000001,       /* send stopc on out q full */
    CBREAK  = 8:00000002,       /* half-cooked mode */
    LCASE   = 8:00000004,       /* simulate lower case */
    ECHO    = 8:00000010,       /* echo input */
    CRMOD   = 8:00000020,       /* map \r to \r\n on output */
    RAW     = 8:00000040,       /* no i/o processing */
    ODDP    = 8:00000100,       /* get/send odd parity */
    EVENP   = 8:00000200,       /* get/send even parity */
    ANYP    = 8:00000400,       /* get any parity/send none */
    NLDELAY = 8:00001400,       /* \n delay */
    NL0     = 8:00000000,
    NL1     = 8:00000400,       /* tty 37 */
    NL2     = 8:00001000,       /* vt05 */
    NL3     = 8:00001400,
    TBDELAY = 8:00006000,       /* horizontal tab delay */
    TAB0    = 8:00000000,
    TAB1    = 8:00001000,       /* tty 37 */
    TAB2    = 8:00004000,
    XTABS   = 8:00006000,       /* expand tabs on output */
    CRDELAY = 8:00030000,       /* \r delay */
    CR0     = 8:00000000,
    CR1     = 8:00010000,       /* tn 300 */
    CR2     = 8:00020000,       /* tty 37 */
    CR3     = 8:00030000,       /* concept 100 */
    VTDELAY = 8:00040000,       /* vertical tab delay */
    FF0     = 8:00000000,
    FF1     = 8:00100000,       /* tty 37 */
    BSDELAY = 8:00100000,       /* \b delay - not implemented */
    BS0     = 8:00000000,
    BS1     = 8:00100000,
    ALLDELAY    = (NLDELAY||TBDELAY||CRDELAY||VTDELAY||BSDELAY),


    ;;; other codes for use with the sgttyb structures

    TIOCGETP    = ioctl_IOR(`t`, 8, 6), /* get parameters -- gtty */
    TIOCSETP    = ioctl_IOW(`t`, 9, 6), /* set parameters -- stty */
    TIOCSETN    = ioctl_IOW(`t`,10, 6), /* as above, but no flushtty */
    TIOCEXCL    = ioctl_IO(`t`, 13),    /* set exclusive use of tty */
    TIOCNXCL    = ioctl_IO(`t`, 14),    /* reset exclusive use of tty */
    TIOCFLUSH   = ioctl_IOW(`t`, 16, 4),/* flush buffers */
    TIOCSBRK    = ioctl_IO(`t`, 123),   /* set break bit */
    TIOCCBRK    = ioctl_IO(`t`, 122),   /* clear break bit */
    TIOCSDTR    = ioctl_IO(`t`, 121),   /* set data terminal ready */
    TIOCCDTR    = ioctl_IO(`t`, 120),   /* clear data terminal ready */
    TIOCGPGRP   = ioctl_IOR(`t`, 119, 4),   /* get pgrp of tty */
    TIOCSPGRP   = ioctl_IOW(`t`, 118, 4),   /* set pgrp of tty */
    TIOCSTI     = ioctl_IOW(`t`, 114, 1),   /* simulate terminal input */
    TIOCOUTQ    = ioctl_IOR(`t`, 115, 4),   /* output queue size */
    FIONREAD    = ioctl_IOR(`f`, 127, 4),   /* get # bytes to read */
    FIONBIO     = ioctl_IOW(`f`, 126, 4),   /* ??? (ask Clark Morton) */

    TIOCSETC    = ioctl_IOW(`t`, 17, 6),    /* set special chars */
    TIOCGETC    = ioctl_IOR(`t`,18,6),      /* get special chars */


    ;;; window size on Sun
    TIOCGWINSZ = ioctl_IOR(`t`, 104, 8),
);


iconstant UNIX_IOCTL_INCLUDED = true;

endsection;
