/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:           C.unix/lib/lib/termio.p
 > Purpose:        Constants for termio structure
 > Author:         Mark Rubinstein, Jan  9 1986 (see revisions)
 > Documentation:  HELP * TERMIO, REF * SYS_IO_CONTROL, UNIX MAN 7 TERMIO
 > Related Files:  BSD version (HELP * IOCTL).
 */

#_TERMIN_IF DEF POPC_COMPILING          ;;; needs fixing for popc

/*  The primary calls of the Unix system V terminal io interface use a
    structure as defined in the Unix manual termio(7).  This file defines
    constants for accessing (setting and getting) this structure and
    manipulating the fields within it.  The basic structure is as follows:

        #define     NCC 8
        struct termio {
            unsigned short  c_iflag;        /* input modes */
            unsigned short  c_oflag;        /* output modes */
            unsigned short  c_cflag;        /* control modes */
            unsigned short  c_lflag;        /* local modes */
            char            c_line;         /* line discipline */
            unsigned char   c_cc[NCC];      /* control characters */
        };

    From POPLOG then this can be made up a 17 byte structure as follows:

*/

compile_mode:pop11 +strict;

section;

recordclass constant termio_struct
    termio_dummy1:full          /* dummy word */
    termio_iflag:16             /* input modes */
    termio_oflag:16             /* output modes */
    termio_cflag:16             /* control modes */
    termio_lflag:16             /* local modes */
    termio_line:8               /* line discipline */
        termio_vintr:8          /* interrupt */
        termio_vquit:8          /* quit */
        termio_verase:8         /* erase <DEL> */
        termio_vkill:8          /* kill (line) */
        termio_veof:8           /* end-of-file */
        termio_veol:8           /* end-of-line delimiter */
        termio_dummy2:8         /* reserved */
        termio_switch:8         /* switch, used for job control */
;

;;; the c_iflag field describes the basic terminal input control
global constant
    IGNBRK  = 8:0000001,    /* ignore break condition */
    BRKINT  = 8:0000002,    /* Signal interrupt on break. */
    IGNPAR  = 8:0000004,    /* Ignore characters with parity errors. */
    PARMRK  = 8:0000010,    /* Mark parity errors. */
    INPCK   = 8:0000020,    /* Enable input parity check. */
    ISTRIP  = 8:0000040,    /* Strip character. */
    INLCR   = 8:0000100,    /* Map NL to CR on input. */
    IGNCR   = 8:0000200,    /* Ignore CR. */
    ICRNL   = 8:0000400,    /* Map CR to NL on input. */
    IUCLC   = 8:0001000,    /* Map upper-case to lower-case on input. */
    IXON    = 8:0002000,    /* Enable start/stop output control. */
    IXANY   = 8:0004000,    /* Enable any character to restart output. */
    IXOFF   = 8:0010000,    /* Enable start/stop input control. */
    ;

;;; the c_oflag  field specifies the system treatement of output
global constant
    OPOST   = 8:0000001,    /* Postprocess output. */
    OLCUC   = 8:0000002,    /* Map lower case to upper on output. */
    ONLCR   = 8:0000004,    /* Map NL to CR-NL on output. */
    OCRNL   = 8:0000010,    /* Map CR to NL on output. */
    ONOCR   = 8:0000020,    /* No CR output at column 0. */
    ONLRET  = 8:0000040,    /* NL performs CR function. */
    OFILL   = 8:0000100,    /* Use fill characters for delay. */
    OFDEL   = 8:0000200,    /* Fill is DEL, else NUL. */
    NLDLY   = 8:0000400,    /* Select new-line delays: */
    NL0     = 8:0,
    NL1     = 8:0000400,
    CRDLY   = 8:0003000,    /* Select carriage-return delays: */
    CR0     = 8:0,
    CR1     = 8:0001000,
    CR2     = 8:0002000,
    CR3     = 8:0003000,
    TABDLY  = 8:0014000,    /* Select horizontal-tab delays: */
    TAB0    = 8:0,
    TAB1    = 8:0004000,
    TAB2    = 8:0010000,
    TAB3    = 8:0014000,    /* Expand tabs to spaces. */
    BSDLY   = 8:0020000,    /* Select backspace delays: */
    BS0     = 8:0,
    BS1     = 8:0020000,
    VTDLY   = 8:0040000,    /* Select vertical-tab delays: */
    VT0     = 8:0,
    VT1     = 8:0040000,
    FFDLY   = 8:0100000,    /* Select form-feed delays: */
    FF0     = 8:0,
    FF1     = 8:0100000,
    ;

;;; The c_cflag field describes the hardware control of the terminal:
global constant
    CBAUD   = 8:0000017,    /* Baud rate: */
    B0      = 8:0,          /* Hang up */
    B50     = 8:0000001,    /* 50 baud */
    B75     = 8:0000002,    /* 75 baud */
    B110    = 8:0000003,    /* 110 baud */
    B134    = 8:0000004,    /* 134.5 baud */
    B150    = 8:0000005,    /* 150 baud */
    B200    = 8:0000006,    /* 200 baud */
    B300    = 8:0000007,    /* 300 baud */
    B600    = 8:0000010,    /* 600 baud */
    B1200   = 8:0000011,    /* 1200 baud */
    B1800   = 8:0000012,    /* 1800 baud */
    B2400   = 8:0000013,    /* 2400 baud */
    B4800   = 8:0000014,    /* 4800 baud */
    B9600   = 8:0000015,    /* 9600 baud */
    EXTA    = 8:0000016,    /* External A */
    EXTB    = 8:0000017,    /* External B */
    CSIZE   = 8:0000060,    /* Character size: */
    CS5     = 8:0,          /* 5 bits */
    CS6     = 8:0000020,    /* 6 bits */
    CS7     = 8:0000040,    /* 7 bits */
    CS8     = 8:0000060,    /* 8 bits */
    CSTOPB  = 8:0000100,    /* Send two stop bits, else one. */
    CREAD   = 8:0000200,    /* Enable receiver. */
    PARENB  = 8:0000400,    /* Parity enable. */
    PARODD  = 8:0001000,    /* Odd parity, else even. */
    HUPCL   = 8:0002000,    /* Hang up on last close. */
    CLOCAL  = 8:0004000,    /* Local line, else dial-up. */
    LOBLK   = 8:0010000,    /* Block layer output. */
    ;

;;; The c_lflag field of the argument structure is used by the line
;;; discipline to control terminal functions.  The basic line discipline
;;; (0) provides the following:
global constant
    ISIG    = 8:0000001,    /* enable signals */
    ICANON  = 8:0000002,    /* canonical input (erase and kill processing) */
    XCASE   = 8:0000004,    /* canonical upper/lower presentation */
    ECHO    = 8:0000010,    /* enable echo */
    ECHOE   = 8:0000020,    /* echo erase character as BS-SP-BS */
    ECHOK   = 8:0000040,    /* echo NL after kill character */
    ECHONL  = 8:0000100,    /* echo NL */
    NOFLSH  = 8:0000200,    /* disable flush after interrupt or quit */
    ;

;;; Used to convert two bytes into a 16 bit short as used by the termio
;;; libraries.
define global ioctl_IO(x,y);
lvars x y;
    (x<<8) + y
enddefine;

;;; the following command requests are used in the form
;;; sys_io_control(<device>, request, arg) -> success_boolean;
global constant
    TCGETA  = ioctl_IO(`T`, 1), /* Get the parameters associated with the
                                    terminal and store in the termio structure
                                    referenced by arg. */
    TCSETA  = ioctl_IO(`T`, 2), /* Set the parameters associated with the
                                    terminal from the structure referenced by
                                    arg.  The change is immediate. */
    TCSETAW = ioctl_IO(`T`, 3), /* Wait for the output to drain before setting
                                    the new parameters.  This form should be
                                    used when changing parameters that will
                                    affect output. */
    TCSETAF = ioctl_IO(`T`, 4), /* Wait for the output to drain, then flush
                                    the input queue and set the new
                                    parameters. */
    TCSBRK  = ioctl_IO(`T`, 5), /* Wait for the output to drain.  If arg is 0,
                                    then send a break (zero bits for 0.25
                                    seconds). */
    TCXONC  = ioctl_IO(`T`, 6), /* Start/stop control.  If arg is 0, suspend
                                    output; if 1, restart suspended output. */
    TCFLSH  = ioctl_IO(`T`, 7), /* If arg is 0, flush the input queue; if 1,
                                    flush the output queue; if 2, flush both
                                    the input and output queues. */
    ;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 26 1992
        Moved to C.unix
 */
