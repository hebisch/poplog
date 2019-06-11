/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/vedwordstar.p
 > Purpose:         Modify VED to simulate WordStar editor key controls.
 > Author:          Andreas Schoter, April 3 1990 (see revisions)
 > Documentation:   HELP * VEDWORDSTAR
 > Related Files:   HELP * VEDSET, HELP * VEDKEYS
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

include sysdefs.ph;

define lconstant enterandinsert(string);
    lvars string;
    vedenter();
    vedinsertstring(string)
enddefine;

lconstant vedsearchws   = enterandinsert(% '/' %);
lconstant vedreplacews  = enterandinsert(% 's' %);

vedset keys
    chardown            = ^X
    charup              = ^E
    charright           = ^D
    charleft            = ^S
    wordleft            = ^A
    wordright           = ^F
    screenleft          = ^Q s
    screenleft          = ^Q S
    textright           = ^Q d
    textright           = ^Q D
    screenup            = ^Q e
    screenup            = ^Q E
    screendown          = ^Q x
    screendown          = ^Q X
    topfile             = ^Q r
    topfile             = ^Q R
    endfile             = ^Q c
    endfile             = ^Q C

    scrolldown          = ^Z
    scrollup            = ^W

    linedelete          = ^Y
    clearhead           = ^Q del
    cleartail           = ^Q y
    cleartail           = ^Q Y
    wordrightdelete     = ^T
    dotdelete           = ^G
    ENTER d             = ^K y
    ENTER d             = ^K Y
    ENTER clear         = ^K j
    ENTER clear         = ^K J
    ENTER yankl         = esc ^Y
    ENTER yankw         = esc ^T
    ENTER yankw         = esc ^Q
    ENTER y             = esc ^K

    ENTER bye           = ^K d
    ENTER bye           = ^K D
    ENTER w1            = ^K s
    ENTER w1            = ^K S
    ENTER wq            = ^K x
    ENTER wq            = ^K X
    ENTER q             = ^K q
    ENTER q             = ^K Q

    ENTER lcol          = ^O l
    ENTER lcol          = ^O L
    ENTER rcol          = ^O r
    ENTER rcol          = ^O R
    ENTER centre        = ^O c
    ENTER centre        = ^O C

    marklo              = ^K b
    marklo              = ^K B
    markhi              = ^K k
    markhi              = ^K K
    ENTER m             = ^K v
    ENTER m             = ^K V
    ENTER t             = ^K c
    ENTER t             = ^K C
    markfind            = ^Q b
    markfind            = ^Q B
    endrange            = ^Q k
    endrange            = ^Q K

    searchws            = ^Q f
    searchws            = ^Q F
    replacews           = ^Q a
    replacews           = ^Q A

    ENTER jjp           = ^B
    setstatic           = ^V
    ENTER break         = ^O w
    ENTER break         = ^O W

;;; because WordStar uses ^D for cursor movement
;;; the following usual VED bindings have been redone
    ENTER lmr           = esc d
    ENTER lmr           = esc D
    loadline            = esc l
    loadline            = esc L
endvedset;

/*
    ************************************************************
                        NECESSARY UNIX TERMINAL EXTRAS
                           lifted from LIB *VEDEMACS
*/

;;; The method of changing the control characters is different for
;;; BSD unix and SystemV  unix.
#_IF DEF BERKELEY

uses ioctl_tchars;
uses ioctl_ltchars;

lconstant setpdr =
    procedure(old) with_props wordstar_setup;
    lvars old;
    lconstant tchars = inits(6);
        if isprocedure(old) then old -> vedinit; old() endif;
        unless sys_io_control(poprawdevin, TIOCGETC, tchars) do   ;;; get the TCHARS
            mishap('SYS_IO_CONTROL error (using TIOCGETC)', nil);
        endunless;
        `\^U` -> tchars(1);                             ;;; control U becomes interrupt
        255 ->> tchars(2) ->> tchars(3) -> tchars(4);   ;;; disable other special chars
        unless sys_io_control(poprawdevin, TIOCSETC, tchars) do   ;;; set the TCHARS
            mishap('SYS_IO_CONTROL error (using TIOCSETC)', nil);
        endunless;
        unless sys_io_control(poprawdevin, TIOCGLTC, tchars) do   ;;; get lcoal TCHARS
            mishap('SYS_IO_CONTROL error (using TIOCGLTC)', nil);
        endunless;
        255 ->> tchars(2) ->> tchars(4) -> tchars(6);   ;;; disable required chars
        unless sys_io_control(poprawdevin, TIOCSLTC, tchars) do   ;;; set lcoal TCHARS
            mishap('SYS_IO_CONTROL error (using TIOCSLTC)', nil);
        endunless;
    endprocedure;

#_ELSEIF DEF SYSTEM_V

uses termio;

lconstant setpdr =
    procedure(old) with_props wordstar_setup;
    lvars old;
    lconstant tstruct = constermio_struct(0,0,0,0,0,0,0,0,0,0,0,0,0,0);
        if isprocedure(old) then old -> vedinit; old() endif;
        unless sys_io_control(poprawdevin, TCGETA, tstruct) do   ;;; set lcoal TCHARS
            mishap(0, 'SYS_IO_CONTROL error (using TCGETA)');
        endunless;
        `\^U` -> termio_vintr(tstruct);             ;;; control U becomes interrupt
        255 -> termio_vquit(tstruct);                 ;;; disable quit.
        ;;; disable XON (control Q control S processing)
        termio_iflag(tstruct) &&~~ IXON -> termio_iflag(tstruct);
        ;;; disable Canonical input, but enable signals.
        (termio_lflag(tstruct) &&~~ ICANON) || ISIG -> termio_lflag(tstruct);
        unless sys_io_control(poprawdevin, TCSETA, tstruct) do   ;;; set lcoal TCHARS
            mishap(0, 'SYS_IO_CONTROL error (using TCSETA)');
        endunless;
    endprocedure;

#_ELSE

;;; Changed so that it works after a fashion without changing the control
;;; characters. (A.S. Jan 1988)
lconstant setpdr =
    procedure(old) with_props wordstar_setup;
    lvars old;
        if isprocedure(old) then old -> vedinit; old() endif;
    endprocedure;

#_ENDIF

define lconstant procedure ved_setuppdr();
    if vedsetupdone then
        setpdr(false)
    else
        setpdr(% vedinit %) -> vedinit
    endif;
enddefine;

ved_setuppdr();

global constant vedwordstar = true;     ;;; for uses

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct 11 1990
    Fixed spelling of "costant"
--- Aaron Sloman, Jun  3 1990 added declaration of vedwordstar
    Made ved_setuppdr lconstant. Put in public library
 */
