/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/charin.p
 > Purpose:         Read characters from standard input
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *CHARIO
 > Related Files:   C.all/src/charout.p
 */

#_INCLUDE 'declare.ph'

global constant
        procedure (sysobey, Sys$-Io$-Get_char, Sys$-Io$-Log_char),
    ;

global vars
        procedure (pop_chario_trap),
        pop_charout_col, Sys$-charin_dev
    ;


;;; ---------------------------------------------------------------------

section $-Sys => charin, pop_charin_escapes;

vars
    pop_charin_escapes = #_IF DEF VMS
                            [`$`]
                         #_ELSE
                            [ `%` `$` `!`]
                         #_ENDIF;



define charin() -> _char;
    lvars dev = charin_dev, _char, _count, _svchar;
    dlocal weakref cucharout;

    ;;; This procedure is also called by -charout- and -charerr- with 2nd arg
    ;;; true to mean output rather than input. Initially -pop_chario_trap-
    ;;; is erasenum(2), but is redefined by VED etc.
    pop_chario_trap(dev, true);

    Io$-Get_char(dev) -> _char;
    if _char == `\^G` then
        setpop()
    elseif poplastchar == `\n` and lmember(_char, pop_charin_escapes) then
        _char -> _svchar;
        0 -> poplastchar;
        charin() -> _char;
        0 -> _count;
        until _char == `\n` do
            _char;
            _count fi_+ 1 -> _count;
            charin() -> _char
        enduntil;
        sysobey(consstring(_count), _svchar)
    endif;
    _char -> poplastchar;
    if _char == `\n` and systrmdev(dev) then
        0 -> weakref pop_charout_col
    endif;
    Io$-Log_char(_char)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 16 1990
        Changed call to chario_trap to pop_chario_trap taking 2nd arg true.
--- Aaron Sloman, Nov 15 1990
    added syscharintrap
--- John Gibson, Oct 26 1990
        Moved -pop_charin_device- to popdevin.p
--- John Gibson, Jun  6 1990
        -charin_dev- now contains device, not an ident
--- Aaron Sloman, Dec 28 1989
        Added pop_charin_escapes
--- John Williams, Oct  6 1988
        Added -pop_charin_device-
--- John Gibson, Mar 16 1988
        Previously in iochar.p
 */
