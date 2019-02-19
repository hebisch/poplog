/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/chario.p
 > Purpose:
 > Author:          John Gibson, Mar 15 1988 (see revisions)
 */

;;; ------------- MISCELLANEOUS CHARACTER I/O STUFF ----------------------

#_INCLUDE 'declare.ph'

;;; ----------------------------------------------------------------------

section $-Sys => poplogfile, cucharin, pop_chario_trap;

vars
    poplogfile              = false,
    procedure cucharin      = identfn(% termin %),
    ;

    /*  Called by -charin-, -charout- and -charerr-
    */
protected
define vars pop_chario_trap(/*dev, is_input*/) with_nargs 2;
    -> (,)
enddefine;


define Io$-Log_char(_char);
    lvars _char;
    ;;; for logging input and output
    if poplogfile then
        if isprocedure(poplogfile) then
            fast_apply(if _char == termin then `\n` else _char endif,
                                                            poplogfile)
        else
            false -> poplogfile
        endif
    endif
enddefine;


endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 16 1990
        Exported chario_trap as pop_chario_trap (taking 2 args).
 */
