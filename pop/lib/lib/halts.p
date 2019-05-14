/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           C.all/lib/lib/halts.p
 > Purpose:        deciding whether a procedure will ever terminate.
 > Author:         Godel's Ghost (see revisions)
 > Documentation:  HELP * HALTS
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +oldvar;

;;; This procedure can be used to decide whether a procedure applied to
;;; some argument will ever terminate.

vars inside_halts;
false -> inside_halts;

define halts(proc, arg);
    vars cucharout oldcucharout inside_halts halts_counter len;
    stacklength() -> len;
    0 -> halts_counter;
    true -> inside_halts;
    cucharout -> oldcucharout;
    define soothe();
        vars cucharout;
        if inside_halts then
            oldcucharout -> cucharout;
            erasenum(stacklength() - len);
            halts_counter + 1 -> halts_counter;
            if halts_counter rem 5 == 0 then
                'Are you sure you really want to know? Interrupt if not' =>
            endif;
            halts_counter >< ' Still testing, please wait' =>
            syssettimer(500,soothe);
        endif;
    enddefine;

    syssettimer(500,soothe);
    erase -> cucharout;             ;;; suppress print out
    proc(arg);
    erasenum(stacklength() - len);
    true
enddefine;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, May 15 1986 Author inserted
*/
