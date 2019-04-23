/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/ved/src/vdpdtbls.p
 > Purpose:
 > Author:          John Gibson & Aaron Sloman (see revisions)
 */

;;;-------------- ACCESSING VEDS PROCEDURE TABLES -------------------------

#_INCLUDE 'vddeclare.ph'

global constant
        procedure (vedgetinput, vedinascii, Sys$-Ved$-Im$-Input_keyboard_intr)
    ;

global vars
        vednormaltable,
        vedlastcommand,     ;;; initialised in vdprocess.p
        veduserdocommand,   ;;; ditto
        vedscreeninasciicursor,
    ;

;;; ----------------------------------------------------------------------

section $-Sys$-Ved =>   ved_last_char, ved_apply_action, vedgetproctable,
                        vedprocesschar, vedinsertvedchar
                    ;

global vars
    ved_last_char   = 0,    ;;; used non-locally by vedinsertvedchar
    ;

    ;;; all actions applied through this (redefined by XVed)
    ;;; IM creates processes upto below this procedure
protected
define vars ved_apply_action(/*p*/) with_nargs 1 with_props false;
    fast_apply()
enddefine;


    /*  Use character to address vector in vednormaltable. If what is found
        is a vector, read in another character and do the same again.
        If inchar is a string, then read characters from it. The characters
        may run out before getting to the end of the table.
        Stop with the value of next when either a non-vector is found,
        or there are no more characters left in the string.
        uses ved_last_char non-locally to record last character for
        vedprocesschar, etc.
    */
define vedgetproctable(inchar);
    lvars next, char, key, item, inchar, dchar, val, got_cursor = false;
    if isinteger(inchar) or inchar == termin then
        inchar -> dchar;
        false -> inchar
    else
        ;;; allow char rep directly or string/word
        unless isprocedure(inchar) then
            stringin(inchar) -> inchar        ;;; a character repeater
        endunless;
        fast_apply(inchar) -> dchar
    endif;

    vednormaltable -> next;
    datakey(next) -> key;

    repeat
        if dchar == termin then
            dchar -> ved_last_char;
            return("undef")
        endif;
        Checkr_dchar(dchar) -> ved_last_char;
        dchar fi_&& 16:FFFF -> char;
        returnif(char == 0) (identfn);

        if key == vector_key then
            if datalength(next) fi_>= char then
                ;;; Use the character to access the vector
                fast_subscrv(char, next)
            elseif next == vednormaltable then
                vedinsertvedchar
            else
                "undef"
            endif -> next
        else
            ;;; use it search the list
            "undef" -> item;
            while ispair(next) do
                if iscompound(item) then
                    quitif((fast_front(next) ->> item) == char)
                else
                    "undef" -> item
                endif;
                fast_back(next) -> next
            endwhile;
            returnif(iscompound(item)) (item);  ;;; assume default value
            front(back(next)) -> next
        endif;

        while ((datakey(next) ->> key) == word_key or key == ident_key)
        and next /== "undef" do
            _CHECKINTERRUPT;
            if key == word_key then valof(next) else idval(next) endif -> val;
            quitif(val == next);
            val -> next
        endwhile;

        returnunless(key == vector_key or key == pair_key) (next);

        if inchar then
            ;;; get next character from the string character repeater
            fast_apply(inchar)
        else
            ;;; Get next input character
            unless got_cursor then
                ;;; stop vedinascii from changing the cursor
                dlocal vedscreeninasciicursor = vedscreencursoron;
                true -> got_cursor
            endunless;
            vedinascii()
        endif -> dchar
    endrepeat
enddefine;

define Do_char_action(action);
    lvars action, a;
    if isprocedure(action) then
        action -> a;
        if isprocedure(veduserdocommand) then
            false, action, veduserdocommand -> a
        endif;
        ved_apply_action(a);
        action -> vedlastcommand;
    elseif isstring(action) then
        action :: ved_char_in_stream -> ved_char_in_stream
    elseif action == "undef" then
        vedscreenbell();
        vedclearinput()
    else
        vederror('\{b}unrecognized key')
    endif;
enddefine;

define vedprocesschar();
    ;;; process characters read in. See vedprocess.
    lvars item, old_kbintr = keyboard_interrupt;
    if VDDEV_LOADED and VDDEV_WEAK vedprocswaiting() then
        dlocal keyboard_interrupt = VDDEV_WEAK Im$-Input_keyboard_intr;
    endif;
    vedgetinput() -> ved_last_char;
    old_kbintr -> keyboard_interrupt;
    if isprocedure(ved_last_char) then
        ved_apply_action(ved_last_char)
    else
        Do_char_action(vedgetproctable(ved_last_char));
    endif
enddefine;

define vedinsertvedchar();
   ;;; This is called only in the environment of vedprocesschar, where
   ;;; the value of ved_last_char is the last character read in.
   vedcharinsert(ved_last_char)
enddefine;

endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 16 1996
        vedprocesschar now locally sets keyboard_interrupt
--- John Gibson, Jan 16 1994
        Renamed w*ved_apply_action as ved_apply_action and protected it.
--- John Gibson, Jan 13 1994
        Changed vedgetproctable to use vedinascii instead of vedgetinput
        for getting further characters
--- John Gibson, May 13 1993
        Changed vedgetproctable to deal with chars-with-attributes, and
        to assume the default vednormaltable action for an 8-bit char is
        vedinsertvedchar
--- John Gibson, Jan  4 1993
        Made vedgetproctable deal with idents as well as words
--- John Gibson, Dec 18 1991
        Made all actions be executed through ved_apply_action (so that
        IM can construct processes up to it)
--- John Gibson, Aug 30 1991
        Made -vedgetproctable- allow a character repeater as arg
--- John Gibson, Jun  6 1991
        Added -ved_apply_action-
--- John Gibson, Jun  5 1991
        vedina*scii replaced with -vedgetinput-
--- John Gibson, Jun  4 1991
        Sectionised, added Do_proc_interrupt
--- Aaron Sloman, Oct 31 1990
        Added vedlastcommand, veduserdocommand
--- John Gibson, Jun  5 1990
        Changed arg to Pass*_input to be ident interrupt rather than
        "interrupt".
--- John Gibson, Oct 13 1989
        Procedure -Pass*_input- into section Sys$-Ved$-Im
--- Aaron Sloman, Mar  4 1989
    Exported ved_last_char. It is also no longer local to vedgetproctable
--- John Gibson, Aug 16 1987
        Tidied up
 */
