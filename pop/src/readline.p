/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/readline.p
 > Purpose:
 > Author:          John Gibson, Feb 14 1988 (see revisions)
 > Documentation:   REF *PROGLIST
 */

;;; ------------------------ READLINE ------------------------------------

#_INCLUDE 'declare.ph'

constant
        procedure (isincharitem)
    ;


;;; ---------------------------------------------------------------------

vars
    pop_readline_prompt = '? ';

define vars readline();
    lvars item, list;
    dlocal popprompt = pop_readline_prompt, popnewline;
    if (isincharitem(readitem) ->> item) and fast_back(proglist) == item then
        fast_frozval(1, item) -> item;
        fast_cont(item) -> list;
        if ispair(list) and fast_front(list) == `\n` then
            fast_back(list) -> fast_cont(item)
        endif
    endif;
    true -> popnewline;
    [% until (readitem() -> item; item == newline or item == termin) do
            item
       enduntil
    %] -> list;
    if list == [] and item == termin then termin else list endif
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
 */
