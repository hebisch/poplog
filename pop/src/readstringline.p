/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/readstringline.p
 > Purpose:
 > Author:          John Gibson, Mar  8 1988 (see revisions)
 > Documentation:   REF *PROGLIST
 */

;;; ----------- PRODUCING STRINGS FROM PROGLIST ITEMS, ETC -------------------

#_INCLUDE 'declare.ph'

constant
        procedure nextchar
    ;

;;; ----------------------------------------------------------------------

define rdstringto(terminators) -> string;
    lvars item, terminators, string = nullstring;
    unless ispair(terminators) then [%terminators%] -> terminators endunless;
    until lmember((readitem() ->> item), terminators) or item == termin do
        string sys_>< item -> string
    enduntil
enddefine;

define readstringline();
    lvars char, started, _sl;
    dlocal popnewline;
    if ispair(proglist) and not(isprocedure(fast_back(proglist))) then
        true -> popnewline;
        rdstringto([^newline ^termin])
    elseif proglist == [] then
        nullstring
    else
        false -> started;
        stacklength() -> _sl;
        repeat
            nextchar(readitem) -> char;
            if (char == `\s` or char == `\t`) and not(started) then
                nextloop
            elseif char == termin then
                quitloop
            elseif char == `\\` then
                nextchar(readitem) -> char;
                if char == termin then `\\`, quitloop endif
            elseif char == `\n` then
                quitloop
            endif;
            true -> started;
            char
        endrepeat;
        /* Now delete trailing spaces */
        stacklength() fi_- _sl -> _sl;
        while _sl fi_> 0 and dup() == `\s` do
            ->;
            _sl fi_- 1 -> _sl;
        endwhile;
        consstring(_sl)
    endif
enddefine;


/* --- Revision History ---------------------------------------------------
--- John Williams, May 18 1993
        readstringline now ignores trailing spaces (cf. BR isl-er.223)
 */
