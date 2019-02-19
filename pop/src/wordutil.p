/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/wordutil.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; --------------------- WORD UTILITIES -------------------------------------

#_INCLUDE 'declare.ph'


define subscrw(_wsub, word);
    lvars _wsub, word;
    unless isword(word) then Sys$-Check_word(word) endunless;
    word!W_STRING -> word;
    Sys$-Check_vsubscr(_wsub, word);
    fast_subscrs(_wsub, word)
enddefine;
;;;
define updaterof subscrw(_char, _wsub, word);
    lvars _wsub, word, _char;
    mishap(word, 1, 'CAN\'T ALTER WORD CHARACTERS')
enddefine;

define fast_subscrw(_wsub, word);
;;; non-checking version of subscrw
    lvars _wsub, word;
    word!W_STRING -> word;
    fast_subscrs(_wsub, word)
enddefine;
;;;
define updaterof fast_subscrw() with_nargs 3;
    -> subscrw()
enddefine;

define word_dict_status(word);
    lvars word;
    Sys$-Check_word(word);
    word!W_DICT_NEXT -> word;
    if isword(word) then
        ;;; word in dictionary
        true
    elseif ispair(word) then
        ;;; word identifier (pair is [word|section])
        word
    else
        false
    endif
enddefine;

define word_string(word);
    lvars word;
    Sys$-Check_word(word);
    copy(word!W_STRING)
enddefine;

define fast_word_string(/*word*/) with_nargs 1;
    ()!W_STRING
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 26 1988
        Weakref'ed prolog references
--- John Gibson, Feb 22 1988
        Various procedures into section Sys
--- John Gibson, Nov 26 1987
        Added -word_dict_status-, -word_string- and -fast_word_string-.
 */
