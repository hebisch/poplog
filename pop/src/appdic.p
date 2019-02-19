/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/src/appdic.p
 > Purpose:
 > Author:          John Gibson & Aaron Sloman (see revisions)
 > Documentation:   REF *WORDS
 */

;;; ---------------- APPDIC AND DIC_DISTRIB -------------------------------

#_INCLUDE 'declare.ph'

constant
        Sys$-dict_chain_end
    ;

;;; ---------------------------------------------------------------------

section $-Sys => fast_appdic, dic_distrib;

define fast_appdic(app_p);
    lvars word, procedure app_p, _cell, _lim;
    Check_procedure(app_p);
    dictionary@RAW_WORDS -> _cell;
    dictionary@POPBASE{dictionary!RAW_SIZE} -> _lim;
    while _cell <@(w) _lim do
        _cell!(w)++ -> _cell -> word;
        until word == dict_chain_end do
            _CHECKUSER;
            app_p(word);
            word!W_DICT_NEXT -> word
        enduntil
    endwhile
enddefine;

define dic_distrib();
    lvars word, len, _cell, _lim;
    dictionary@RAW_WORDS -> _cell;
    dictionary@POPBASE{dictionary!RAW_SIZE} -> _lim;
    while _cell <@(w) _lim do
        _cell!(w)++ -> _cell -> word;
        0 -> len;
        until word == dict_chain_end do
            len fi_+ 1 -> len;
            word!W_DICT_NEXT -> word
        enduntil;
        if len == 0 then cucharout(`.`) else spr(len) endif
    endwhile
enddefine;

endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 14 1991
        Dictionary now a rawstruct rather than a string
--- John Gibson, Jan 12 1989
        Replaced -appdic- with -fast_appdic- (which doesn't produce a vector
        of all words first). -appdic- is now a library procedure that uses
        -fast_appdic-.
--- John Gibson, Dec 21 1987
        -dictionary- now a string rather than a vector -- rewrote dictionary
        procedures accordingly.
--- John Gibson, Nov  5 1987
        Word chains in dictionary now terminated by -dict_chain_end-
 */
