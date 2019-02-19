/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/src/syscancelword.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *WORDS
 */

;;; --------------- REMOVE A WORD FROM THE DICTIONARY --------------------

#_INCLUDE 'declare.ph'

section $-Sys;

constant
        procedure Bytevec_hashint,
        dict_chain_end
    ;

endsection;

;;; --------------------------------------------------------------------

section $-Sys => syscancelword;

lconstant
    _DICT_HASH_MASK = _1023;

define syscancelword(word);
    lvars nextword, word, _cell;
    Check_word(word);
    returnunless(isword(word!W_DICT_NEXT)); ;;; not in dictionary anyway
    Bytevec_hashint(word!W_STRING) _bimask _DICT_HASH_MASK -> _cell;
    dictionary@RAW_WORDS[_cell] -> _cell;
    _cell!(w) -> nextword;
    while nextword /== word do
        if nextword == dict_chain_end then
            ;;; word not found in chain.
            mishap(word, 1, 'SYSTEM ERROR IN syscancelword')
        endif;
        nextword@W_DICT_NEXT -> _cell;
        nextword!W_DICT_NEXT -> nextword
    endwhile;
    word!W_DICT_NEXT -> _cell!(w);
    false -> word!W_DICT_NEXT   ;;; mark not in dictionary
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 14 1991
        Dictionary now a rawstruct rather than a string
--- John Gibson, Apr  6 1988
        Moved out of words.p
 */
