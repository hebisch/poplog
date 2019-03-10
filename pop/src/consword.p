/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/consword.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *WORDS
 */

;;; ----------------- CONSTRUCTING NEW WORDS -----------------------------

#_INCLUDE 'declare.ph'

section $-Sys;

constant
        procedure (Byte_hash, Short_hash, Substring, Get_string, Get_string16),
        dict_chain_end
    ;

endsection;

;;; --------------------------------------------------------------------

section $-Sys => consword, subword;

lconstant
    macro DICT_HASH_MASK = 16:3FF,  ;;; = 1023
    tmpstring = writeable inits(1),
    ;

    ;;; Determine if there is word in the dictionary that matches item
    ;;; (if it's a word) or that has string item (if it's a string)
    ;;; if not, add item if it's a word, or construct and add one with
    ;;; string item.
    ;;; Return word and true if word was in dictionary, false if not
define Cons_word(item, _garbage_string) -> word;
    lvars   word, key, item, string, _wstring, _len, _offs, _saddr, _cell,
            _garbage_string;

    if item!KEY == word_key then
        ;;; used by sr_incr.p only
        item!W_STRING
    else
        item
    endif -> string;

    string!V_LENGTH -> _len;
    string!KEY -> key;
    if key == string_key then
        @@(w)[_len|b.r] -> _offs;
        Byte_hash(string@V_BYTES, _len)
    else
        @@(w)[_len|s.r] -> _offs;
        Short_hash(string@V_SHORTS, _len)
    endif _bimask _:DICT_HASH_MASK -> _cell;

    dictionary@RAW_WORDS[_cell] -> _cell;
    _cell!(w) -> word;
    string@V_WORDS -> _saddr;

    ;;; search down chain of words -- ends with dict_chain_end
    until word == dict_chain_end do
        word!W_STRING -> _wstring;
        ;;; word strings are always in canonical form, so keys must be
        ;;; the same (i.e. string_key or string16_key)
        if _wstring!V_LENGTH == _len
        and _wstring!KEY == key
        ;;; can use _cmp because any padding bytes/shorts are
        ;;; guaranteed to be zero
        and _cmp(_offs, _wstring@V_WORDS, _saddr) then
            ;;; found it
            if _garbage_string and item /== nullstring then
                ;;; the word was already in the dictionary, so the string just
                ;;; constructed is garbage. No other structures have been
                ;;; constructed, so it's safe to return it to free space
                item -> Get_store()
            endif;
            return
        else
            word!W_DICT_NEXT -> word
        endif
    enduntil;

    ;;; word not found in chain. Add new one at front
    if item == string then              ;;; wasn't a word
        if item == tmpstring then copy(item) -> item endif;
        Get_record(word_key) -> word;
        0 -> word!W_IDENTIFIER;         ;;; 0 says undef
        item -> word!W_STRING
    else                                ;;; was a word
        item -> word
    endif;
    _cell!(w) -> word!W_DICT_NEXT;
    word -> _cell!(w)
enddefine;

    /*  Construct a word or find it in the dictionary
    */
define consword(arg);
    lvars arg;
    if isinteger(arg) then
        if arg == 1 then
            Checkr_dchar() fi_&& 16:FFFF -> arg;
            if arg fi_<= `,` then
                if arg == `(` then return("(")
                elseif arg == `)` then return(")")
                elseif arg == `,` then return(",")
                endif
            elseif arg fi_> 16:FF then
                return(Cons_word(consstring(arg,1), true))
            endif;
            _int(arg) -> tmpstring!V_BYTES[_0];
            return(Cons_word(tmpstring, false))
        else
            consstring(arg) -> arg
        endif
    else
        Check_string(arg);
        ;;; Copy the string, remove any attributes, and ensure
        ;;; that the string is in canonical form
        Substring(_0, arg, arg!V_LENGTH, Get_string, Get_string16) -> arg
    endif;
    Cons_word(arg, true)
enddefine;

define subword(/*wd_or_string*/) with_nargs 3;
    if iskey(dup()) then
        mishap((), 1, 'KEY ARGUMENT NOT ALLOWED FOR subword')
    endif;
    Cons_word(substring(), true)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 11 1997
        String16 changes
--- John Gibson, Apr 13 1992
        Changed Cons_word to take arg saying whether to garbage string
        if word already in dictionary
--- John Gibson, Jan 28 1992
        Fixed -consword- so it doesn't construct words from dstrings
--- John Gibson, Sep 14 1991
        Dictionary now a rawstruct rather than a string
--- John Gibson, Aug  2 1989
        Fixed problem in -subword- when -substring- returns -nullstring-
 */
