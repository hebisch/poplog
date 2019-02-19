/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/word.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *WORDS
 */

;;; ---------------- WORDS AND THE DICTIONARY ----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

section $-Sys;

constant
        procedure (Dest_string, Bytevec_hashint)
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys => isword, destword, word_key;

constant
    dictionary      = dictionary,           ;;; set up by poplink

    ;;; The word at the end of word chains in the dictionary
    dict_chain_end  = struct WORD =>> {%0, word_key, '', false%},
    ;;; Short name for it used by poplink
    $-K$-d          = dict_chain_end,
    ;


define Check_word(item);
    lvars item;
    unless iscompound(item) and item!KEY == word_key then
        mishap(item, 1, 'WORD NEEDED')
    endunless
enddefine;

define isword(item);
    lvars item;
    if iscompound(item) and item!KEY==word_key then true else false endif
enddefine;

define destword(word);
    lvars word;
    Check_word(word);
    Dest_string(word!W_STRING)
enddefine;


;;; --- WORD KEY -------------------------------------------------------

define lconstant Word_apply(word);
    lvars word, string, _wsub;
    word!W_STRING -> string;
    unless _nonzero(_stklength()) and (->> _wsub; isinteger(_wsub)) then
        ;;; assume should be executing non-procedure
        Exec_nonpd(word)
    elseif _int(_wsub) _sub _1 _greq string!V_LENGTH then
        ;;; subscript out of range - make this give the error
        Check_vsubscr((), string)
    else
        fast_subscrs((), string)
    endunless
enddefine;
;;;
define updaterof Word_apply(word);
    lvars word, _wsub;
    unless _nonzero(_stklength()) and (->> _wsub; isinteger(_wsub)) then
        ;;; assume should be executing non-procedure
        -> Exec_nonpd(word)
    else
        mishap(word, 1, 'CAN\'T ALTER WORD CHARACTERS')
    endunless
enddefine;

define lconstant Eq__Word(item, word);
    lvars item, word, _key, _len;
    if item == word then
        true
    elseif issimple(item) then
        false
    elseif (item!KEY ->> _key) == word_key then
        item!W_STRING -> item;
        word!W_STRING -> word;
        word!V_LENGTH -> _len;
        returnunless(item!V_LENGTH == _len) (false);
        ;;; word strings are always in canonical form
        item!KEY!K_FLAGS -> _key;
        if word!KEY!K_FLAGS _bitst _:M_K_STRING16 then
            _key _bitst _:M_K_STRING16
            ;;; can compare by words because any padding shorts are
            ;;; guaranteed to be zero
            and _cmp( @@(w)[_len | s.r], item@V_WORDS, word@V_WORDS)
        else
            not(_key _bitst _:M_K_STRING16)
            ;;; can compare by words because any padding bytes are
            ;;; guaranteed to be zero
            and _cmp( @@(w)[_len | b.r], item@V_WORDS, word@V_WORDS)
        endif
    elseif _key!K_FLAGS _bitst _:M_K_MATCH_VAR then
        fast_chain(word, item, _key!K_SYS_EQUALS)
    else
        false
    endif
enddefine;

define lconstant Word_print() with_nargs 1;
    Print_str(()!W_STRING)
enddefine;

define lconstant Word_hash() with_nargs 1;
    _pint(Bytevec_hashint(()!W_STRING))
enddefine;

constant
    word_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD _biset _:M_K_ID_TOKEN
            _biset _:M_K_COPY _biset _:M_K_WRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_WORD,          ;;; K_GC_TYPE
        Record_getsize,         ;;; K_GET_SIZE

        "word",                 ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isword,                 ;;; K_RECOGNISER
        WREF Word_apply,        ;;; K_APPLY
        Eq__Word,               ;;; K_SYS_EQUALS
        WREF Eq__Word,          ;;; K_EQUALS
        Word_print,             ;;; K_SYS_PRINT
        WREF Word_print,        ;;; K_PRINT
        WREF Word_hash,         ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_CONST,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct WORD)++,      ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R
        %},

    ;;; short name for this key used by poplink
    ;;; (saves space on the assembler output!)

    $-K$-w  = word_key,
    ;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 11 1997
        String16 changes
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Dec  7 1988
        Moved -Check_token- to perm_ident.p
--- John Gibson, Feb 22 1988
        Various procedures into section Sys
--- John Gibson, Feb 21 1988
        Made -Cons_word- etc use Sys$-Bytevec_hashint (string.p)
--- John Gibson, Jan 17 1988
        Improved dictionary hashing algorithm.
            Different data sections for words and identifiers generated by
        poplink now replaced by area from _data_seg_start to
        _data_seg_end; 'App_' procedures for the old sections therefore
        no longer required.
--- John Gibson, Dec  6 1987
        Added "nonexported" for -dictionary-.
--- John Gibson, Nov  5 1987
        Word chains in dictionary now terminated by special word
        -dict_chain_end- (means that the test for a word being in the
        dictionary is now isword(word!W_DICT_NEXT) )
 */
