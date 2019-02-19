/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/perm_ident.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *IDENT
 */

;;; ------------ PERMANENT IDENTIFIERS ATTACHED TO WORDS --------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'memseg.ph'       ;;; for SLS_ flags

constant
        procedure (undefword)
    ;

section $-Sys;

constant
        procedure (Vm$-Assign_pas_token, Sect$-New_ident, Sect$-word_idents,
        Sect$-Curr_word_id, Get_perm_ident, cons_assoc, del_assoc_val,
        )
    ;

endsection;

weak constant
        procedure (sys_autoload, sys_autoload_action, sys_writeable_prop)
    ;

weak vars
        pop_record_writeable, Sys$- _writeable_ignored
    ;


;;; -------------------------------------------------------------------------

section $-Sys => sysdeclare, syscancel;

lvars
    perm_depend_assoc = [];


define Check_token(item);
    lvars item;
    unless iscompound(item) and item!KEY!K_FLAGS _bitst _:M_K_ID_TOKEN then
        mishap(item, 1, 'WORD NEEDED')
    endunless
enddefine;

define Cons_perm_id(token) -> id;
    lvars id, token;
    Get_record(undef_key) -> id;    ;;; undef record
    if isword(token) and ispair(token!W_DICT_NEXT) then
        fast_front(token!W_DICT_NEXT)
    else
        token
    endif -> id!U_WORD;
    id, Get_record(ident_key) -> id -> fast_idval(id);
    _0 -> id!ID_PERM_FLAGS
enddefine;

define Assign_perm_id(token, id);
    lvars token, id;
    if token!W_IDENTIFIER /== id then
        if isword(token)
        and (testdef current_section or testdef Sect$-word_idents) then
            weakref[current_section, Sect$-word_idents]
                                            Sect$-New_ident(token, id)
        else
            id -> token!W_IDENTIFIER
        endif;
        if testdef sys_autoload and iscompound(id)
        and not(id!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF)
        then
            false -> weakref[sys_autoload] sys_autoload_action(token)
        endif
    endif;

    if VM_LOADED and vm_pas_mode then
        VM_WEAK Vm$-Assign_pas_token(id, token)
    endif
enddefine;

define Make_perm_def(token);
    lvars id = token!W_IDENTIFIER, wid, wid_list, token;
    returnunless(id!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF);
    id!ID_PERM_FLAGS _biclear _:M_PERM_NOT_DEF -> id!ID_PERM_FLAGS;
    if testdef sys_autoload then
        false -> weakref[sys_autoload] sys_autoload_action(token)
    endif;
    del_assoc_val(id, perm_depend_assoc) -> (wid_list, perm_depend_assoc);
    if wid_list then
        fast_for wid in wid_list do
            Get_perm_ident(wid, false) ->
        endfast_for
    endif
enddefine;

define Note_perm_dependency(word, depend_vec);
    lvars w, word, l, id, depend_vec;
    returnunless(word!W_IDENTIFIER!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF);
    if testdef current_section then
        weakref[current_section] Sect$-Curr_word_id(word) -> word
    endif;
    fast_for w in_vector depend_vec do
        unless w!W_IDENTIFIER!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF then
            Get_perm_ident(word, false) -> ;
            return
        endunless
    endfor;
    fast_for w in_vector depend_vec do
        w!W_IDENTIFIER -> id;
        if list_assoc(id, perm_depend_assoc) ->> l then
            nextif(fast_lmember(word, fast_front(l)));
            word :: fast_front(l) -> fast_front(l)
        else
            cons_assoc(id, word::[], perm_depend_assoc) -> perm_depend_assoc;
            ;;; ensure value pairs are kept writeable
            nextunless(testdef sys_writeable_prop);
            lconstant macro SWP_WEAK = [weakref[sys_writeable_prop]];
            if SWP_WEAK pop_record_writeable then
                true -> weakref sys_writeable_prop(fast_back(perm_depend_assoc))
            else
                ;;; makes sys_lock_system mishap if given the
                ;;; SLS_NONWRITEABLE_DEFAULT flag
                SWP_WEAK _writeable_ignored _biset _:SLS_NONWRITEABLE_DEFAULT
                        -> SWP_WEAK _writeable_ignored
            endif
        endif
    endfor
enddefine;

protected
define vars sysdeclare(token);
    lvars id, token;
    Check_token(token);
    if testdef sys_autoload then weakref sys_autoload(token) -> endif;
    if issimple(token!W_IDENTIFIER ->> id) then
        Cons_perm_id(token) -> id;
        _0 ->> id!ID_IDENTPROPS -> id!ID_NUM_ATTRIBUTE;
        Assign_perm_id(token, id)
    elseif id!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF then
        Make_perm_def(token)
    else
        return
    endif;
    sys_raise_exception(token, 1, {'%DECLARING VARIABLE %P' '' 16:01},
                                        'vm-ident:name-ref-none', `W`)
enddefine;

define Get_perm_ident(token, allow_weak) -> id;
    lvars id, token, allow_weak;
    unless iscompound(token) and token!KEY!K_FLAGS _bitst _:M_K_ID_TOKEN then
        mishap(token, 1, 'WORD NEEDED')
    endunless;
    if issimple(token!W_IDENTIFIER ->> id)
    or (id!ID_PERM_FLAGS _bitst _:M_PERM_NOT_DEF and not(allow_weak)) then
        sysdeclare(token);
        if iscompound(token!W_IDENTIFIER ->> id) then
            Make_perm_def(token)
        else
            mishap(token, 1, 'NOT A PERMANENT IDENTIFIER')
        endif
    endif
enddefine;

updater(Exec_nonpd) -> ;    ;;; declares Exec_nonpd as having an updater

define Undef_cancel(word, id);
    lvars word, id, val;

    lconstant undef_canc = struct UNDEF =>> {%'\<cancelled\ ident\>',
                                                            undef_key%};
    returnif(id <@(w) _system_end);
    fast_idval(id) -> val;
    if isundef(val) and undefword(val) == word then
        if val!KEY == undef_key then
            undef_canc
        else
            #_< Exec_nonpd(% undef_canc %) >_#
        endif -> fast_idval(id)
    endif
enddefine;

define syscancel(token);
    lvars token;
    Check_token(token);
    if iscompound(token!W_IDENTIFIER) then
        Undef_cancel(token, token!W_IDENTIFIER)
    endif;
    Assign_perm_id(token, 0)
enddefine;


endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 30 1996
        Changed exception id-string in sysdeclare.
--- John Gibson, Feb  6 1996
        sysdeclare now calls sys_raise_exception (old pr*warning stuff
        now in errors.p)
--- John Gibson, May 15 1995
        Changed writeable_ignored to nonpop flags variable _writeable_ignored
--- John Gibson, Feb  2 1995
        Added setting of writeable_ignored in Note_perm_dependency when
        pop_record_writeable is false
--- John Gibson, Aug 16 1993
        Replaced pop_p*as_mode with internal var vm_pas_mode
--- John Gibson, Nov  7 1992
        Made Make_perm_def take token as arg instead of id
--- John Gibson, Oct 16 1992
        Changed Note_perm_dependency to take a vector of words instead of
        a list
--- John Gibson, Oct 16 1992
        Redid
--- John Gibson, Oct  6 1992
        Absorbed F*orce_perm_declare into Get_perm_ident plus extra arg
--- John Gibson, May 15 1992
        Changed -Note_perm_dependency- so that value pairs in dependency list
        are kept writeable
--- John Williams, Nov 16 1990
        -sysprw*arning- no longer prints filename and line number
--- John Williams, Oct 19 1990
        -sysprw*arning- now checks if its argument is a word.
--- Jonathan Meyer, Oct 19 1990
        Made sysprw*arning call sys*prmessage to print the message.
        Removed popfilename and popfilenum.
--- James Goodlet, Sep 26 1990
        Modified declarations for popfilename and poplinenum
        in keeping with errors.p
--- Aaron Sloman, Sep 11 1990
        Put in declarations for popfilename and poplinenum
--- James Goodlet, Sep  7 1990
        sysprw*arning now prints filename and line number if a
        permanent identifier is declared while compiling a file.
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Feb 14 1989
        -Make_perm_def- leaves identifier alone if _:M_PERM_NOT_DEF not set.
--- John Gibson, Jan 29 1989
        Changes for 'note' declarations
--- John Gibson, Dec  7 1988
        Moved -Check_token- here from word.p
--- John Gibson, Mar 14 1988
        Created from stuff in ident.p
 */
