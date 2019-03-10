/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/lispcore.p
 > Purpose:         Lisp stuff
 > Author:          John Williams (see revisions)
*/


#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

global constant
    procedure (
     Sys$-Bytevec_hashint,
     Sys$-Hash_vector_between,
     Sys$-Prop$-Round_tabsize,
     Sys$-Set_undef_clos,
     Sys$-list_assoc_val,
     sys_message_printf,
     fast_appproperty,
     fast_ncdelete,
     isproperty,
     maplist,
     newproperty,
    );

global constant
    _setstklen,
    _setstklen_diff,
    _useras,
    ;


section $-lisp => lisp_true, pop_true, setstacklength;

vars closed_read_ok = false;


/* Early typed error signalling procedures. These are defined properly
    in C.all/lisp/src/util.p
*/

define vars type_error(got, expecting);
    mishap(got, 1, expecting sys_>< ' NEEDED')
enddefine;


define vars name_error(item, kind);
    mishap(item, 1,
            'Invalid ' <> uppertolower(fast_word_string(kind)) <> ' name')
enddefine;


define vars control_error() with_nargs 2;     /* i.e. message, involving */
    mishap()
enddefine;


define vars package_error(message, involving, pkg);
    mishap(message, involving)
enddefine;


/* Keys */

define fast_datakey() with_nargs 1;
    ()!KEY
enddefine;

define is_vector_key() with_nargs 1;
    ()!K_FLAGS _bitst _:M_K_VECTOR
enddefine;

define is_record_key() with_nargs 1;
    ()!K_FLAGS _bitst _:M_K_RECORD
enddefine;

define is_number_key() with_nargs 1;
    ()!K_NUMBER_TYPE /== _:NUMTYPE_NON_NUMBER
enddefine;


/* Lists */

define list_assoc() with_nargs 2;
    $-Sys$-list_assoc_val()
enddefine;

define $-lisp$-destlist(list);
    lvars _len;
    _stklength() -> _len;
    while ispair(list) do
        _CHECKUSER;
        fast_destpair(list) -> list
    endwhile;
    unless list == nil then
        type_error(list, "LIST")
    endunless;
    _pint(##(w){_stklength() _sub _len})
enddefine;

define car(list);
    if ispair(list) then
        fast_front(list)
    elseif list == nil then
        list
    else
        type_error(list, "LIST")
    endif
enddefine;

define updaterof car(list);
    if ispair(list) then
        -> fast_front(list)
    else
        type_error(list, "LIST")
    endif
enddefine;

define cdr(list);
    if ispair(list) then
        fast_back(list)
    elseif list == nil then
        list
    else
        type_error(list, "LIST")
    endif
enddefine;

define updaterof cdr(list);
    if ispair(list) then
        -> fast_back(list)
    else
        type_error(list, "LIST")
    endif
enddefine;

define rplaca(list, item) -> list;
    if ispair(list) then
        item -> fast_front(list)
    else
        type_error(list, "LIST")
    endif
enddefine;

define rplacd(list, item) -> list;
    if ispair(list) then
        item -> fast_back(list)
    else
        type_error(list, "LIST")
    endif
enddefine;

define fast_rplaca(list, item) -> list;
    item -> fast_front(list)
enddefine;

define fast_rplacd(list, item) -> list;
    item -> fast_back(list)
enddefine;

define endp(item);
    if ispair(item) then
        false
    elseif item == nil then
        true
    else
        type_error(item, "LIST")
    endif
enddefine;


/* Vectors (see below for character strings) */

define fast_vector_length() with_nargs 1;
    _pint(()!V_LENGTH)
enddefine;


define fast_vmember(item, vector);
    lvars _start, _lim, _ptr;
    vector@V_WORDS[_0] ->> _ptr -> _start;
    _start@(w)[vector!V_LENGTH] -> _lim;
    while _ptr <@(w) _lim do
        if (_ptr!(w)++ -> _ptr) == item then
            return(_pint( ##(w){_ptr _sub _start} ))
        endif
    endwhile;
    false
enddefine;


/* The Lisp VM */

vars LISP_N_ARGS = false;

define lisp_true() with_nargs 1;
    unless dup() then ->; nil endunless
enddefine;

define pop_true() with_nargs 1;
    if dup() == nil then ->; false endif
enddefine;

define lisp_true_apply() with_nargs 1;
    fast_apply() or nil
enddefine;


/* Applying Lisp functions */

define lisp_apply(pdr, nargs, nresults);
    lvars sl;
    nargs -> LISP_N_ARGS;
    if isinteger(nresults) then
        stacklength() fi_- nargs -> sl;
        fast_apply(pdr);
        _setstklen(sl, nresults)
    else
        fast_chain(pdr)
    endif
enddefine;

define Lisp_setstacklength(/* saved stklen, nresults */) with_nargs 2;
    ;;; normally optimised to an I_SETSTACKLENGTH instruction
    _setstklen()
enddefine;

define setstacklength(_n);
    ##(w){_stklength()} _sub _int(_n) -> _n;
    unless _zero(_n) do
        if _neg(_n) then
            repeat
                _CHECKUSER;
                nil;
                quitif(_zero(_n _add _1 ->> _n));
            endrepeat;
        else
            _useras(@@(w)[_n])
        endif
    endunless
enddefine;


/* Catch and Throw */

vars Init_stklen = 0;               /* Used by the Lisp debugger */

protected vars
    Catch_ok    =   pop_undef,
    Catch_tag   =   pop_undef,
    Throw_tag   =   pop_undef,
    ;

lvars
    _Catch_slen         =   _0,
    _Cok_frame_offs     =   _0,
    _Ctag_frame_offs    =   _0,
    _Csl_frame_offs     =   _0,
    ;


define catch(Catch_tag, p) with_props 'Catch';
    dlocal Catch_tag, Catch_ok = true, _Catch_slen, Init_stklen;
    _stklength() -> _Catch_slen;
    _pint(##(w){_Catch_slen}) -> Init_stklen;   /* for the debugger */
    fast_apply(p)
enddefine;



define throw(Throw_tag, result, exec_result) with_props 'Throw';
    lvars ok, tag, was_ok,
            _sf      =  _sp_flush(),
            _sflim   =  _call_stack_seg_hi,
            _sl,
            _save_sf =  _0,
            _save_sflim,
        ;
    dlocal Throw_tag;

    define lconstant Unwind() with_nargs 1;
        if _int(dup()) == @@(csword){_call_stack_hi, _caller_sp()} then
            ->
        else
            _fast_chainfrom_caller(Unwind)
        endif
    enddefine;

    define lconstant Unwind_nearest();
        unless _caller_sp()!SF_OWNER == catch do
            _fast_chainfrom_caller(Unwind_nearest)
        endunless
    enddefine;

    lconstant macro EVAL_RESULT =
        [if ((_stklength() _sub _sl) ->> _sl) _sgr _0 then
            _useras(_sl)
         endif;
         if exec_result then
            fast_apply(result)
         else
            result
         endif;
        ];

    lconstant DEAD_CATCH = 'Cannot throw to abandoned catcher';


    /* Code starts here */

    if Catch_ok == pop_undef then
        control_error('Cannot throw - no catchers in existence', [^Throw_tag])
    elseif Throw_tag == Catch_tag then
        if Catch_ok == nil then
            control_error(DEAD_CATCH, [^Throw_tag])
        else
            _Catch_slen -> _sl;
            EVAL_RESULT;
            fast_chain(Unwind_nearest)
        endif
    else
        Catch_ok -> was_ok;
        nil -> Catch_ok;
        if _zero(_Csl_frame_offs) then
            $-Sys$-Dlocal_frame_offset(ident Catch_ok, catch, true)
                -> _Cok_frame_offs;
            $-Sys$-Dlocal_frame_offset(ident Catch_tag, catch, true)
                -> _Ctag_frame_offs;
            $-Sys$-Dlocal_frame_offset(ident _Catch_slen, catch, true)
                -> _Csl_frame_offs
        endif
    endif;

    lconstant macro RESTORE_CATCHERS =
        [was_ok -> Catch_ok;
         if _nonzero(_save_sf) then
            _save_sf -> _sf;
            _save_sflim -> _sflim;
            repeat
                if _sf!SF_OWNER == catch then
                    true -> _sf!(csword){_Cok_frame_offs}
                endif;
                _nextframe(_sf) -> _sf;
                if _sf == _sflim then
                    quitif(_sflim == _call_stack_hi);
                    _sf!SF_NEXT_SEG_HI -> _sflim;
                    _sf!SF_NEXT_SEG_SP -> _sf
                endif
             endrepeat
         endif;
        ];

    lconstant macro FIND_CATCH_SF =
        [repeat
            _nextframe(_sf) -> _sf;
            if _sf == _sflim then
                if _sflim == _call_stack_hi then
                    RESTORE_CATCHERS;
                    control_error('Cannot throw - catcher not found',
                                  conspair(Throw_tag, []))
                else
                    _sf!SF_NEXT_SEG_HI -> _sflim;
                    _sf!SF_NEXT_SEG_SP -> _sf
                endif
            endif;
            quitif(_sf!SF_OWNER == catch)
         endrepeat;
        ];

    /* Now try remaining catchers. The first catch frame is NOT the one
        we want (we've already checked above), but it contains the tag,
        ok flag, and stack length of the PREVIOUS catch frame, which
        might be the target.
    */

    repeat
        FIND_CATCH_SF;
        _sf!(csword){_Cok_frame_offs} -> ok;
        _sf!(csword){_Ctag_frame_offs} -> tag;
        if tag == Throw_tag then
            if ok == nil then
                control_error(DEAD_CATCH, [^Throw_tag])
            else
                _sf!(csword){_Csl_frame_offs} -> _sl;
                FIND_CATCH_SF;
                EVAL_RESULT;
                fast_chain(_pint(@@(csword){_call_stack_hi, _sf}), Unwind)
            endif
        else
            if ok /== nil then
                nil -> _sf!(csword){_Cok_frame_offs};
                if _zero(_save_sf) then
                    ;;; save info for first catch that we've clobbered
                    _sf -> _save_sf;
                    _sflim -> _save_sflim
                endif
            endif
        endif
    endrepeat
enddefine;


/* Symbols and Packages */

struct PACKAGE
  { full    PKG_VECTOR,
            KEY,
>->         PKG_NAMES,
            PKG_SHADOWS,
            PKG_USES,
            PKG_USERS;
  };


struct PKG_ENTRY
  { full    PGE_NEXT,
            KEY,
>->         PGE_SYM;
  };


struct FUNCTION_TOKEN
  { full    W_IDENTIFIER,
            KEY,
>->         FTOK_SYM,
            FTOK_FLAGS;
  };


struct SYMBOL
  { full    W_IDENTIFIER,
            KEY,
>->         SYM_NAME,
            SYM_FTOK,
            SYM_PKG,
            SYM_PLIST;
  };


define lconstant Hash_token() with_nargs 1;
    _pint($-Sys$-Bytevec_hashint(()!SYM_NAME))
enddefine;

define lconstant Hash_package() with_nargs 1;
    _pint($-Sys$-Bytevec_hashint(()!PKG_NAMES!P_FRONT))
enddefine;

define lconstant Hash_pkg_entry() with_nargs 1;
    _pint($-Sys$-Bytevec_hashint(()!PGE_SYM!SYM_NAME))
enddefine;

define lconstant Lisp_obj_print() with_nargs 1;
    $-Sys$-Default_print(dup(), recursive_front(()!SECOND))
enddefine;

constant procedure (sf_token);

define lconstant Apply_symbol(nargs, sym);
    unless isinteger(nargs) and nargs fi_>= 0 do
        mishap(nargs, sym, 2,
                'MUST SPECIFY NUMBER OF ARGUMENTS WHEN CALLING SYMBOL')
    endunless;
    nargs -> LISP_N_ARGS;
    fast_chain(valof(sf_token(sym)))
enddefine;



constant

    package_key = struct KEY_R =>> {%
        _NULL,                      ;;; K_GC_RELOC
        key_key,                    ;;; KEY
        _:M_K_SPECIAL_RECORD
            _biset _:M_K_WRITEABLE, ;;; K_FLAGS
        _:GCTYPE_FULLREC,           ;;; K_GC_TYPE
        $-Sys$-Record_getsize,      ;;; K_GET_SIZE

        "package",                  ;;; K_DATAWORD
        false,                      ;;; K_SPEC
        false,                      ;;; K_RECOGNISER
        WREF $-Sys$-Exec_nonpd,     ;;; K_APPLY
        nonop ==,                   ;;; K_SYS_EQUALS
        WREF nonop ==,              ;;; K_EQUALS
        Lisp_obj_print,             ;;; K_SYS_PRINT
        WREF Lisp_obj_print,        ;;; K_PRINT
        WREF Hash_package,          ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,       ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,        ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,       ;;; K_EXTERN_TYPE
        _0,                         ;;; K_SPARE_BYTE

        @@(struct PACKAGE)++,       ;;; K_RECSIZE_R
        false,                      ;;; K_CONS_R
        false,                      ;;; K_DEST_R
        false,                      ;;; K_ACCESS_R
        %},

    package_internal_entry_key = struct KEY_R =>> {%
        _NULL,                      ;;; K_GC_RELOC
        key_key,                    ;;; KEY
        _:M_K_SPECIAL_RECORD
            _biset _:M_K_WRITEABLE, ;;; K_FLAGS
        _:GCTYPE_FULLREC2,          ;;; K_GC_TYPE
        $-Sys$-Rec2_getsize,        ;;; K_GET_SIZE

        "package_internal_entry",   ;;; K_DATAWORD
        false,                      ;;; K_SPEC
        false,                      ;;; K_RECOGNISER
        WREF $-Sys$-Exec_nonpd,     ;;; K_APPLY
        nonop ==,                   ;;; K_SYS_EQUALS
        WREF nonop ==,              ;;; K_EQUALS
        Lisp_obj_print,             ;;; K_SYS_PRINT
        WREF Lisp_obj_print,        ;;; K_PRINT
        WREF Hash_pkg_entry,        ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,       ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,        ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,       ;;; K_EXTERN_TYPE
        _0,                         ;;; K_SPARE_BYTE

        @@(struct PKG_ENTRY)++,     ;;; K_RECSIZE_R
        false,                      ;;; K_CONS_R
        false,                      ;;; K_DEST_R
        false,                      ;;; K_ACCESS_R
        %},

    package_external_entry_key = struct KEY_R =>> {%
        _NULL,                      ;;; K_GC_RELOC
        key_key,                    ;;; KEY
        _:M_K_SPECIAL_RECORD
            _biset _:M_K_WRITEABLE, ;;; K_FLAGS
        _:GCTYPE_FULLREC2,          ;;; K_GC_TYPE
        $-Sys$-Rec2_getsize,        ;;; K_GET_SIZE

        "package_external_entry",   ;;; K_DATAWORD
        false,                      ;;; K_SPEC
        false,                      ;;; K_RECOGNISER
        WREF $-Sys$-Exec_nonpd,     ;;; K_APPLY
        nonop ==,                   ;;; K_SYS_EQUALS
        WREF nonop ==,              ;;; K_EQUALS
        Lisp_obj_print,             ;;; K_SYS_PRINT
        WREF Lisp_obj_print,        ;;; K_PRINT
        WREF Hash_pkg_entry,        ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,       ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,        ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,       ;;; K_EXTERN_TYPE
        _0,                         ;;; K_SPARE_BYTE

        @@(struct PKG_ENTRY)++,     ;;; K_RECSIZE_R
        false,                      ;;; K_CONS_R
        false,                      ;;; K_DEST_R
        false,                      ;;; K_ACCESS_R
        %},


    function_token_key = struct KEY_R =>> {%
        _NULL,                      ;;; K_GC_RELOC
        key_key,                    ;;; KEY
        _:M_K_SPECIAL_RECORD
            _biset _:M_K_ID_TOKEN
            _biset _:M_K_COPY
            _biset _:M_K_WRITEABLE, ;;; K_FLAGS
        _:GCTYPE_FULLREC,           ;;; K_GC_TYPE
        $-Sys$-Record_getsize,      ;;; K_GET_SIZE

        "function_token",           ;;; K_DATAWORD
        false,                      ;;; K_SPEC
        false,                      ;;; K_RECOGNISER
        WREF $-Sys$-Exec_nonpd,     ;;; K_APPLY
        nonop ==,                   ;;; K_SYS_EQUALS
        WREF nonop ==,              ;;; K_EQUALS
        Lisp_obj_print,             ;;; K_SYS_PRINT
        WREF Lisp_obj_print,        ;;; K_PRINT
        WREF Hash_token,            ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,       ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,        ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,       ;;; K_EXTERN_TYPE
        _0,                         ;;; K_SPARE_BYTE

        @@(struct FUNCTION_TOKEN)++,    ;;; K_RECSIZE_R
        false,                          ;;; K_CONS_R
        false,                          ;;; K_DEST_R
        false,                          ;;; K_ACCESS_R
        %},

    symbol_key = struct KEY_R =>> {%
        _NULL,                      ;;; K_GC_RELOC
        key_key,                    ;;; KEY
        _:M_K_SPECIAL_RECORD
            _biset _:M_K_ID_TOKEN
            _biset _:M_K_WRITEABLE, ;;; K_FLAGS
        _:GCTYPE_FULLREC,           ;;; K_GC_TYPE
        $-Sys$-Record_getsize,      ;;; K_GET_SIZE

        "symbol",                   ;;; K_DATAWORD
        false,                      ;;; K_SPEC
        false,                      ;;; K_RECOGNISER
        WREF Apply_symbol,          ;;; K_APPLY
        nonop ==,                   ;;; K_SYS_EQUALS
        WREF nonop ==,              ;;; K_EQUALS
        Lisp_obj_print,             ;;; K_SYS_PRINT
        WREF Lisp_obj_print,        ;;; K_PRINT
        WREF Hash_token,            ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,       ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,        ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,       ;;; K_EXTERN_TYPE
        _0,                         ;;; K_SPARE_BYTE

        @@(struct SYMBOL)++,        ;;; K_RECSIZE_R
        false,                      ;;; K_CONS_R
        false,                      ;;; K_DEST_R
        false,                      ;;; K_ACCESS_R
        %},

    ;


/* Dummy symbols for NIL and T */

constant Nil_sym
    = writeable struct SYMBOL
        =>> {% ident nil, symbol_key, 'NIL', 0, nil, nil %};

constant T_sym
    = writeable struct SYMBOL
        =>> {% ident true, symbol_key, 'T', 0, nil, nil %};


/* Symbols */

lvars Non_symbol_error = "SYMBOL";

define lconstant Checkr_symbol(item) -> item;
    unless iscompound(item) and item!KEY == symbol_key do
        if item == nil then
            Nil_sym -> item
        elseif item == true then
            T_sym -> item
        else
            if isstring(Non_symbol_error) then
                name_error(item, consword(Non_symbol_error))
            else
                type_error(item, Non_symbol_error)
            endif
        endif
    endunless
enddefine;


define lconstant Return_symbol(sym) -> sym;
    if sym == Nil_sym then
        nil -> sym
    elseif sym == T_sym then
        true -> sym
    endif
enddefine;


define issymbol(item);
    (iscompound(item) and item!KEY == symbol_key)
        or item == nil
        or item == true
enddefine;


define sv_token() with_nargs 1;
    dlocal Non_symbol_error = 'VARIABLE';
    Checkr_symbol()
enddefine;


define symbol_string() with_nargs 1;
    Checkr_symbol()!SYM_NAME
enddefine;


define symbol_package() with_nargs 1;
    Checkr_symbol()!SYM_PKG
enddefine;


define symbol_plist() with_nargs 1;
    Checkr_symbol()!SYM_PLIST
enddefine;


define updaterof symbol_plist() with_nargs 2;
    -> Checkr_symbol()!SYM_PLIST
enddefine;


define lconstant Newsymbol(name, pkg) -> sym;
    $-Sys$-Get_record(symbol_key) -> sym;
    name -> sym!SYM_NAME;
    0 ->> sym!W_IDENTIFIER -> sym!SYM_FTOK;
    pkg -> sym!SYM_PKG;
    [] -> sym!SYM_PLIST;
enddefine;


define lconstant Checkr_sym_name(item, sym_ok);
    lvars array, vec, _lo, _hi;
    if isstring(item) then
        item
    elseif (isarray(item) ->> array)
    and array!PD_NARGS == _1
    and isstring(array!PD_ARRAY_VECTOR ->> vec) then
        array!PD_ARRAY_MIN_SUBSCR -> _lo;
        if isinteger(item!PD_PROPS) then
            item!PD_PROPS
        else
            array!PD_ARRAY_MAX_SUBSCR
        endif -> _hi;
        if _lo == 1 and _hi == _pint(vec!V_LENGTH) then
            vec
        else
            substring(_lo, _hi fi_- _lo fi_+ 1, vec)
        endif
    elseif sym_ok then
        fast_apply(symbol_string(item), sym_ok)
    else
        type_error(item, "STRING")
    endif
enddefine;


define lconstant Checkr_pkg_name() with_nargs 1;
    dlocal Non_symbol_error = 'PACKAGE';
    Checkr_sym_name(identfn)
enddefine;


define get_simple_string() with_nargs 1;
    dlocal Non_symbol_error = [OR STRING SYMBOL];
    Checkr_sym_name(copy)
enddefine;


define make_symbol() with_nargs 1;
    Newsymbol(get_simple_string(), [])
enddefine;


define copy_symbol(sym, cpy) -> newsym;
    Checkr_symbol(sym) -> sym;
    Newsymbol(sym!SYM_NAME, nil) -> newsym;
    if pop_true(cpy) then
        if iscompound(sym!W_IDENTIFIER) then
            copy(sym!W_IDENTIFIER) -> newsym!W_IDENTIFIER
        endif;
        if iscompound(sym!SYM_FTOK) then
            copy(sym!SYM_FTOK) ->> cpy -> newsym!SYM_FTOK;
            if iscompound(cpy!W_IDENTIFIER) then
                copy(cpy!W_IDENTIFIER) -> cpy!W_IDENTIFIER
            endif
        endif;
        copylist(sym!SYM_PLIST) -> newsym!SYM_PLIST
    endif
enddefine;


/* Function tokens */

define isfunction_token(item);
    if iscompound(item) and item!KEY == function_token_key then
        Return_symbol(item!FTOK_SYM)
    else
        false
    endif
enddefine;


define lconstant Checkr_function_token(item) -> item;
    unless iscompound(item) and item!KEY == function_token_key do
        mishap(item, 1, 'FUNCTION TOKEN NEEDED')
    endunless
enddefine;


define ft_name() with_nargs 1;
    Checkr_function_token()!FTOK_SYM
enddefine;


define ft_flags() with_nargs 1;
    Checkr_function_token()!FTOK_FLAGS
enddefine;


define updaterof ft_flags() with_nargs 2;
    -> Checkr_function_token()!FTOK_FLAGS
enddefine;


define lconstant Cons_function_token(sym) -> ftok;
    $-Sys$-Get_record(function_token_key) -> ftok;
    0 -> ftok!W_IDENTIFIER;
    sym -> ftok!FTOK_SYM;
    1 -> ftok!FTOK_FLAGS;           ;;; 1 = function
enddefine;


define sf_token(sym) -> ftok;
    dlocal Non_symbol_error = 'FUNCTION';
    Checkr_symbol(sym) -> sym;
    unless iscompound(sym!SYM_FTOK ->> ftok) do
        Cons_function_token(sym) ->> ftok -> sym!SYM_FTOK
    endunless
enddefine;


define consfunction_token() with_nargs 1;
    Cons_function_token(Checkr_symbol())
enddefine;


define set_undef_clos() with_nargs 1;
    $-Sys$-Set_undef_clos()
enddefine;


/* Packages */

lvars Packages = [];

lconstant procedure Word_to_pkg = newproperty([], 31, false, "perm");


vars
    keyword_package     =   false,
    lisp_package        =   false,
    current_package     =   false,
    ;


define list_all_packages();
    copylist(Packages)
enddefine;


define lconstant Name_to_package() with_nargs 1;
    Word_to_pkg(consword())
enddefine;


define updaterof Name_to_package();
    -> Word_to_pkg(consword())
enddefine;


define ispackage(item);
    iscompound(item) and item!KEY == package_key
enddefine;


define find_package(name);
    if ispackage(name) then
        name
    else
        Name_to_package(Checkr_pkg_name(name))
    endif
enddefine;


define lconstant Checkr_package(item) -> item;
    lvars p;
    unless ispackage(item) do
        if item == false then
            current_package -> item
        elseif (find_package(item) ->> p) then
            p -> item
        else
            mishap(item, 1, 'UNKNOWN PACKAGE(NAME)')
        endif
    endunless
enddefine;


define lconstant Check_not_keyword_pkg() with_nargs 1;
    if () == keyword_package then
        package_error('Illegal operation on keyword package', [],
                      keyword_package)
    endif
enddefine;


define lconstant Dup_pkg_name_err() with_nargs 1;
    mishap(1, 'PACKAGE NAME ALREADY IN USE')
enddefine;


define package_name() with_nargs 1;
    Checkr_package()!PKG_NAMES!P_FRONT
enddefine;


define package_nicknames() with_nargs 1;
    Checkr_package()!PKG_NAMES!P_BACK
enddefine;


define package_use_list() with_nargs 1;
    Checkr_package()!PKG_USES
enddefine;


define package_used_by_list() with_nargs 1;
    Checkr_package()!PKG_USERS
enddefine;


define package_shadowing_symbols(pkg);
    Checkr_package(pkg)!PKG_SHADOWS -> pkg;
    [% fast_appproperty(pkg, procedure() with_nargs 2;
                                ->;
                                Return_symbol(!PGE_SYM)
                             endprocedure) %]
enddefine;


/* Making and renaming packages */

constant procedure use_package;

define lconstant Newpackage(names, size) -> pkg;
    lvars shadows;
    $-Sys$-Initv($-Sys$-Prop$-Round_tabsize(size), 0) -> size;
    newproperty([], 16, false, "perm") -> shadows;
    $-Sys$-Get_record(package_key) -> pkg;
    size -> pkg!PKG_VECTOR;
    names -> pkg!PKG_NAMES;
    shadows -> pkg!PKG_SHADOWS;
    [] ->> pkg!PKG_USES -> pkg!PKG_USERS;
    until names == [] do
        pkg -> Name_to_package(fast_destpair(names) -> names)
    enduntil;
    conspair(pkg, Packages) -> Packages
enddefine;


define make_package(names, used, size) -> pkg;
    lvars name, u;

    /* Check names unused */
    [% until endp(names) do
        Checkr_pkg_name(fast_destpair(names) -> names) -> name;
        if Name_to_package(name) then
            Dup_pkg_name_err(name)
        endif;
        name
    enduntil %] -> names;

    Newpackage(names, size) -> pkg;

    unless endp(used) do
        /* First package used cannot cause name conflicts when pkg is new */
        Checkr_package(used!P_FRONT) -> u;
        [^u] -> pkg!PKG_USES;
        conspair(pkg, u!PKG_USERS) -> u!PKG_USERS;

        /* Now call use_package on the rest */
        use_package(used!P_BACK, pkg)
    endunless
enddefine;


define delete_package(pkg);
    lvars save, names;
    lconstant DELETED_PKG_NAMES = [[]];

    pkg -> save;
    unless (find_package(pkg) ->> pkg) do
        mishap(save, 1, 'CANNOT DELETE NON-EXISTENT PACKAGE')
    endunless;

    pkg!PKG_NAMES -> names;
    if names == DELETED_PKG_NAMES then
        false
    else
        unless pkg!PKG_USERS == [] do
            package_error(
                'Cannot delete package - it\'s still being used', [^pkg], pkg)
        endunless;
        until names == [] do
            false -> Name_to_package(fast_destpair(names) -> names)
        enduntil;
        DELETED_PKG_NAMES -> pkg!PKG_NAMES;
        fast_ncdelete(pkg, Packages, nonop ==) -> Packages;
        true
    endif
enddefine;


define rename_package(pkg, names) -> pkg;
    lvars name, p, old;
    Checkr_package(pkg) -> pkg;

    /* Check names unused (except by this package) */
    [% until endp(names) do
        Checkr_pkg_name(fast_destpair(names) -> names) -> name;
        if (Name_to_package(name) ->> p)
        and p /== pkg then
             Dup_pkg_name_err(name)
        endif;
        name
    enduntil %] -> names;

    /* Kill old names of package */
    pkg!PKG_NAMES -> old;
    unless old!P_FRONT == [] do     /* deleted package */
        until old == [] do
            false -> Name_to_package(fast_destpair(old) -> old)
        enduntil
    endunless;

    /* Assign new names to package */
    names -> pkg!PKG_NAMES;
    until names == [] do
        pkg -> Name_to_package(fast_destpair(names) -> names)
    enduntil
enddefine;


/* Symbols in packages */

vars
    symbol_status_internal
    symbol_status_external
    symbol_status_inherited
    symbol_status_new
    ;


define lconstant Find_pkg_entry(str, pkg, uselist);
    lvars vec, entry, _str, _saddr, _slen, _soffs, _voffs;
    $-Sys$-Bytevec_hashint(str) -> _str;
    str@V_WORDS -> _saddr;
    str!V_LENGTH -> _slen;
    @@(w)[_slen|b.r] -> _soffs;
    ;;; look in "current" package
    pkg!PKG_VECTOR -> vec;
    @@V_WORDS[_str _bimask (vec!V_LENGTH _sub _1)] -> _voffs;
    vec!(w){_voffs} -> entry;
    while iscompound(entry) do
        entry!PGE_SYM!SYM_NAME -> str;
        if (str!V_LENGTH == _slen) and _cmp(_soffs, str@V_WORDS, _saddr) then
            return(entry, false, _voffs)
        endif;
        entry!PGE_NEXT -> entry
    endwhile;
    ;;; look in "used" packages
    until uselist == [] do
        uselist!P_FRONT!PKG_VECTOR -> vec;
        vec!V_WORDS[_str _bimask (vec!V_LENGTH _sub _1)] -> entry;
        while iscompound(entry) do
            entry!PGE_SYM!SYM_NAME -> str;
            if (str!V_LENGTH == _slen) and _cmp(_soffs, str@V_WORDS, _saddr) then
                if entry!KEY == package_external_entry_key then
                    return(entry, true, _voffs)
                else
                    quitloop
                endif
            endif;
            entry!PGE_NEXT -> entry
        endwhile;
        uselist!P_BACK -> uselist
    enduntil;
    false, false, _voffs
enddefine;


define lconstant Insert_symbol(sym, pkg, _voffs) -> entry;
    if sym!KEY == string_key do
        Newsymbol(sym, pkg) -> sym
    elseif sym!SYM_PKG == nil then
        pkg -> sym!SYM_PKG
    endif;
    $-Sys$-Get_record(  if pkg == keyword_package then
                            package_external_entry_key
                        else
                            package_internal_entry_key
                        endif   )
        -> entry;
    sym -> entry!PGE_SYM;
    (pkg!PKG_VECTOR)@(w){_voffs} -> _voffs;
    _voffs!(w) -> entry!PGE_NEXT;
    entry -> _voffs!(w);
enddefine;


define lconstant Remove_pkg_entry(entry, pkg);
    entry!PGE_SYM -> entry;
    if entry!SYM_PKG == pkg then
        nil -> entry!SYM_PKG
    endif
enddefine;


define lconstant Intern(string, pkg, _insert);
    lvars entry, used, _voffs;
    Checkr_sym_name(string, false) -> string;
    Checkr_package(pkg) -> pkg;
    Find_pkg_entry(string, pkg, pkg!PKG_USES) -> (entry, used, _voffs);
    if entry then
        Return_symbol(entry!PGE_SYM);
        if used then
            symbol_status_inherited
        elseif entry!KEY == package_external_entry_key then
            symbol_status_external
        else
            symbol_status_internal
        endif
    else
        if _insert then
            Insert_symbol(copy(string), pkg, _voffs)!PGE_SYM
        else
            nil
        endif;
        symbol_status_new
    endif
enddefine;


define find_symbol() with_nargs 2;
    Intern(false)
enddefine;


define sysintern() with_nargs 2;
    Intern(true) ->
enddefine;


define intern() with_nargs 2;
    Intern(true)
enddefine;


define reader_intern(string, pkg);
    lvars entry, _voffs;
    consstring(string) -> string;
    Find_pkg_entry(string, pkg, pkg!PKG_USES) -> (entry, , _voffs);
    if entry then
        ;;; string not needed, no intermediate structures produced, so junk it
        string -> $-Sys$-Get_store();
        Return_symbol(entry!PGE_SYM)
    else
        Insert_symbol(string, pkg, _voffs)!PGE_SYM
    endif
enddefine;


define lconstant Check_unintern_shadowing_sym(sym, pkg);
    lvars string, other, entry, used;
    sym!SYM_NAME -> string;
    false -> other;
    for used in pkg!PKG_USES do
        Find_pkg_entry(string, used, []) -> (entry, , );
        if entry and (entry!PGE_SYM ->> entry) /== sym then
            if other and (entry /== other) then
                package_error(
                    'Uninterning symbol reveals name conflict', [^sym], pkg)
            else
                entry -> other
            endif
        endif
    endfor
enddefine;


define unintern(sym, pkg);
    lvars vec, _ptr, entry;
    Checkr_symbol(sym) -> sym;
    Checkr_package(pkg) -> pkg;
    pkg!PKG_VECTOR -> vec;
    vec@V_WORDS[$-Sys$-Bytevec_hashint(sym!SYM_NAME) _bimask (vec!V_LENGTH _sub _1)]
        -> _ptr;
    while iscompound(_ptr!(w) ->> entry) do
        if entry!PGE_SYM == sym then
            if fast_apply(entry, pkg!PKG_SHADOWS) then
                ;;; check used packages for similarly named exports
                Check_unintern_shadowing_sym(entry!PGE_SYM, pkg);
                false -> pkg!PKG_SHADOWS(entry)
            endif;
            entry!PGE_NEXT -> _ptr!(w);
            Remove_pkg_entry(entry, pkg);
            return(true)
        endif;
        entry@PGE_NEXT -> _ptr
    endwhile;
    nil
enddefine;


/* Importing and exporting symbols */

define lconstant Checkr_syms(syms) -> n;
    0 -> n;
    if ispair(syms) then
        until endp(syms) do
            Checkr_symbol(fast_destpair(syms) -> syms);
            n fi_+ 1 -> n
        enduntil
    elseunless syms == [] do
        Checkr_symbol(syms);
        1 -> n
    endif
enddefine;


define lconstant Sym_absent_err(sym, pkg);
    package_error('Symbol not accessible in package', [^sym ^pkg], pkg)
enddefine;


define export(sym, pkg);
    lvars name, p, entry, used, _voffs;
    Checkr_package(pkg) -> pkg;
    fast_repeat Checkr_syms(sym) times
            -> sym;
        sym!SYM_NAME -> name;
        ;;; check symbol not accessible in packages that use this one
        for p in pkg!PKG_USERS do
            Find_pkg_entry(name, p, p!PKG_USES) -> (entry, , );
            if entry then
                unless entry!PGE_SYM == sym
                or fast_apply(entry, p!PKG_SHADOWS) do
                    package_error(
                        'Cannot export ~S from ~S because ~S is already present in ~S which uses ~S',
                        [% sym, pkg, entry!PGE_SYM, p, pkg %], pkg)
                endunless
            endif
        endfor;
        ;;; now locate symbol and make it external
        Find_pkg_entry(name, pkg, pkg!PKG_USES) -> (entry, used, _voffs);
        if entry and (entry!PGE_SYM == sym) then
            if used then
                Insert_symbol(sym, pkg, _voffs) -> entry
            endif;
            package_external_entry_key -> entry!KEY
        else
            Sym_absent_err(sym, pkg)
        endif
    endfast_repeat
enddefine;


define unexport(sym, pkg);
    lvars entry, used;
    Checkr_package(pkg) -> pkg;
    Check_not_keyword_pkg(pkg);
    fast_repeat Checkr_syms(sym) times
        -> sym;
        Find_pkg_entry(sym!SYM_NAME, pkg, pkg!PKG_USES) -> (entry, used, );
        if entry and (entry!PGE_SYM == sym) then
            unless used do
                package_internal_entry_key -> entry!KEY
            endunless
        else
            Sym_absent_err(sym, pkg)
        endif
    endfast_repeat
enddefine;


define import(sym, pkg);
    lvars entry, _voffs;
    Checkr_package(pkg) -> pkg;
    fast_repeat Checkr_syms(sym) times
        -> sym;
        Find_pkg_entry(sym!SYM_NAME, pkg, pkg!PKG_USES) -> (entry, , _voffs);
        if entry then
            unless entry!PGE_SYM == sym do
                package_error(
                    'Cannot import ~S into ~S because ~S already present',
                    [% sym, pkg, entry!PGE_SYM %], pkg)
            endunless
        else
            Insert_symbol(sym, pkg, _voffs) ->
        endif
    endfast_repeat
enddefine;


define shadowing_import(sym, pkg);
    lvars shadows, entry, _voffs;
    Checkr_package(pkg) -> pkg;
    pkg!PKG_SHADOWS -> shadows;
    fast_repeat Checkr_syms(sym) times
        -> sym;
        Find_pkg_entry(sym!SYM_NAME, pkg, []) -> (entry, , _voffs);
        if entry then
            unless entry!PGE_SYM == sym do
                unless pkg == keyword_package do
                    package_internal_entry_key -> entry!KEY
                endunless;
                sym -> entry!PGE_SYM;
                Remove_pkg_entry(entry, pkg)
            endunless
        else
            Insert_symbol(sym, pkg, _voffs) -> entry
        endif;
        true -> shadows(entry)
    endfast_repeat
enddefine;

define shadow(syms, pkg);
    lvars shadows, item, string, entry, _voffs;
    unless ispair(syms) or syms == [] do
        conspair(syms, []) -> syms
    endunless;
    Checkr_package(pkg) -> pkg;
    pkg!PKG_SHADOWS -> shadows;
    fast_for item in syms do
        get_simple_string(item) -> string;
        Find_pkg_entry(string, pkg, []) -> (entry, , _voffs);
        unless entry do
            Insert_symbol(copy(string), pkg, _voffs) -> entry
        endunless;
        true -> shadows(entry)
    endfast_for
enddefine;


/* Using packages */

define lconstant Checkr_pkgs(pkgs) -> n;
    0 -> n;
    if ispair(pkgs) then
        until endp(pkgs) do
            Checkr_package(fast_destpair(pkgs) -> pkgs);
            n fi_+ 1 -> n
        enduntil
    elseunless pkgs == [] do
        Checkr_package(pkgs);
        1 -> n
    endif
enddefine;


define lconstant Check_pkg_externals(used, user);
    lvars uselist, shadows, vec, i, bkt, sym, entry;

    user!PKG_USES -> uselist;
    user!PKG_SHADOWS -> shadows;

    used!PKG_VECTOR -> vec;
    fast_for i from 1 to fast_vector_length(vec) do
        fast_subscrv(i, vec) -> bkt;
        while iscompound(bkt) do
            if bkt!KEY == package_external_entry_key then
                bkt!PGE_SYM -> sym;
                Find_pkg_entry(sym!SYM_NAME, user, uselist) -> (entry, , );
                if entry then
                    unless entry!PGE_SYM == sym
                    or fast_apply(entry, shadows) do
                        package_error(
                            '~S cannot use ~S because the latter exports ~S, and the former contains ~S',
                            [% user, used, sym, entry!PGE_SYM %], user)
                    endunless
                endif
            endif;
            bkt!PGE_NEXT -> bkt
        endwhile
    endfast_for
enddefine;


define use_package(used, user);
    Checkr_package(user) -> user;
    fast_repeat Checkr_pkgs(used) times
        -> used;
        nextif(used == user);
        Check_not_keyword_pkg(used);
        unless lmember(used, user!PKG_USES) do
            Check_pkg_externals(used, user);
            conspair(used, user!PKG_USES) -> user!PKG_USES;
            conspair(user, used!PKG_USERS) -> used!PKG_USERS
        endunless
    endfast_repeat
enddefine;


define unuse_package(used, user);
    lvars uselist;
    Checkr_package(user) -> user;
    user!PKG_USES -> uselist;
    fast_repeat Checkr_pkgs(used) times
        -> used;
        Check_not_keyword_pkg(used);
        fast_ncdelete(used, uselist, nonop ==) -> uselist;
        fast_ncdelete(user, used!PKG_USERS, nonop ==) -> used!PKG_USERS
    endfast_repeat;
    uselist -> user!PKG_USES
enddefine;


define lconstant App_package(pkg, procedure pdr, _do_internals, check_access);
    lvars _sl, _offs, _lim, entry;
    stacklength() -> _sl;
    pkg!PKG_VECTOR -> pkg;
    @@V_WORDS[_0] -> _offs;
    @@V_WORDS[pkg!V_LENGTH] -> _lim;
    while _offs _lt _lim do
        pkg!(w){_offs} -> entry;
        while iscompound(entry) do
            if (_do_internals or 
                             (entry!KEY == package_external_entry_key))
                           and (not(check_access) 
                                or check_access(entry!PGE_SYM)) then
                _CHECKUSER;
                lisp_apply(Return_symbol(entry!PGE_SYM), pdr, 1, 0)
            endif;
            entry!PGE_NEXT -> entry
        endwhile;
        @@(w){_offs}++ -> _offs
    endwhile
enddefine;


define apppackage(pkg, pdr, do_internals, do_inherits);
    lvars p;
    Checkr_package(pkg) -> pkg;
    $-Sys$-Check_procedure(pdr);
    App_package(pkg, pdr, pop_true(do_internals), false);
    if pop_true(do_inherits) then
        for p in pkg!PKG_USES do
            App_package(p, pdr, false,
                 procedure(sym);
                     lvars entry, name;
                     sym!SYM_NAME -> name;
                     Find_pkg_entry(name, pkg, pkg!PKG_USES) -> (entry, , );
                     entry and (entry!PGE_SYM == sym);
                 endprocedure)
        endfor
    endif
enddefine;


define make_package_iterator(pkglist, types);
    lvars item, want_internal, want_external, want_inherited,
            pkg, uselist, donelist, vsub, vlen, entry;

    if pkglist == [] or types == [] then
        return( procedure(); nil, nil, nil, nil endprocedure )
    endif;

    if ispair(pkglist) then
        destlist(pkglist);
        [] -> pkglist;
        fast_repeat (/* result of destlist */) times
            Checkr_package() -> pkg;
            unless fast_lmember(pkg, pkglist) do
                conspair(pkg, pkglist) -> pkglist
            endunless
        endfast_repeat
    else
        [^(Checkr_package(pkglist))] -> pkglist
    endif;

    false ->> want_internal ->> want_external -> want_inherited;
    until endp(types) do
        fast_destpair(types) -> (item, types);
        if item == symbol_status_internal then
            true -> want_internal
        elseif item == symbol_status_external then
            true -> want_external
        elseif item == symbol_status_inherited then
            true -> want_inherited
        else
            type_error(item,
                [% "MEMBER", "':INTERNAL'", "':EXTERNAL'", "':INHERITED'" %])
        endif
    enduntil;

    false -> pkg;
    [] -> uselist;
    [] -> donelist;
    1 -> vsub;
    0 -> vlen;
    0 -> entry;

    define lconstant Next_sym();
        lvars key, sym;
        until iscompound(entry) do
            if vsub > vlen then
                ;;; Get next package
                repeat
                    if uselist == []
                    or not(want_inherited) then
                        if ispair(pkglist) then
                            fast_destpair(pkglist) -> (pkg, pkglist);
                            false -> uselist;
                            quitloop
                        else
                            return(nil, nil, nil, nil)
                        endif
                    else
                        unless uselist do
                            pkg!PKG_USES -> uselist;
                            pkg -> want_inherited
                                    ;;; save for returning as 4th result, below
                        endunless;
                        if ispair(uselist)
                        and (fast_destpair(uselist) -> (pkg, uselist),
                             not(fast_lmember(pkg, donelist)))
                        then
                            conspair(pkg, donelist) -> donelist;
                            quitloop
                        endif
                    endif
                endrepeat;
                1 -> vsub;
                fast_vector_length(pkg!PKG_VECTOR) -> vlen
            else
                ;;; Get next entry chain from PKG_VECTOR
                fast_subscrv(vsub, pkg!PKG_VECTOR) -> entry;
                vsub fi_+ 1 -> vsub
            endif
        enduntil;

        entry!KEY -> key;
        Return_symbol(entry!PGE_SYM) -> sym;
        entry!PGE_NEXT -> entry;

        if uselist then
            returnif
                (key == package_external_entry_key)
                (true, sym, symbol_status_inherited, want_inherited)
        elseif key == package_internal_entry_key then
            returnif
                (want_internal) (true, sym, symbol_status_internal, pkg)
        else
            returnif
                (want_external) (true, sym, symbol_status_external, pkg)
        endif;

        fast_chain(Next_sym)
    enddefine;

    Next_sym
enddefine;


define apropos_list(string, pkgs) -> list;
    dlvars string, list = [];
    lvars do_inherits, pkg;

    Checkr_sym_name(string, identfn) -> string;

    if pop_true(pkgs) then
        [^pkgs], true
    else
        Packages, false
    endif -> (pkgs, do_inherits);

    fast_for pkg in pkgs do
            pkg,
        apppackage(
            procedure(sym);
                if issubstring(string, symbol_string(sym)) then
                    conspair(sym, list) -> list
                endif
            endprocedure,
            true,
            do_inherits)
    endfast_for
enddefine;


/* Basic character handling */

constant character_key, character_table;

struct CHARACTER
    { int       CHAR_CODE;      ;;; Could be byte really
      full      KEY;
>->
    };

define lconstant Char_print(char);
    $-Sys$-Default_print("character", _pint(char!CHAR_CODE))
enddefine;

define lconstant Char_hash() with_nargs 1;
    _pint(()!CHAR_CODE);
enddefine;

define Check_char(item);
    unless iscompound(item) and item!KEY == character_key do
        type_error(item, "CHARACTER")
    endunless
enddefine;

define ischaracter(item);
    iscompound(item) and item!KEY == character_key
enddefine;

define fast_char_code() with_nargs 1;
    _pint(()!CHAR_CODE)
enddefine;

define fast_code_char() with_nargs 1;
    lvars _i;
    _int() _bimask _16:FF -> _i;
    character_table!V_WORDS[_i]
enddefine;

define conscharacter() with_nargs 1;
    fast_code_char($-Sys$-Checkr_dchar())
enddefine;

define destcharacter(char);
    Check_char(char);
    fast_char_code(char)
enddefine;

define char_code(char);
    Check_char(char);
    fast_char_code(char)
enddefine;

define updaterof char_code() with_nargs 2;
    mishap(2, 'CANNOT ALTER CHARACTER CODE')
enddefine;

define lisp_subscrs(index, string);
    character_table!V_WORDS[string!V_BYTES[_int(index) _sub _1]]
enddefine;

define updaterof lisp_subscrs(char, index, string);
    Check_char(char);
    char!CHAR_CODE -> string!V_BYTES[_int(index) _sub _1]
enddefine;

define fast_schar(string, index);
    character_table!V_WORDS[string!V_BYTES[_int(index)]]
enddefine;

define updaterof fast_schar(char, string, index);
    Check_char(char);
    char!CHAR_CODE -> string!V_BYTES[_int(index)]
enddefine;


/* Define character key */

constant
    character_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_RECORD _biset _:M_K_NONWRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_NONE,          ;;; K_GC_TYPE
        $-Sys$-Record_getsize,  ;;; K_GET_SIZE

        "character",            ;;; K_DATAWORD
        [32],                   ;;; K_SPEC
        ischaracter,            ;;; K_RECOGNISER
        WREF $-Sys$-Exec_nonpd, ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Char_print,             ;;; K_SYS_PRINT
        WREF Char_print,        ;;; K_PRINT
        WREF Char_hash,         ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct CHARACTER)++, ;;; K_RECSIZE_R
        conscharacter,          ;;; K_CONS_R
        destcharacter,          ;;; K_DEST_R
        {^char_code},           ;;; K_ACCESS_R
        %},
    ;


/* Set up character table */

lvars i;
constant character_table
        = {% for i from 0 to 255 do
                struct CHARACTER =>> {% _int(i), character_key %}
          endfor %};


/* Some vector, array, and hash-table utility procedures */

define lisp_fast_subscr_p() with_nargs 1;
    lvars key;
    ()!KEY -> key;
    if key == string_key then
        lisp_subscrs
    else
        key!K_FAST_SUBSCR_V
    endif
enddefine;


define fast_1d_array_info(array, outer);
    array!PD_ARRAY_VECTOR;
    dup()!KEY;
    array!PD_ARRAY_MIN_SUBSCR;
    if isinteger(outer!PD_PROPS) then
        outer!PD_PROPS
    else
        array!PD_ARRAY_MAX_SUBSCR
    endif;
    array!PD_ARRAY_SUBSCR_PDR
enddefine;


define fast_array_info(array, outer);
    lvars r;
    _pint(array!PD_NARGS) ->> r;
    array!PD_ARRAY_BOUNDSLIST;
    array!PD_ARRAY_VECTOR;
    array!PD_ARRAY_MIN_SUBSCR;
    if r == 1 and isinteger(outer!PD_PROPS) then
        outer!PD_PROPS, true
    else
        array!PD_ARRAY_MAX_SUBSCR, false
    endif;
    array!PD_ARRAY_SUBSCR_PDR
enddefine;


define checkr_hash_table(prop) -> prop;
    unless isproperty(prop) do
        type_error(prop, "'HASH-TABLE'")
    endunless
enddefine;


define hash_table_info(prop);
    checkr_hash_table(prop)!PD_CLOS_FROZVALS[_0] -> prop;
    fast_vector_length(prop!PT_TABLE),
    prop!PT_COUNT,
    prop!PT_EXPAND,
    prop!PT_EQ_PDR
enddefine;


define round_hash_table_size() with_nargs 1;
    $-Sys$-Prop$-Round_tabsize()
enddefine;


define hash_vector_between() with_nargs 3;
    $-Sys$-Hash_vector_between()
enddefine;


define fast_record_access(_n, item);
    lvars pdrvec;
    item!KEY!K_ACCESS_R -> pdrvec;
    if _n then
        _int(_n)
    else
        pdrvec!V_LENGTH
    endif -> _n;
    returnif (_zero(_n)) (0);
    fast_apply(item, pdrvec!V_WORDS[_n _sub _1])
enddefine;


define updaterof fast_record_access(_n, item) with_nargs 3;
    lvars pdrvec;
    item!KEY!K_ACCESS_R -> pdrvec;
    if _n then
        _int(_n)
    else
        pdrvec!V_LENGTH
    endif -> _n;
    -> fast_apply(item, pdrvec!V_WORDS[_n _sub _1])
enddefine;


lconstant macro CHECKR_VECTOR_INDEX =
    [unless isinteger(i) and
     i fi_>= 0
     and _int(i) _lt vec!V_LENGTH do
         mishap(i, vec, 2,
                'VECTOR INDEX IS OUT OF RANGE', 'array-subscr:type-intrange')
     endunless;
     i fi_+ 1
    ];


define fast_vector_access(i, vec);              /* N.B. i is 0-origin */
    lvars key;
    vec!KEY -> key;
    CHECKR_VECTOR_INDEX -> i;
    fast_apply(i, vec, key!K_FAST_SUBSCR_V);
    if key == string_key then
        fast_code_char()
    endif
enddefine;


define updaterof fast_vector_access(i, vec) with_nargs 3;
                                                /* N.B. i is 0-origin */
    lvars key;
    vec!KEY -> key;
    CHECKR_VECTOR_INDEX -> i;
    if key == string_key then
        char_code()
    endif
    -> fast_apply(i, vec, key!K_FAST_SUBSCR_V)
enddefine;


define address_of() with_nargs 1;
    ;;; for PRINT-UNREADABLE-OBJECT
    if iscompound(dup()) then _pint() endif
enddefine;


endsection;     /* $-lisp */


/* --- Revision History ---------------------------------------------------
--- John Williams, Sep  5 1996
        Removed Message_sprintf (now defined in C.all/lisp/src/errors.p).
--- John Gibson, May 13 1996
        Message_printf -> sys_message_printf
--- John Williams, May 10 1996
        Added Message_sprintf (exported so Lisp can cope with Pop error
        messages that start with `%`).
--- John Williams, Apr 11 1996
        CHECKR_VECTOR_INDEX now signals an error with idstring
        'array-subscr:type-intrange' (used in lisp/src/arrays.p).
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Gibson, May 27 1995
        Dlocal_frame_offset now takes a 3rd _nonactive argument
--- John Williams, May 16 1995
        Fixed bug in call to type_error in Checkr_sym_name.
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Williams, Mar 15 1995
        Added name_error.
--- John Williams, Mar 14 1995
        Check_pkg_externals uses fast_subscrv instead of pointer arithmetic.
--- John Williams, Mar 14 1995
        Added `early' versions of type_error, control_error & package_error.
--- John Williams, Dec  6 1994
        Added updater for fast_record_access.
--- John Williams, Oct 18 1994
        Added address_of (used by PRINT-UNREADABLE-OBJECT).
--- John Williams, Jul 28 1994
        Throw now marks intervening catch frames as `dead'.
--- John Williams, Jul 25 1994
        Catch_tag now a (protected) permanent identifier. Added Throw_tag.
--- John Williams, Jun 14 1994
        Added fast_vector_access.
--- John Williams, Jun  6 1994
        Added delete_package. Package accessor functions no longer copy
        name strings or use lists. copy_symbol no longer copies symbol name.
--- John Williams, Mar 18 1994
        Added fast_vmember (for CLOS).
--- John Williams, Dec 20 1993
        Added hash_vector_between.
--- John Williams, Dec 17 1993
        Added fast_record_access.
--- John Williams, Dec 16 1993
        fast_subscr_p renamed to lisp_fast_subscr_p (and returns
        lisp_subscrs if its argument is a string).
--- John Williams, Dec 14 1993
        Added fast_subscr_p, fast*array_info.
--- John Williams, Dec 10 1993
        Added key utility procedures.
--- John Williams, Dec  9 1993
        Added checkr_hash_table, hash_table_info, and round_hash_table_size.
        Renamed Gen_package_iterator to make_package_iterator.
--- John Williams, Dec  9 1993
        Gen_package_iterator now always returns 4 values.
--- John Williams, Dec  8 1993
        Modified Gen_package_iterator's handling of uselist.
--- John Williams, Dec  7 1993
        Added Gen_package_iterator.
--- John Williams, Nov  2 1993
        Renamed package to current_package (package is now defined as
        an active variable in C.all/lisp/src/packages.p). Moved definition
        of apropos_list in from that file.
--- John Williams, Oct 29 1993
        find_package now accepts a package object, as per Steele 1990.
        rename_package now returns the renamed package, as per Steele 1990.
--- John Williams, Oct 25 1993
        shadow now accepts symbol names (i.e. strings) as per Steele 1990.
--- John Gibson, Jan  4 1993
        Moved l*ispfiletypes to vdfiles.p
--- John Williams, Nov 21 1990
        Added -closed_read_ok- (see also C.all/src/miscio.p)
--- John Williams, Sep 10 1990
        Now uses -fast_repeat- when appropriate.
        Minor change to -Checkr_sym_name-.
        -use_package- now ignores requests for a packaghe to use itself.
--- John Williams, Jul 20 1990
        Fixed GC bug in -Newpackage-
--- John Williams, Jul 19 1990
        -Apply_symbol- now checks that the symbol has a functional value
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Jan  7 1990
        Changes for new pointers.
 */
