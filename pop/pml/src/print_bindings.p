/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/pml/src/print_bindings.p
 > Purpose:         PML: Printing top-level bindings
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

/*
 *  Printers for Environment Entries
 */

constant procedure print_signature;

define print_val(val);
    lvars val, id, ty, full = false;
    if isboolean(val) then val -> full -> val endif;
    unless pr == ml_pr then cucharout(`<`) endunless;
    fullname(val_parent(val), val_name(val)) -> id;
    val_type(val) -> ty;
    if val_isexn(val) then
        printf(id, 'exception %p');
        if val_arity(val) == 1 then
            printf(' of ');
            print_type(type_domain(ty));
        endif;
    elseif val_iscon(val) then
        printf('con ');
        if full and lookup_opr(id) then printf('op ') endif;
        printf(id, '%p : ');
        print_type(ty);
    else
        printf('val ');
        if full and lookup_opr(id) then printf('op ') endif;
        printf(id, '%p ');
        if full then
            printf('= ');
            val_value(val) -> val;
            if val == undef then
                print_any(val, false);
            else
                compile_toplevel_printer(ty)(val, false);
            endif;
            printf(' ');
        endif;
        printf(': ');
        print_type(ty);
    endif;
    unless pr == ml_pr then cucharout(`>`) endunless;
enddefine;

define print_tycon(tycon);
    lvars tycon, ty, full = false;
    if isboolean(tycon) then tycon -> full -> tycon endif;
    most_general_type(tycon) -> ty;
    unless pr == ml_pr then cucharout(`<`) endunless;
    if type_cons(ty) /== [] then
        printf('datatype ');
    elseif is_equality_type(ty) then
        printf('eqtype ');
    else
        printf('type ');
    endif;
    print_type(ty);
    if full and is_alias_type(ty) then
        printf(' = ');
        print_type(type_expand(ty));
    endif;
    unless pr == ml_pr then cucharout(`>`) endunless;
enddefine;

define print_str(str);
    lvars str, strenv, full = false;
    if isboolean(str) then str -> full -> str endif;
    unless pr == ml_pr then cucharout(`<`) endunless;
    printf(fullname(str_strname(str)), 'structure %p');
    str_env(str) -> strenv;
    if structenv_signame(strenv) then
        printf(signame_name(structenv_signame(strenv)), ' : %p');
    elseif full then
        printf(' : ');
        print_signature(strenv, str_strname(str));
    endif;
    unless pr == ml_pr then cucharout(`>`) endunless;
enddefine;

define print_sig(sig);
    lvars sig, strenv, full = false;
    if isboolean(sig) then sig -> full -> sig endif;
    unless pr == ml_pr then cucharout(`<`) endunless;
    printf(sig_name(sig), 'signature %p');
    if full then
        printf(' = ');
        sig_env(sig) -> strenv;
        if sig_signame(sig) /== structenv_signame(strenv) then
            ;;; renaming
            printf(signame_name(structenv_signame(strenv)), '%p');
        else
            ;;; defining occurrence
            print_signature(strenv);
        endif;
    endif;
    unless pr == ml_pr then cucharout(`>`) endunless;
enddefine;

define lconstant print_fnc_args(strenv);
    lvars strenv, sep = false;

    define lconstant print_seq(seq, p);
        lvars item, seq, procedure p;
        For item in seq do
            if sep then printf('; ') else true -> sep endif;
            p(item);
        endfor;
    enddefine;

    print_seq(structenv_strs(strenv), print_str);
    print_seq(structenv_tycons(strenv), print_tycon);
    print_seq(structenv_vals(strenv), print_val);
enddefine;

define print_fnc(fnc);
    lvars   fnc, strenv, full = false;
    if isboolean(fnc) then fnc -> full -> fnc endif;
    unless pr == ml_pr then cucharout(`<`) endunless;
    printf(fnc_name(fnc), 'functor %p');
    if full then
        printf('(');
        fnc_arg(fnc) -> strenv;
        if strname_name(structenv_strname(strenv)) then
            printf(strname_name(structenv_strname(strenv)), '%p : ');
            if structenv_signame(strenv) then
                printf(signame_name(structenv_signame(strenv)), '%p');
            else
                print_signature(strenv);
            endif;
        else
            print_fnc_args(strenv);
        endif;
        printf(') : ');
        fnc_env(fnc) -> strenv;
        if structenv_signame(strenv) then
            printf(signame_name(structenv_signame(strenv)), '%p');
        else
            print_signature(strenv);
        endif;
    endif;
    unless pr == ml_pr then cucharout(`>`) endunless;
enddefine;

print_val -> class_print(val_key);
print_tycon -> class_print(tycon_key);
print_str -> class_print(str_key);
print_sig -> class_print(sig_key);
print_fnc -> class_print(fnc_key);


/*
 *  Printing Top Level Bindings
 */

define print_strenv(strenv, p);
    lvars entry, p, strenv;
    For entry in structenv_strs(strenv) do
        p(entry);
    endfor;
    For entry in structenv_tycons(strenv) do
        if tycon_cons(entry) == [] then p(entry) endif;
    endfor;
    For entry in structenv_tycons(strenv) do
        if tycon_cons(entry) /== [] then p(entry) endif;
    endfor;
    For entry in structenv_vals(strenv) do
        if val_isvalcon(entry) then p(entry) endif;
    endfor;
    For entry in structenv_vals(strenv) do
        if val_isexn(entry) then p(entry) endif;
    endfor;
    For entry in structenv_vals(strenv) do
        if val_isvar(entry) then p(entry) endif;
    endfor;
enddefine;

define print_sig_binding(entry);
    lvars entry;
    sp(6);
    print_entry(entry, false);
    pr(newline);
enddefine;

define print_top_binding(entry);
    lvars entry;
    sp(2);
    print_entry(entry, true);
    pr(newline);
enddefine;

define print_signature(strenv);
    lvars   strenv, strname;
    if isstrname(strenv) then
        strenv -> strname -> strenv;
    else
        structenv_strname(strenv) -> strname;
    endif;
    printf('sig\n');
    procedure;
        dlocal  % strname_open(strname) % = true;
        print_strenv(strenv, print_sig_binding);
    endprocedure();
    printf('  end');
enddefine;

define print_str_bindings(str);
    lvars str;
    print_strenv(str_env(str), print_top_binding);
enddefine;

define print_directive(assoc, prec, ids);
    lvars id, assoc, prec, ids;
    sp(2);
    if assoc == "nonfix" then
        printf('nonfix');
    else
        printf(prec, assoc, '%p %p');
    endif;
    For id in ids do printf(id, ' %p') endfor;
    pr(newline);
enddefine;

define print_top_bindings(dec);
    lvars   dec;
    dlocal  pop_pr_quotes = false, pr = syspr;
    if isValDec(dec) or isFunDec(dec) or isExceptionDec(dec) or isTypeDec(dec)
    or isStructureDec(dec) or isSignatureDec(dec) or isFunctorDec(dec)
    then
        revapp(first(dec), print_top_binding);
    elseif isDatatypeDec(dec) then
        revapp(first(dec), print_top_binding);
        revapp(second(dec), print_top_binding);
    elseif isAbstypeDec(dec) then
        revapp(first(dec), print_top_binding);
        app(third(dec), print_top_bindings);
    elseif isLocalDec(dec) or isLocalStrDec(dec) then
        app(second(dec), print_top_bindings);
    elseif isDirective(dec) then
        print_directive(first(dec), second(dec), third(dec));
    elseif isOpenDec(dec) or isPervasiveDec(dec) then
        app(first(dec), print_str_bindings);
    elseif isExternalDec(dec) then
        print_top_binding(Front(second(dec)));
    else
        bad_syntax_tree(dec);
    endif;
enddefine;

define print_bindings(dec);
    lvars dec;
    if isStrTopDec(dec) or isSigTopDec(dec) or isFncTopDec(dec)
    or isPerTopDec(dec) or isExtTopDec(dec)
    then
        applist(first(dec), print_top_bindings);
    else
        bad_syntax_tree(dec);
    endif;
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, May 30 1991
        Fixed typo in -print_sig-
--- Robert John Duncan, Feb 11 1991
        Exceptions, constructors and variables now represented by a common
        "val" record.
        Abolished -entry*_printer-: replaced by -print*_entry- in "env.p"
--- Robert John Duncan, Feb  4 1991
        Changes for new environment interface.
        Constructors can no longer be "undeclared".
--- Robert John Duncan, Nov 13 1990
        Fix to -print_top_binding- to stop a blank line appearing for each
        "hidden" constructor.
--- Robert John Duncan, Oct  2 1990
        Added local structure declarations.
--- Robert John Duncan, Aug  8 1990
        Removed debugging code.
--- Rob Duncan, Sep 12 1989
        Changes to cope with TopDec sequences.
 */
