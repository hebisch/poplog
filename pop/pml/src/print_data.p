/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/pml/src/print_data.p
 > Purpose:         PML: Printing values
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

vars
    ml_pr_level = 1000,
        ;;; maximum printing depth
    ml_pr_quotes = true,
        ;;; controls printing of strings
    ml_pr_places = 6,
    ml_pr_allplaces = false,
    ml_pr_exponent = false,
        ;;; control printing of reals
    ml_pr_newline = true,
        ;;; controls output from the -print- function
    bracketed,
        ;;; true if the item should be printed in parentheses
;

;;; Printing utilities:

lconstant
    macro prc = "cucharout",
;

define lconstant prs(/* s */) with_nargs 1;
    appdata(/* s, */ prc);
enddefine;

define lconstant prlpar();
    if bracketed then prc(`(`) endif;
enddefine;

define lconstant prrpar();
    if bracketed then prc(`)`) endif;
enddefine;

;;; check_level:
;;;     checks the print level and abandons printing if too deep

define lconstant check_level();
    ml_pr_level - 1 -> ml_pr_level;
    if ml_pr_level < 0 then
        prs(if bracketed then '(#)' else '#' endif);
        ;;; exit from caller
        chain(identfn, chain);
    endif;
enddefine;

;;; Printers for primitive types:

define print_any(x, bracketed);
    lvars   x;
    dlocal  bracketed, ml_pr_level;
    check_level();
    prs(if bracketed then '(-)' else '-' endif);
enddefine;

define print_fun(f, bracketed);
    lvars   f;
    dlocal  bracketed, ml_pr_level;
    check_level();
    prs(if bracketed then '(fn)' else 'fn' endif);
enddefine;

define print_unit(u, bracketed);
    lvars   u;
    dlocal  bracketed, ml_pr_level;
    check_level();
    prs('()');
enddefine;

define print_tuple(cnt);
    lvars   i, cnt, printer, arg;
    dlocal  bracketed, ml_pr_level;
    consvector(cnt) -> printer -> bracketed -> arg;
    check_level();
    prc(`(`);
    if cnt == 2 then
        Subscrv(1, printer)(Front(arg), false);
        prs(', ');
        Subscrv(2, printer)(Back(arg), false);
    else
        For i to cnt do
            Subscrv(i, printer)(Subscrv(i, arg), false);
            unless i == cnt then prs(', ') endunless;
        endfor;
    endif;
    prc(`)`);
enddefine;

define print_record(labels);
    lvars   i, label, labels, printer, record, cnt;
    dlocal  bracketed, ml_pr_level;

    define lconstant prfield(label, item, print);
        lvars label, item, print;
        if isintegral(label) then label sys_>< nullstring -> label endif;
        prs(label), prs(' = '), print(item, false);
    enddefine;

    listlength(labels) -> cnt;
    consvector(cnt) -> printer -> bracketed -> record;
    check_level();
    prlpar(); prc(`{`);
    if cnt == 2 then
        Destpair(labels) -> labels -> label;
        prfield(label, Front(record), Subscrv(1, printer));
        prs(', ');
        prfield(Front(labels), Back(record), Subscrv(2, printer));
    else
        For i to cnt do
            Destpair(labels) -> labels -> label;
            prfield(label, Subscrv(i, record), Subscrv(i, printer));
            unless i == cnt then prs(', ') endunless;
        endfor;
    endif;
    prc(`}`); prrpar();
enddefine;

define print_num(n, bracketed);
    lvars   n, outc = cucharout;
    dlocal  bracketed, ml_pr_level,
            pop_pr_ratios = false,
            pop_pr_radix = 10,
            pop_pr_exponent = ml_pr_exponent,
            pop_pr_places,
            ;;; this is needed because we change cucharout
            popgctrace = false;

    define dlocal cucharout(c);
        lvars c;
        if c == `-` then
            outc(`~`);
        elseif c == `e` or c == `s` or c == `d` then
            outc(`E`);
        elseif c /== `+` then
            outc(c);
        endif;
    enddefine;

    check_level();
    prlpar();
    if isddecimal(n) then
        if ml_pr_places <= 0 then
            if ml_pr_exponent then 0 else 1 endif -> pop_pr_places;
        else
            ml_pr_places && 16:FFFF -> pop_pr_places;
        endif;
        if ml_pr_allplaces then
            ;;; pad out with `0`
            (`0` << 16) || pop_pr_places -> pop_pr_places;
        endif;
    endif;
    syspr(n);
    prrpar();
enddefine;

define print_string(s, bracketed);
    lconstant sc = consscon("string", nullstring);
    lvars   s;
    dlocal  bracketed, ml_pr_level;
    check_level();
    prlpar();
    if ml_pr_quotes then
        s -> scon_string(sc);
        ml_pr(sc);
        nullstring -> scon_string(sc);
    else
        prs(s);
    endif;
    prrpar();
enddefine;

define print_exception(pkt, bracketed);
    lvars   pkt;
    dlocal  bracketed, ml_pr_level;
    check_level();
    prlpar();
    ml_pr(pkt);
    prrpar();
enddefine;

define print_ref(r, bracketed, p);
    lvars   r, p;
    dlocal  bracketed, ml_pr_level;
    check_level();
    prlpar(); prs("ref"); p(ml_cont(r), true); prrpar();
enddefine;

define print_list(l, bracketed, p);
    lvars   l, procedure p;
    dlocal  bracketed, ml_pr_level;
    check_level();
    prlpar(); prc(`[`);
    unless l == [] then
        repeat
            if ml_pr_level <= 0 and Back(l) /== [] then
                prs('...');
                quitloop;
            endif;
            p(Destpair(l) -> l, false);
            quitif(l == []);
            prs(', ');
            ml_pr_level - 1 -> ml_pr_level;
        endrepeat;
    endunless;
    prc(`]`); prrpar();
enddefine;


/*
 *  Printer compilation
 */

lvars
    printers,
        ;;; association list of type variables and their print functions
;

define lconstant PUSHQ(w);
    lvars w;
    if isword(w) then valof(w) else w endif;
enddefine;

define lconstant PUSH(w);
    lvars w;
    if isword(w) then sysPUSH(w) else sysPUSHQ(w) endif;
enddefine;

define lconstant CALL(w);
    lvars w;
    if isword(w) then sysCALL(w) else sysCALLQ(w) endif;
enddefine;

;;; compile_printer:
;;;     constructs a print procedure for objects of type -ty-.
;;;     The result may be pushed or called depending on the value of -push-

define compile_printer(ty, push);
    lvars arg, ty, push;
    type_deref(ty) -> ty;
    while is_alias_type(ty) do type_expand(ty) -> ty endwhile;
    if isvartype(ty) then
        push(alookup(ty, printers) or print_any);
    elseif isrecordtype(ty) then
        recordtype_deref(ty) -> ty;
        if type_fields(ty) == [] then
            push(print_unit);
        else
            unless push == CALL then sysPROCEDURE(false, 2) endunless;
            For arg in type_fields(ty) do compile_printer(arg, PUSH) endfor;
            if isinteger(type_labels(ty)) then
                sysPUSHQ(type_labels(ty)), sysCALLQ(print_tuple);
            else
                sysPUSHQ(Back(type_labels(ty))), sysCALLQ(print_record);
            endif;
            unless push == CALL then push(sysENDPROCEDURE()) endunless;
        endif;
    elseif isfuntype(ty) then
        push(print_fun);
    elseif isconstype(ty) and type_printer(ty) then
        if type_arity(ty) == 0 then
            push(type_printer(ty));
        else
            unless push == CALL then sysPROCEDURE(false, 2) endunless;
            For arg in type_arguments(ty) do compile_printer(arg, PUSH) endfor;
            sysCALLQ(type_printer(ty));
            unless push == CALL then push(sysENDPROCEDURE()) endunless;
        endif;
    else
        push(print_any);
    endif;
enddefine;

;;; compile_data_printer:
;;;     compiles a print procedure for type constructor -tycon- with
;;;     arguments -tyvars- and constructors -cons-

define compile_data_printer(tycon, tyvars, cons);
    lvars   tyvar, printer, tycon, tyvars, cons;
    dlocal  printers = [];

    define lconstant compile_switch(rep, cons);
        lvars con, lab1, lab2, rep, cons;

        define lconstant compile_con(con, tagged);
            lvars con, tagged;
            sysPUSHQ(val_name(con)), sysCALLQ(prs);
            if val_arity(con) == 1 then
                sysPUSH("item");
                if tagged then sysCALL("fast_front") endif;
                sysPUSH("true");
                compile_printer(type_domain(val_type(con)), CALL);
            endif;
        enddefine;

        go_on rep to
            t_unit t_bool t_enum t_ident error t_list t_binary t_user
        else
            error;

        t_unit:
        t_ident:
            compile_con(Front(cons), false);
            return;

        t_bool:
        t_binary:
            sysPUSH("item");
            if rep == T_BINARY then sysCALL("fast_back") endif;
            sysIFSO(sysNEW_LABEL() ->> lab1);
            compile_con(Front(cons), rep == T_BINARY);
            sysGOTO(sysNEW_LABEL() ->> lab2), sysLABEL(lab1);
            compile_con(Front(Back(cons)), rep == T_BINARY);
            sysLABEL(lab2);
            return;

        t_list:
            sysPUSH("item"), sysPUSH("nil"), sysCALL("==");
            sysNEW_LABEL() ->> lab1,
            if val_arity(Front(cons)) == 0 then sysIFNOT() else sysIFSO() endif;
            compile_con(Front(cons), false);
            sysGOTO(sysNEW_LABEL() ->> lab2), sysLABEL(lab1);
            compile_con(Front(Back(cons)), false);
            sysLABEL(lab2);
            return;

        t_enum:
            sysPUSH("item");
            sysPUSHQ({% app(cons, val_name) %});
            sysCALL("fast_subscrv");
            sysCALLQ(prs);
            return;

        t_user:
            sysNEW_LABEL() -> lab1;
            sysPUSH("item"), sysCALL("fast_back");
            sysGO_ON(map(cons, val_name), false);
            For con in cons do
                sysLABEL(val_name(con));
                compile_con(con, true);
                sysGOTO(lab1);
            endfor;
            sysLABEL(lab1);
            return;

        error:
            bad_data_rep(rep);
    enddefine;

    sysPROCEDURE("print_" <> tycon_name(tycon), listlength(tyvars) + 2);
        ;;; declare "item"; localise "bracketed" and "ml_pr_level"
        sysLVARS("item", 0); sysLOCAL(MLWID(bracketed)), sysLOCAL(MLWID(ml_pr_level));
        ;;; pop the printer arguments, one for each type argument
        For tyvar in rev(tyvars) do
            dest(pop_new_lvar_list) -> pop_new_lvar_list -> printer;
            sysDLVARS(printer, 0), sysPOP(printer);
            acons(tyvar_type(tyvar), printer, printers) -> printers;
        endfor;
        ;;; pop the "bracketed" flag and the item to print
        sysPOP(MLWID(bracketed)), sysPOP("item");
        ;;; check the print level and print an opening parenthesis
        sysCALLQ(check_level), sysCALLQ(prlpar);
        ;;; print the item according to the tycon rep and its constructors
        compile_switch(tycon_datarep(tycon), cons);
        ;;; print closing parenthesis
        sysCALLQ(prrpar);
    sysENDPROCEDURE();
enddefine;

;;; compile_print:
;;;     compiles a procedure to be used in a call to the ML "print"
;;;     function

define compile_print(ty);
    lvars   ty;
    dlocal  printers = [];
    procedure(x, p) -> x with_props print;
        lvars x, p;
        p(x, false);
        if ml_pr_newline then prc(`\n`) endif;
    endprocedure(% compile_printer(ty, PUSHQ) %);
enddefine;

;;; compile_makestring:
;;;     compiles a procedure to be used in a call to the ML "makestring"
;;;     function

define compile_makestring(ty);
    lvars   ty;
    dlocal  printers = [];
    procedure(x, p) with_props makestring;
        lvars   x, p, sl = stacklength();
        dlocal  cucharout = identfn;
        mlstring(p(x, false), stacklength() fi_- sl);
    endprocedure(% compile_printer(ty, PUSHQ) %);
enddefine;

;;; compile_toplevel_printer:
;;;     constructs a procedure for printing a top-level value of type -ty-

define compile_toplevel_printer(ty);
    lvars   ty;
    dlocal  printers = [];
    compile_printer(ty, PUSHQ);
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 30 1994
        Added dlocal popgctrace into print_num because of the local
        definition of cucharout. If print_num is called from makestring
        and causes the GC trace message to be printed, all kinds of junk
        ends up on the stack.
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Feb 11 1991
        Exceptions, constructors and variables now represented by a common
        "val" record.
        Simplified printing of strings and exceptions.
--- Robert John Duncan, Dec 13 1990
        Fixed a bug in record printing -- now allows for numeric labels
--- Rob Duncan, Jan 31 1990
        Replaced -fast_cont- with -ml_cont- for proper handling of refs
        as identifiers.
--- Rob Duncan, Jan  2 1990
        Added -ml_pr_newline-; changed -compile_print- & -compile_makestring-
        to return closures; fixed list printing; changed max printing depth
        to 1000.
--- Rob Duncan, Oct 26 1989
        Added -ml_pr_places-, -ml_pr_allplaces- and -ml_pr_exponent-;
        changed -print_num- to take account of them.
 */
