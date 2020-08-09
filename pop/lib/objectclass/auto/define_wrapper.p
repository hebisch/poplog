/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/define_wrapper.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => define_wrapper;

define lconstant iswrapper =
    newanyproperty([], 16, 1, 12, false, false, "tmparg", false, false);
enddefine;

define wrapMethodTable( wrap, mode, path, generic ); lvars procedure wrap, mode, path, generic;
    lvars field = check_mode( mode ) == UCALL_MODE and uEntriesMethodTable or cEntriesMethodTable;
    path.truncate_path -> path;
    lvars e = method_entry( path, generic.method_table, field );
    if e then
        lvars a = actionEntry( e );
        ;;; remove any existing wrapper(s)
        while isclosure(a) and iswrapper(pdpart(a)) do
            frozval(1, a) -> a;
        endwhile;
        if a.isprocedure then
            wrap(% a %) -> a;
        elseif a.isxslot then
            ;;; Have to promote slot!
            lvars G = newgeneric( '(' sys_>< xslot_name( a ) sys_>< ')' );
            update_method( a, mode, [% path.explode %], G );
            wrap(%
                if mode.check_mode == CALL_MODE then
                    G
                else
                    ;;; call the updater indirectly so that it benefits
                    ;;; from any relinking of G, otherwise even
                    ;;; optimise_objectclass("all") will miss it
                    sysPROCEDURE(pdprops(G), 2);
                        sysUCALLQ(G);
                    sysENDPROCEDURE()
                endif
            %) -> a;
        else
            internal_error()
        endif;
        a -> actionEntry( e );
    else
        mishap( 'ocnw: FOUND NO METHOD TO WRAP', [^generic] )
    endif;
enddefine;

define wrap_method( wrap, mode, classes, generic ); lvars wrap, mode, classes, generic;
    generic.detrace_generic -> generic;
    check_classes( classes );
    unlink_method( generic );
    lvars path = classes.destlist.consvector;
    wrapMethodTable( wrap, mode, path, generic );
    if mode.check_mode == CALL_MODE then
        lvars u = updater( wrap );
        if u then
            wrap_method( u, UCALL_MODE, classes, generic )
        endif;
    endif;
    true -> iswrapper(wrap);
enddefine;

define :define_form wrapper;

    lvars mode = pop11_try_nextreaditem("updaterof").flag_to_mode;
    lvars name = readitem();

    lvars (ilocals, olocals) = get_locals(true);
    if length(ilocals) == 0 then
        mishap('WRAPPER MUST HAVE ARGUMENTS', [^name]);
    elseif local_class(ilocals(1)) then
        ;;; arguments are reversed at this point, so the first is the
        ;;; procedure being wrapped
        mishap('TYPE CONSTRAINT NOT ALLOWED HERE', [%
            ilocals(1).local_name, ":", ilocals(1).local_class.class_name
        %]);
    endif;

    wrap_method(
        comp_method("enddefine", name, ilocals, olocals),
        mode,
        maplist(tl(ilocals), local_class),
        currentval(name));
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 29 1995
        Changed define_wrapper to read input and output locals using new
        procedures from "method_form.p", i.e. to be the same as
        define_method
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
--- Robert John Duncan, Oct 13 1995
        Changed to allow only one wrapper per method; this is so that
        recompiling a wrapper will replace the earlier version rather than
        duplicating it
--- Robert John Duncan, Oct  4 1995
        Popc changes, and bug fix to error-checking in define_wrapper
 */
