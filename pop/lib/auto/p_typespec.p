/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/p_typespec.p
 > Purpose:         (Permanent) typespecs for defclass/defexacc/exacc etc
 > Author:          John Gibson, Apr 22 1990 (see revisions)
 > Documentation:   REF *DEFSTRUCT, REF *KEYS
 > Related Files:   LIB *TYPESPEC_UTILS, LIB *DEFCLASS, LIB *EXACC
 */
compile_mode:pop11 +strict;

uses typespec_utils;


section $-typespec_utils => p_typespec, l_typespec, i_typespec;

    /*  Defines these autoloadable syntax words (the files for the others
        just have 'uses p_typespec')
    */
lconstant names = [ l_typespec p_typespec i_typespec ];
applist(names, sysunprotect);


define def_typespec(typename, lex, spec, fldmode);
    lvars typename, spec, fldmode, lex;
    dlocal pop_pas_mode = false;

    type_idname(typename) -> typename;
    ;;; do this to check spec only
    field_spec_info(spec) -> (,);

    ;;; declare/initialise spec identifier
    if lex then
        sysLCONSTANT(typename, 0)
    else
        sysCONSTANT(typename, 0), sysGLOBAL(typename)
    endif;
    sysPASSIGN(conspair(fldmode, spec), typename);

    if popclosebracket == popclosebracket_exec then sysEXECUTE() endif
enddefine;

define lconstant x_typespec(lex);
    lvars   lex;
    dlocal  pop_autoload = false;

    repeat
        def_typespec(checkr_name(itemread(), false), lex,
                                    read_typespec(false, false) -> );

        quitif(need_nextitem([, ;]) == ";");
        quitif(nextitem() == ";")
    endrepeat;

    ";" :: proglist -> proglist
enddefine;  /* x_typespec */

    /* lexical */
define global syntax l_typespec;
    x_typespec(true)
enddefine;

    /* permanent */
define global syntax p_typespec;
    x_typespec(false)
enddefine;

    /* lex if #_INCLUDEd, perm if compiled */
define global syntax i_typespec;
    x_typespec(pop_#_include_stack)
enddefine;

applist(names, sysprotect);

endsection;     /* $-typespec_utils */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1993
        pop_pas_mode now an active var
--- John Gibson, May 22 1992
        Made -pop_pas_mode- locally false inside -def_typespec-
--- John Gibson, Jul  2 1990
        Split off from defclass.p
 */
