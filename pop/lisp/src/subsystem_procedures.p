/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/subsystem_procedures.p
 > Purpose:         Define SS_PROCEDURES field for Lisp subsystem entry
 > Author:          John Williams, Jan 13 1993 (see revisions)
 > Documentation:
 > Related Files:   LIB * LISP_SUBSYSTEM
 */

lisp_compile_mode;

section $-lisp => clisp lisp lisp_subsystem_procedures setlisp;


define lconstant Add_lisp_extn(file);
    consstring(#| explode(file), `.`, explode(pt_type(default_pathname)) |#)
enddefine;


lvars Lisp_setup_done = false;

define lvars Lisp_setup();
    unless Lisp_setup_done do
        reset_streams();
        true -> Lisp_setup_done
    endunless
enddefine;


define lconstant Lisp_vedsetup();
 #_IF VED_LOADED
    unless vedsetupdone do
        Add_lisp_extn('temp') -> vedvedname;
        'clisp' ->> vedhelpname -> vedteachname;
        'index' -> vedrefname;
        Add_lisp_extn('output')
            ->> vedlmr_errs_in_file -> vedlmr_print_in_file
    endunless
 #_ENDIF
enddefine;


define lconstant Lisp_banner();
    nprintf('%S (Version %P)', [^lisptitle ^lispversion])
enddefine;


define lconstant Lisp_initcomp();
    trylisp_compile(Add_lisp_extn('$popsys/init')) ->;
    unless trylisp_compile(Add_lisp_extn('$poplib/init')) do
        trylisp_compile(Add_lisp_extn('init')) ->
    endunless
enddefine;


weak global constant procedure
                ($-subsystem_button_selectable, $-pop_ui_add_property);

define lconstant Lisp_xsetup();
    lvars argv;
    if testdef subsystem_button_selectable then
        true -> weakref subsystem_button_selectable("lisp");
    endif;
    if testdef pop_ui_add_property then
        for argv in lisp_property_sheets do
            weakref pop_ui_add_property(explode(argv))
        endfor
    endif
enddefine;


define lconstant Lisp_poparg1();
    dlocal lisp_interrupt = sysexit;

    define lconstant Expr_comp() with_nargs 1;
        dlocal load_print = true;
        lisp_compile()
    enddefine;

    sys_process_poparg1(Expr_comp, trylisp_compile, Add_lisp_extn(''))
enddefine;


define lconstant Lisp_reset();
    reset_streams();
    nil -> read_suppress;
    10 -> read_base;
    if systrmdev(pop_charin_device) then
        charout('\nSetlisp\n')
    endif
enddefine;


global constant lisp_subsystem_procedures =
    {^lisp_compile          ;;; SS_COMPILER
     ^Lisp_reset            ;;; SS_RESET
     ^Lisp_setup            ;;; SS_SETUP
     ^Lisp_banner           ;;; SS_BANNER
     ^Lisp_initcomp         ;;; SS_INITCOMP
     ^Lisp_poparg1          ;;; SS_POPARG1
     ^Lisp_vedsetup         ;;; SS_VEDSETUP
     ^Lisp_xsetup           ;;; SS_XSETUP
    };


/* Procedures for switching into / running Poplog Common Lisp */

define global macro lisp;
    "lisp" -> sys_compiler_subsystem(`c`);
enddefine;

identof("lisp") -> identof("clisp");


define global setlisp();
    "lisp" -> subsystem;
    setpop()
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 11 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  1 1995
        Ved identifiers now guarded with #_IF VED_LOADED.
--- Robert John Duncan, Apr 27 1995
        Made Lisp_xsetup register Lisp with the UI if it's there
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Jul 12 1993
        No longer uses cons_with.
--- John Williams, Jan 25 1993
        Uses new %S option to printf
--- John Williams, Jan 18 1993
        Now includes the definitions of lisp, clisp, and setlisp.
--- John Gibson, Jan 15 1993
        Added declaration/export for lisp_subsystem_procedures
 */
