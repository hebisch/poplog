/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/ved/src/vdcompile.p
 > Purpose:         Provides utilities for compiling from ved buffer
 > Author:          John Gibson & Aaron Sloman (see revisions)
 */

;;; ------------ COMMANDS FOR COMPILING FILES --------------------------------

#_INCLUDE 'vddeclare.ph'

global constant
        procedure (vedappfiles, Sys$-Ved$-Lmr, Sys$-Ved$-Write_one_file),
        active (ved_chario_file),
    ;

global vars
        vedlmr_print_in_file, Sys$-Ved$-lmr_ignore_colons
    ;

;;; -----------------------------------------------------------------------

section $-Sys$-Ved =>
                    ved_pop_back_file,
                    ved_l, ved_l1, vedcompilefiles,
                    ved_c, ved_c1, ved_x, ved_x1,
                    ;

vars
    ;;; file to go to after ved_x or ved_c, etc.
    ;;; true means stay in current file, false means go to pop
    ved_pop_back_file   = false,
    ;


;;; --- LOADING FILES --------------------------------------------------

define lconstant Compile_files(do_all, pop_back);
    lvars do_all, pop_back;

    define lconstant Compile_1();
        lvars file;
        dlocal vedpositionstack, lmr_ignore_colons;
        unless vedcompileable and vedneedscompiling and vvedbuffersize /== 0
        then return
        endunless;
        vedpositionpush();
        vedcurrentfile -> file;
        vedputmessage('\{b}loading ' <> vedcurrent);
        vedmarkpush();
        false -> vvedmarkprops;
        1 -> vvedmarklo;
        vvedbuffersize -> vvedmarkhi;
        ;;; don't ignore beginning-of-line colons
        false -> lmr_ignore_colons;
        Lmr(false) -> ;
        file -> ved_current_file;
        Clear_temp_mark();
        ;;; restore position
        vedpositionpop();
        false -> vedneedscompiling;
    enddefine;

    if do_all then
        vedappfiles(Compile_1)
    else
        ;;; do current file
        true -> vedneedscompiling;
        procedure();
            dlocal vedcompileable = true;
            Compile_1();
        endprocedure();
    endif;
    if vedediting then
        if isstring(pop_back) then
            vededit(pop_back)
        elseif pop_back then
            vedsetonscreen(vedcurrentfile, false)
        else
            vedexit(identfn)
        endif
    endif;
enddefine;

define vars ved_l = Compile_files(%true, true%) enddefine;

define vars ved_l1 = Compile_files(%false, true%) enddefine;

define vedcompilefiles = ved_l <> identfn enddefine;


define lconstant Pb_compile(compile_arg, write_pdr);
    lvars compile_arg, procedure write_pdr;
    dlocal vedlmr_print_in_file, ved_pop_back_file;
    unless ved_pop_back_file then
        ved_chario_file ->> vedlmr_print_in_file -> ved_pop_back_file;
        unless ved_pop_back_file then
            pr(newline);
        endunless;
    endunless;
    write_pdr();
    Compile_files(compile_arg, ved_pop_back_file);
enddefine;

define vars ved_c   = Pb_compile(%true, identfn%) enddefine;

define vars ved_c1  = Pb_compile(%false, identfn%) enddefine;

define vars ved_x   = Pb_compile(%true, vedwritefiles%) enddefine;

define vars ved_x1  = Pb_compile(%false, Write_one_file%) enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 21 1992
        Moved vedcompilevedargument & ved_load to vdcomparg.p
--- Robert John Duncan, Mar 13 1992
        Changed -Pb_compile- to to take account of -ved_chario_file-
        if -ved_pop_back_file- is not set.
        Moved the dlocal of -vedcompileable- in -Compile_files- in case
        -pop_back- causes a different file to be selected.
--- John Gibson, Apr  5 1991
--- John Gibson, Dec 12 1990
        Removed syssleep in Compile_1
--- Aaron Sloman, Jul 10 1990
        changed vedwriteone to Write_one_file (non exported)
        ved_pop moved to vdprocess.p
--- John Gibson, Oct 17 1989
        Put into section Sys$-Ved.
--- Aaron Sloman, Apr 30 1989
        put back define ... = forms.
--- Aaron Sloman, Apr 23 1989
        Temporarily commented out uses of define ... = until POPC can
        handle them.
--- Aaron Sloman, Apr 22 1989
        Replaced pdr_valof with new syntax define foo = ...
--- Aaron Sloman, Apr  9 1989
        Replaced top-level assignments with define :pdr_valof
 */
