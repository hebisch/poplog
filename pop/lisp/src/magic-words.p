/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/magic-words.p
 > Purpose:         Magic-word facility for Common Lisp
 > Author:          John Williams, Nov  9 1989 (see revisions)
 > Documentation:   HELP * MAGIC-WORDS
 > Related Files:   C.all/lisp/src/break.p, C.all/lisp/modules/magic-words.lsp
 */

lisp_compile_mode;

section $-lisp;


/* Basics */

defprop magic_word_handler;

vars procedure is_magic_word
    = magic_word_handler;           ;;; Re-defined during BREAK


define apply_magic_word_handler(pdr);
    lisp_apply(checkr_function(pdr), 0, 0);
    true
enddefine;


/* Some built in magic-words
    (see also C.all/lisp/src/compile.p for POP11
     and C.all/lisp/modules/prolog.lsp for PROLOG)
*/

constant procedure
    (apply_in_pop11_environment, lisp_compile,
        lisploadwarn, string_trim, top_level_listen);


define lconstant Read_arg(default);
    if top_level_listen() then
        string_trim('\s\t\n', read_line(standard_input, nil, nil, nil) ->)
    else
        default
    endif
enddefine;


define lconstant BYE();
    exitfrom(lisp_compile)
enddefine;


define lconstant PWD();
    write_string(current_directory, standard_output, nil, nil, true) ->
enddefine;


define lconstant CD();
    Read_arg(popdirectory) -> current_directory;
    PWD()
enddefine;


define lconstant LIB();
    lvars file;
    unless (Read_arg(false) ->> file) do
        chain('No argument given to LIB', advise)
    endunless;
    lisploadwarn(file, true);
    apply_in_pop11_environment(file, loadlib)
enddefine;


define lconstant LOAD();
    lvars file;
    unless (Read_arg(false) ->> file) do
        chain('No argument given to LOAD', advise)
    endunless;
    lisploadwarn(file, false);
    apply_in_pop11_environment(file, loadcompiler)
enddefine;


BYE     -> magic_word_handler(@POPLOG:BYE);
CD      -> magic_word_handler(@POPLOG:CD);
LIB     -> magic_word_handler(@POPLOG:LIB);
LOAD    -> magic_word_handler(@LOAD);
PWD     -> magic_word_handler(@POPLOG:PWD);

#_IF hd(sys_os_type) == "unix"
    nonsyntax stop -> magic_word_handler(@POPLOG:STOP);
#_ENDIF


#_IF VED_LOADED

define lispvedcommand(p_name);
    Read_arg(nullstring) -> vedargument;
    if vedbufferlist = [] then
        namestring(default_pathname) -> vedvedname
    endif;
    vedputcommand(allbutfirst(4, word_string(p_name)) <> '\s' <> vedargument);
    apply_in_pop11_environment(valof(p_name))
enddefine;

lispvedcommand(% "ved_help"    %) -> magic_word_handler(@POPLOG:HELP);
lispvedcommand(% "ved_im"      %) -> magic_word_handler(@POPLOG:IM);
lispvedcommand(% "ved_ref"     %) -> magic_word_handler(@POPLOG:REF);
lispvedcommand(% "ved_showlib" %) -> magic_word_handler(@POPLOG:SHOWLIB);
lispvedcommand(% "ved_src"     %) -> magic_word_handler(@POPLOG:SRC);
lispvedcommand(% "ved_teach"   %) -> magic_word_handler(@POPLOG:TEACH);
lispvedcommand(% "ved_ved"     %) -> magic_word_handler(@POPLOG:VED);

#_ENDIF


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  1 1995
        VED identifiers now guarded with #_IF VED_LOADED.
--- John Williams, May 22 1995
        LIB and LOAD now use apply_in_pop11_environment (as do lisp_load
        and require).
--- John Williams, Jun  7 1994
        Magic words now in POPLOG package.
--- John Williams, Aug 11 1993
        sys_os_type instead of lisp_os_type.
--- John Williams, Mar 19 1993
        nonsyntax stop instead of nonmac stop
--- John Williams, Jan 15 1993
        Removed reference to C.all/lisp/src/poplisp.p (file deleted)
--- Robert John Duncan, Aug 21 1991
        Fixed conditional compilation for magic word STOP
--- John Williams, Nov  1 1990
        New interface to VED commands
--- John Williams, Jul 17 1990
        Revised for new LIB SUBSYSTEM
 */
