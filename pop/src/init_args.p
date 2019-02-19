/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/init_args.p
 > Purpose:
 > Author:          John Gibson, Aug  9 1989 (see revisions)
 > Documentation:   REF *SYSTEM
 */

;;; ------------------ INITIAL ARGUMENT PROCESSING ---------------------

#_INCLUDE 'declare.ph'

global constant
        procedure stringin,
    ;

global vars
        poparglist,
    ;


;;; ------------------------------------------------------------------------

section $-Sys => sys_process_poparg1;

#_IF DEF UNIX
lconstant macro PATH_SEP_CHAR = `:`;
#_ELSEIF DEF VMS
lconstant macro PATH_SEP_CHAR = `|`;
#_ELSEIF DEF WINDOWS
lconstant macro PATH_SEP_CHAR = `;`;
#_ELSE_ERROR
#_ENDIF


    /*  Given a searchpath environment variable/logical name -pathvar-,
        generate a list of directory names from it. Similar to the Unix PATH
        env variable (in VMS the separator char is "|" instead of ":").
    */
define Init_path_dir_list(pathvar);
    lvars sc = 0, n, len, string, pathvar;
    systranslate(pathvar) or nullstring -> string;
    datalength(string) -> len;
    [%  while sc fi_<= len do
            sc fi_+ 1 -> n;
            unless locchar(PATH_SEP_CHAR, n, string) ->> sc then
                len fi_+ 1 -> sc
            endunless;
            substring(n, sc fi_- n, string)
        endwhile
    %]
enddefine;

    /*  Given an -arg- from -poparglist-, apply -app_p- to that name
        in each of the directories from -dir_list- until one returns
        true (otherwise mishap with -nf_mess- if none do).
        -default_extn- is given to -arg- if it doesn't have one.
    */
define Init_arg_search(arg, dir_list, default_extn, nf_mess, app_p);
    lvars   arg, dir_list, default_extn, nf_mess, dir,
            ext_sub, nam_sub, procedure app_p;

    sysfileok(arg, true) -> -> ext_sub -> nam_sub -> -> -> arg;
    ;;; use default extension if there's no extension/version
    if ext_sub fi_> datalength(arg) then
        arg sys_>< default_extn -> arg
    endif;
    if nam_sub /== 1 or dir_list == [] then
        ;;; just try arg if it has a non-empty path component, or
        ;;; dir_list is empty
        #_< [^nullstring] >_# -> dir_list
    endif;

    ;;; try dirs in order, applying -app_p- to each filename
    ;;; until it returns true
    fast_for dir in dir_list do
        returnif(app_p(dir dir_>< arg))
    endfast_for;

    ;;; not found
    mishap(arg, 1, nf_mess);
    sysexit()
enddefine;


    /*  This procedure is generally run by the SS_POPARG1 procedures of
        subsystems. It processes the first argument in -poparglist-,
        if there is one.
    */
define sys_process_poparg1(expr_compiler, file_trycompiler, extn);
    lvars   extn, procedure (expr_compiler, file_trycompiler),
            arg, i, len;

    returnif(poparglist == []);
    fast_destpair(poparglist) -> poparglist -> arg;
    Check_string(arg);      ;;; could be a list ...
    datalength(arg) -> len;

    if len /== 0 and fast_subscrs(1, arg) == `:` then
        expr_compiler(stringin(consstring(#|
            fast_for i from 2 to len do
                fast_subscrs(i, arg)
            endfor;
            fast_for i in poparglist do
                `\s`, explode(i)
            endfor
        |#)))
    else
        Init_arg_search(arg,
                        Init_path_dir_list('popcomppath'),
                        extn,
                        'FILE NOT FOUND',
                        file_trycompiler)
    endif;
enddefine;



endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Sep  5 1994
        Added definition of PATH_SEP_CHAR for Windows (uses `;` from the
        PATH environment variable)
--- John Gibson, Jan 23 1993
        Removed call of interrupt at end of sys_process_poparg1
--- John Gibson, May 23 1991
        Processing of special `%` args moved to setpop.p
        (sys_process_special_popargs now defunct)
--- John Williams, Jan 11 1991
        -sys_process_poparg1- no longer locally binds -interrupt-
--- John Williams, Oct 18 1990
        Complains if `%` is followed by unrecognised special parameter
--- John Williams, Oct  9 1990
        Now uses `%` instead of `=` to flag special args.
--- Simon Nichols, Sep  7 1990
        Added -sys_process_special_popargs- and associated variables
        -pop_nobanner-, -pop_noinit- and -popunderx-.
--- John Gibson, Sep  1 1990
        File renamed from init_restore.p to init_args.p. Moved in
        -sys_process_arg1- from poplog_main.p and moved out -Init_restore-
        to sr_sys.p
--- John Gibson, Aug 30 1990
        Changed -Next_is_restore_arg- to check for string arg
 */
