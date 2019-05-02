/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/lispved.p
 > Purpose:         Basic Common Lisp/VED interface
 > Author:          John Williams, April 27 1987 (see revisions)
 > Documentation:   HELP * LISPVED
 > Related Files:
 */

lisp_compile_mode;

section $-lisp => ved_lisp, ved_==, ved_dmr, ved_mx, ved_cmx, ved_ed;


/* Set up wiggly brackets */

define :ved_runtime_action;
    lvars type;
    for type in lispfiletypes do
        vedsetwiggle(')', type)
    endfor
enddefine;


define :ved_runtime_action;
    vedsetkey('\^[)', vedclosebrackets(% `)` %))
enddefine;


/* Lisp text item boundaries */

lvars procedure Saved_vedchartype;

define lconstant Lisp_vedchartype(char);
    lvars pdr;
    if subsystem == "lisp"
    and caller(1) /== veddocommand then
        readtable_pdr(fast_code_char(char), readtable) -> pdr;
        if pdr == white then
            `\s`
        elseif pdr == dot or pdpart(pdr) == tmac then
            `.`
        else
            `a`
        endif
    else
        Saved_vedchartype(char)
    endif
enddefine;


define :ved_runtime_action;
    vedchartype -> Saved_vedchartype;
    Lisp_vedchartype -> vedchartype;
enddefine;


/* Compilation */

define global ved_==();
    lvars string;
    lconstant Ved_status_output = make_stringout_stream(false);

    define lconstant Ved_status_interrupt();
        get_stringout_string(Ved_status_output) ->;
        'INTERRUPTED' -> vedmessage;
        vedscreenbell();
        veddointerrupt()
    enddefine;

    dlocal
        error_output        =   Err_out,
        load_print          =   true,
        lisp_interrupt      =   Ved_status_interrupt,
        standard_output     =   Ved_status_output,
        ;
    lisp_compile(make_stringin_stream(vedargument, false, false));
    get_stringout_string(Ved_status_output) -> string;
    consstring
        (#| `=`, `>`, `\s`,
           appdata(string, procedure();
                               if dup() == `\n` then ->; `\s` endif
                           endprocedure)
        |#) -> string;
    vedrestorescreen();
    vedsetstatus(string, false, true);
    vedsetcursor();
    chainfrom(vedprocesschar, vedprocesschar);
enddefine;


/* The Common Lisp function ED */

define ed(x);
    lvars name, file, tpi;
    false ->> name -> file;

    if isprocedure(x) then
        f_name(x) -> name;
        f_file(x) -> file
    elseif issymbol(x) and x /== nil then
        x -> name;
        if fboundp(x) then
            f_file(fdefinition(x))
        elseif isstring(is_global_special(x)) then
            is_global_special(x)
        elseif (type_info(x) ->> tpi) then
            tpi_file(tpi)
        else
            advise('ED: no definition known for ~S', [^x]);
            false
        endif -> file
    elseif is_instance_of(x, get_class_by_name(@CLASS)) then
        Accept_ioc(x, false) -> x;
        class_name(x) -> name;
        (type_info(name) ->> tpi) and tpi_file(tpi) -> file
    elseif is_instance_of(x, get_class_by_name(@METHOD)) then
        Accept_iom(x) -> x;
        method_name(x) -> name;
        f_file(method_body(x)) -> file
    elseif pop_true(x) then
        checkr_filename(x) -> file
    endif;

    unless isstring(file) do
        vedvedname -> file;
        if file = nullstring then
            namestring(default_pathname) ->> vedvedname -> file
        endif
    endunless;

    if issymbol(name) then
        vedinput(
            procedure();
                '-x ' <> symbol_string(name) -> vedargument;
                chain(ved_f)
            endprocedure)
    endif;

    if vedinvedprocess then
        vedinput(vededit(% file %))
    else
        apply_in_pop11_environment(file, vededit)
    endif
enddefine;


/* <ENTER> DMR */

global vars pop_show_code = false;


define global ved_dmr();
    dlocal pop_show_code = false, vedlmr_print_in_file;
    useslib("showcode");
    if vedlmr_print_in_file == true then
        systmpfile(false, 'dmr_output_', '') -> vedlmr_print_in_file
    endif;
    true -> pop_show_code;
    if vvedmarkhi == 0 then
        ved_lcp()
    else
        ved_lmr()
    endif
enddefine;


/* Macro expanding current form */

define lconstant Vedout() with_nargs 1;
    if isinteger(dup()) then vedcharinsert() else vedinsertstring() endif
enddefine;

constant ved_io = make_stream(vedrepeater, Vedout);


define ved_lisp_read(gbl);
    dlocal vedline, vedcolumn, vvedlinesize;
    if vedargument = nullstring then
        if gbl then
            ved_gbl()
        endif;
        @CLEAR-INPUT(ved_io, 1) ->;
        @READ(ved_io, 1)
    else
        @READ(@MAKE-STRING-INPUT-STREAM(vedargument, 1), 1)
    endif
enddefine;


define ved_lisp_print(form);
    lvars startfile;
    dlocal vedbreak = false, print_right_margin = vedlinemax;

    if vedlmr_print_in_file then
        false -> startfile;
        if isstring(vedlmr_print_in_file) then
            ved_current_file -> startfile;
            vededit(vedlmr_print_in_file, false);
            vedendfile()
        endif;
        false -> vedbreak;
        @PPRINT(form, ved_io, 2);
        if startfile then
            vededit(startfile)
        endif
    else
        @PPRINT(form, 1)
    endif
enddefine;


define global ved_mx();
    lvars form, flag;
    @MACROEXPAND-1(ved_lisp_read(true), 1) -> (form, flag);
    if flag == nil then
        vederror('Not a macro')
    else
        ved_lisp_print(form)
    endif
enddefine;


define global ved_cmx();
    lvars form, flag;
    @COMPILER-MACROEXPAND-1(ved_lisp_read(true), 1) -> (form, flag);
    if flag == nil then
        vederror('Not a compiler macro')
    else
        ved_lisp_print(form)
    endif
enddefine;


/* Invoking ED from the command line */

define global ved_ed();
    @ED(item_or_front(ved_lisp_read(false)), 1)
enddefine;



endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Dec  6 1995
        Uses vedsetwiggle instead of ved_wiggle.
--- John Williams, Aug 11 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  8 1995
        Added ved_mx, ved_cmx, and ved_ed.
--- John Williams, Jun  1 1995
        disassemble now defined in C.all/lisp/src/compile.p.
--- John Williams, Apr 29 1994
        ed bug fix.
--- John Williams, Apr 26 1994
        Enhancements to ed.
--- John Williams, Jul 23 1993
        Fixed bug in Ved runtime action for vedchartype.
--- John Williams, Jul 12 1993
        No longer uses cons_with.
--- John Williams, Mar 19 1993
        Now uses ved_runtime_action define form.
--- John Williams, Jun 17 1992
        Replaced iscaller(vedprocess) with -vedinvedprocess-
--- John Williams, Apr  9 1992
        Removed key binding for <ESC> .  (clashes with -vednewkeys-)
--- John Williams, Apr  9 1992
        Added key binding for <ESC> )
--- John Williams, Nov 26 1990
        -ed- now works first time, and when called from -ved_lmr-
--- John Williams, Jul 17 1990
        Revised for new LIB SUBSYSTEM
 */
